#include "CodeGenLLVM.H"

#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

// This file translates the already type-checked BNFC AST into textual LLVM IR.
// It does not perform semantic validation; failed assumptions are guarded with
// asserts because TypeChecker is expected to reject invalid programs earlier.

namespace {

// Convert a Javalette string literal into LLVM's c"..." byte-string syntax.
// outLen is the number of bytes in the emitted global, including the final NUL.
std::string escapeForC(const std::string &s, int &outLen) {
  std::ostringstream o;
  o << "c\"";
  outLen = 0;
  for (unsigned char ch : s) {
    switch (ch) {
    case '\n':
      o << "\\0A";
      break;
    case '\t':
      o << "\\09";
      break;
    case '"':
      o << "\\22";
      break;
    case '\\':
      o << "\\5C";
      break;
    default:
      if (ch >= 32 && ch <= 126) {
        o << static_cast<char>(ch);
      } else {
        static const char *hex = "0123456789ABCDEF";
        o << '\\' << hex[(ch >> 4) & 0xF] << hex[ch & 0xF];
      }
    }
    outLen += 1;
  }
  o << "\\00\"";
  outLen += 1;
  return o.str();
}

// Compact code-generation type model. arrayDepth records how many layers of
// Javalette arrays wrap a scalar base type; for example int[][] is INT,
// depth 2.
enum class CGBase { INT, DOUBLE, BOOL, VOID, STRING };

struct CGType {
  CGBase base = CGBase::VOID;
  int arrayDepth = 0;

  bool operator==(const CGType &other) const {
    return base == other.base && arrayDepth == other.arrayDepth;
  }
};

// Drop one array layer. Callers only use this when arrayDepth > 0.
CGType elementType(const CGType &t) { return {t.base, t.arrayDepth - 1}; }

// Used to build stable names for generated LLVM array struct types and globals.
std::string scalarSuffix(CGBase base) {
  switch (base) {
  case CGBase::INT:
    return "int";
  case CGBase::DOUBLE:
    return "double";
  case CGBase::BOOL:
    return "bool";
  case CGBase::VOID:
    return "void";
  case CGBase::STRING:
    return "string";
  }
  return "unknown";
}

// Result of generating an expression.
struct RVal {
  std::string v;   // LLVM value name / literal
  CGType ty;       // generated value type
  std::string blk; // records the block that produced the value
};

// Local variable information: the alloca slot holding the value, plus its type.
struct VarInfo {
  std::string alloca;
  CGType ty;
};

// L-values are represented as typed pointers so assignment and ++/-- can store
// through either a plain variable slot or an array-element pointer.
struct LRef {
  std::string ptr;
  CGType ty;
};

// Per-function state. LLVM temporary names and labels only need to be unique
// within one function, while scopes model Javalette block-local variables.
struct FnCtx {
  std::ostringstream body;
  std::vector<std::map<std::string, VarInfo>> scopes;
  CGType retTy;
  int tmp = 0;
  int lab = 0;
  bool blockTerminated = false;

  std::string t() { return "%t" + std::to_string(tmp++); }
  std::string L() { return "L" + std::to_string(lab++); }
  void emit(const std::string &s) {
    // Statement generation sets blockTerminated when the current sequential
    // path has ended, for example after ret, so later statements do not append
    // dead instructions to that path.
    if (!blockTerminated) {
      body << s << "\n";
    }
  }
};

struct CodeGen {
  // Output is accumulated in separate streams so type declarations and globals
  // can be printed before function bodies, even if they are discovered later.
  std::ostringstream prelude;
  std::ostringstream globals;
  std::ostringstream funs;

  // String literals are interned into private LLVM globals and reused when the
  // same literal occurs more than once.
  std::map<std::string, std::string> strSym;
  int strCount = 0;

  // Function return types and generated array artifacts are collected before
  // function bodies are emitted.
  std::map<std::string, CGType> fnSigs;
  std::set<std::string> emittedArrayTypes;
  std::set<std::string> emittedEmptyArrays;

  // Current function context. It is non-null only while emitFunction is active.
  FnCtx *F = nullptr;

  // Convert a BNFC Type node to the smaller type representation used by LLVM
  // generation. Arrays are flattened into base type + depth.
  CGType astType(Type *ty) const {
    if (dynamic_cast<Int *>(ty)) {
      return {CGBase::INT, 0};
    }
    if (dynamic_cast<Doub *>(ty)) {
      return {CGBase::DOUBLE, 0};
    }
    if (dynamic_cast<Bool *>(ty)) {
      return {CGBase::BOOL, 0};
    }
    if (dynamic_cast<Void *>(ty)) {
      return {CGBase::VOID, 0};
    }
    if (auto *arr = dynamic_cast<Arr *>(ty)) {
      CGType inner = astType(arr->type_);
      return {inner.base, inner.arrayDepth + 1};
    }
    assert(false && "unknown type");
    return {};
  }

  // new T[...][...] stores only the scalar base type in the AST, so this helper
  // translates that base-type node before array dimensions are added.
  CGType astBaseType(::BaseType *ty) const {
    if (dynamic_cast<IntBase *>(ty)) {
      return {CGBase::INT, 0};
    }
    if (dynamic_cast<DoubBase *>(ty)) {
      return {CGBase::DOUBLE, 0};
    }
    if (dynamic_cast<BoolBase *>(ty)) {
      return {CGBase::BOOL, 0};
    }
    assert(false && "unknown base type");
    return {};
  }

  // Javalette arrays are emitted as pointers to named structs:
  //   %arr_int_1 = type { i32, [0 x i32] }
  // The first field is length; the zero-length trailing array is the element
  // storage addressed with getelementptr.
  std::string arrayStructName(CGType t) const {
    assert(t.arrayDepth > 0);
    return "%arr_" + scalarSuffix(t.base) + "_" + std::to_string(t.arrayDepth);
  }

  // Map the codegen type model to LLVM textual types. Array values are pointers
  // to the generated array structs; strings are i8* runtime pointers.
  std::string llvmType(CGType t) const {
    if (t.arrayDepth > 0) {
      return arrayStructName(t) + "*";
    }
    switch (t.base) {
    case CGBase::INT:
      return "i32";
    case CGBase::DOUBLE:
      return "double";
    case CGBase::BOOL:
      return "i1";
    case CGBase::VOID:
      return "void";
    case CGBase::STRING:
      return "i8*";
    }
    assert(false && "unreachable");
    return "void";
  }

  // Global zero-initialized array object used as the default value for array
  // variables without explicit initialization.
  std::string emptyArrayGlobal(CGType t) const {
    assert(t.arrayDepth > 0);
    return "@.emptyarr." + scalarSuffix(t.base) + "." +
           std::to_string(t.arrayDepth);
  }

  // Lazily emit LLVM declarations needed for a particular array type. This also
  // recursively emits element array types for multidimensional arrays.
  void ensureArrayArtifacts(CGType t) {
    if (t.arrayDepth == 0) {
      return;
    }
    ensureArrayArtifacts(elementType(t));
    std::string typeName = arrayStructName(t);
    if (!emittedArrayTypes.count(typeName)) {
      emittedArrayTypes.insert(typeName);
      prelude << typeName << " = type { i32, [0 x " << llvmType(elementType(t))
              << "] }\n";
    }
    std::string emptyName = emptyArrayGlobal(t);
    if (!emittedEmptyArrays.count(emptyName)) {
      emittedEmptyArrays.insert(emptyName);
      globals << emptyName << " = private global " << arrayStructName(t)
              << " zeroinitializer\n";
    }
  }

  // Look up a local variable from innermost block scope outward.
  VarInfo *lookup(const std::string &name) {
    size_t i = F->scopes.size();
    while (i-- > 0) {
      auto it = F->scopes[i].find(name);
      if (it != F->scopes[i].end()) {
        return &it->second;
      }
    }
    return nullptr;
  }

  void pushScope() { F->scopes.emplace_back(); }
  void popScope() { F->scopes.pop_back(); }
  void bind(const std::string &name, const VarInfo &vi) {
    F->scopes.back()[name] = vi;
  }

  // Create or reuse the LLVM global backing a string literal.
  std::string internString(const std::string &lit) {
    auto it = strSym.find(lit);
    if (it != strSym.end()) {
      return it->second;
    }
    std::string g = "@.str." + std::to_string(strCount++);
    int len = 0;
    std::string enc = escapeForC(lit, len);
    globals << g << " = private constant [" << len << " x i8] " << enc << "\n";
    strSym[lit] = g;
    return g;
  }

  // Runtime support functions supplied by lib/runtime.* plus calloc for arrays.
  void emitPrelude() {
    prelude << "declare void @printInt(i32)\n";
    prelude << "declare void @printDouble(double)\n";
    prelude << "declare void @printString(i8*)\n";
    prelude << "declare i32 @readInt()\n";
    prelude << "declare double @readDouble()\n";
    prelude << "declare i8* @calloc(i64, i64)\n";
  }

  // First pass over a function definition. Bodies are not emitted here; we only
  // record function result types and make sure parameter/return array types
  // have corresponding LLVM struct declarations.
  void collectSig(FnDef *f) {
    fnSigs[f->ident_] = astType(f->type_);
    ensureArrayArtifacts(fnSigs[f->ident_]);
    std::vector<CGType> params;
    if (f->listarg_) {
      for (Arg *a : *f->listarg_) {
        auto *arg = dynamic_cast<Argument *>(a);
        CGType ty = astType(arg->type_);
        ensureArrayArtifacts(ty);
        params.push_back(ty);
      }
    }
  }

  // Allocate a new Javalette array. For multidimensional arrays this
  // recursively creates each nested sub-array and stores it into the outer
  // array elements.
  RVal genNewArray(CGBase base, const std::vector<RVal> &dims,
                   std::size_t idx) {
    CGType arrTy{base, static_cast<int>(dims.size() - idx)};
    CGType elemTy = elementType(arrTy);
    ensureArrayArtifacts(arrTy);

    const RVal &len = dims[idx];
    std::string sizePtr = F->t();
    // Use a null getelementptr into the trailing [0 x elem] field to compute
    // the byte size needed for length + len elements, then pass that to calloc.
    F->emit("  " + sizePtr + " = getelementptr " + arrayStructName(arrTy) +
            ", " + arrayStructName(arrTy) + "* null, i32 0, i32 1, i32 " +
            len.v);
    std::string size = F->t();
    F->emit("  " + size + " = ptrtoint " + llvmType(elemTy) + "* " + sizePtr +
            " to i64");
    std::string raw = F->t();
    F->emit("  " + raw + " = call i8* @calloc(i64 1, i64 " + size + ")");
    std::string arr = F->t();
    F->emit("  " + arr + " = bitcast i8* " + raw + " to " + llvmType(arrTy));
    std::string lenPtr = F->t();
    F->emit("  " + lenPtr + " = getelementptr " + arrayStructName(arrTy) +
            ", " + llvmType(arrTy) + " " + arr + ", i32 0, i32 0");
    F->emit("  store i32 " + len.v + ", i32* " + lenPtr);

    // For arrays of arrays, fill every outer slot with a freshly allocated
    // inner array so arr[i] is valid immediately after construction.
    if (arrTy.arrayDepth > 1) {
      std::string idxSlot = F->t();
      F->emit("  " + idxSlot + " = alloca i32");
      F->emit("  store i32 0, i32* " + idxSlot);

      std::string lcond = F->L(), lbody = F->L(), lend = F->L();
      F->emit("  br label %" + lcond);
      F->emit(lcond + ":");
      std::string i = F->t();
      F->emit("  " + i + " = load i32, i32* " + idxSlot);
      std::string cond = F->t();
      F->emit("  " + cond + " = icmp slt i32 " + i + ", " + len.v);
      F->emit("  br i1 " + cond + ", label %" + lbody + ", label %" + lend);

      F->emit(lbody + ":");
      RVal inner = genNewArray(base, dims, idx + 1);
      std::string elemPtr = F->t();
      F->emit("  " + elemPtr + " = getelementptr " + arrayStructName(arrTy) +
              ", " + llvmType(arrTy) + " " + arr + ", i32 0, i32 1, i32 " + i);
      F->emit("  store " + llvmType(elemTy) + " " + inner.v + ", " +
              llvmType(elemTy) + "* " + elemPtr);
      std::string next = F->t();
      F->emit("  " + next + " = add i32 " + i + ", 1");
      F->emit("  store i32 " + next + ", i32* " + idxSlot);
      F->emit("  br label %" + lcond);
      F->emit(lend + ":");
    }

    return {arr, arrTy, ""};
  }

  // Generate a pointer to array[index]. The pointer is kept as an LRef so it
  // can either be loaded for expression use or stored through for assignment.
  LRef genIndexRef(Expr *arrayExpr, Expr *indexExpr) {
    RVal arr = genExpr(arrayExpr);
    assert(arr.ty.arrayDepth > 0);
    RVal idx = genExpr(indexExpr);
    assert((idx.ty == CGType{CGBase::INT, 0}));

    std::string ptr = F->t();
    F->emit("  " + ptr + " = getelementptr " + arrayStructName(arr.ty) + ", " +
            llvmType(arr.ty) + " " + arr.v + ", i32 0, i32 1, i32 " + idx.v);
    return {ptr, elementType(arr.ty)};
  }

  // Convert any left-hand-side AST node into a typed storage location.
  LRef genLhs(Lhs *lhs) {
    if (auto *lv = dynamic_cast<LhsVar *>(lhs)) {
      VarInfo *vi = lookup(lv->ident_);
      assert(vi);
      return {vi->alloca, vi->ty};
    }
    if (auto *li = dynamic_cast<LhsIndex *>(lhs)) {
      return genIndexRef(li->expr_1, li->expr_2);
    }
    assert(false && "unsupported lhs");
    return {};
  }

  // Common load helper for variables and array elements.
  RVal loadFromPtr(const LRef &ref) {
    std::string tmp = F->t();
    F->emit("  " + tmp + " = load " + llvmType(ref.ty) + ", " +
            llvmType(ref.ty) + "* " + ref.ptr);
    return {tmp, ref.ty, ""};
  }

  // Recursively generate code for an expression and return the LLVM value that
  // holds the result. The type checker guarantees that each dynamic_cast branch
  // is used with operands of a compatible type.
  RVal genExpr(Expr *e) {
    // Literal constants can be emitted directly as LLVM immediate values.
    if (dynamic_cast<ELitTrue *>(e)) {
      return {"1", {CGBase::BOOL, 0}, ""};
    }
    if (dynamic_cast<ELitFalse *>(e)) {
      return {"0", {CGBase::BOOL, 0}, ""};
    }
    if (auto *lit = dynamic_cast<ELitInt *>(e)) {
      return {std::to_string(lit->integer_), {CGBase::INT, 0}, ""};
    }
    if (auto *lit = dynamic_cast<ELitDoub *>(e)) {
      std::ostringstream o;
      o.setf(std::ios::fixed);
      o << lit->double_;
      return {o.str(), {CGBase::DOUBLE, 0}, ""};
    }
    if (auto *var = dynamic_cast<EVar *>(e)) {
      VarInfo *vi = lookup(var->ident_);
      assert(vi);
      return loadFromPtr({vi->alloca, vi->ty});
    }
    // Strings are globals; the expression value is an i8* pointer to byte 0.
    if (auto *str = dynamic_cast<EString *>(e)) {
      std::string g = internString(str->string_);
      int len = 0;
      (void)escapeForC(str->string_, len);
      std::string p = F->t();
      F->emit("  " + p + " = getelementptr [" + std::to_string(len) +
              " x i8], [" + std::to_string(len) + " x i8]* " + g +
              ", i32 0, i32 0");
      return {p, {CGBase::STRING, 0}, ""};
    }
    // new base[d1][d2] first evaluates the dimension expressions, then lets
    // genNewArray allocate and initialize the nested array structure.
    if (auto *nw = dynamic_cast<ENew *>(e)) {
      CGType elemTy = astBaseType(nw->basetype_);
      std::vector<RVal> dims;
      for (ArrSize *sz : *nw->listarrsize_) {
        auto *dim = dynamic_cast<NewDim *>(sz);
        dims.push_back(genExpr(dim->expr_));
      }
      return genNewArray(elemTy.base, dims, 0);
    }
    // Reading arr[i] is just loading from the same pointer form used by
    // assignment to arr[i].
    if (auto *idx = dynamic_cast<EIndex *>(e)) {
      return loadFromPtr(genIndexRef(idx->expr_1, idx->expr_2));
    }
    // arr.length is stored in field 0 of every generated array struct.
    if (auto *len = dynamic_cast<ELength *>(e)) {
      assert(len->ident_ == "length");
      RVal arr = genExpr(len->expr_);
      assert(arr.ty.arrayDepth > 0);
      std::string ptr = F->t();
      F->emit("  " + ptr + " = getelementptr " + arrayStructName(arr.ty) +
              ", " + llvmType(arr.ty) + " " + arr.v + ", i32 0, i32 0");
      std::string out = F->t();
      F->emit("  " + out + " = load i32, i32* " + ptr);
      return {out, {CGBase::INT, 0}, ""};
    }
    // Numeric unary minus maps to integer sub or floating-point fsub.
    if (auto *neg = dynamic_cast<Neg *>(e)) {
      RVal v = genExpr(neg->expr_);
      std::string r = F->t();
      if (v.ty.base == CGBase::INT) {
        F->emit("  " + r + " = sub i32 0, " + v.v);
      } else {
        F->emit("  " + r + " = fsub double 0.0, " + v.v);
      }
      return {r, v.ty, ""};
    }
    // Boolean not flips the single i1 bit.
    if (auto *nt = dynamic_cast<Not *>(e)) {
      RVal v = genExpr(nt->expr_);
      std::string r = F->t();
      F->emit("  " + r + " = xor i1 " + v.v + ", 1");
      return {r, {CGBase::BOOL, 0}, ""};
    }
    // Addition/subtraction use different LLVM opcodes for ints and doubles.
    if (auto *add = dynamic_cast<EAdd *>(e)) {
      RVal l = genExpr(add->expr_1);
      RVal r = genExpr(add->expr_2);
      std::string dst = F->t();
      if (l.ty.base == CGBase::INT) {
        F->emit("  " + dst + " = " +
                std::string(dynamic_cast<Plus *>(add->addop_) ? "add" : "sub") +
                " i32 " + l.v + ", " + r.v);
      } else {
        F->emit(
            "  " + dst + " = " +
            std::string(dynamic_cast<Plus *>(add->addop_) ? "fadd" : "fsub") +
            " double " + l.v + ", " + r.v);
      }
      return {dst, l.ty, ""};
    }
    // Multiplication/division/modulo. Javalette only has modulo for integers.
    if (auto *mul = dynamic_cast<EMul *>(e)) {
      RVal l = genExpr(mul->expr_1);
      RVal r = genExpr(mul->expr_2);
      std::string dst = F->t();
      if (l.ty.base == CGBase::INT) {
        if (dynamic_cast<Times *>(mul->mulop_)) {
          F->emit("  " + dst + " = mul i32 " + l.v + ", " + r.v);
        } else if (dynamic_cast<Div *>(mul->mulop_)) {
          F->emit("  " + dst + " = sdiv i32 " + l.v + ", " + r.v);
        } else {
          F->emit("  " + dst + " = srem i32 " + l.v + ", " + r.v);
        }
      } else {
        F->emit(
            "  " + dst + " = " +
            std::string(dynamic_cast<Times *>(mul->mulop_) ? "fmul" : "fdiv") +
            " double " + l.v + ", " + r.v);
      }
      return {dst, l.ty, ""};
    }
    // Relational operators produce i1. Doubles use ordered floating compares;
    // integers, booleans, and array/string pointer equality use icmp.
    if (auto *rel = dynamic_cast<ERel *>(e)) {
      RVal l = genExpr(rel->expr_1);
      RVal r = genExpr(rel->expr_2);
      std::string dst = F->t();
      if (dynamic_cast<EQU *>(rel->relop_)) {
        if (l.ty.base == CGBase::DOUBLE) {
          F->emit("  " + dst + " = fcmp oeq double " + l.v + ", " + r.v);
        } else {
          F->emit("  " + dst + " = icmp eq " + llvmType(l.ty) + " " + l.v +
                  ", " + r.v);
        }
      } else if (dynamic_cast<NE *>(rel->relop_)) {
        if (l.ty.base == CGBase::DOUBLE) {
          F->emit("  " + dst + " = fcmp one double " + l.v + ", " + r.v);
        } else {
          F->emit("  " + dst + " = icmp ne " + llvmType(l.ty) + " " + l.v +
                  ", " + r.v);
        }
      } else if (dynamic_cast<LTH *>(rel->relop_)) {
        if (l.ty.base == CGBase::DOUBLE) {
          F->emit("  " + dst + " = fcmp olt double " + l.v + ", " + r.v);
        } else {
          F->emit("  " + dst + " = icmp slt i32 " + l.v + ", " + r.v);
        }
      } else if (dynamic_cast<LE *>(rel->relop_)) {
        if (l.ty.base == CGBase::DOUBLE) {
          F->emit("  " + dst + " = fcmp ole double " + l.v + ", " + r.v);
        } else {
          F->emit("  " + dst + " = icmp sle i32 " + l.v + ", " + r.v);
        }
      } else if (dynamic_cast<GTH *>(rel->relop_)) {
        if (l.ty.base == CGBase::DOUBLE) {
          F->emit("  " + dst + " = fcmp ogt double " + l.v + ", " + r.v);
        } else {
          F->emit("  " + dst + " = icmp sgt i32 " + l.v + ", " + r.v);
        }
      } else {
        if (l.ty.base == CGBase::DOUBLE) {
          F->emit("  " + dst + " = fcmp oge double " + l.v + ", " + r.v);
        } else {
          F->emit("  " + dst + " = icmp sge i32 " + l.v + ", " + r.v);
        }
      }
      return {dst, {CGBase::BOOL, 0}, ""};
    }
    // Short-circuit &&: evaluate the RHS only if the LHS is true, then merge
    // the false path and RHS value with a phi node.
    if (auto *land = dynamic_cast<EAnd *>(e)) {
      RVal l = genExpr(land->expr_1);
      std::string lrhs = F->L();
      std::string lfalse = F->L();
      std::string ldone = F->L();
      F->emit("  br i1 " + l.v + ", label %" + lrhs + ", label %" + lfalse);
      F->emit(lfalse + ":");
      F->emit("  br label %" + ldone);
      F->emit(lrhs + ":");
      RVal r = genExpr(land->expr_2);
      std::string rblk = r.blk.empty() ? lrhs : r.blk;
      F->emit("  br label %" + ldone);
      F->emit(ldone + ":");
      std::string res = F->t();
      F->emit("  " + res + " = phi i1 [ 0, %" + lfalse + " ], [ " + r.v +
              ", %" + rblk + " ]");
      return {res, {CGBase::BOOL, 0}, ldone};
    }
    // Short-circuit || mirrors &&, but the early path contributes true.
    if (auto *lor = dynamic_cast<EOr *>(e)) {
      RVal l = genExpr(lor->expr_1);
      std::string ltrue = F->L();
      std::string lrhs = F->L();
      std::string ldone = F->L();
      F->emit("  br i1 " + l.v + ", label %" + ltrue + ", label %" + lrhs);
      F->emit(ltrue + ":");
      F->emit("  br label %" + ldone);
      F->emit(lrhs + ":");
      RVal r = genExpr(lor->expr_2);
      std::string rblk = r.blk.empty() ? lrhs : r.blk;
      F->emit("  br label %" + ldone);
      F->emit(ldone + ":");
      std::string res = F->t();
      F->emit("  " + res + " = phi i1 [ 1, %" + ltrue + " ], [ " + r.v + ", %" +
              rblk + " ]");
      return {res, {CGBase::BOOL, 0}, ldone};
    }
    // Calls to built-in runtime functions are emitted directly. User functions
    // use fnSigs for the return type and infer argument types from expressions.
    if (auto *app = dynamic_cast<EApp *>(e)) {
      std::vector<RVal> args;
      if (app->listexpr_) {
        for (Expr *x : *app->listexpr_) {
          args.push_back(genExpr(x));
        }
      }

      if (app->ident_ == "printInt") {
        F->emit("  call void @printInt(i32 " + args[0].v + ")");
        return {"", {CGBase::VOID, 0}, ""};
      }
      if (app->ident_ == "printDouble") {
        F->emit("  call void @printDouble(double " + args[0].v + ")");
        return {"", {CGBase::VOID, 0}, ""};
      }
      if (app->ident_ == "printString") {
        F->emit("  call void @printString(i8* " + args[0].v + ")");
        return {"", {CGBase::VOID, 0}, ""};
      }
      if (app->ident_ == "readInt") {
        std::string r = F->t();
        F->emit("  " + r + " = call i32 @readInt()");
        return {r, {CGBase::INT, 0}, ""};
      }
      if (app->ident_ == "readDouble") {
        std::string r = F->t();
        F->emit("  " + r + " = call double @readDouble()");
        return {r, {CGBase::DOUBLE, 0}, ""};
      }

      std::ostringstream plist;
      for (size_t i = 0; i < args.size(); ++i) {
        if (i) {
          plist << ", ";
        }
        plist << llvmType(args[i].ty) << " " << args[i].v;
      }

      CGType retTy = fnSigs[app->ident_];
      if (retTy == CGType{CGBase::VOID, 0}) {
        F->emit("  call void @" + app->ident_ + "(" + plist.str() + ")");
        return {"", retTy, ""};
      }
      std::string r = F->t();
      F->emit("  " + r + " = call " + llvmType(retTy) + " @" + app->ident_ +
              "(" + plist.str() + ")");
      return {r, retTy, ""};
    }

    assert(false && "unhandled Expr variant");
    return {};
  }

  // Default initialization follows the Javalette runtime model: numbers and
  // booleans become zero/false, while arrays point at a shared empty array.
  void storeDefault(const VarInfo &vi) {
    if (vi.ty.arrayDepth > 0) {
      ensureArrayArtifacts(vi.ty);
      F->emit("  store " + llvmType(vi.ty) + " " + emptyArrayGlobal(vi.ty) +
              ", " + llvmType(vi.ty) + "* " + vi.alloca);
      return;
    }
    if (vi.ty.base == CGBase::INT) {
      F->emit("  store i32 0, i32* " + vi.alloca);
    } else if (vi.ty.base == CGBase::DOUBLE) {
      F->emit("  store double 0.0, double* " + vi.alloca);
    } else if (vi.ty.base == CGBase::BOOL) {
      F->emit("  store i1 0, i1* " + vi.alloca);
    }
  }

  // Generate code for one statement. Each branch handles one BNFC Stmt variant
  // and emits the necessary LLVM control flow and scope management.
  void genStmt(Stmt *s) {
    // Expression statements are evaluated only for side effects, such as calls.
    if (auto *sexp = dynamic_cast<SExp *>(s)) {
      (void)genExpr(sexp->expr_);
      return;
    }
    // Return statements terminate the current LLVM basic block.
    if (auto *ret = dynamic_cast<Ret *>(s)) {
      RVal v = genExpr(ret->expr_);
      F->emit("  ret " + llvmType(v.ty) + " " + v.v);
      F->blockTerminated = true;
      return;
    }
    // Void return has no expression value to emit.
    if (dynamic_cast<VRet *>(s)) {
      F->emit("  ret void");
      F->blockTerminated = true;
      return;
    }
    // Declarations allocate stack slots. Initialized declarations evaluate the
    // initializer before binding the name in the current scope.
    if (auto *decl = dynamic_cast<Decl *>(s)) {
      CGType t = astType(decl->type_);
      ensureArrayArtifacts(t);
      for (Item *it : *decl->listitem_) {
        std::string slot = F->t();
        F->emit("  " + slot + " = alloca " + llvmType(t));
        VarInfo vi{slot, t};
        if (auto *ni = dynamic_cast<NoInit *>(it)) {
          storeDefault(vi);
          bind(ni->ident_, vi);
        } else if (auto *in = dynamic_cast<Init *>(it)) {
          RVal v = genExpr(in->expr_);
          F->emit("  store " + llvmType(t) + " " + v.v + ", " + llvmType(t) +
                  "* " + slot);
          bind(in->ident_, vi);
        }
      }
      return;
    }
    // Assignment stores into either a local alloca or an array-element pointer.
    if (auto *ass = dynamic_cast<Ass *>(s)) {
      LRef ref = genLhs(ass->lhs_);
      RVal v = genExpr(ass->expr_);
      F->emit("  store " + llvmType(ref.ty) + " " + v.v + ", " +
              llvmType(ref.ty) + "* " + ref.ptr);
      return;
    }
    // ++ and -- are supported only on integer l-values after type checking.
    if (auto *inc = dynamic_cast<Incr *>(s)) {
      LRef ref = genLhs(inc->lhs_);
      RVal v = loadFromPtr(ref);
      std::string out = F->t();
      F->emit("  " + out + " = add i32 " + v.v + ", 1");
      F->emit("  store i32 " + out + ", i32* " + ref.ptr);
      return;
    }
    if (auto *dec = dynamic_cast<Decr *>(s)) {
      LRef ref = genLhs(dec->lhs_);
      RVal v = loadFromPtr(ref);
      std::string out = F->t();
      F->emit("  " + out + " = sub i32 " + v.v + ", 1");
      F->emit("  store i32 " + out + ", i32* " + ref.ptr);
      return;
    }
    // if without else jumps over the then block when the condition is false.
    if (auto *cond = dynamic_cast<Cond *>(s)) {
      std::string lt = F->L(), le = F->L();
      RVal c = genExpr(cond->expr_);
      F->emit("  br i1 " + c.v + ", label %" + lt + ", label %" + le);
      F->emit(lt + ":");
      pushScope();
      genStmt(cond->stmt_);
      popScope();
      if (!F->blockTerminated) {
        F->emit("  br label %" + le);
      }
      F->blockTerminated = false;
      F->emit(le + ":");
      return;
    }
    // if/else emits separate then/else labels and only creates a join label if
    // at least one branch can fall through.
    if (auto *cond = dynamic_cast<CondElse *>(s)) {
      std::string lt = F->L(), lf = F->L(), le = F->L();
      RVal c = genExpr(cond->expr_);
      F->emit("  br i1 " + c.v + ", label %" + lt + ", label %" + lf);
      F->emit(lt + ":");
      pushScope();
      genStmt(cond->stmt_1);
      popScope();
      bool thenTerm = F->blockTerminated;
      if (!thenTerm) {
        F->emit("  br label %" + le);
      }
      F->blockTerminated = false;
      F->emit(lf + ":");
      pushScope();
      genStmt(cond->stmt_2);
      popScope();
      bool elseTerm = F->blockTerminated;
      if (!elseTerm) {
        F->emit("  br label %" + le);
      }
      F->blockTerminated = thenTerm && elseTerm;
      if (!F->blockTerminated) {
        F->emit(le + ":");
      }
      return;
    }
    // while loops have explicit condition, body, and exit labels.
    if (auto *wh = dynamic_cast<While *>(s)) {
      std::string lcond = F->L(), lbody = F->L(), lend = F->L();
      F->emit("  br label %" + lcond);
      F->emit(lcond + ":");
      RVal c = genExpr(wh->expr_);
      F->emit("  br i1 " + c.v + ", label %" + lbody + ", label %" + lend);
      F->emit(lbody + ":");
      pushScope();
      genStmt(wh->stmt_);
      popScope();
      if (!F->blockTerminated) {
        F->emit("  br label %" + lcond);
      }
      F->blockTerminated = false;
      F->emit(lend + ":");
      return;
    }
    // foreach lowers to an index loop over the array length. The loop variable
    // is a fresh local slot bound only inside the loop body scope.
    if (auto *fe = dynamic_cast<ForEach *>(s)) {
      CGType itemTy = astType(fe->type_);
      CGType arrTy{itemTy.base, itemTy.arrayDepth + 1};
      ensureArrayArtifacts(arrTy);
      RVal arr = genExpr(fe->expr_);
      std::string idxSlot = F->t();
      F->emit("  " + idxSlot + " = alloca i32");
      F->emit("  store i32 0, i32* " + idxSlot);

      std::string lcond = F->L(), lbody = F->L(), lend = F->L();
      F->emit("  br label %" + lcond);
      F->emit(lcond + ":");
      std::string idx = F->t();
      F->emit("  " + idx + " = load i32, i32* " + idxSlot);
      std::string lenPtr = F->t();
      F->emit("  " + lenPtr + " = getelementptr " + arrayStructName(arrTy) +
              ", " + llvmType(arrTy) + " " + arr.v + ", i32 0, i32 0");
      std::string len = F->t();
      F->emit("  " + len + " = load i32, i32* " + lenPtr);
      std::string cond = F->t();
      F->emit("  " + cond + " = icmp slt i32 " + idx + ", " + len);
      F->emit("  br i1 " + cond + ", label %" + lbody + ", label %" + lend);

      F->emit(lbody + ":");
      pushScope();
      std::string elemPtr = F->t();
      F->emit("  " + elemPtr + " = getelementptr " + arrayStructName(arrTy) +
              ", " + llvmType(arrTy) + " " + arr.v + ", i32 0, i32 1, i32 " +
              idx);
      std::string loopSlot = F->t();
      F->emit("  " + loopSlot + " = alloca " + llvmType(itemTy));
      std::string elem = F->t();
      F->emit("  " + elem + " = load " + llvmType(itemTy) + ", " +
              llvmType(itemTy) + "* " + elemPtr);
      F->emit("  store " + llvmType(itemTy) + " " + elem + ", " +
              llvmType(itemTy) + "* " + loopSlot);
      bind(fe->ident_, {loopSlot, itemTy});
      genStmt(fe->stmt_);
      popScope();
      if (!F->blockTerminated) {
        std::string next = F->t();
        F->emit("  " + next + " = add i32 " + idx + ", 1");
        F->emit("  store i32 " + next + ", i32* " + idxSlot);
        F->emit("  br label %" + lcond);
      }
      F->blockTerminated = false;
      F->emit(lend + ":");
      return;
    }
    // Blocks introduce a lexical scope and then generate their statements in
    // order.
    if (auto *bs = dynamic_cast<BStmt *>(s)) {
      auto *block = dynamic_cast<Block *>(bs->blk_);
      pushScope();
      for (Stmt *x : *block->liststmt_) {
        genStmt(x);
      }
      popScope();
      return;
    }
    // Empty statements emit no LLVM.
    if (dynamic_cast<Empty *>(s)) {
      return;
    }

    assert(false && "unhandled Stmt variant");
  }

  // Emit one LLVM function definition. Parameters arrive as SSA values, then
  // are copied into allocas so the rest of codegen can treat parameters and
  // locals uniformly as mutable variables.
  void emitFunction(FnDef *f) {
    FnCtx local;
    F = &local;
    local.retTy = astType(f->type_);
    ensureArrayArtifacts(local.retTy);
    local.body << "define " << llvmType(local.retTy) << " @" << f->ident_
               << "(";

    std::vector<std::string> formals;
    std::vector<CGType> formalTypes;
    if (f->listarg_) {
      bool first = true;
      for (Arg *a : *f->listarg_) {
        auto *arg = dynamic_cast<Argument *>(a);
        CGType t = astType(arg->type_);
        ensureArrayArtifacts(t);
        if (!first) {
          local.body << ", ";
        }
        first = false;
        local.body << llvmType(t) << " %__p__" << arg->ident_;
        formals.push_back(arg->ident_);
        formalTypes.push_back(t);
      }
    }
    local.body << ") {\nentry:\n";

    // Initial function scope contains all parameters.
    pushScope();
    for (size_t i = 0; i < formals.size(); ++i) {
      std::string slot = local.t();
      std::string ty = llvmType(formalTypes[i]);
      local.emit("  " + slot + " = alloca " + ty);
      local.emit("  store " + ty + " %__p__" + formals[i] + ", " + ty + "* " +
                 slot);
      bind(formals[i], {slot, formalTypes[i]});
    }

    auto *block = dynamic_cast<Block *>(f->blk_);
    for (Stmt *x : *block->liststmt_) {
      genStmt(x);
    }
    // TypeChecker ensures non-void functions return on all paths. For void
    // functions, synthesize the implicit final return if the body falls
    // through.
    if (local.retTy == CGType{CGBase::VOID, 0} && !local.blockTerminated) {
      local.emit("  ret void");
    }
    local.body << "}\n\n";
    funs << local.body.str();
    popScope();
    F = nullptr;
  }

  // Full program generation is two-pass: collect function signatures first so
  // calls can refer to functions declared later, then emit each function body.
  void gen(Program *p, std::ostream &out) {
    emitPrelude();
    for (TopDef *td : *p->listtopdef_) {
      if (auto *fn = dynamic_cast<FnDef *>(td)) {
        collectSig(fn);
      }
    }
    for (TopDef *td : *p->listtopdef_) {
      if (auto *fn = dynamic_cast<FnDef *>(td)) {
        emitFunction(fn);
      }
    }

    // LLVM requires type declarations/globals before uses in function bodies.
    out << prelude.str() << "\n" << globals.str();
    if (strCount || !globals.str().empty()) {
      out << "\n";
    }
    out << funs.str();
  }
};

} // namespace

// Public wrapper used by main.cpp. Keeping CodeGen internal avoids exposing all
// of the backend helper types in the header.
void generateLLVM(Program *prog, std::ostream &out) {
  CodeGen cg;
  cg.gen(prog, out);
}
