#include "CodeGenLLVM.H"

#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace {

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

enum class CGBase { INT, DOUBLE, BOOL, VOID, STRING };

struct CGType {
  CGBase base = CGBase::VOID;
  int arrayDepth = 0;

  bool operator==(const CGType &other) const {
    return base == other.base && arrayDepth == other.arrayDepth;
  }
};

CGType elementType(const CGType &t) { return {t.base, t.arrayDepth - 1}; }

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

struct RVal {
  std::string v;
  CGType ty;
  std::string blk;
};

struct VarInfo {
  std::string alloca;
  CGType ty;
};

struct LRef {
  std::string ptr;
  CGType ty;
};

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
    if (!blockTerminated) {
      body << s << "\n";
    }
  }
};

struct CodeGen {
  std::ostringstream prelude;
  std::ostringstream globals;
  std::ostringstream funs;
  std::map<std::string, std::string> strSym;
  int strCount = 0;

  std::map<std::string, CGType> fnSigs;
  std::map<std::string, std::vector<CGType>> fnParamTys;
  std::set<std::string> emittedArrayTypes;
  std::set<std::string> emittedEmptyArrays;

  FnCtx *F = nullptr;

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

  std::string arrayStructName(CGType t) const {
    assert(t.arrayDepth > 0);
    return "%arr_" + scalarSuffix(t.base) + "_" + std::to_string(t.arrayDepth);
  }

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

  std::string emptyArrayGlobal(CGType t) const {
    assert(t.arrayDepth > 0);
    return "@.emptyarr." + scalarSuffix(t.base) + "." + std::to_string(t.arrayDepth);
  }

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
  void bind(const std::string &name, const VarInfo &vi) { F->scopes.back()[name] = vi; }

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

  void emitPrelude() {
    prelude << "declare void @printInt(i32)\n";
    prelude << "declare void @printDouble(double)\n";
    prelude << "declare void @printString(i8*)\n";
    prelude << "declare i32 @readInt()\n";
    prelude << "declare double @readDouble()\n";
    prelude << "declare i8* @calloc(i64, i64)\n";
  }

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
    fnParamTys[f->ident_] = params;
  }

  RVal genNewArray(CGBase base, const std::vector<RVal> &dims, std::size_t idx) {
    CGType arrTy{base, static_cast<int>(dims.size() - idx)};
    CGType elemTy = elementType(arrTy);
    ensureArrayArtifacts(arrTy);

    const RVal &len = dims[idx];
    std::string sizePtr = F->t();
    F->emit("  " + sizePtr + " = getelementptr " + arrayStructName(arrTy) + ", " +
            arrayStructName(arrTy) + "* null, i32 0, i32 1, i32 " + len.v);
    std::string size = F->t();
    F->emit("  " + size + " = ptrtoint " + llvmType(elemTy) + "* " + sizePtr +
            " to i64");
    std::string raw = F->t();
    F->emit("  " + raw + " = call i8* @calloc(i64 1, i64 " + size + ")");
    std::string arr = F->t();
    F->emit("  " + arr + " = bitcast i8* " + raw + " to " + llvmType(arrTy));
    std::string lenPtr = F->t();
    F->emit("  " + lenPtr + " = getelementptr " + arrayStructName(arrTy) + ", " +
            llvmType(arrTy) + " " + arr + ", i32 0, i32 0");
    F->emit("  store i32 " + len.v + ", i32* " + lenPtr);

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
      F->emit("  " + elemPtr + " = getelementptr " + arrayStructName(arrTy) + ", " +
              llvmType(arrTy) + " " + arr + ", i32 0, i32 1, i32 " + i);
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

  RVal loadFromPtr(const LRef &ref) {
    std::string tmp = F->t();
    F->emit("  " + tmp + " = load " + llvmType(ref.ty) + ", " + llvmType(ref.ty) +
            "* " + ref.ptr);
    return {tmp, ref.ty, ""};
  }

  RVal genExpr(Expr *e) {
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
    if (auto *str = dynamic_cast<EString *>(e)) {
      std::string g = internString(str->string_);
      int len = 0;
      (void)escapeForC(str->string_, len);
      std::string p = F->t();
      F->emit("  " + p + " = getelementptr [" + std::to_string(len) + " x i8], [" +
              std::to_string(len) + " x i8]* " + g + ", i32 0, i32 0");
      return {p, {CGBase::STRING, 0}, ""};
    }
    if (auto *nw = dynamic_cast<ENew *>(e)) {
      CGType elemTy = astBaseType(nw->basetype_);
      std::vector<RVal> dims;
      for (ArrSize *sz : *nw->listarrsize_) {
        auto *dim = dynamic_cast<NewDim *>(sz);
        dims.push_back(genExpr(dim->expr_));
      }
      return genNewArray(elemTy.base, dims, 0);
    }
    if (auto *idx = dynamic_cast<EIndex *>(e)) {
      return loadFromPtr(genIndexRef(idx->expr_1, idx->expr_2));
    }
    if (auto *len = dynamic_cast<ELength *>(e)) {
      assert(len->ident_ == "length");
      RVal arr = genExpr(len->expr_);
      assert(arr.ty.arrayDepth > 0);
      std::string ptr = F->t();
      F->emit("  " + ptr + " = getelementptr " + arrayStructName(arr.ty) + ", " +
              llvmType(arr.ty) + " " + arr.v + ", i32 0, i32 0");
      std::string out = F->t();
      F->emit("  " + out + " = load i32, i32* " + ptr);
      return {out, {CGBase::INT, 0}, ""};
    }
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
    if (auto *nt = dynamic_cast<Not *>(e)) {
      RVal v = genExpr(nt->expr_);
      std::string r = F->t();
      F->emit("  " + r + " = xor i1 " + v.v + ", 1");
      return {r, {CGBase::BOOL, 0}, ""};
    }
    if (auto *add = dynamic_cast<EAdd *>(e)) {
      RVal l = genExpr(add->expr_1);
      RVal r = genExpr(add->expr_2);
      std::string dst = F->t();
      if (l.ty.base == CGBase::INT) {
        F->emit("  " + dst + " = " +
                std::string(dynamic_cast<Plus *>(add->addop_) ? "add" : "sub") +
                " i32 " + l.v + ", " + r.v);
      } else {
        F->emit("  " + dst + " = " +
                std::string(dynamic_cast<Plus *>(add->addop_) ? "fadd" : "fsub") +
                " double " + l.v + ", " + r.v);
      }
      return {dst, l.ty, ""};
    }
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
        F->emit("  " + dst + " = " +
                std::string(dynamic_cast<Times *>(mul->mulop_) ? "fmul" : "fdiv") +
                " double " + l.v + ", " + r.v);
      }
      return {dst, l.ty, ""};
    }
    if (auto *rel = dynamic_cast<ERel *>(e)) {
      RVal l = genExpr(rel->expr_1);
      RVal r = genExpr(rel->expr_2);
      std::string dst = F->t();
      if (dynamic_cast<EQU *>(rel->relop_)) {
        if (l.ty.base == CGBase::DOUBLE) {
          F->emit("  " + dst + " = fcmp oeq double " + l.v + ", " + r.v);
        } else {
          F->emit("  " + dst + " = icmp eq " + llvmType(l.ty) + " " + l.v + ", " +
                  r.v);
        }
      } else if (dynamic_cast<NE *>(rel->relop_)) {
        if (l.ty.base == CGBase::DOUBLE) {
          F->emit("  " + dst + " = fcmp one double " + l.v + ", " + r.v);
        } else {
          F->emit("  " + dst + " = icmp ne " + llvmType(l.ty) + " " + l.v + ", " +
                  r.v);
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
      F->emit("  " + res + " = phi i1 [ 0, %" + lfalse + " ], [ " + r.v + ", %" +
              rblk + " ]");
      return {res, {CGBase::BOOL, 0}, ldone};
    }
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
      F->emit("  " + r + " = call " + llvmType(retTy) + " @" + app->ident_ + "(" +
              plist.str() + ")");
      return {r, retTy, ""};
    }

    assert(false && "unhandled Expr variant");
    return {};
  }

  void storeDefault(const VarInfo &vi) {
    if (vi.ty.arrayDepth > 0) {
      ensureArrayArtifacts(vi.ty);
      F->emit("  store " + llvmType(vi.ty) + " " + emptyArrayGlobal(vi.ty) + ", " +
              llvmType(vi.ty) + "* " + vi.alloca);
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

  void genStmt(Stmt *s) {
    if (auto *sexp = dynamic_cast<SExp *>(s)) {
      (void)genExpr(sexp->expr_);
      return;
    }
    if (auto *ret = dynamic_cast<Ret *>(s)) {
      RVal v = genExpr(ret->expr_);
      F->emit("  ret " + llvmType(v.ty) + " " + v.v);
      F->blockTerminated = true;
      return;
    }
    if (dynamic_cast<VRet *>(s)) {
      F->emit("  ret void");
      F->blockTerminated = true;
      return;
    }
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
    if (auto *ass = dynamic_cast<Ass *>(s)) {
      LRef ref = genLhs(ass->lhs_);
      RVal v = genExpr(ass->expr_);
      F->emit("  store " + llvmType(ref.ty) + " " + v.v + ", " + llvmType(ref.ty) +
              "* " + ref.ptr);
      return;
    }
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
      F->emit("  " + lenPtr + " = getelementptr " + arrayStructName(arrTy) + ", " +
              llvmType(arrTy) + " " + arr.v + ", i32 0, i32 0");
      std::string len = F->t();
      F->emit("  " + len + " = load i32, i32* " + lenPtr);
      std::string cond = F->t();
      F->emit("  " + cond + " = icmp slt i32 " + idx + ", " + len);
      F->emit("  br i1 " + cond + ", label %" + lbody + ", label %" + lend);

      F->emit(lbody + ":");
      pushScope();
      std::string elemPtr = F->t();
      F->emit("  " + elemPtr + " = getelementptr " + arrayStructName(arrTy) + ", " +
              llvmType(arrTy) + " " + arr.v + ", i32 0, i32 1, i32 " + idx);
      std::string loopSlot = F->t();
      F->emit("  " + loopSlot + " = alloca " + llvmType(itemTy));
      std::string elem = F->t();
      F->emit("  " + elem + " = load " + llvmType(itemTy) + ", " + llvmType(itemTy) +
              "* " + elemPtr);
      F->emit("  store " + llvmType(itemTy) + " " + elem + ", " + llvmType(itemTy) +
              "* " + loopSlot);
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
    if (auto *bs = dynamic_cast<BStmt *>(s)) {
      auto *block = dynamic_cast<Block *>(bs->blk_);
      pushScope();
      for (Stmt *x : *block->liststmt_) {
        genStmt(x);
      }
      popScope();
      return;
    }
    if (dynamic_cast<Empty *>(s)) {
      return;
    }

    assert(false && "unhandled Stmt variant");
  }

  void emitFunction(FnDef *f) {
    FnCtx local;
    F = &local;
    local.retTy = astType(f->type_);
    ensureArrayArtifacts(local.retTy);
    local.body << "define " << llvmType(local.retTy) << " @" << f->ident_ << "(";

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

    pushScope();
    for (size_t i = 0; i < formals.size(); ++i) {
      std::string slot = local.t();
      std::string ty = llvmType(formalTypes[i]);
      local.emit("  " + slot + " = alloca " + ty);
      local.emit("  store " + ty + " %__p__" + formals[i] + ", " + ty + "* " + slot);
      bind(formals[i], {slot, formalTypes[i]});
    }

    auto *block = dynamic_cast<Block *>(f->blk_);
    for (Stmt *x : *block->liststmt_) {
      genStmt(x);
    }
    if (local.retTy == CGType{CGBase::VOID, 0} && !local.blockTerminated) {
      local.emit("  ret void");
    }
    local.body << "}\n\n";
    funs << local.body.str();
    popScope();
    F = nullptr;
  }

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

    out << prelude.str() << "\n" << globals.str();
    if (strCount || !globals.str().empty()) {
      out << "\n";
    }
    out << funs.str();
  }
};

} // namespace

void generateLLVM(Program *prog, std::ostream &out) {
  CodeGen cg;
  cg.gen(prog, out);
}
