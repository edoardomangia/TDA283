#include "CodeGenRISCV.H"

#include <cassert>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace {

enum class CGBase { INT, DOUBLE, BOOL, VOID, STRING };

struct CGType {
  CGBase base = CGBase::VOID;
  int arrayDepth = 0;

  bool operator==(const CGType &other) const {
    return base == other.base && arrayDepth == other.arrayDepth;
  }
  bool operator!=(const CGType &other) const { return !(*this == other); }
  bool isDouble() const { return arrayDepth == 0 && base == CGBase::DOUBLE; }
  bool isIntLike() const {
    return arrayDepth == 0 && (base == CGBase::INT || base == CGBase::BOOL);
  }
  bool isPtrLike() const {
    return arrayDepth > 0 || base == CGBase::STRING;
  }
};

CGType elementType(const CGType &t) {
  return {t.base, t.arrayDepth - 1};
}

int alignUp(int value, int align) {
  return ((value + align - 1) / align) * align;
}

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

std::string escapeAsmString(const std::string &s) {
  std::ostringstream out;
  for (unsigned char ch : s) {
    switch (ch) {
    case '\\':
      out << "\\\\";
      break;
    case '"':
      out << "\\\"";
      break;
    case '\n':
      out << "\\n";
      break;
    case '\t':
      out << "\\t";
      break;
    default:
      if (ch >= 32 && ch <= 126) {
        out << static_cast<char>(ch);
      } else {
        out << "\\";
        static const char *oct = "01234567";
        out << oct[(ch >> 6) & 7] << oct[(ch >> 3) & 7] << oct[ch & 7];
      }
    }
  }
  return out.str();
}

struct VarInfo {
  int offset = 0;
  CGType ty;
};

struct LRef {
  bool isStack = false;
  int offset = 0;
  std::string addrReg;
  CGType ty;
};

struct FnCtx {
  std::ostringstream body;
  std::vector<std::map<std::string, VarInfo>> scopes;
  CGType retTy;
  std::string name;
  std::string epilogueLabel;
  int label = 0;
  int stackUsed = 16;

  std::string L(const std::string &prefix = ".L") {
    return prefix + name + "_" + std::to_string(label++);
  }

  int allocRaw(int size, int align) {
    stackUsed = alignUp(stackUsed, align);
    stackUsed += size;
    return stackUsed;
  }

  int frameSize() const { return alignUp(stackUsed, 16); }

  void emit(const std::string &s) { body << s << '\n'; }
};

struct CodeGen {
  std::ostringstream rodata;
  std::ostringstream data;
  std::ostringstream text;
  std::map<std::string, std::string> strLabels;
  std::map<std::uint64_t, std::string> doubleLabels;
  std::set<std::string> emptyArrays;
  int strCount = 0;
  int dblCount = 0;

  std::map<std::string, CGType> fnSigs;
  std::map<std::string, std::vector<CGType>> fnParamTys;

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

  int sizeOf(CGType ty) const {
    if (ty.arrayDepth > 0 || ty.base == CGBase::STRING || ty.base == CGBase::DOUBLE) {
      return 8;
    }
    return 4;
  }

  int alignOf(CGType ty) const {
    if (ty.arrayDepth > 0 || ty.base == CGBase::STRING || ty.base == CGBase::DOUBLE) {
      return 8;
    }
    return 4;
  }

  bool isDouble(CGType ty) const { return ty.isDouble(); }

  void pushScope() { F->scopes.emplace_back(); }
  void popScope() { F->scopes.pop_back(); }

  VarInfo *lookup(const std::string &name) {
    for (auto it = F->scopes.rbegin(); it != F->scopes.rend(); ++it) {
      auto found = it->find(name);
      if (found != it->end()) {
        return &found->second;
      }
    }
    return nullptr;
  }

  void bind(const std::string &name, const VarInfo &vi) {
    F->scopes.back()[name] = vi;
  }

  int allocSlot(CGType ty) { return F->allocRaw(sizeOf(ty), alignOf(ty)); }
  int allocIntTemp() { return F->allocRaw(8, 8); }

  void emitLoadStack(CGType ty, const std::string &reg, int offset) {
    if (ty.isDouble()) {
      F->emit("  fld " + reg + ", -" + std::to_string(offset) + "(s0)");
    } else if (sizeOf(ty) == 8) {
      F->emit("  ld " + reg + ", -" + std::to_string(offset) + "(s0)");
    } else {
      F->emit("  lw " + reg + ", -" + std::to_string(offset) + "(s0)");
    }
  }

  void emitStoreStack(CGType ty, const std::string &reg, int offset) {
    if (ty.isDouble()) {
      F->emit("  fsd " + reg + ", -" + std::to_string(offset) + "(s0)");
    } else if (sizeOf(ty) == 8) {
      F->emit("  sd " + reg + ", -" + std::to_string(offset) + "(s0)");
    } else {
      F->emit("  sw " + reg + ", -" + std::to_string(offset) + "(s0)");
    }
  }

  void emitLoadAddr(CGType ty, const std::string &reg, const std::string &addrReg) {
    if (ty.isDouble()) {
      F->emit("  fld " + reg + ", 0(" + addrReg + ")");
    } else if (sizeOf(ty) == 8) {
      F->emit("  ld " + reg + ", 0(" + addrReg + ")");
    } else {
      F->emit("  lw " + reg + ", 0(" + addrReg + ")");
    }
  }

  void emitStoreAddr(CGType ty, const std::string &reg, const std::string &addrReg) {
    if (ty.isDouble()) {
      F->emit("  fsd " + reg + ", 0(" + addrReg + ")");
    } else if (sizeOf(ty) == 8) {
      F->emit("  sd " + reg + ", 0(" + addrReg + ")");
    } else {
      F->emit("  sw " + reg + ", 0(" + addrReg + ")");
    }
  }

  void emitLoadIncomingArg(CGType ty, const std::string &reg, int offset) {
    if (ty.isDouble()) {
      F->emit("  fld " + reg + ", " + std::to_string(offset) + "(s0)");
    } else if (sizeOf(ty) == 8) {
      F->emit("  ld " + reg + ", " + std::to_string(offset) + "(s0)");
    } else {
      F->emit("  lw " + reg + ", " + std::to_string(offset) + "(s0)");
    }
  }

  void emitStoreOutgoingArg(CGType ty, const std::string &reg, int offset) {
    if (ty.isDouble()) {
      F->emit("  fsd " + reg + ", " + std::to_string(offset) + "(sp)");
    } else {
      F->emit("  sd " + reg + ", " + std::to_string(offset) + "(sp)");
    }
  }

  std::string ensureString(const std::string &lit) {
    auto it = strLabels.find(lit);
    if (it != strLabels.end()) {
      return it->second;
    }
    std::string label = ".LCSTR" + std::to_string(strCount++);
    rodata << "  .balign 8\n" << label << ":\n"
           << "  .asciz \"" << escapeAsmString(lit) << "\"\n";
    strLabels[lit] = label;
    return label;
  }

  std::string ensureDouble(double value) {
    std::uint64_t bits = 0;
    static_assert(sizeof(bits) == sizeof(value), "double size mismatch");
    std::memcpy(&bits, &value, sizeof(bits));
    auto it = doubleLabels.find(bits);
    if (it != doubleLabels.end()) {
      return it->second;
    }
    std::string label = ".LCDBL" + std::to_string(dblCount++);
    std::ostringstream hex;
    hex << std::hex << bits;
    rodata << "  .balign 8\n" << label << ":\n"
           << "  .dword 0x" << hex.str() << "\n";
    doubleLabels[bits] = label;
    return label;
  }

  std::string emptyArrayLabel(CGType ty) {
    assert(ty.arrayDepth > 0);
    std::string label = "empty_arr_" + scalarSuffix(ty.base) + "_" + std::to_string(ty.arrayDepth);
    if (!emptyArrays.count(label)) {
      emptyArrays.insert(label);
      data << "  .balign 8\n" << label << ":\n  .quad 0\n";
    }
    return label;
  }

  void collectSig(FnDef *f) {
    fnSigs[f->ident_] = astType(f->type_);
    std::vector<CGType> pts;
    if (f->listarg_) {
      for (Arg *a : *f->listarg_) {
        auto *arg = dynamic_cast<Argument *>(a);
        pts.push_back(astType(arg->type_));
      }
    }
    fnParamTys[f->ident_] = pts;
  }

  void saveCurrentResult(CGType ty, int offset) {
    emitStoreStack(ty, ty.isDouble() ? "ft0" : "t0", offset);
  }

  void loadResultFromSlot(CGType ty, const std::string &reg, int offset) {
    emitLoadStack(ty, reg, offset);
  }

  void emitIntConst(const std::string &reg, int value) {
    F->emit("  li " + reg + ", " + std::to_string(value));
  }

  void emitMove(CGType ty, const std::string &dst, const std::string &src) {
    if (ty.isDouble()) {
      F->emit("  fmv.d " + dst + ", " + src);
    } else {
      F->emit("  mv " + dst + ", " + src);
    }
  }

  void emitDefaultInit(CGType ty, int offset) {
    if (ty.arrayDepth > 0) {
      F->emit("  la t0, " + emptyArrayLabel(ty));
      emitStoreStack(ty, "t0", offset);
      return;
    }
    if (ty.base == CGBase::DOUBLE) {
      F->emit("  fmv.d.x ft0, zero");
      emitStoreStack(ty, "ft0", offset);
      return;
    }
    emitIntConst("t0", 0);
    emitStoreStack(ty, "t0", offset);
  }

  CGType genExpr(Expr *e) {
    if (dynamic_cast<ELitTrue *>(e)) {
      emitIntConst("t0", 1);
      return {CGBase::BOOL, 0};
    }
    if (dynamic_cast<ELitFalse *>(e)) {
      emitIntConst("t0", 0);
      return {CGBase::BOOL, 0};
    }
    if (auto *lit = dynamic_cast<ELitInt *>(e)) {
      emitIntConst("t0", lit->integer_);
      return {CGBase::INT, 0};
    }
    if (auto *lit = dynamic_cast<ELitDoub *>(e)) {
      F->emit("  la t0, " + ensureDouble(lit->double_));
      F->emit("  fld ft0, 0(t0)");
      return {CGBase::DOUBLE, 0};
    }
    if (auto *var = dynamic_cast<EVar *>(e)) {
      VarInfo *vi = lookup(var->ident_);
      assert(vi);
      emitLoadStack(vi->ty, vi->ty.isDouble() ? "ft0" : "t0", vi->offset);
      return vi->ty;
    }
    if (auto *str = dynamic_cast<EString *>(e)) {
      F->emit("  la t0, " + ensureString(str->string_));
      return {CGBase::STRING, 0};
    }
    if (auto *nw = dynamic_cast<ENew *>(e)) {
      std::vector<int> dims;
      dims.reserve(nw->listarrsize_->size());
      for (ArrSize *sz : *nw->listarrsize_) {
        auto *dim = dynamic_cast<NewDim *>(sz);
        CGType dt = genExpr(dim->expr_);
        assert((dt == CGType{CGBase::INT, 0}));
        int off = allocIntTemp();
        saveCurrentResult(dt, off);
        dims.push_back(off);
      }
      return genNewArray(astBaseType(nw->basetype_).base, dims, 0);
    }
    if (auto *idx = dynamic_cast<EIndex *>(e)) {
      LRef ref = genIndexRef(idx->expr_1, idx->expr_2);
      emitLoadAddr(ref.ty, ref.ty.isDouble() ? "ft0" : "t0", ref.addrReg);
      return ref.ty;
    }
    if (auto *len = dynamic_cast<ELength *>(e)) {
      assert(len->ident_ == "length");
      CGType ty = genExpr(len->expr_);
      assert(ty.arrayDepth > 0);
      F->emit("  lw t0, 0(t0)");
      return {CGBase::INT, 0};
    }
    if (auto *neg = dynamic_cast<Neg *>(e)) {
      CGType ty = genExpr(neg->expr_);
      if (ty.isDouble()) {
        F->emit("  fneg.d ft0, ft0");
      } else {
        F->emit("  negw t0, t0");
      }
      return ty;
    }
    if (auto *nt = dynamic_cast<Not *>(e)) {
      CGType ty = genExpr(nt->expr_);
      assert((ty == CGType{CGBase::BOOL, 0}));
      F->emit("  seqz t0, t0");
      return ty;
    }
    if (auto *add = dynamic_cast<EAdd *>(e)) {
      CGType lty = genExpr(add->expr_1);
      int left = allocSlot(lty);
      saveCurrentResult(lty, left);
      CGType rty = genExpr(add->expr_2);
      assert(lty == rty);
      if (lty.isDouble()) {
        loadResultFromSlot(lty, "ft1", left);
        F->emit(std::string("  ") + (dynamic_cast<Plus *>(add->addop_) ? "fadd.d" : "fsub.d") + " ft0, ft1, ft0");
      } else {
        loadResultFromSlot(lty, "t1", left);
        F->emit(std::string("  ") + (dynamic_cast<Plus *>(add->addop_) ? "addw" : "subw") + " t0, t1, t0");
      }
      return lty;
    }
    if (auto *mul = dynamic_cast<EMul *>(e)) {
      CGType lty = genExpr(mul->expr_1);
      int left = allocSlot(lty);
      saveCurrentResult(lty, left);
      CGType rty = genExpr(mul->expr_2);
      assert(lty == rty);
      if (lty.isDouble()) {
        loadResultFromSlot(lty, "ft1", left);
        if (dynamic_cast<Times *>(mul->mulop_)) {
          F->emit("  fmul.d ft0, ft1, ft0");
        } else {
          F->emit("  fdiv.d ft0, ft1, ft0");
        }
      } else {
        loadResultFromSlot(lty, "t1", left);
        if (dynamic_cast<Times *>(mul->mulop_)) {
          F->emit("  mulw t0, t1, t0");
        } else if (dynamic_cast<Div *>(mul->mulop_)) {
          F->emit("  divw t0, t1, t0");
        } else {
          F->emit("  remw t0, t1, t0");
        }
      }
      return lty;
    }
    if (auto *rel = dynamic_cast<ERel *>(e)) {
      CGType lty = genExpr(rel->expr_1);
      int left = allocSlot(lty);
      saveCurrentResult(lty, left);
      CGType rty = genExpr(rel->expr_2);
      assert(lty == rty);
      if (lty.isDouble()) {
        loadResultFromSlot(lty, "ft1", left);
        if (dynamic_cast<EQU *>(rel->relop_)) {
          F->emit("  feq.d t0, ft1, ft0");
        } else if (dynamic_cast<NE *>(rel->relop_)) {
          F->emit("  feq.d t0, ft1, ft0");
          F->emit("  seqz t0, t0");
        } else if (dynamic_cast<LTH *>(rel->relop_)) {
          F->emit("  flt.d t0, ft1, ft0");
        } else if (dynamic_cast<LE *>(rel->relop_)) {
          F->emit("  fle.d t0, ft1, ft0");
        } else if (dynamic_cast<GTH *>(rel->relop_)) {
          F->emit("  flt.d t0, ft0, ft1");
        } else {
          F->emit("  fle.d t0, ft0, ft1");
        }
      } else {
        loadResultFromSlot(lty, "t1", left);
        if (dynamic_cast<EQU *>(rel->relop_)) {
          F->emit("  sub t0, t1, t0");
          F->emit("  seqz t0, t0");
        } else if (dynamic_cast<NE *>(rel->relop_)) {
          F->emit("  sub t0, t1, t0");
          F->emit("  snez t0, t0");
        } else if (dynamic_cast<LTH *>(rel->relop_)) {
          F->emit("  slt t0, t1, t0");
        } else if (dynamic_cast<LE *>(rel->relop_)) {
          F->emit("  slt t0, t0, t1");
          F->emit("  xori t0, t0, 1");
        } else if (dynamic_cast<GTH *>(rel->relop_)) {
          F->emit("  slt t0, t0, t1");
        } else {
          F->emit("  slt t0, t1, t0");
          F->emit("  xori t0, t0, 1");
        }
      }
      return {CGBase::BOOL, 0};
    }
    if (auto *land = dynamic_cast<EAnd *>(e)) {
      std::string lfalse = F->L();
      std::string lend = F->L();
      genExpr(land->expr_1);
      F->emit("  beqz t0, " + lfalse);
      genExpr(land->expr_2);
      F->emit("  beqz t0, " + lfalse);
      emitIntConst("t0", 1);
      F->emit("  j " + lend);
      F->emit(lfalse + ":");
      emitIntConst("t0", 0);
      F->emit(lend + ":");
      return {CGBase::BOOL, 0};
    }
    if (auto *lor = dynamic_cast<EOr *>(e)) {
      std::string ltrue = F->L();
      std::string lend = F->L();
      genExpr(lor->expr_1);
      F->emit("  bnez t0, " + ltrue);
      genExpr(lor->expr_2);
      F->emit("  bnez t0, " + ltrue);
      emitIntConst("t0", 0);
      F->emit("  j " + lend);
      F->emit(ltrue + ":");
      emitIntConst("t0", 1);
      F->emit(lend + ":");
      return {CGBase::BOOL, 0};
    }
    if (auto *app = dynamic_cast<EApp *>(e)) {
      std::vector<int> argSlots;
      std::vector<CGType> argTypes;
      if (app->listexpr_) {
        for (Expr *x : *app->listexpr_) {
          CGType ty = genExpr(x);
          int slot = allocSlot(ty);
          saveCurrentResult(ty, slot);
          argSlots.push_back(slot);
          argTypes.push_back(ty);
        }
      }

      int iArg = 0;
      int fArg = 0;
      int stackArgBytes = 0;
      std::vector<int> stackOffsets(argSlots.size(), -1);
      for (std::size_t i = 0; i < argSlots.size(); ++i) {
        const CGType &ty = argTypes[i];
        if (ty.isDouble()) {
          if (fArg < 8) {
            ++fArg;
          } else {
            stackOffsets[i] = stackArgBytes;
            stackArgBytes += 8;
          }
        } else {
          if (iArg < 8) {
            ++iArg;
          } else {
            stackOffsets[i] = stackArgBytes;
            stackArgBytes += 8;
          }
        }
      }

      int callFrame = alignUp(stackArgBytes, 16);
      if (callFrame > 0) {
        F->emit("  addi sp, sp, -" + std::to_string(callFrame));
      }

      iArg = 0;
      fArg = 0;
      for (std::size_t i = 0; i < argSlots.size(); ++i) {
        const CGType &ty = argTypes[i];
        if (ty.isDouble()) {
          emitLoadStack(ty, "ft0", argSlots[i]);
          if (stackOffsets[i] >= 0) {
            emitStoreOutgoingArg(ty, "ft0", stackOffsets[i]);
          } else {
            emitMove(ty, "fa" + std::to_string(fArg++), "ft0");
          }
        } else {
          emitLoadStack(ty, "t0", argSlots[i]);
          if (stackOffsets[i] >= 0) {
            emitStoreOutgoingArg(ty, "t0", stackOffsets[i]);
          } else {
            F->emit("  mv a" + std::to_string(iArg++) + ", t0");
          }
        }
      }

      F->emit("  call " + app->ident_);
      if (callFrame > 0) {
        F->emit("  addi sp, sp, " + std::to_string(callFrame));
      }
      CGType retTy;
      if (app->ident_ == "printInt" || app->ident_ == "printDouble" || app->ident_ == "printString") {
        retTy = {CGBase::VOID, 0};
      } else if (app->ident_ == "readInt") {
        retTy = {CGBase::INT, 0};
      } else if (app->ident_ == "readDouble") {
        retTy = {CGBase::DOUBLE, 0};
      } else {
        retTy = fnSigs[app->ident_];
      }

      if (retTy == CGType{CGBase::VOID, 0}) {
        return retTy;
      }
      if (retTy.isDouble()) {
        F->emit("  fmv.d ft0, fa0");
      } else {
        F->emit("  mv t0, a0");
      }
      return retTy;
    }
    if (auto *ann = dynamic_cast<EAnnotExp *>(e)) {
      return genExpr(ann->expr_);
    }

    assert(false && "unhandled Expr variant");
    return {};
  }

  LRef genIndexRef(Expr *arrayExpr, Expr *indexExpr) {
    CGType arrTy = genExpr(arrayExpr);
    assert(arrTy.arrayDepth > 0);
    int arrOff = allocSlot(arrTy);
    saveCurrentResult(arrTy, arrOff);
    CGType idxTy = genExpr(indexExpr);
    assert((idxTy == CGType{CGBase::INT, 0}));
    emitLoadStack(arrTy, "t1", arrOff);
    int elemSize = sizeOf(elementType(arrTy));
    if (elemSize == 4) {
      F->emit("  slli t0, t0, 2");
    } else if (elemSize == 8) {
      F->emit("  slli t0, t0, 3");
    } else {
      emitIntConst("t2", elemSize);
      F->emit("  mul t0, t0, t2");
    }
    F->emit("  addi t1, t1, 8");
    F->emit("  add t2, t1, t0");
    return {false, 0, "t2", elementType(arrTy)};
  }

  LRef genLhs(Lhs *lhs) {
    if (auto *lv = dynamic_cast<LhsVar *>(lhs)) {
      VarInfo *vi = lookup(lv->ident_);
      assert(vi);
      return {true, vi->offset, "", vi->ty};
    }
    if (auto *li = dynamic_cast<LhsIndex *>(lhs)) {
      return genIndexRef(li->expr_1, li->expr_2);
    }
    assert(false && "unsupported lhs");
    return {};
  }

  void loadLhsValue(const LRef &ref) {
    if (ref.isStack) {
      emitLoadStack(ref.ty, ref.ty.isDouble() ? "ft0" : "t0", ref.offset);
    } else {
      emitLoadAddr(ref.ty, ref.ty.isDouble() ? "ft0" : "t0", ref.addrReg);
    }
  }

  void storeLhsValue(const LRef &ref) {
    if (ref.isStack) {
      emitStoreStack(ref.ty, ref.ty.isDouble() ? "ft0" : "t0", ref.offset);
    } else {
      emitStoreAddr(ref.ty, ref.ty.isDouble() ? "ft0" : "t0", ref.addrReg);
    }
  }

  CGType genNewArray(CGBase base, const std::vector<int> &dims, std::size_t idx) {
    CGType arrTy{base, static_cast<int>(dims.size() - idx)};
    CGType elemTy = elementType(arrTy);

    emitLoadStack({CGBase::INT, 0}, "t0", dims[idx]);
    int lenOff = allocIntTemp();
    emitStoreStack({CGBase::INT, 0}, "t0", lenOff);
    int elemSize = sizeOf(elemTy);
    if (elemSize == 4) {
      F->emit("  slli t1, t0, 2");
    } else if (elemSize == 8) {
      F->emit("  slli t1, t0, 3");
    } else {
      emitIntConst("t1", elemSize);
      F->emit("  mul t1, t0, t1");
    }
    F->emit("  addi t1, t1, 8");
    emitIntConst("a0", 1);
    F->emit("  mv a1, t1");
    F->emit("  call calloc");
    F->emit("  mv t0, a0");
    int arrOff = allocSlot(arrTy);
    emitStoreStack(arrTy, "t0", arrOff);
    emitLoadStack({CGBase::INT, 0}, "t1", lenOff);
    F->emit("  sw t1, 0(t0)");

    if (arrTy.arrayDepth > 1) {
      int idxOff = allocIntTemp();
      emitIntConst("t0", 0);
      emitStoreStack({CGBase::INT, 0}, "t0", idxOff);
      std::string lcond = F->L();
      std::string lbody = F->L();
      std::string lend = F->L();
      F->emit(lcond + ":");
      emitLoadStack({CGBase::INT, 0}, "t0", idxOff);
      emitLoadStack({CGBase::INT, 0}, "t1", lenOff);
      F->emit("  bge t0, t1, " + lend);
      F->emit(lbody + ":");
      CGType innerTy = genNewArray(base, dims, idx + 1);
      int childOff = allocSlot(innerTy);
      emitStoreStack(innerTy, "t0", childOff);
      emitLoadStack({CGBase::INT, 0}, "t0", idxOff);
      F->emit("  slli t0, t0, 3");
      emitLoadStack(arrTy, "t1", arrOff);
      F->emit("  addi t1, t1, 8");
      F->emit("  add t2, t1, t0");
      emitLoadStack(innerTy, "t0", childOff);
      F->emit("  sd t0, 0(t2)");
      emitLoadStack({CGBase::INT, 0}, "t0", idxOff);
      F->emit("  addiw t0, t0, 1");
      emitStoreStack({CGBase::INT, 0}, "t0", idxOff);
      F->emit("  j " + lcond);
      F->emit(lend + ":");
    }

    emitLoadStack(arrTy, "t0", arrOff);
    return arrTy;
  }

  void genStmt(Stmt *s) {
    if (dynamic_cast<Empty *>(s)) {
      return;
    }
    if (auto *sexp = dynamic_cast<SExp *>(s)) {
      (void)genExpr(sexp->expr_);
      return;
    }
    if (auto *ret = dynamic_cast<Ret *>(s)) {
      CGType ty = genExpr(ret->expr_);
      if (ty.isDouble()) {
        F->emit("  fmv.d fa0, ft0");
      } else {
        F->emit("  mv a0, t0");
      }
      F->emit("  j " + F->epilogueLabel);
      return;
    }
    if (dynamic_cast<VRet *>(s)) {
      F->emit("  j " + F->epilogueLabel);
      return;
    }
    if (auto *decl = dynamic_cast<Decl *>(s)) {
      CGType ty = astType(decl->type_);
      for (Item *it : *decl->listitem_) {
        int off = allocSlot(ty);
        if (auto *ni = dynamic_cast<NoInit *>(it)) {
          bind(ni->ident_, {off, ty});
          emitDefaultInit(ty, off);
        } else if (auto *in = dynamic_cast<Init *>(it)) {
          CGType rhs = genExpr(in->expr_);
          assert(rhs == ty);
          bind(in->ident_, {off, ty});
          emitStoreStack(ty, ty.isDouble() ? "ft0" : "t0", off);
        }
      }
      return;
    }
    if (auto *ass = dynamic_cast<Ass *>(s)) {
      LRef ref = genLhs(ass->lhs_);
      CGType rhs = genExpr(ass->expr_);
      assert(rhs == ref.ty);
      storeLhsValue(ref);
      return;
    }
    if (auto *inc = dynamic_cast<Incr *>(s)) {
      LRef ref = genLhs(inc->lhs_);
      if (ref.isStack) {
        emitLoadStack(ref.ty, "t0", ref.offset);
        F->emit("  addiw t0, t0, 1");
        emitStoreStack(ref.ty, "t0", ref.offset);
      } else {
        emitLoadAddr(ref.ty, "t0", ref.addrReg);
        F->emit("  addiw t0, t0, 1");
        emitStoreAddr(ref.ty, "t0", ref.addrReg);
      }
      return;
    }
    if (auto *dec = dynamic_cast<Decr *>(s)) {
      LRef ref = genLhs(dec->lhs_);
      if (ref.isStack) {
        emitLoadStack(ref.ty, "t0", ref.offset);
        F->emit("  addiw t0, t0, -1");
        emitStoreStack(ref.ty, "t0", ref.offset);
      } else {
        emitLoadAddr(ref.ty, "t0", ref.addrReg);
        F->emit("  addiw t0, t0, -1");
        emitStoreAddr(ref.ty, "t0", ref.addrReg);
      }
      return;
    }
    if (auto *cond = dynamic_cast<Cond *>(s)) {
      std::string lend = F->L();
      CGType ty = genExpr(cond->expr_);
      assert((ty == CGType{CGBase::BOOL, 0}));
      F->emit("  beqz t0, " + lend);
      pushScope();
      genStmt(cond->stmt_);
      popScope();
      F->emit(lend + ":");
      return;
    }
    if (auto *cond = dynamic_cast<CondElse *>(s)) {
      std::string lelse = F->L();
      std::string lend = F->L();
      CGType ty = genExpr(cond->expr_);
      assert((ty == CGType{CGBase::BOOL, 0}));
      F->emit("  beqz t0, " + lelse);
      pushScope();
      genStmt(cond->stmt_1);
      popScope();
      F->emit("  j " + lend);
      F->emit(lelse + ":");
      pushScope();
      genStmt(cond->stmt_2);
      popScope();
      F->emit(lend + ":");
      return;
    }
    if (auto *wh = dynamic_cast<While *>(s)) {
      std::string lcond = F->L();
      std::string lend = F->L();
      F->emit(lcond + ":");
      CGType ty = genExpr(wh->expr_);
      assert((ty == CGType{CGBase::BOOL, 0}));
      F->emit("  beqz t0, " + lend);
      pushScope();
      genStmt(wh->stmt_);
      popScope();
      F->emit("  j " + lcond);
      F->emit(lend + ":");
      return;
    }
    if (auto *fe = dynamic_cast<ForEach *>(s)) {
      CGType itemTy = astType(fe->type_);
      CGType arrTy{itemTy.base, itemTy.arrayDepth + 1};
      CGType exprTy = genExpr(fe->expr_);
      assert(exprTy == arrTy);
      int arrOff = allocSlot(arrTy);
      emitStoreStack(arrTy, "t0", arrOff);
      int idxOff = allocIntTemp();
      emitIntConst("t0", 0);
      emitStoreStack({CGBase::INT, 0}, "t0", idxOff);
      std::string lcond = F->L();
      std::string lbody = F->L();
      std::string lend = F->L();
      F->emit(lcond + ":");
      emitLoadStack({CGBase::INT, 0}, "t0", idxOff);
      emitLoadStack(arrTy, "t1", arrOff);
      F->emit("  lw t1, 0(t1)");
      F->emit("  bge t0, t1, " + lend);
      F->emit(lbody + ":");
      pushScope();
      int loopOff = allocSlot(itemTy);
      bind(fe->ident_, {loopOff, itemTy});
      emitLoadStack({CGBase::INT, 0}, "t0", idxOff);
      if (sizeOf(itemTy) == 4) {
        F->emit("  slli t0, t0, 2");
      } else {
        F->emit("  slli t0, t0, 3");
      }
      emitLoadStack(arrTy, "t1", arrOff);
      F->emit("  addi t1, t1, 8");
      F->emit("  add t2, t1, t0");
      emitLoadAddr(itemTy, itemTy.isDouble() ? "ft0" : "t0", "t2");
      emitStoreStack(itemTy, itemTy.isDouble() ? "ft0" : "t0", loopOff);
      genStmt(fe->stmt_);
      popScope();
      emitLoadStack({CGBase::INT, 0}, "t0", idxOff);
      F->emit("  addiw t0, t0, 1");
      emitStoreStack({CGBase::INT, 0}, "t0", idxOff);
      F->emit("  j " + lcond);
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

    assert(false && "unhandled statement");
  }

  void emitFunction(FnDef *f) {
    FnCtx ctx;
    F = &ctx;
    ctx.name = f->ident_;
    ctx.retTy = astType(f->type_);
    ctx.epilogueLabel = ".Lreturn_" + f->ident_;

    pushScope();
    int iArg = 0;
    int fArg = 0;
    int stackArgOff = 0;
    if (f->listarg_) {
      for (Arg *a : *f->listarg_) {
        auto *arg = dynamic_cast<Argument *>(a);
        CGType ty = astType(arg->type_);
        int off = allocSlot(ty);
        bind(arg->ident_, {off, ty});
        if (ty.isDouble()) {
          if (fArg < 8) {
            emitStoreStack(ty, "fa" + std::to_string(fArg++), off);
          } else {
            emitLoadIncomingArg(ty, "ft0", stackArgOff);
            emitStoreStack(ty, "ft0", off);
            stackArgOff += 8;
          }
        } else {
          if (iArg < 8) {
            emitStoreStack(ty, "a" + std::to_string(iArg++), off);
          } else {
            emitLoadIncomingArg(ty, "t0", stackArgOff);
            emitStoreStack(ty, "t0", off);
            stackArgOff += 8;
          }
        }
      }
    }

    auto *block = dynamic_cast<Block *>(f->blk_);
    for (Stmt *s : *block->liststmt_) {
      genStmt(s);
    }
    if (ctx.retTy == CGType{CGBase::VOID, 0}) {
      ctx.emit("  j " + ctx.epilogueLabel);
    }

    int frame = ctx.frameSize();
    text << "  .globl " << f->ident_ << "\n" << f->ident_ << ":\n";
    text << "  addi sp, sp, -" << frame << "\n";
    text << "  sd ra, " << (frame - 8) << "(sp)\n";
    text << "  sd s0, " << (frame - 16) << "(sp)\n";
    text << "  addi s0, sp, " << frame << "\n";
    text << ctx.body.str();
    text << ctx.epilogueLabel << ":\n";
    text << "  ld ra, -8(s0)\n";
    text << "  ld s0, -16(s0)\n";
    text << "  addi sp, sp, " << frame << "\n";
    text << "  ret\n\n";

    popScope();
    F = nullptr;
  }

  void gen(Program *p, std::ostream &out) {
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

    if (!rodata.str().empty()) {
      out << "  .section .rodata\n" << rodata.str() << '\n';
    }
    if (!data.str().empty()) {
      out << "  .data\n" << data.str() << '\n';
    }
    out << "  .text\n" << text.str();
  }
};

} // namespace

void generateRISCV(Program *prog, std::ostream &out) {
  CodeGen cg;
  cg.gen(prog, out);
}
