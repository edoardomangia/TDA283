// TypeChecker.H
#pragma once

#include "Skeleton.H"
#include <map>
#include <stdexcept>
#include <string>
#include <vector>

class TypeError : public std::runtime_error {
public:
  using std::runtime_error::runtime_error;
};

enum class PrimType { INT, DOUBLE, BOOL, VOID };

struct SemType {
  PrimType base = PrimType::VOID;
  int arrayDepth = 0;

  bool operator==(const SemType &other) const {
    return base == other.base && arrayDepth == other.arrayDepth;
  }

  bool operator!=(const SemType &other) const { return !(*this == other); }

  bool isScalar() const { return arrayDepth == 0; }
  bool isArray() const { return arrayDepth > 0; }
};

struct FunType {
  SemType result;
  std::vector<SemType> args;
};

class TypeChecker : public Skeleton {
public:
  void checkProgram(Prog *p);

  void visitProgram(Program *p) override;
  void visitFnDef(FnDef *p) override;

  void visitBlock(Block *p) override;
  void visitDecl(Decl *p) override;
  void visitAss(Ass *p) override;
  void visitIncr(Incr *p) override;
  void visitDecr(Decr *p) override;
  void visitRet(Ret *p) override;
  void visitVRet(VRet *p) override;
  void visitCond(Cond *p) override;
  void visitCondElse(CondElse *p) override;
  void visitWhile(While *p) override;
  void visitForEach(ForEach *p) override;
  void visitSExp(SExp *p) override;

  void visitEVar(EVar *p) override;
  void visitELitInt(ELitInt *p) override;
  void visitELitDoub(ELitDoub *p) override;
  void visitELitTrue(ELitTrue *p) override;
  void visitELitFalse(ELitFalse *p) override;
  void visitEString(EString *p) override;
  void visitENew(ENew *p) override;
  void visitEIndex(EIndex *p) override;
  void visitELength(ELength *p) override;
  void visitNeg(Neg *p) override;
  void visitNot(Not *p) override;
  void visitEApp(EApp *p) override;
  void visitEMul(EMul *p) override;
  void visitEAdd(EAdd *p) override;
  void visitERel(ERel *p) override;
  void visitEAnd(EAnd *p) override;
  void visitEOr(EOr *p) override;
  void visitEAnnotExp(EAnnotExp *p) override;

private:
  std::map<std::string, FunType> funEnv;
  std::vector<std::map<std::string, SemType>> varStack;
  SemType currentExprType;
  SemType currentFunResult;

  void pushScope();
  void popScope();
  void bindVar(const std::string &name, const SemType &t);
  bool hasLocalVar(const std::string &name) const;
  SemType lookupVar(const std::string &name) const;
  SemType fromAstType(Type *t) const;
  SemType fromAstBaseType(::BaseType *t) const;
  SemType checkLhs(Lhs *lhs) const;
};
