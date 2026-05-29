/*
 * Public interface and shared data structures for the semantic/type checker.
 * The parser builds BNFC AST nodes. TypeChecker walks those nodes with the
 * visitor interface from Skeleton.H and throws TypeError as soon as it finds a
 * semantic error.
 */

#pragma once

#include "Skeleton.H"
#include <map>
#include <stdexcept>
#include <string>
#include <vector>

// The compiler front end catches this exception and reports the program as
// rejected.
class TypeError : public std::runtime_error {
public:
  using std::runtime_error::runtime_error;
};

// Primitive scalar types understood by the type checker.
enum class PrimType { INT, DOUBLE, BOOL, VOID };

// Semantic type used after translating the BNFC Type nodes.
//
// Examples:
//   int       -> {INT, 0}
//   int[]     -> {INT, 1}
//   double[][] -> {DOUBLE, 2}
struct SemType {
  PrimType base = PrimType::VOID;
  int arrayDepth = 0;

  bool operator==(const SemType &other) const {
    return base == other.base && arrayDepth == other.arrayDepth;
  }

  bool operator!=(const SemType &other) const { return !(*this == other); }

  bool isArray() const { return arrayDepth > 0; }
};

// Function signature stored in the function environment.
struct FunType {
  SemType result;
  std::vector<SemType> args;
};

// Visitor-based type checker.
// Each visit method validates one AST node kind and updates currentExprType
// when the node is an expression.
class TypeChecker : public Skeleton {
public:
  // Entry point used by main.cpp.
  void checkProgram(Prog *p);

  // Top-level visitors.
  // checkProgram does the real top-level work explicitly,
  // but these overrides keep the class complete as a Skeleton visitor.
  void visitProgram(Program *p) override;
  void visitFnDef(FnDef *p) override;

  // Statement visitors.
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

  // Expression visitors.
  // Each one leaves the expression's semantic type in
  // currentExprType for its parent expression/statement to inspect.
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

private:
  // All known functions.
  std::map<std::string, FunType> funEnv;

  // Stack of block scopes.
  std::vector<std::map<std::string, SemType>> varStack;

  // Scratch result written by expression visitors.
  SemType currentExprType;

  // Return type of the function currently being checked.
  SemType currentFunResult;

  // Scope and variable environment helpers.
  void pushScope();
  void popScope();
  void bindVar(const std::string &name, const SemType &t);
  bool hasLocalVar(const std::string &name) const;
  SemType lookupVar(const std::string &name) const;

  // Helpers to go from AST types to SemType form.
  SemType fromAstType(Type *t) const;
  SemType fromAstBaseType(::BaseType *t) const;

  // Validate a left-hand side expression and return the type assigned to.
  SemType checkLhs(Lhs *lhs) const;
};
