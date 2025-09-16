#ifndef CODEGEN_H
#define CODEGEN_H

#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include "Absyn.H"
#include "TypeChecker.H"
#include "Skeleton.H"

inline std::string llvmType(BaseType t) {
  switch (t) {
    case TYPE_INT:    return "i32";
    case TYPE_DOUBLE: return "double";
    case TYPE_BOOL:   return "i1";
    case TYPE_VOID:   return "void";
    default:          return "i8*";  // for global strings
  }
}

struct VarInfo {
  BaseType    ty;
  std::string ptr;
};

class CodeGen : public Skeleton {
public:
  explicit CodeGen(std::ostream &out);
  void generate(Program *p);

private:
  std::ostream                         &out;
  std::stringstream                    globals;
  int                                   nextTmp = 0;
  int                                   nextLbl = 0;
  int                                   nextStr = 0;
  BaseType                              currentRetType;
  bool                                  hasReturn = false;
  std::unordered_map<std::string,VarInfo> env;

  std::string freshTmp();
  std::string freshLabel();
  std::string addGlobalString(const std::string &lit);
  BaseType    exprType(Expr *e);

  void visitProgram  (Program   *p)    override;
  void visitFnDef    (FnDef     *f)    override;
  void visitBStmt    (BStmt     *b)    override;
  void visitDecl     (Decl      *d)    override;
  void visitNoInit   (NoInit    *ni)   override;
  void visitInit     (Init      *in)   override;
  void visitAss      (Ass       *x)    override;
  void visitIncr     (Incr      *x)    override;
  void visitDecr     (Decr      *x)    override;
  void visitCond     (Cond      *c)    override;
  void visitCondElse (CondElse  *c)    override;
  void visitWhile    (While     *w)    override;
  void visitRet      (Ret       *r)    override;
  void visitVRet     (VRet      *r)    override;
  void visitSExp     (SExp      *s)    override;

  std::string genExpr(Expr *x);
};

#endif // CODEGEN_H