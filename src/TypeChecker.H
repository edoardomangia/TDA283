// TypeChecker.H
#pragma once
#include "Skeleton.H"
#include <map>
#include <vector>
#include <stdexcept>
#include <string>

class TypeError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

enum class TypeKind { INT, DOUBLE, BOOL, VOID };

struct FunType {
    TypeKind result;
    std::vector<TypeKind> args;
};

class TypeChecker : public Skeleton {
public:
    // Entry point
    void checkProgram(Prog *p);

    // Visitor overrides we actually use
    void visitProgram(Program *p) override;      // (not strictly needed, but provided)
    void visitFnDef(FnDef *p) override;          // (ditto, not used by checkProgram)

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
    void visitSExp(SExp *p) override;

    // Expressions
    void visitEVar(EVar *p) override;
    void visitELitInt(ELitInt *p) override;
    void visitELitDoub(ELitDoub *p) override;
    void visitELitTrue(ELitTrue *p) override;
    void visitELitFalse(ELitFalse *p) override;
    void visitEString(EString *p) override;
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
    std::map<std::string, FunType> funEnv;                  // function types
    std::vector<std::map<std::string, TypeKind>> varStack;  // variable scopes
    TypeKind currentExprType { TypeKind::INT };             // “result” of expr visitors
    TypeKind currentFunResult { TypeKind::VOID };           // type of current function
    bool currentFunDefinitelyReturns { false };             // (not used, but kept)

    void pushScope();
    void popScope();
    void bindVar(const std::string &name, TypeKind t);
    TypeKind lookupVar(const std::string &name);
    TypeKind fromAstType(Type *t);
};

