// TypeChecker.C
#include "TypeChecker.H"
#include "Absyn.H"

#include <set>
#include <sstream>

// ---------- Small helpers in an anonymous namespace ----------

namespace {

std::string typeToString(TypeKind t) {
    switch (t) {
        case TypeKind::INT:    return "int";
        case TypeKind::DOUBLE: return "double";
        case TypeKind::BOOL:   return "boolean";
        case TypeKind::VOID:   return "void";
    }
    return "<unknown-type>";
}

// Forward decls for definite-return analysis
bool stmtReturns(Stmt *s);
bool blockReturns(Blk *b);

bool blockReturns(Blk *b) {
    if (!b) return false;
    Block *block = dynamic_cast<Block*>(b);
    if (!block || !block->liststmt_) return false;

    for (Stmt *s : *block->liststmt_) {
        if (stmtReturns(s)) return true;
    }
    return false;
}

bool stmtReturns(Stmt *s) {
    if (!s) return false;

    if (dynamic_cast<Ret*>(s))  return true;
    if (dynamic_cast<VRet*>(s)) return true;

    if (auto bs = dynamic_cast<BStmt*>(s)) {
        return blockReturns(bs->blk_);
    }

    if (auto ce = dynamic_cast<CondElse*>(s)) {
        // if (e) s1 else s2  returns iff both branches return
        return stmtReturns(ce->stmt_1) && stmtReturns(ce->stmt_2);
    }

    // while, if-without-else, decls, ass, incr, decr, sexp, empty, ...
    return false;
}

} // anonymous namespace

// ---------- TypeChecker core API ----------

// Entry point for type checking: resets environments and prepares to analyze program.
void TypeChecker::checkProgram(Prog *p)
{
    funEnv.clear();
    varStack.clear();

    // Built-in functions
    funEnv["printInt"]    = { TypeKind::VOID,   { TypeKind::INT    } };
    funEnv["printDouble"] = { TypeKind::VOID,   { TypeKind::DOUBLE } };
    // printString is special: it only accepts string literals; we don't give it a normal argument type
    funEnv["readInt"]     = { TypeKind::INT,    { } };
    funEnv["readDouble"]  = { TypeKind::DOUBLE, { } };

    Program *prog = dynamic_cast<Program*>(p);
    if (!prog) {
        throw TypeError("Internal error: expected Program node");
    }

    ListTopDef *defs = prog->listtopdef_;
    if (!defs || defs->empty()) {
        throw TypeError("Program must contain at least one function definition");
    }

    // First pass: gather all function signatures into funEnv.
    // Checks duplicate functions, invalid parameters, and prepares function type info.
    bool hasValidMain = false;

    for (TopDef *td : *defs) {
        FnDef *fn = dynamic_cast<FnDef*>(td);
        if (!fn) {
            throw TypeError("Only function definitions are allowed at top level");
        }

        std::string name = fn->ident_;

        if (funEnv.count(name)) {
            std::ostringstream oss;
            oss << "Function '" << name << "' is defined more than once";
            throw TypeError(oss.str());
        }

        TypeKind resultType = fromAstType(fn->type_);

        std::vector<TypeKind> argTypes;
        std::set<std::string> argNames;

        if (fn->listarg_) {
            for (Arg *a : *fn->listarg_) {
                Argument *arg = dynamic_cast<Argument*>(a);
                if (!arg) {
                    throw TypeError("Internal error: expected Argument");
                }
                std::string argName = arg->ident_;
                // // Ensure all parameters in a function have unique names.
                if (!argNames.insert(argName).second) {
                    std::ostringstream oss;
                    oss << "Duplicate parameter name '" << argName
                        << "' in function '" << name << "'";
                    throw TypeError(oss.str());
                }

                TypeKind at = fromAstType(arg->type_);
                // Parameters cannot have type void.
				if (at == TypeKind::VOID) {
                    std::ostringstream oss;
                    oss << "Parameter '" << argName
                        << "' of function '" << name
                        << "' cannot have type void";
                    throw TypeError(oss.str());
                }
                argTypes.push_back(at);
            }
        }

        funEnv[name] = { resultType, argTypes };
		
		// main must be int main()
        if (name == "main") {
            if (resultType != TypeKind::INT || !argTypes.empty()) {
                throw TypeError("main must have type 'int' and no parameters");
            }
            hasValidMain = true;
        }
    }

    if (!hasValidMain) {
        throw TypeError("Program must define 'int main()'");
    }

    // --- Second pass: type-check bodies ---
	// Second pass: use visitor to type-check all statements and expressions inside functions.

    for (TopDef *td : *defs) {
        FnDef *fn = dynamic_cast<FnDef*>(td);
        if (!fn) continue; // already checked

        currentFunResult = fromAstType(fn->type_);
        currentFunDefinitelyReturns = false;

		// Set expected return type for this function and reset return tracking.
        pushScope();
        if (fn->listarg_) {
            for (Arg *a : *fn->listarg_) {
                Argument *arg = dynamic_cast<Argument*>(a);
                TypeKind at = fromAstType(arg->type_);
                bindVar(arg->ident_, at);
            }
        }

        if (!fn->blk_) {
            throw TypeError("Internal error: function with no body");
        }

        // Type-check body using visitor
        fn->blk_->accept(this);

        // Check definite return for non-void functions
        if (currentFunResult != TypeKind::VOID) {
            if (!blockReturns(fn->blk_)) {
                std::ostringstream oss;
                oss << "Function '" << fn->ident_
                    << "' may finish without returning a value of type "
                    << typeToString(currentFunResult);
                throw TypeError(oss.str());
            }
        }

        popScope();
    }
}

// ---------- Visitor methods (some are just "nice to have") ----------

void TypeChecker::visitProgram(Program *p)
{
    // Normally we drive from checkProgram instead of via visitor,
    // but we implement this to be polite.
    if (p->listtopdef_) {
        for (TopDef *td : *p->listtopdef_) {
            td->accept(this);
        }
    }
}

void TypeChecker::visitFnDef(FnDef *p)
{
    // Not used by checkProgram; all work is done there.
    // Provide a no-op implementation to satisfy the interface.
    (void)p;
}

// ---------- Scopes & environment ----------

void TypeChecker::pushScope()
{
    varStack.emplace_back();
}

void TypeChecker::popScope()
{
    if (varStack.empty()) {
        throw TypeError("Internal error: scope stack underflow");
    }
    varStack.pop_back();
}

void TypeChecker::bindVar(const std::string &name, TypeKind t)
{
    if (varStack.empty()) {
        throw TypeError("Internal error: no active scope to bind variable");
    }
    auto &scope = varStack.back();
    if (scope.count(name)) {
        std::ostringstream oss;
        oss << "Variable '" << name << "' is already declared in this block";
        throw TypeError(oss.str());
    }
    scope[name] = t;
}

TypeKind TypeChecker::lookupVar(const std::string &name)
{
    for (auto it = varStack.rbegin(); it != varStack.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            return found->second;
        }
    }

    // NEW: distinguish between "function name" and "undeclared variable"
    if (funEnv.count(name)) {
        std::ostringstream oss;
        oss << "'" << name << "' is a function, not a variable";
        throw TypeError(oss.str());
    }

    std::ostringstream oss;
    oss << "Use of undeclared variable '" << name << "'";
    throw TypeError(oss.str());
}

TypeKind TypeChecker::fromAstType(Type *t)
{
    if (dynamic_cast<Int*>(t))   return TypeKind::INT;
    if (dynamic_cast<Doub*>(t))  return TypeKind::DOUBLE;
    if (dynamic_cast<Bool*>(t))  return TypeKind::BOOL;
    if (dynamic_cast<Void*>(t))  return TypeKind::VOID;

    // No other types in basic Javalette
    throw TypeError("Unsupported type in declaration");
}

// ---------- Statements ----------

void TypeChecker::visitBlock(Block *p)
{
    // New scope for this block
    pushScope();
    if (p->liststmt_) {
        for (Stmt *s : *p->liststmt_) {
            s->accept(this);
        }
    }
    popScope();
}

void TypeChecker::visitDecl(Decl *p)
{
    // Variable declarations: Type (NoInit/Init, ...)
    if (!p->type_) {
        throw TypeError("Internal error: declaration without type");
    }

    // Variables are not allowed to have type void
    if (dynamic_cast<Void*>(p->type_) != nullptr) {
        throw TypeError("Variables cannot have type void");
    }

    TypeKind t = fromAstType(p->type_);

    if (!p->listitem_) return;

    for (Item *it : *p->listitem_) {
        if (auto ni = dynamic_cast<NoInit*>(it)) {
            bindVar(ni->ident_, t);
        } else if (auto in = dynamic_cast<Init*>(it)) {
            // expr must have same type
            if (!in->expr_) {
                throw TypeError("Internal error: Init without expression");
            }
            in->expr_->accept(this);
            if (currentExprType != t) {
                std::ostringstream oss;
                oss << "Type mismatch in initialization of variable '"
                    << in->ident_ << "': expected " << typeToString(t)
                    << ", got " << typeToString(currentExprType);
                throw TypeError(oss.str());
            }
            bindVar(in->ident_, t);
        } else {
            throw TypeError("Internal error: unknown Item in Decl");
        }
    }
}

void TypeChecker::visitAss(Ass *p)
{
    // NEW: disallow assigning to a function
    if (funEnv.count(p->ident_)) {
        std::ostringstream oss;
        oss << "Cannot assign to function '" << p->ident_ << "'";
        throw TypeError(oss.str());
    }

    TypeKind vt = lookupVar(p->ident_);
    if (!p->expr_) {
        throw TypeError("Internal error: assignment without expression");
    }
    // Disallow assigning string literals to variables at all
    if (dynamic_cast<EString*>(p->expr_) != nullptr) {
        throw TypeError("String literals may only be used as arguments to printString");
    }
    p->expr_->accept(this);
    if (currentExprType != vt) {
        std::ostringstream oss;
        oss << "Type mismatch in assignment to '" << p->ident_
            << "': expected " << typeToString(vt)
            << ", got " << typeToString(currentExprType);
        throw TypeError(oss.str());
    }
}

void TypeChecker::visitIncr(Incr *p)
{
    TypeKind vt = lookupVar(p->ident_);
    if (vt != TypeKind::INT) {
        std::ostringstream oss;
        oss << "Operator '++' requires operand of type int, but variable '"
            << p->ident_ << "' has type " << typeToString(vt);
        throw TypeError(oss.str());
    }
}

void TypeChecker::visitDecr(Decr *p)
{
    TypeKind vt = lookupVar(p->ident_);
    if (vt != TypeKind::INT) {
        std::ostringstream oss;
        oss << "Operator '--' requires operand of type int, but variable '"
            << p->ident_ << "' has type " << typeToString(vt);
        throw TypeError(oss.str());
    }
}

void TypeChecker::visitRet(Ret *p)
{
    if (currentFunResult == TypeKind::VOID) {
        throw TypeError("Return with value in function of type void");
    }
    if (!p->expr_) {
        throw TypeError("Return statement without expression in non-void function");
    }
    p->expr_->accept(this);
    if (currentExprType != currentFunResult) {
        std::ostringstream oss;
        oss << "Return type mismatch: expected " << typeToString(currentFunResult)
            << ", got " << typeToString(currentExprType);
        throw TypeError(oss.str());
    }
}

void TypeChecker::visitVRet(VRet *p)
{
    (void)p;
    if (currentFunResult != TypeKind::VOID) {
        std::ostringstream oss;
        oss << "Function with result type " << typeToString(currentFunResult)
            << " must return a value";
        throw TypeError(oss.str());
    }
}

void TypeChecker::visitCond(Cond *p)
{
    if (!p->expr_ || !p->stmt_) {
        throw TypeError("Internal error: malformed if-statement");
    }

    p->expr_->accept(this);
    if (currentExprType != TypeKind::BOOL) {
        throw TypeError("Condition of if-statement must have type boolean");
    }
    p->stmt_->accept(this);
}

void TypeChecker::visitCondElse(CondElse *p)
{
    if (!p->expr_ || !p->stmt_1 || !p->stmt_2) {
        throw TypeError("Internal error: malformed if-else statement");
    }

    p->expr_->accept(this);
    if (currentExprType != TypeKind::BOOL) {
        throw TypeError("Condition of if-else statement must have type boolean");
    }
    p->stmt_1->accept(this);
    p->stmt_2->accept(this);
}

void TypeChecker::visitWhile(While *p)
{
    if (!p->expr_ || !p->stmt_) {
        throw TypeError("Internal error: malformed while-statement");
    }

    p->expr_->accept(this);
    if (currentExprType != TypeKind::BOOL) {
        throw TypeError("Condition of while-statement must have type boolean");
    }
    p->stmt_->accept(this);
}

void TypeChecker::visitSExp(SExp *p)
{
    if (!p->expr_) return;
    p->expr_->accept(this);
    // Only void expressions (i.e., calls to void functions) are allowed as statements
    if (currentExprType != TypeKind::VOID) {
        throw TypeError("Only expressions of type void (calls to void functions) may be used as statements");
    }
}

// ---------- Expressions ----------

void TypeChecker::visitEVar(EVar *p)
{
    currentExprType = lookupVar(p->ident_);
}

void TypeChecker::visitELitInt(ELitInt *p)
{
    (void)p;
    currentExprType = TypeKind::INT;
}

void TypeChecker::visitELitDoub(ELitDoub *p)
{
    (void)p;
    currentExprType = TypeKind::DOUBLE;
}

void TypeChecker::visitELitTrue(ELitTrue *p)
{
    (void)p;
    currentExprType = TypeKind::BOOL;
}

void TypeChecker::visitELitFalse(ELitFalse *p)
{
    (void)p;
    currentExprType = TypeKind::BOOL;
}

void TypeChecker::visitEString(EString *p)
{
    (void)p;
    // String literals have no first-class type in Javalette.
    // We mark them as "void" to make them illegal in normal expressions.
    currentExprType = TypeKind::VOID;
}

void TypeChecker::visitNeg(Neg *p)
{
    if (!p->expr_) {
        throw TypeError("Internal error: negation without operand");
    }
    p->expr_->accept(this);
    if (currentExprType != TypeKind::INT &&
        currentExprType != TypeKind::DOUBLE) {
        throw TypeError("Unary '-' expects operand of type int or double");
    }
    // Result type is same as operand
}

void TypeChecker::visitNot(Not *p)
{
    if (!p->expr_) {
        throw TypeError("Internal error: '!' without operand");
    }
    p->expr_->accept(this);
    if (currentExprType != TypeKind::BOOL) {
        throw TypeError("Unary '!' expects operand of type boolean");
    }
    // Result type is boolean
}

void TypeChecker::visitEApp(EApp *p)
{
    std::string name = p->ident_;

    // Special handling for printString: its argument must be a string literal
    if (name == "printString") {
        if (!p->listexpr_ || p->listexpr_->size() != 1) {
            throw TypeError("printString expects exactly one argument");
        }
        Expr *arg = (*p->listexpr_)[0];
        if (!dynamic_cast<EString*>(arg)) {
            throw TypeError("printString argument must be a string literal");
        }
        // No need to visit arg; there are no subexpressions inside EString
        currentExprType = TypeKind::VOID;
        return;
    }

    auto it = funEnv.find(name);
    if (it == funEnv.end()) {
        std::ostringstream oss;
        oss << "Call to undefined function '" << name << "'";
        throw TypeError(oss.str());
    }

    const FunType &ft = it->second;
    std::size_t numArgs = p->listexpr_ ? p->listexpr_->size() : 0;
    if (numArgs != ft.args.size()) {
        std::ostringstream oss;
        oss << "Function '" << name << "' expects " << ft.args.size()
            << " arguments, but " << numArgs << " given";
        throw TypeError(oss.str());
    }

    for (std::size_t i = 0; i < numArgs; ++i) {
        Expr *arg = (*p->listexpr_)[i];
        arg->accept(this);
        if (currentExprType != ft.args[i]) {
            std::ostringstream oss;
            oss << "Type mismatch in argument " << (i + 1)
                << " of call to '" << name
                << "': expected " << typeToString(ft.args[i])
                << ", got " << typeToString(currentExprType);
            throw TypeError(oss.str());
        }
    }

    currentExprType = ft.result;
}

void TypeChecker::visitEMul(EMul *p)
{
    if (!p->expr_1 || !p->expr_2 || !p->mulop_) {
        throw TypeError("Internal error: malformed multiplicative expression");
    }

    p->expr_1->accept(this);
    TypeKind t1 = currentExprType;
    p->expr_2->accept(this);
    TypeKind t2 = currentExprType;

    bool isMod = (dynamic_cast<Mod*>(p->mulop_) != nullptr);

    if (isMod) {
        // % : int % int -> int
        if (t1 != TypeKind::INT || t2 != TypeKind::INT) {
            throw TypeError("Operator '%' is only defined on integers");
        }
        currentExprType = TypeKind::INT;
    } else {
        // *, / : both operands same and numeric
        if (t1 != t2 || (t1 != TypeKind::INT && t1 != TypeKind::DOUBLE)) {
            throw TypeError("Operators '*' and '/' require both operands to have the same numeric type");
        }
        currentExprType = t1;
    }
}

void TypeChecker::visitEAdd(EAdd *p)
{
    if (!p->expr_1 || !p->expr_2 || !p->addop_) {
        throw TypeError("Internal error: malformed additive expression");
    }

    p->expr_1->accept(this);
    TypeKind t1 = currentExprType;
    p->expr_2->accept(this);
    TypeKind t2 = currentExprType;

    // + and - : both operands same and numeric
    if (t1 != t2 || (t1 != TypeKind::INT && t1 != TypeKind::DOUBLE)) {
        throw TypeError("Operators '+' and '-' require both operands to have the same numeric type");
    }
    currentExprType = t1;
}

void TypeChecker::visitERel(ERel *p)
{
    if (!p->expr_1 || !p->expr_2 || !p->relop_) {
        throw TypeError("Internal error: malformed relational expression");
    }

    p->expr_1->accept(this);
    TypeKind t1 = currentExprType;
    p->expr_2->accept(this);
    TypeKind t2 = currentExprType;

    bool isEq = dynamic_cast<EQU*>(p->relop_) || dynamic_cast<NE*>(p->relop_);

    if (isEq) {
        // ==, != : operands same type; allowed on int, double, bool
        if (t1 != t2 ||
            (t1 != TypeKind::INT &&
             t1 != TypeKind::DOUBLE &&
             t1 != TypeKind::BOOL)) {
            throw TypeError("Operators '==' and '!=' require operands of the same primitive type");
        }
    } else {
        // <, <=, >, >= : operands same numeric type
        if (t1 != t2 ||
            (t1 != TypeKind::INT && t1 != TypeKind::DOUBLE)) {
            throw TypeError("Relational operators (<, <=, >, >=) require operands of the same numeric type");
        }
    }

    currentExprType = TypeKind::BOOL;
}

void TypeChecker::visitEAnd(EAnd *p)
{
    if (!p->expr_1 || !p->expr_2) {
        throw TypeError("Internal error: malformed && expression");
    }

    p->expr_1->accept(this);
    if (currentExprType != TypeKind::BOOL) {
        throw TypeError("Left operand of '&&' must have type boolean");
    }
    p->expr_2->accept(this);
    if (currentExprType != TypeKind::BOOL) {
        throw TypeError("Right operand of '&&' must have type boolean");
    }

    currentExprType = TypeKind::BOOL;
}

void TypeChecker::visitEOr(EOr *p)
{
    if (!p->expr_1 || !p->expr_2) {
        throw TypeError("Internal error: malformed || expression");
    }

    p->expr_1->accept(this);
    if (currentExprType != TypeKind::BOOL) {
        throw TypeError("Left operand of '||' must have type boolean");
    }
    p->expr_2->accept(this);
    if (currentExprType != TypeKind::BOOL) {
        throw TypeError("Right operand of '||' must have type boolean");
    }

    currentExprType = TypeKind::BOOL;
}

void TypeChecker::visitEAnnotExp(EAnnotExp *p)
{
    if (!p->type_ || !p->expr_) {
        throw TypeError("Internal error: malformed annotated expression");
    }

    TypeKind ann = fromAstType(p->type_);
    p->expr_->accept(this);
    if (currentExprType != ann) {
        throw TypeError("Type annotation does not match the expression type");
    }
    currentExprType = ann;
}

