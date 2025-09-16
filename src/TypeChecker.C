#include "TypeChecker.H"
#include <sstream>
#include <algorithm>

TypeChecker::TypeChecker()
  : currentExprType(TYPE_VOID),
    currentFunctionReturnType(TYPE_VOID),
    sawReturnInCurrentFunction(false)
{
    // Start with a global scope for variables (often empty).
    enterScope();

	/* ─── primitive functions required by the spec ─────────────── */
	functionTable["printInt"]    = { TYPE_VOID  , { TYPE_INT    } };
	functionTable["printDouble"] = { TYPE_VOID  , { TYPE_DOUBLE } };
	functionTable["printString"] = { TYPE_VOID  , { TYPE_ERR    } };  // sentinel
	functionTable["readInt"]     = { TYPE_INT   , {             } };
	functionTable["readDouble"]  = { TYPE_DOUBLE, {             } };


}

TypeChecker::~TypeChecker() {
    // Pop all scopes
    while (!varScopes.empty()) {
        varScopes.pop();
    }
}

/** 
 * The main entry point: we do a “two-pass” approach:
 *   1) Collect all function signatures from the entire Program
 *   2) Visit again to type-check each function’s body
 */
Program* TypeChecker::checkProgram(Program *program) {
    if (!program) {
        throw std::runtime_error("Null Program node.");
    }
    // 1) First pass: gather function signatures
    // We can do that by visiting only the top-level definitions
    // but skipping the block bodies. Let’s do a specialized partial visit:
    for (auto def : *(program->listtopdef_)) {
        if (FnDef* f = dynamic_cast<FnDef*>(def)) {
            // Convert the function’s return Type
            BaseType retT = convertType(f->type_);

            // Convert the parameter types
            std::vector<BaseType> paramTs;
            for (auto argNode : *(f->listarg_)) {
                if (Argument* a = dynamic_cast<Argument*>(argNode)) {
                    BaseType t = convertType(a->type_);
                    paramTs.push_back(t);
                }
            }

            // Record the function in the functionTable
            if (functionTable.find(f->ident_) != functionTable.end()) {
                // Already declared? That’s an error in Javalette (no overload).
                throw std::runtime_error(
                    "Function " + f->ident_ + " declared multiple times.");
            }

            functionTable[f->ident_] = { retT, paramTs };
        }
        else {
            // theoretically no other top-level def in basic Javalette
            throw std::runtime_error("Unknown top-level definition encountered.");
        }
    }

	/* ensure   int main()   with no parameters exists */
	auto m = functionTable.find("main");
	if (m == functionTable.end() ||
		m->second.returnType != TYPE_INT ||
		!m->second.paramTypes.empty())
		throw std::runtime_error("Program must define `int main()` with zero parameters.");


    // 2) Second pass: actually type-check
    program->accept(this);

    return program;
}

/**************************************************************
 *                      Helper Methods
 **************************************************************/

BaseType TypeChecker::convertType(Type *t) {
    if (dynamic_cast<Int*>(t)) {
        return TYPE_INT;
    }
    else if (dynamic_cast<Doub*>(t)) {
        return TYPE_DOUBLE;
    }
    else if (dynamic_cast<Bool*>(t)) {
        return TYPE_BOOL;
    }
    else if (dynamic_cast<Void*>(t)) {
        return TYPE_VOID;
    }
    // “Fun(...)” might exist in extended Javalette if we do function types. 
    // If not implemented, throw or handle.
    throw std::runtime_error("Unsupported type node in convertType().");
}

void TypeChecker::enterScope() {
    varScopes.push(std::map<std::string, BaseType>());
}

void TypeChecker::leaveScope() {
    if (varScopes.empty()) {
        throw std::runtime_error("leaveScope() called but no scope to pop!");
    }
    varScopes.pop();
}

void TypeChecker::declareVariable(const std::string &name, BaseType t) {
    if (varScopes.empty()) {
        throw std::runtime_error("No scope to declare variable " + name);
    }
    std::map<std::string, BaseType> &top = varScopes.top();
    if (top.find(name) != top.end()) {
        // Already declared in this scope
        throw std::runtime_error("Redeclaration of variable " + name + " in same block");
    }
    top[name] = t;
}

BaseType TypeChecker::lookupVariable(const std::string &name) {
    // Look from top of stack down
    std::stack< std::map<std::string, BaseType> > tmp = varScopes;
    while (!tmp.empty()) {
        auto &mp = tmp.top();
        auto it = mp.find(name);
        if (it != mp.end()) {
            return it->second;
        }
        tmp.pop();
    }
    // not found
    throw std::runtime_error("Use of undeclared variable: " + name);
}

BaseType TypeChecker::unifyNumeric(BaseType left, BaseType right) {
    // Example policy: if either is double, result is double. 
    // If both int, result is int. Otherwise throw.
    if (!isNumeric(left) || !isNumeric(right)) {
        throw std::runtime_error("unifyNumeric called on non-numeric types.");
    }
    if (left == TYPE_DOUBLE || right == TYPE_DOUBLE) {
        return TYPE_DOUBLE;
    }
    // else both int
    return TYPE_INT;
}

/**************************************************************
 *          1) Program
 **************************************************************/

void TypeChecker::visitProgram(Program *p) {
    // Now that functionTable is prepared, check the top-level defs fully
    p->listtopdef_->accept(this);
}

/**************************************************************
 *          2) TopDef
 **************************************************************/

void TypeChecker::visitFnDef(FnDef *p) {
    // We already have the function’s signature in functionTable.
    // Retrieve it:
    auto it = functionTable.find(p->ident_);
    if (it == functionTable.end()) {
        throw std::runtime_error("FnDef not found in functionTable: " + p->ident_);
    }
    FnSignature &sig = it->second;

    // Set up for checking this function
    currentFunctionReturnType = sig.returnType;
    sawReturnInCurrentFunction = false;

    // Enter a new scope for function parameters
    enterScope();

    // Declare parameters
    // (We can also just visit p->listarg_, but let's do it manually to keep sync.)
    // Actually we do it the BNFC way:
    p->listarg_->accept(this);

    // Now type-check the body
    p->blk_->accept(this);

    // Done with function scope
    leaveScope();

    // Check if a non-void function ended without a return
    if (currentFunctionReturnType != TYPE_VOID && !sawReturnInCurrentFunction) {
        throw std::runtime_error(
            "Function " + p->ident_ + " missing return statement of type.");
    }
}

void TypeChecker::visitListTopDef(ListTopDef *p) {
    for (auto def : *p) {
        def->accept(this); // typically FnDef
    }
}

/**************************************************************
 *          3) Arg
 **************************************************************/

void TypeChecker::visitArgument(Argument *p) {
    // e.g. "int x"
    BaseType t = convertType(p->type_);
    declareVariable(p->ident_, t);
}

void TypeChecker::visitListArg(ListArg *p) {
    for (auto a : *p) {
        a->accept(this); // -> visitArgument
    }
}

/**************************************************************
 *          4) Blk
 **************************************************************/

void TypeChecker::visitBlock(Block *p) {
    // This is a "bare" block node. But in the grammar, 
    // we handle the actual scoping in BStmt or function bodies.
    // If we do want to treat every block as a new scope, we can do:
    // - but BNFC’s BStmt is the real "stmt => { ... }"
    p->liststmt_->accept(this);
}

/**************************************************************
 *          5) Stmt
 **************************************************************/

void TypeChecker::visitEmpty(Empty *p) {
    // No operation: an empty statement: ";"
}

void TypeChecker::visitBStmt(BStmt *p) {
    // BStmt is a statement that is actually a block {...}
    enterScope();
    p->blk_->accept(this);
    leaveScope();
}

void TypeChecker::visitDecl(Decl *p) {
    // "Type x, y = 3;"
    // Remember the declared type in currentExprType or store separately
    BaseType declaredT = convertType(p->type_);
    // But we must keep track of the declaredT so each NoInit/Init knows it:
    // Easiest approach is to stash it in currentExprType temporarily:
    // or store in a field. Let’s do the simpler approach:
    BaseType old = currentExprType;
    currentExprType = declaredT;
    p->listitem_->accept(this);
    currentExprType = old;
}

/** Because BNFC’s grammar often has us do `p->listitem_->accept(this)` again. */
void TypeChecker::visitNoInit(NoInit *p) {
    // "x;"
    // The type for this new var is in currentExprType
    declareVariable(p->ident_, currentExprType);
}

void TypeChecker::visitInit(Init *p) {
    // "x = expr;"
    BaseType varT = currentExprType; // from Decl
    declareVariable(p->ident_, varT);

    // check initializer
    p->expr_->accept(this);
    if (!sameType(varT, currentExprType)) {
        // If you want int/double auto-promotion, do unifyNumeric or similar
        // For now, require exact type
        std::ostringstream oss;
        oss << "Type mismatch in initialization of " << p->ident_
            << ": declared " << varT << ", assigned " << currentExprType;
        throw std::runtime_error(oss.str());
    }
}

void TypeChecker::visitAss(Ass *p) {
    // "x = expr;"
    BaseType varT = lookupVariable(p->ident_);
    p->expr_->accept(this);
    if (!sameType(varT, currentExprType)) {
        // possibly unify numeric or strictly check
        throw std::runtime_error("Type mismatch in assignment to " + p->ident_);
    }
}

void TypeChecker::visitIncr(Incr *p) {
    // "x++"
    BaseType varT = lookupVariable(p->ident_);
    if (varT != TYPE_INT) {
        throw std::runtime_error("++ operator requires int variable: " + p->ident_);
    }
}

void TypeChecker::visitDecr(Decr *p) {
    // "x--"
    BaseType varT = lookupVariable(p->ident_);
    if (varT != TYPE_INT) {
        throw std::runtime_error("-- operator requires int variable: " + p->ident_);
    }
}

void TypeChecker::visitRet(Ret *p) {
    // "return expr;"
    p->expr_->accept(this);
    BaseType exprT = currentExprType;
    if (!sameType(exprT, currentFunctionReturnType)) {
        throw std::runtime_error("Return type mismatch in function body");
    }
    sawReturnInCurrentFunction = true;
}

void TypeChecker::visitVRet(VRet *p) {
    // "return;"
    if (currentFunctionReturnType != TYPE_VOID) {
        throw std::runtime_error("Return without value in non-void function.");
    }
    sawReturnInCurrentFunction = true;
}

void TypeChecker::visitCond(Cond *p) {
    // "if (expr) stmt"
    p->expr_->accept(this);
    if (currentExprType != TYPE_BOOL) {
        throw std::runtime_error("If condition must be boolean");
    }
    p->stmt_->accept(this);
}

void TypeChecker::visitCondElse(CondElse *p) {
    // "if (expr) stmt else stmt"
    p->expr_->accept(this);
    if (currentExprType != TYPE_BOOL) {
        throw std::runtime_error("If condition must be boolean");
    }
    p->stmt_1->accept(this);
    p->stmt_2->accept(this);
}

void TypeChecker::visitWhile(While *p) {
    // "while (expr) stmt"
    p->expr_->accept(this);
    if (currentExprType != TYPE_BOOL) {
        throw std::runtime_error("While condition must be boolean");
    }
    p->stmt_->accept(this);
}

void TypeChecker::visitSExp(SExp *p) {
    // "expr;"
    p->expr_->accept(this);
    // we discard the expression type
}

void TypeChecker::visitListStmt(ListStmt *p) {
    for (auto stmt : *p) {
        stmt->accept(this);
    }
}

/**************************************************************
 *          6) Item
 **************************************************************/
void TypeChecker::visitListItem(ListItem *p) {
    for (auto item : *p) {
        item->accept(this);
    }
}

/**************************************************************
 *          7) Type
 **************************************************************/
void TypeChecker::visitInt(Int *p) {
    // Usually we do not type-check the Type node itself. 
    // convertType(...) is used instead. 
}
void TypeChecker::visitDoub(Doub *p) {}
void TypeChecker::visitBool(Bool *p) {}
void TypeChecker::visitVoid(Void *p) {}
void TypeChecker::visitFun(Fun *p) {
    // If needed, handle function type references. 
}
void TypeChecker::visitListType(ListType *p) {
    // Possibly used for extended syntax
}

/**************************************************************
 *          8) Expr
 **************************************************************/
void TypeChecker::visitEVar(EVar *p) {
    currentExprType = lookupVariable(p->ident_);
}

void TypeChecker::visitELitInt(ELitInt *p) {
    currentExprType = TYPE_INT;
}

void TypeChecker::visitELitDoub(ELitDoub *p) {
    currentExprType = TYPE_DOUBLE;
}

void TypeChecker::visitELitTrue(ELitTrue *p) {
    currentExprType = TYPE_BOOL;
}

void TypeChecker::visitELitFalse(ELitFalse *p) {
    currentExprType = TYPE_BOOL;
}

void TypeChecker::visitEApp(EApp *p) {
    // function call: ident(expr1, expr2, ...)
    auto it = functionTable.find(p->ident_);
    if (it == functionTable.end()) {
        throw std::runtime_error("Call to undefined function: " + p->ident_);
    }
    FnSignature &fnSig = it->second;
    // Check argument count
    if (fnSig.paramTypes.size() != p->listexpr_->size()) {
        throw std::runtime_error("Function " + p->ident_ + 
            " called with wrong number of arguments.");
    }
    // Check each argument
    size_t i = 0;
    for (auto exprNode : *(p->listexpr_)) {
        exprNode->accept(this);
        BaseType argT = currentExprType;
        BaseType paramT = fnSig.paramTypes[i++];
        if (!sameType(argT, paramT)) {
            // or unify numeric, etc. if you want. 
            throw std::runtime_error(
                "Function " + p->ident_ + " argument type mismatch.");
        }
    }
    // The call expression’s type is the function’s return type
    currentExprType = fnSig.returnType;
}

void TypeChecker::visitEString(EString *p) {
    // Javalette only allows string as an argument to printString
    // Typically there's no real type 'string' except as a literal param 
    // We'll treat it as a special “no type” or throw. 
    // For convenience, store some sentinel or throw. 
    // Let's store TYPE_ERR or something.
    currentExprType = TYPE_ERR; 
}

void TypeChecker::visitNeg(Neg *p) {
    // unary minus
    p->expr_->accept(this);
    if (!isNumeric(currentExprType)) {
        throw std::runtime_error("Unary negation on non-numeric type");
    }
    // Type remains numeric
}

void TypeChecker::visitNot(Not *p) {
    // logical not
    p->expr_->accept(this);
    if (currentExprType != TYPE_BOOL) {
        throw std::runtime_error("Logical not on non-boolean");
    }
}

void TypeChecker::visitEMul(EMul *p) {
    // expression is: expr1 (mulOp) expr2
    // We'll check subexpr types, unify them if numeric, else throw.
    // Then the result is numeric type. 
    p->expr_1->accept(this);
    BaseType leftT = currentExprType;
    p->expr_2->accept(this);
    BaseType rightT = currentExprType;

    if (!isNumeric(leftT) || !isNumeric(rightT)) {
        throw std::runtime_error("Mul operand not numeric");
    }
    // unify
    currentExprType = unifyNumeric(leftT, rightT);

    // We also must check the actual mulOp node (Times, Div, Mod).
    p->mulop_->accept(this);
}

void TypeChecker::visitEAdd(EAdd *p) {
    // expr1 (addOp) expr2
    p->expr_1->accept(this);
    BaseType leftT = currentExprType;
    p->expr_2->accept(this);
    BaseType rightT = currentExprType;

    if (!isNumeric(leftT) || !isNumeric(rightT)) {
        throw std::runtime_error("Add/Sub operand not numeric");
    }
    currentExprType = unifyNumeric(leftT, rightT);

    // Visit the addOp node (Plus/Minus), though typically it’s no-ops
    p->addop_->accept(this);
}

void TypeChecker::visitERel(ERel *p) {
    // expr1 (relOp) expr2 => always yields bool
    p->expr_1->accept(this);
    BaseType leftT = currentExprType;
    p->expr_2->accept(this);
    BaseType rightT = currentExprType;

    // Some operators require numeric (like <, <=, >, >=). 
    // eq/neq can allow boolean too. 
    // We'll see the actual relOp in p->relop_, check there.

    // But typically we unify or check if they are the same or numeric.
    p->relop_->accept(this); 
    // The relop visitor sets some internal “expected usage.” We’ll store them in a field or do a small approach. 
    // For simplicity, assume we allow eq/neq on all (bool/bool or numeric/numeric).
    // Otherwise <,>,<=,>= need numeric.

    // We'll do a minimal approach here: if we see LTH, LE, GTH, GE => unify numeric
    // if EQU, NE => either unify numeric or both bool or both int...
    // We'll do it in the relop visitors below.
    currentExprType = TYPE_BOOL;
}

void TypeChecker::visitEAnd(EAnd *p) {
    // expr1 && expr2
    p->expr_1->accept(this);
    BaseType leftT = currentExprType;
    if (leftT != TYPE_BOOL) {
        throw std::runtime_error("Left operand of && not bool");
    }
    p->expr_2->accept(this);
    BaseType rightT = currentExprType;
    if (rightT != TYPE_BOOL) {
        throw std::runtime_error("Right operand of && not bool");
    }
    currentExprType = TYPE_BOOL;
}

void TypeChecker::visitEOr(EOr *p) {
    // expr1 || expr2
    p->expr_1->accept(this);
    BaseType leftT = currentExprType;
    if (leftT != TYPE_BOOL) {
        throw std::runtime_error("Left operand of || not bool");
    }
    p->expr_2->accept(this);
    BaseType rightT = currentExprType;
    if (rightT != TYPE_BOOL) {
        throw std::runtime_error("Right operand of || not bool");
    }
    currentExprType = TYPE_BOOL;
}

void TypeChecker::visitListExpr(ListExpr *p) {
    for (auto e : *p) {
        e->accept(this);
        // typically we do not chain anything, the caller has logic
    }
}

/**************************************************************
 *          9) AddOp
 **************************************************************/
void TypeChecker::visitPlus(Plus *p) {
    // no-op except for logging
}
void TypeChecker::visitMinus(Minus *p) {
    // no-op
}

/**************************************************************
 *          10) MulOp
 **************************************************************/
void TypeChecker::visitTimes(Times *p) {
    // no-op
}
void TypeChecker::visitDiv(Div *p) {
    // no-op
}
void TypeChecker::visitMod(Mod *p) {
    // no-op
}

/**************************************************************
 *          11) RelOp
 **************************************************************/
void TypeChecker::visitLTH(LTH *p) {
    // used for "expr1 < expr2", must unify numeric
}
void TypeChecker::visitLE(LE *p) {
    // must unify numeric
}
void TypeChecker::visitGTH(GTH *p) {
    // must unify numeric
}
void TypeChecker::visitGE(GE *p) {
    // must unify numeric
}
void TypeChecker::visitEQU(EQU *p) {
    // eq => can be numeric or bool, but they must match
}
void TypeChecker::visitNE(NE *p) {
    // not eq => can be numeric or bool, must match
}


