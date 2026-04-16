// TypeChecker.C
#include "TypeChecker.H"
#include "Absyn.H"

#include <set>
#include <sstream>

namespace {

std::string typeToString(const SemType &t) {
  std::string base;
  switch (t.base) {
  case PrimType::INT:
    base = "int";
    break;
  case PrimType::DOUBLE:
    base = "double";
    break;
  case PrimType::BOOL:
    base = "boolean";
    break;
  case PrimType::VOID:
    base = "void";
    break;
  }
  for (int i = 0; i < t.arrayDepth; ++i) {
    base += "[]";
  }
  return base;
}

bool isNumericScalar(const SemType &t) {
  return t.arrayDepth == 0 &&
         (t.base == PrimType::INT || t.base == PrimType::DOUBLE);
}

bool isIntScalar(const SemType &t) {
  return t.arrayDepth == 0 && t.base == PrimType::INT;
}

bool isBoolScalar(const SemType &t) {
  return t.arrayDepth == 0 && t.base == PrimType::BOOL;
}

SemType elementType(const SemType &t) {
  return {t.base, t.arrayDepth - 1};
}

bool stmtReturns(Stmt *s);
bool blockReturns(Blk *b);

bool blockReturns(Blk *b) {
  auto *block = dynamic_cast<Block *>(b);
  if (!block || !block->liststmt_) {
    return false;
  }
  for (Stmt *s : *block->liststmt_) {
    if (stmtReturns(s)) {
      return true;
    }
  }
  return false;
}

bool stmtReturns(Stmt *s) {
  if (!s) {
    return false;
  }
  if (dynamic_cast<Ret *>(s) || dynamic_cast<VRet *>(s)) {
    return true;
  }
  if (auto *bs = dynamic_cast<BStmt *>(s)) {
    return blockReturns(bs->blk_);
  }
  if (auto *ce = dynamic_cast<CondElse *>(s)) {
    return stmtReturns(ce->stmt_1) && stmtReturns(ce->stmt_2);
  }
  return false;
}

} // namespace

void TypeChecker::checkProgram(Prog *p) {
  funEnv.clear();
  varStack.clear();
  currentExprType = {PrimType::VOID, 0};
  currentFunResult = {PrimType::VOID, 0};

  funEnv["printInt"] = {{PrimType::VOID, 0}, {{PrimType::INT, 0}}};
  funEnv["printDouble"] = {{PrimType::VOID, 0}, {{PrimType::DOUBLE, 0}}};
  funEnv["readInt"] = {{PrimType::INT, 0}, {}};
  funEnv["readDouble"] = {{PrimType::DOUBLE, 0}, {}};

  auto *prog = dynamic_cast<Program *>(p);
  if (!prog) {
    throw TypeError("Internal error: expected Program node");
  }
  if (!prog->listtopdef_ || prog->listtopdef_->empty()) {
    throw TypeError("Program must contain at least one function definition");
  }

  bool hasValidMain = false;
  for (TopDef *td : *prog->listtopdef_) {
    auto *fn = dynamic_cast<FnDef *>(td);
    if (!fn) {
      throw TypeError("Only function definitions are allowed at top level");
    }

    if (funEnv.count(fn->ident_)) {
      throw TypeError("Function '" + fn->ident_ + "' is defined more than once");
    }

    FunType ft;
    ft.result = fromAstType(fn->type_);
    std::set<std::string> argNames;

    if (fn->listarg_) {
      for (Arg *a : *fn->listarg_) {
        auto *arg = dynamic_cast<Argument *>(a);
        if (!arg) {
          throw TypeError("Internal error: expected Argument");
        }
        if (!argNames.insert(arg->ident_).second) {
          throw TypeError("Duplicate parameter name '" + arg->ident_ +
                          "' in function '" + fn->ident_ + "'");
        }
        SemType at = fromAstType(arg->type_);
        if (at.base == PrimType::VOID && at.arrayDepth == 0) {
          throw TypeError("Parameter '" + arg->ident_ + "' of function '" +
                          fn->ident_ + "' cannot have type void");
        }
        ft.args.push_back(at);
      }
    }

    funEnv[fn->ident_] = ft;
    if (fn->ident_ == "main") {
      if (ft.result != SemType{PrimType::INT, 0} || !ft.args.empty()) {
        throw TypeError("main must have type 'int' and no parameters");
      }
      hasValidMain = true;
    }
  }

  if (!hasValidMain) {
    throw TypeError("Program must define 'int main()'");
  }

  for (TopDef *td : *prog->listtopdef_) {
    auto *fn = dynamic_cast<FnDef *>(td);
    currentFunResult = fromAstType(fn->type_);
    pushScope();
    if (fn->listarg_) {
      for (Arg *a : *fn->listarg_) {
        auto *arg = dynamic_cast<Argument *>(a);
        bindVar(arg->ident_, fromAstType(arg->type_));
      }
    }

    fn->blk_->accept(this);
    if (currentFunResult != SemType{PrimType::VOID, 0} &&
        !blockReturns(fn->blk_)) {
      throw TypeError("Function '" + fn->ident_ +
                      "' may finish without returning a value of type " +
                      typeToString(currentFunResult));
    }
    popScope();
  }
}

void TypeChecker::visitProgram(Program *p) {
  if (p->listtopdef_) {
    for (TopDef *td : *p->listtopdef_) {
      td->accept(this);
    }
  }
}

void TypeChecker::visitFnDef(FnDef *p) { (void)p; }

void TypeChecker::pushScope() { varStack.emplace_back(); }

void TypeChecker::popScope() {
  if (varStack.empty()) {
    throw TypeError("Internal error: scope stack underflow");
  }
  varStack.pop_back();
}

void TypeChecker::bindVar(const std::string &name, const SemType &t) {
  if (varStack.empty()) {
    throw TypeError("Internal error: no active scope to bind variable");
  }
  auto &scope = varStack.back();
  if (scope.count(name)) {
    throw TypeError("Variable '" + name + "' is already declared in this block");
  }
  scope[name] = t;
}

bool TypeChecker::hasLocalVar(const std::string &name) const {
  for (auto it = varStack.rbegin(); it != varStack.rend(); ++it) {
    if (it->count(name)) {
      return true;
    }
  }
  return false;
}

SemType TypeChecker::lookupVar(const std::string &name) const {
  for (auto it = varStack.rbegin(); it != varStack.rend(); ++it) {
    auto found = it->find(name);
    if (found != it->end()) {
      return found->second;
    }
  }
  if (funEnv.count(name)) {
    throw TypeError("'" + name + "' is a function, not a variable");
  }
  throw TypeError("Use of undeclared variable '" + name + "'");
}

SemType TypeChecker::fromAstType(Type *t) const {
  if (dynamic_cast<Int *>(t)) {
    return {PrimType::INT, 0};
  }
  if (dynamic_cast<Doub *>(t)) {
    return {PrimType::DOUBLE, 0};
  }
  if (dynamic_cast<Bool *>(t)) {
    return {PrimType::BOOL, 0};
  }
  if (dynamic_cast<Void *>(t)) {
    return {PrimType::VOID, 0};
  }
  if (auto *arr = dynamic_cast<Arr *>(t)) {
    SemType inner = fromAstType(arr->type_);
    if (inner.base == PrimType::VOID && inner.arrayDepth == 0) {
      throw TypeError("Arrays of void are not allowed");
    }
    return {inner.base, inner.arrayDepth + 1};
  }
  throw TypeError("Unsupported type in declaration");
}

SemType TypeChecker::fromAstBaseType(::BaseType *t) const {
  if (dynamic_cast<IntBase *>(t)) {
    return {PrimType::INT, 0};
  }
  if (dynamic_cast<DoubBase *>(t)) {
    return {PrimType::DOUBLE, 0};
  }
  if (dynamic_cast<BoolBase *>(t)) {
    return {PrimType::BOOL, 0};
  }
  throw TypeError("Unsupported base type in array creation");
}

SemType TypeChecker::checkLhs(Lhs *lhs) const {
  if (auto *lv = dynamic_cast<LhsVar *>(lhs)) {
    if (funEnv.count(lv->ident_) && !hasLocalVar(lv->ident_)) {
      throw TypeError("Cannot assign to function '" + lv->ident_ + "'");
    }
    return lookupVar(lv->ident_);
  }

  if (auto *li = dynamic_cast<LhsIndex *>(lhs)) {
    TypeChecker *self = const_cast<TypeChecker *>(this);
    li->expr_1->accept(self);
    SemType arrTy = currentExprType;
    if (!arrTy.isArray()) {
      throw TypeError("Indexed assignment requires an array expression");
    }
    li->expr_2->accept(self);
    if (!isIntScalar(currentExprType)) {
      throw TypeError("Array index must have type int");
    }
    return elementType(arrTy);
  }

  throw TypeError("Internal error: unsupported left-hand side");
}

void TypeChecker::visitBlock(Block *p) {
  pushScope();
  if (p->liststmt_) {
    for (Stmt *s : *p->liststmt_) {
      s->accept(this);
    }
  }
  popScope();
}

void TypeChecker::visitDecl(Decl *p) {
  SemType t = fromAstType(p->type_);
  if (t == SemType{PrimType::VOID, 0}) {
    throw TypeError("Variables cannot have type void");
  }

  if (!p->listitem_) {
    return;
  }

  for (Item *it : *p->listitem_) {
    if (auto *ni = dynamic_cast<NoInit *>(it)) {
      bindVar(ni->ident_, t);
    } else if (auto *in = dynamic_cast<Init *>(it)) {
      in->expr_->accept(this);
      if (currentExprType != t) {
        throw TypeError("Type mismatch in initialization of variable '" +
                        in->ident_ + "': expected " + typeToString(t) +
                        ", got " + typeToString(currentExprType));
      }
      bindVar(in->ident_, t);
    } else {
      throw TypeError("Internal error: unknown Item in Decl");
    }
  }
}

void TypeChecker::visitAss(Ass *p) {
  SemType lhsType = checkLhs(p->lhs_);
  if (dynamic_cast<EString *>(p->expr_)) {
    throw TypeError("String literals may only be used as arguments to printString");
  }
  p->expr_->accept(this);
  if (currentExprType != lhsType) {
    throw TypeError("Type mismatch in assignment: expected " +
                    typeToString(lhsType) + ", got " +
                    typeToString(currentExprType));
  }
}

void TypeChecker::visitIncr(Incr *p) {
  SemType t = checkLhs(p->lhs_);
  if (!isIntScalar(t)) {
    throw TypeError("Operator '++' requires operand of type int");
  }
}

void TypeChecker::visitDecr(Decr *p) {
  SemType t = checkLhs(p->lhs_);
  if (!isIntScalar(t)) {
    throw TypeError("Operator '--' requires operand of type int");
  }
}

void TypeChecker::visitRet(Ret *p) {
  if (currentFunResult == SemType{PrimType::VOID, 0}) {
    throw TypeError("Return with value in function of type void");
  }
  p->expr_->accept(this);
  if (currentExprType != currentFunResult) {
    throw TypeError("Return type mismatch: expected " +
                    typeToString(currentFunResult) + ", got " +
                    typeToString(currentExprType));
  }
}

void TypeChecker::visitVRet(VRet *p) {
  (void)p;
  if (currentFunResult != SemType{PrimType::VOID, 0}) {
    throw TypeError("Function with result type " + typeToString(currentFunResult) +
                    " must return a value");
  }
}

void TypeChecker::visitCond(Cond *p) {
  p->expr_->accept(this);
  if (!isBoolScalar(currentExprType)) {
    throw TypeError("Condition of if-statement must have type boolean");
  }
  p->stmt_->accept(this);
}

void TypeChecker::visitCondElse(CondElse *p) {
  p->expr_->accept(this);
  if (!isBoolScalar(currentExprType)) {
    throw TypeError("Condition of if-else statement must have type boolean");
  }
  p->stmt_1->accept(this);
  p->stmt_2->accept(this);
}

void TypeChecker::visitWhile(While *p) {
  p->expr_->accept(this);
  if (!isBoolScalar(currentExprType)) {
    throw TypeError("Condition of while-statement must have type boolean");
  }
  p->stmt_->accept(this);
}

void TypeChecker::visitForEach(ForEach *p) {
  SemType itemType = fromAstType(p->type_);
  if (itemType == SemType{PrimType::VOID, 0}) {
    throw TypeError("foreach variable cannot have type void");
  }

  p->expr_->accept(this);
  SemType arrType = currentExprType;
  if (!arrType.isArray()) {
    throw TypeError("foreach expects an array expression");
  }
  if (elementType(arrType) != itemType) {
    throw TypeError("foreach variable type " + typeToString(itemType) +
                    " does not match array element type " +
                    typeToString(elementType(arrType)));
  }

  pushScope();
  bindVar(p->ident_, itemType);
  p->stmt_->accept(this);
  popScope();
}

void TypeChecker::visitSExp(SExp *p) {
  p->expr_->accept(this);
  if (currentExprType != SemType{PrimType::VOID, 0}) {
    throw TypeError(
        "Only expressions of type void may be used as statements");
  }
}

void TypeChecker::visitEVar(EVar *p) { currentExprType = lookupVar(p->ident_); }

void TypeChecker::visitELitInt(ELitInt *p) {
  (void)p;
  currentExprType = {PrimType::INT, 0};
}

void TypeChecker::visitELitDoub(ELitDoub *p) {
  (void)p;
  currentExprType = {PrimType::DOUBLE, 0};
}

void TypeChecker::visitELitTrue(ELitTrue *p) {
  (void)p;
  currentExprType = {PrimType::BOOL, 0};
}

void TypeChecker::visitELitFalse(ELitFalse *p) {
  (void)p;
  currentExprType = {PrimType::BOOL, 0};
}

void TypeChecker::visitEString(EString *p) {
  (void)p;
  currentExprType = {PrimType::VOID, 0};
}

void TypeChecker::visitENew(ENew *p) {
  SemType elemType = fromAstBaseType(p->basetype_);
  if (p->listarrsize_->empty()) {
    throw TypeError("Array creation requires at least one dimension");
  }
  int depth = 0;
  for (ArrSize *sz : *p->listarrsize_) {
    auto *dim = dynamic_cast<NewDim *>(sz);
    dim->expr_->accept(this);
    if (!isIntScalar(currentExprType)) {
      throw TypeError("Array length expression must have type int");
    }
    ++depth;
  }
  currentExprType = {elemType.base, depth};
}

void TypeChecker::visitEIndex(EIndex *p) {
  p->expr_1->accept(this);
  SemType arrType = currentExprType;
  if (!arrType.isArray()) {
    throw TypeError("Indexing requires an array expression");
  }
  p->expr_2->accept(this);
  if (!isIntScalar(currentExprType)) {
    throw TypeError("Array index must have type int");
  }
  currentExprType = elementType(arrType);
}

void TypeChecker::visitELength(ELength *p) {
  if (p->ident_ != "length") {
    throw TypeError("Only the 'length' attribute is supported on arrays");
  }
  p->expr_->accept(this);
  if (!currentExprType.isArray()) {
    throw TypeError("Only arrays have a length attribute");
  }
  currentExprType = {PrimType::INT, 0};
}

void TypeChecker::visitNeg(Neg *p) {
  p->expr_->accept(this);
  if (!isNumericScalar(currentExprType)) {
    throw TypeError("Unary '-' expects operand of type int or double");
  }
}

void TypeChecker::visitNot(Not *p) {
  p->expr_->accept(this);
  if (!isBoolScalar(currentExprType)) {
    throw TypeError("Unary '!' expects operand of type boolean");
  }
}

void TypeChecker::visitEApp(EApp *p) {
  const std::string &name = p->ident_;
  if (hasLocalVar(name)) {
    throw TypeError("'" + name + "' is a variable, not a function");
  }

  if (name == "printString") {
    if (!p->listexpr_ || p->listexpr_->size() != 1) {
      throw TypeError("printString expects exactly one argument");
    }
    if (!dynamic_cast<EString *>((*p->listexpr_)[0])) {
      throw TypeError("printString argument must be a string literal");
    }
    currentExprType = {PrimType::VOID, 0};
    return;
  }

  auto it = funEnv.find(name);
  if (it == funEnv.end()) {
    throw TypeError("Call to undefined function '" + name + "'");
  }
  const FunType &ft = it->second;
  const std::size_t numArgs = p->listexpr_ ? p->listexpr_->size() : 0;
  if (numArgs != ft.args.size()) {
    std::ostringstream oss;
    oss << "Function '" << name << "' expects " << ft.args.size()
        << " arguments, but " << numArgs << " given";
    throw TypeError(oss.str());
  }

  for (std::size_t i = 0; i < numArgs; ++i) {
    (*p->listexpr_)[i]->accept(this);
    if (currentExprType != ft.args[i]) {
      std::ostringstream oss;
      oss << "Type mismatch in argument " << (i + 1) << " of call to '"
          << name << "': expected " << typeToString(ft.args[i]) << ", got "
          << typeToString(currentExprType);
      throw TypeError(oss.str());
    }
  }
  currentExprType = ft.result;
}

void TypeChecker::visitEMul(EMul *p) {
  p->expr_1->accept(this);
  SemType t1 = currentExprType;
  p->expr_2->accept(this);
  SemType t2 = currentExprType;

  const bool isMod = dynamic_cast<Mod *>(p->mulop_) != nullptr;
  if (isMod) {
    if (!isIntScalar(t1) || !isIntScalar(t2)) {
      throw TypeError("Operator '%' is only defined on integers");
    }
    currentExprType = {PrimType::INT, 0};
    return;
  }

  if (t1 != t2 || !isNumericScalar(t1)) {
    throw TypeError(
        "Operators '*' and '/' require both operands to have the same numeric type");
  }
  currentExprType = t1;
}

void TypeChecker::visitEAdd(EAdd *p) {
  p->expr_1->accept(this);
  SemType t1 = currentExprType;
  p->expr_2->accept(this);
  SemType t2 = currentExprType;
  if (t1 != t2 || !isNumericScalar(t1)) {
    throw TypeError(
        "Operators '+' and '-' require both operands to have the same numeric type");
  }
  currentExprType = t1;
}

void TypeChecker::visitERel(ERel *p) {
  p->expr_1->accept(this);
  SemType t1 = currentExprType;
  p->expr_2->accept(this);
  SemType t2 = currentExprType;

  if (t1 != t2 || t1.isArray() || t1 == SemType{PrimType::VOID, 0}) {
    throw TypeError("Invalid operand types for relational operator");
  }

  const bool eqOnly = dynamic_cast<EQU *>(p->relop_) || dynamic_cast<NE *>(p->relop_);
  if (!eqOnly && !isNumericScalar(t1)) {
    throw TypeError("Ordering comparisons require operands of type int or double");
  }
  currentExprType = {PrimType::BOOL, 0};
}

void TypeChecker::visitEAnd(EAnd *p) {
  p->expr_1->accept(this);
  if (!isBoolScalar(currentExprType)) {
    throw TypeError("Operator '&&' requires boolean operands");
  }
  p->expr_2->accept(this);
  if (!isBoolScalar(currentExprType)) {
    throw TypeError("Operator '&&' requires boolean operands");
  }
  currentExprType = {PrimType::BOOL, 0};
}

void TypeChecker::visitEOr(EOr *p) {
  p->expr_1->accept(this);
  if (!isBoolScalar(currentExprType)) {
    throw TypeError("Operator '||' requires boolean operands");
  }
  p->expr_2->accept(this);
  if (!isBoolScalar(currentExprType)) {
    throw TypeError("Operator '||' requires boolean operands");
  }
  currentExprType = {PrimType::BOOL, 0};
}

void TypeChecker::visitEAnnotExp(EAnnotExp *p) {
  SemType annotated = fromAstType(p->type_);
  p->expr_->accept(this);
  if (currentExprType != annotated) {
    throw TypeError("Annotated expression has type " +
                    typeToString(currentExprType) + ", not " +
                    typeToString(annotated));
  }
  currentExprType = annotated;
}
