#include "CodeGen.H"

CodeGen::CodeGen(std::ostream &o)
  : out(o), nextTmp(0), nextLbl(0), nextStr(0), currentRetType(TYPE_VOID), hasReturn(false) {}

std::string CodeGen::freshTmp() {
  return "%t" + std::to_string(nextTmp++);
}

std::string CodeGen::freshLabel() {
  return "L" + std::to_string(nextLbl++);
}

std::string CodeGen::addGlobalString(const std::string &lit) {
  std::string name = ".str" + std::to_string(nextStr++);
  int len = static_cast<int>(lit.size()) + 1;
  // emit constant
  globals << "@" << name
          << " = private unnamed_addr constant [" << len << " x i8] c\"";
  for (char c : lit) {
    switch (c) {
      case '\n': globals << "\\0A"; break;
      case '\t': globals << "\\09"; break;
      case '"':  globals << "\\22"; break;
      case '\\': globals << "\\5C"; break;
      default:    globals << c;     break;
    }
  }
  globals << "\\00\"\n";
  // inline GEP into function
  std::string ptr = freshTmp();
  out << "  " << ptr
      << " = getelementptr [" << len << " x i8], [" << len << " x i8]* @"
      << name << ", i32 0, i32 0\n";
  return ptr;
}

BaseType CodeGen::exprType(Expr *e) {
  TypeChecker tc;
  e->accept(&tc);
  return tc.currentExprType;
}

void CodeGen::generate(Program *p) {
  // declare primitives
  out << "declare void @printInt(i32)\n"
         "declare void @printDouble(double)\n"
         "declare void @printString(i8*)\n"
         "declare i32 @readInt()\n"
         "declare double @readDouble()\n\n";
  // emit functions
  p->accept(this);
  // emit string constants
  out << "\n" << globals.str();
}

void CodeGen::visitProgram(Program *p) {
  for (auto td : *p->listtopdef_)
    if (auto f = dynamic_cast<FnDef*>(td))
      f->accept(this);
}

void CodeGen::visitFnDef(FnDef *f) {
  env.clear(); nextTmp = nextLbl = 0; hasReturn = false;
  TypeChecker tc;
  currentRetType = tc.convertType(f->type_);

  // function signature
  out << "define " << llvmType(currentRetType)
      << " @" << f->ident_ << "(";
  for (size_t i = 0; i < f->listarg_->size(); ++i) {
    auto a = dynamic_cast<Argument*>((*f->listarg_)[i]);
    BaseType t = tc.convertType(a->type_);
    out << llvmType(t) << " %p" << i
        << (i + 1 < f->listarg_->size() ? ", " : "");
  }
  out << ") {\nentry:\n";

  // parameter allocas
  for (size_t i = 0; i < f->listarg_->size(); ++i) {
    auto a = dynamic_cast<Argument*>((*f->listarg_)[i]);
    BaseType t = tc.convertType(a->type_);
    std::string ptr = freshTmp();
    out << "  " << ptr << " = alloca " << llvmType(t) << "\n"
           "  store " << llvmType(t) << " %p" << i
        << ", " << llvmType(t) << "* " << ptr << "\n";
    env[a->ident_] = {t, ptr};
  }

  // body
  f->blk_->accept(this);

  // epilogue: default return if none
  if (!hasReturn && currentRetType != TYPE_VOID)
    out << "  ret " << llvmType(currentRetType) << " 0\n";

  out << "}\n\n";
}

void CodeGen::visitBStmt(BStmt *b) {
  auto blk = dynamic_cast<Block*>(b->blk_);
  for (auto s : *blk->liststmt_)
    s->accept(this);
}

void CodeGen::visitDecl(Decl *d) {
  TypeChecker tc;
  BaseType t = tc.convertType(d->type_);
  for (auto itm : *d->listitem_) {
    if (auto ni = dynamic_cast<NoInit*>(itm)) {
      std::string ptr = freshTmp();
      out << "  " << ptr << " = alloca " << llvmType(t) << "\n"
             "  store " << llvmType(t)
          << (t == TYPE_DOUBLE ? " 0.0" : " 0")
          << ", " << llvmType(t) << "* " << ptr << "\n";
      env[ni->ident_] = {t, ptr};
    } else if (auto in = dynamic_cast<Init*>(itm)) {
      std::string ptr = freshTmp();
      out << "  " << ptr << " = alloca " << llvmType(t) << "\n";
      std::string v = genExpr(in->expr_);
      out << "  store " << llvmType(t) << " " << v
          << ", " << llvmType(t) << "* " << ptr << "\n";
      env[in->ident_] = {t, ptr};
    }
  }
}

void CodeGen::visitNoInit(NoInit*) {}
void CodeGen::visitInit(Init*) {}

void CodeGen::visitAss(Ass *x) {
  auto &vi = env[x->ident_];
  std::string v = genExpr(x->expr_);
  out << "  store " << llvmType(vi.ty) << " " << v
      << ", " << llvmType(vi.ty) << "* " << vi.ptr << "\n";
}

void CodeGen::visitIncr(Incr *x) {
  auto &vi = env[x->ident_];
  std::string a = freshTmp(), b = freshTmp();
  out << "  " << a << " = load " << llvmType(vi.ty)
      << ", " << llvmType(vi.ty) << "* " << vi.ptr << "\n"
         "  " << b << " = add " << llvmType(vi.ty)
      << " " << a << ", 1\n"
         "  store " << llvmType(vi.ty)
      << " " << b << ", " << llvmType(vi.ty)
      << "* " << vi.ptr << "\n";
}

void CodeGen::visitDecr(Decr *x) {
  auto &vi = env[x->ident_];
  std::string a = freshTmp(), b = freshTmp();
  out << "  " << a << " = load " << llvmType(vi.ty)
      << ", " << llvmType(vi.ty) << "* " << vi.ptr << "\n"
         "  " << b << " = sub " << llvmType(vi.ty)
      << " " << a << ", 1\n"
         "  store " << llvmType(vi.ty)
      << " " << b << ", " << llvmType(vi.ty)
      << "* " << vi.ptr << "\n";
}

void CodeGen::visitCond(Cond *c) {
  std::string cond = genExpr(c->expr_);
  std::string l1 = freshLabel(), l2 = freshLabel();
  out << "  br i1 " << cond
      << ", label %" << l1 << ", label %" << l2 << "\n"
      << l1 << ":\n";
  c->stmt_->accept(this);
  out << "  br label %" << l2 << "\n"
      << l2 << ":\n";
}

void CodeGen::visitCondElse(CondElse *c) {
  std::string cond = genExpr(c->expr_);
  std::string l1 = freshLabel(), l2 = freshLabel(), l3 = freshLabel();
  out << "  br i1 " << cond
      << ", label %" << l1 << ", label %" << l2 << "\n"
      << l1 << ":\n";
  c->stmt_1->accept(this);
  out << "  br label %" << l3 << "\n"
      << l2 << ":\n";
  c->stmt_2->accept(this);
  out << "  br label %" << l3 << "\n"
      << l3 << ":\n";
}

void CodeGen::visitWhile(While *w) {
  std::string l0 = freshLabel(), l1 = freshLabel(), l2 = freshLabel();
  out << "  br label %" << l0 << "\n";
  out << l0 << ":\n";
  std::string cond = genExpr(w->expr_);
  out << "  br i1 " << cond
      << ", label %" << l1 << ", label %" << l2 << "\n"
      << l1 << ":\n";
  w->stmt_->accept(this);
  out << "  br label %" << l0 << "\n"
      << l2 << ":\n";
}

void CodeGen::visitRet(Ret *r) {
  hasReturn = true;
  std::string v = genExpr(r->expr_);
  out << "  ret " << llvmType(currentRetType) << " " << v << "\n";
}

void CodeGen::visitVRet(VRet*) {
  hasReturn = true;
  out << "  ret void\n";
}

void CodeGen::visitSExp(SExp *s) {
  genExpr(s->expr_);  // side-effect only
}

std::string CodeGen::genExpr(Expr *x) {
    // integer literal
    if (auto l = dynamic_cast<ELitInt*>(x))
      return std::to_string(l->integer_);
    // double literal
    if (auto d = dynamic_cast<ELitDoub*>(x))
      return std::to_string(d->double_);
    // boolean
    if (dynamic_cast<ELitTrue*>(x))  return "1";
    if (dynamic_cast<ELitFalse*>(x)) return "0";
    // string literal
    if (auto s = dynamic_cast<EString*>(x))
      return addGlobalString(s->string_);
    // variable load
    if (auto v = dynamic_cast<EVar*>(x)) {
      auto &vi = env[v->ident_];
      std::string tmp = freshTmp();
      out << "  " << tmp
          << " = load " << llvmType(vi.ty)
          << ", " << llvmType(vi.ty) << "* " << vi.ptr
          << "\n";
      return tmp;
    }
    // function call
    if (auto a = dynamic_cast<EApp*>(x)) {
      // built-in printString
      if (a->ident_ == "printString") {
        std::string arg = genExpr(a->listexpr_->at(0));
        out << "  call void @printString(i8* " << arg << ")\n";
        return "";
      }
      // built-in readInt
      if (a->ident_ == "readInt") {
        std::string tmp = freshTmp();
        out << "  " << tmp << " = call i32 @readInt()\n";
        return tmp;
      }
      // user-defined call
      std::string tmp = freshTmp();
      out << "  " << tmp
          << " = call " << llvmType(exprType(x))
          << " @" << a->ident_ << "(";
      for (size_t i = 0; i < a->listexpr_->size(); ++i) {
        auto &args = *a->listexpr_;
        std::string v = genExpr(args[i]);
        out << llvmType(exprType(args[i])) << " " << v
            << (i+1 < args.size() ? ", " : "");
      }
      out << ")\n";
      return tmp;
    }
    // (other expression kinds go here…)
    return "";
  }
  
