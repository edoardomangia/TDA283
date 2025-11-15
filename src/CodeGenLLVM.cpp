#include "CodeGenLLVM.H"
// #include "Skeleton.H"   // BNFC visitor base
#include <map>
#include <vector>
#include <string>
#include <sstream>
#include <iostream>
#include <cassert>

// ---------- Small helpers ----------
static std::string escapeForC(const std::string& s, int& outLen) {
    // Encode to LLVM c"..." with \xx escapes and trailing \00
    std::ostringstream o;
    o << "c\"";
    outLen = 0;
    for (size_t i = 0; i < s.size(); ++i) {
        unsigned char ch = static_cast<unsigned char>(s[i]);
        switch (ch) {
            case '\n': o << "\\0A"; outLen += 1; break;
            case '\t': o << "\\09"; outLen += 1; break;
            case '\"': o << "\\22"; outLen += 1; break;
            case '\\': o << "\\5C"; outLen += 1; break;
            default:
                if (ch >= 32 && ch <= 126) { o << ch; outLen += 1; }
                else { o << '\\';
                       static const char* hex = "0123456789ABCDEF";
                       o << hex[(ch >> 4) & 0xF] << hex[ch & 0xF];
                       outLen += 1;
                }
        }
    }
    o << "\\00\"";
    outLen += 1; // for NUL
    return o.str();
}

struct RVal {            // the result of an expression
    std::string v;       // LLVM SSA name or immediate
    std::string ty;      // "i32" | "double" | "i1" | "i8*"
};

struct VarInfo {
    std::string alloca;  // e.g. "%x"
    std::string ty;      // LLVM type string
};

struct FnCtx {
    std::ostringstream body;        // function body text
    std::vector<std::map<std::string, VarInfo>> scopes;
    std::string retTy;
    int tmp = 0;
    int lab = 0;
    bool blockTerminated = false;

    std::string t()  { return "%t" + std::to_string(tmp++); }
    std::string L()  { return "L" + std::to_string(lab++); }
    void emit(const std::string& s) {
        if (!blockTerminated) body << s << "\n";
    }
};

struct CodeGen {
    std::ostringstream decls;       // runtime + prototypes
    std::ostringstream globals;     // string constants
    std::ostringstream funs;        // all functions
    std::map<std::string, std::string> strSym; // lit -> @.str.N
    int strCount = 0;

    FnCtx* F = nullptr;             // current function


    // ---- Decls for the runtime ----
    void emitRuntimeDecls() {
        decls <<
        "declare void @printInt(i32)\n"
        "declare void @printDouble(double)\n"
        "declare void @printString(i8*)\n"
        "declare i32 @readInt()\n"
        "declare double @readDouble()\n";
    }

    // ---- Strings ----
    std::string internString(const std::string& lit) {
        auto it = strSym.find(lit);
        if (it != strSym.end()) return it->second;
        std::string g = "@.str." + std::to_string(strCount++);
        int L = 0;
        std::string enc = escapeForC(lit, L);
        globals << g << " = private constant [" << L << " x i8] " << enc << "\n";
        strSym[lit] = g;
        return g;
    }

    // ---- Scopes ----
    void pushScope() { F->scopes.emplace_back(); }
    void popScope()  { F->scopes.pop_back(); }
    VarInfo* lookup(const std::string& name) {
        size_t i = F->scopes.size();
        while (i-- > 0) {
            auto it = F->scopes[i].find(name);
            if (it != F->scopes[i].end()) 
                return &it->second;
        }
        return nullptr;
    }
    void bind(const std::string& name, const VarInfo& vi) {
        F->scopes.back()[name] = vi;
    }

    // ---- Type mapping (Absyn types -> LLVM) ----
    std::string tyToLlvm(Type* ty) {
        if (dynamic_cast<Int*>(ty))    return "i32";
        if (dynamic_cast<Doub*>(ty))   return "double";
        if (dynamic_cast<Bool*>(ty))   return "i1";
        if (dynamic_cast<Void*>(ty))   return "void";
        // if (dynamic_cast<Type_string*>(ty)) return "i8*";
        // Extend for arrays if you implement extensions.
        assert(false && "unknown type");
        return "void";
    }

    // ---- Expr codegen (returns SSA + type) ----
RVal genExpr(Expr* e) {
    // literals
    if (dynamic_cast<ELitTrue*>(e))   return {"1", "i1"};
    if (dynamic_cast<ELitFalse*>(e))  return {"0", "i1"};
    if (auto E = dynamic_cast<ELitInt*>(e))    return {std::to_string(E->integer_), "i32"};
    if (auto E = dynamic_cast<ELitDoub*>(e)) {
        std::ostringstream o;
        o.setf(std::ios::fixed);
        o << E->double_;
        return {o.str(), "double"};
    }

    // variable and string literal
    if (auto E = dynamic_cast<EVar*>(e)) {
        VarInfo* vi = lookup(E->ident_);
        assert(vi);
        std::string r = F->t();
        F->emit("  " + r + " = load " + vi->ty + ", " + vi->ty + "* " + vi->alloca);
        return {r, vi->ty};
    }

    if (auto E = dynamic_cast<EString*>(e)) {
        std::string g = internString(E->string_);
        int L = 0;
        (void)escapeForC(E->string_, L); // recompute length
        std::string p = F->t();
        F->emit("  " + p + " = getelementptr [" + std::to_string(L) + " x i8], [" +
                std::to_string(L) + " x i8]* " + g + ", i32 0, i32 0");
        return {p, "i8*"};
    }

    // prefix ops
    if (auto E = dynamic_cast<Neg*>(e)) {
        RVal v = genExpr(E->expr_);
        std::string r = F->t();
        if (v.ty == "i32") {
            F->emit("  " + r + " = sub i32 0, " + v.v);
        } else {
            F->emit("  " + r + " = fsub double 0.0, " + v.v);
        }
        return {r, v.ty};
    }

    if (auto E = dynamic_cast<Not*>(e)) {
        RVal v = genExpr(E->expr_);
        assert(v.ty == "i1");
        std::string r = F->t();
        F->emit("  " + r + " = xor i1 " + v.v + ", 1");
        return {r, "i1"};
    }

    // addition / subtraction
    if (auto E = dynamic_cast<EAdd*>(e)) {
        RVal l = genExpr(E->expr_1), r = genExpr(E->expr_2);
        std::string t = l.ty;
        std::string dst = F->t();
        if (t == "i32") {
            if (dynamic_cast<Plus*>(E->addop_))
                F->emit("  " + dst + " = add i32 " + l.v + ", " + r.v);
            else
                F->emit("  " + dst + " = sub i32 " + l.v + ", " + r.v);
        } else {
            if (dynamic_cast<Plus*>(E->addop_))
                F->emit("  " + dst + " = fadd double " + l.v + ", " + r.v);
            else
                F->emit("  " + dst + " = fsub double " + l.v + ", " + r.v);
        }
        return {dst, t};
    }

    // multiplication / division / mod
    if (auto E = dynamic_cast<EMul*>(e)) {
        RVal l = genExpr(E->expr_1), r = genExpr(E->expr_2);
        std::string t = l.ty;
        std::string dst = F->t();
        if (t == "i32") {
            if (dynamic_cast<Times*>(E->mulop_))
                F->emit("  " + dst + " = mul i32 " + l.v + ", " + r.v);
            else if (dynamic_cast<Div*>(E->mulop_))
                F->emit("  " + dst + " = sdiv i32 " + l.v + ", " + r.v);
            else
                F->emit("  " + dst + " = srem i32 " + l.v + ", " + r.v);
        } else {
            if (dynamic_cast<Times*>(E->mulop_))
                F->emit("  " + dst + " = fmul double " + l.v + ", " + r.v);
            else
                F->emit("  " + dst + " = fdiv double " + l.v + ", " + r.v);
        }
        return {dst, t};
    }

    // relational ops (EQU, NE, LTH, LE, GTH, GE)
    if (auto E = dynamic_cast<ERel*>(e)) {
        RVal l = genExpr(E->expr_1), r = genExpr(E->expr_2);
        std::string dst = F->t();
        if (dynamic_cast<EQU*>(E->relop_)) {
            if (l.ty == "double")
                F->emit("  " + dst + " = fcmp oeq double " + l.v + ", " + r.v);
            else
                F->emit("  " + dst + " = icmp eq " + l.ty + " " + l.v + ", " + r.v);
        } else if (dynamic_cast<NE*>(E->relop_)) {
            if (l.ty == "double")
                F->emit("  " + dst + " = fcmp one double " + l.v + ", " + r.v);
            else
                F->emit("  " + dst + " = icmp ne " + l.ty + " " + l.v + ", " + r.v);
        } else if (dynamic_cast<LTH*>(E->relop_)) {
            if (l.ty == "double")
                F->emit("  " + dst + " = fcmp olt double " + l.v + ", " + r.v);
            else
                F->emit("  " + dst + " = icmp slt i32 " + l.v + ", " + r.v);
        } else if (dynamic_cast<LE*>(E->relop_)) {
            if (l.ty == "double")
                F->emit("  " + dst + " = fcmp ole double " + l.v + ", " + r.v);
            else
                F->emit("  " + dst + " = icmp sle i32 " + l.v + ", " + r.v);
        } else if (dynamic_cast<GTH*>(E->relop_)) {
            if (l.ty == "double")
                F->emit("  " + dst + " = fcmp ogt double " + l.v + ", " + r.v);
            else
                F->emit("  " + dst + " = icmp sgt i32 " + l.v + ", " + r.v);
        } else { // GE
            if (l.ty == "double")
                F->emit("  " + dst + " = fcmp oge double " + l.v + ", " + r.v);
            else
                F->emit("  " + dst + " = icmp sge i32 " + l.v + ", " + r.v);
        }
        return {dst, "i1"};
    }

    // TODO: proper short-circuit && and || (your current PHI labels are wrong).
    // For the basic milestone, you can skip EAnd/EOr if not required or leave as is
    // and fix later once everything compiles.

    // function calls
    if (auto E = dynamic_cast<EApp*>(e)) {
        std::vector<RVal> args;
        for (Expr* x : *E->listexpr_) args.push_back(genExpr(x));

        if (E->ident_ == "printInt") {
            assert(args.size() == 1 && args[0].ty == "i32");
            F->emit("  call void @printInt(i32 " + args[0].v + ")");
            return {"", "void"};
        }
        if (E->ident_ == "printDouble") {
            assert(args.size() == 1 && args[0].ty == "double");
            F->emit("  call void @printDouble(double " + args[0].v + ")");
            return {"", "void"};
        }
        if (E->ident_ == "printString") {
            assert(args.size() == 1 && args[0].ty == "i8*");
            F->emit("  call void @printString(i8* " + args[0].v + ")");
            return {"", "void"};
        }
        if (E->ident_ == "readInt") {
            assert(args.empty());
            std::string r = F->t();
            F->emit("  " + r + " = call i32 @readInt()");
            return {r, "i32"};
        }
        if (E->ident_ == "readDouble") {
            assert(args.empty());
            std::string r = F->t();
            F->emit("  " + r + " = call double @readDouble()");
            return {r, "double"};
        }

        // user-defined functions
        std::ostringstream plist;
        for (size_t i = 0; i < args.size(); ++i) {
            if (i) plist << ", ";
            plist << args[i].ty << " " << args[i].v;
        }

        auto it = fnSigs.find(E->ident_);
        std::string retTy = (it == fnSigs.end() ? "i32" : it->second);
        if (retTy == "void") {
            F->emit("  call void @" + E->ident_ + "(" + plist.str() + ")");
            return {"", "void"};
        } else {
            std::string r = F->t();
            F->emit("  " + r + " = call " + retTy + " @" + E->ident_ + "(" + plist.str() + ")");
            return {r, retTy};
        }
    }

    assert(false && "unhandled Expr variant");
    return {"", "void"};
}

    // ---- Statements ----
void genStmt(Stmt* s) {
    // expression statement
    if (auto S = dynamic_cast<SExp*>(s)) {
        (void)genExpr(S->expr_);
        return;
    }

    // return with value
    if (auto S = dynamic_cast<Ret*>(s)) {
        RVal v = genExpr(S->expr_);
        F->emit("  ret " + v.ty + " " + v.v);
        F->blockTerminated = true;
        return;
    }

    // void return
    if (dynamic_cast<VRet*>(s)) {
        F->emit("  ret void");
        F->blockTerminated = true;
        return;
    }

    // declaration (possibly with initialization)
    if (auto S = dynamic_cast<Decl*>(s)) {
        std::string T = tyToLlvm(S->type_);
        for (Item* it : *S->listitem_) {
            if (auto ni = dynamic_cast<NoInit*>(it)) {
                std::string a = "%" + ni->ident_;
                F->emit("  " + a + " = alloca " + T);
                if (T == "i32")
                    F->emit("  store i32 0, i32* " + a);
                else if (T == "double")
                    F->emit("  store double 0.0, double* " + a);
                else if (T == "i1")
                    F->emit("  store i1 0, i1* " + a);
                bind(ni->ident_, VarInfo{a, T});
            } else if (auto in = dynamic_cast<Init*>(it)) {
                std::string a = "%" + in->ident_;
                F->emit("  " + a + " = alloca " + T);
                RVal v = genExpr(in->expr_);
                F->emit("  store " + T + " " + v.v + ", " + T + "* " + a);
                bind(in->ident_, VarInfo{a, T});
            }
        }
        return;
    }

    // simple assignment
    if (auto S = dynamic_cast<Ass*>(s)) {
        VarInfo* vi = lookup(S->ident_);
        assert(vi);
        RVal v = genExpr(S->expr_);
        F->emit("  store " + vi->ty + " " + v.v + ", " + vi->ty + "* " + vi->alloca);
        return;
    }

    // i++
    if (auto S = dynamic_cast<Incr*>(s)) {
        VarInfo* vi = lookup(S->ident_);
        assert(vi && vi->ty == "i32");
        std::string t = F->t();
        F->emit("  " + t + " = load i32, i32* " + vi->alloca);
        std::string u = F->t();
        F->emit("  " + u + " = add i32 " + t + ", 1");
        F->emit("  store i32 " + u + ", i32* " + vi->alloca);
        return;
    }

    // i--
    if (auto S = dynamic_cast<Decr*>(s)) {
        VarInfo* vi = lookup(S->ident_);
        assert(vi && vi->ty == "i32");
        std::string t = F->t();
        F->emit("  " + t + " = load i32, i32* " + vi->alloca);
        std::string u = F->t();
        F->emit("  " + u + " = sub i32 " + t + ", 1");
        F->emit("  store i32 " + u + ", i32* " + vi->alloca);
        return;
    }

    // if (e) stmt;
    if (auto S = dynamic_cast<Cond*>(s)) {
        std::string Lt = F->L(), Le = F->L();
        RVal c = genExpr(S->expr_);
        assert(c.ty == "i1");

        F->emit("  br i1 " + c.v + ", label %" + Lt + ", label %" + Le);

        // then
        F->emit(Lt + ":");
        pushScope();
        genStmt(S->stmt_);
        popScope();
        if (!F->blockTerminated)
            F->emit("  br label %" + Le);
        F->blockTerminated = false;

        // end
        F->emit(Le + ":");
        return;
    }

    // if (e) stmt1 else stmt2;
    if (auto S = dynamic_cast<CondElse*>(s)) {
        std::string Lt = F->L(), Lf = F->L(), Le = F->L();
        RVal c = genExpr(S->expr_);
        assert(c.ty == "i1");

        F->emit("  br i1 " + c.v + ", label %" + Lt + ", label %" + Lf);

        // then
        F->emit(Lt + ":");
        pushScope();
        genStmt(S->stmt_1);
        popScope();
        if (!F->blockTerminated)
            F->emit("  br label %" + Le);
        F->blockTerminated = false;

        // else
        F->emit(Lf + ":");
        pushScope();
        genStmt(S->stmt_2);
        popScope();
        if (!F->blockTerminated)
            F->emit("  br label %" + Le);
        F->blockTerminated = false;

        // end
        F->emit(Le + ":");
        return;
    }

    // while (e) stmt;
    if (auto S = dynamic_cast<While*>(s)) {
        std::string Lcond = F->L(), Lbody = F->L(), Lend = F->L();

        // jump to cond
        F->emit("  br label %" + Lcond);

        // cond
        F->emit(Lcond + ":");
        RVal c = genExpr(S->expr_);
        assert(c.ty == "i1");
        F->emit("  br i1 " + c.v + ", label %" + Lbody + ", label %" + Lend);

        // body
        F->emit(Lbody + ":");
        pushScope();
        genStmt(S->stmt_);
        popScope();
        if (!F->blockTerminated)
            F->emit("  br label %" + Lcond);
        F->blockTerminated = false;

        // end
        F->emit(Lend + ":");
        return;
    }

    // block statement { ... }
    if (auto S = dynamic_cast<BStmt*>(s)) {
        auto B = dynamic_cast<Block*>(S->blk_);
        assert(B);
        pushScope();
        for (Stmt* x : *B->liststmt_)
            genStmt(x);
        popScope();
        return;
    }

    // empty
    if (dynamic_cast<Empty*>(s)) {
        return;
    }

    assert(false && "unhandled Stmt variant");
}





    // ---- Function signatures (for user-defined calls) ----
    std::map<std::string, std::string> fnSigs; // name -> retTy
    std::map<std::string, std::vector<std::string>> fnParamTys;

    // Collect signatures first so calls know types
void collectSig(FnDef* f) {
    fnSigs[f->ident_] = tyToLlvm(f->type_);
    std::vector<std::string> pts;
    if (f->listarg_) {
        for (Arg* a : *f->listarg_) {
            auto A = dynamic_cast<Argument*>(a);
            assert(A);
            pts.push_back(tyToLlvm(A->type_));
        }
    }
    fnParamTys[f->ident_] = pts;
}


void emitFunction(FnDef* f) {
    FnCtx local; F = &local;
    local.retTy = tyToLlvm(f->type_);
    local.body << "define " << local.retTy << " @" << f->ident_ << "(";

    std::vector<std::string> formals; std::vector<std::string> ptys;
    if (f->listarg_) {
        bool first=true;
        for (Arg* a : *f->listarg_) {
            auto A = dynamic_cast<Argument*>(a);
            std::string T = tyToLlvm(A->type_);
            std::string pname = "%__p__" + A->ident_;
            if (!first)
                local.body << ", "; 
            first=false;
            local.body << T << " " << pname;
            formals.push_back(A->ident_); ptys.push_back(T);
        }
    }
    local.body << ") {\nentry:\n";
    pushScope();
    for (size_t i=0;i<formals.size();++i) {
        std::string a = "%" + formals[i];
        std::string T = ptys[i];
        local.emit("  " + a + " = alloca " + T);
        local.emit("  store " + T + " %__p__" + formals[i] + ", " + T + "* " + a);
        bind(formals[i], VarInfo{a, T});
    }

    // Body: f->blk_ is a Block
    auto B = dynamic_cast<Block*>(f->blk_); assert(B);
    for (Stmt* x : *B->liststmt_) genStmt(x);

    if (local.retTy=="void" && !local.blockTerminated) local.emit("  ret void");
    local.body << "}\n\n";
    funs << local.body.str();
    popScope();
    F = nullptr;
}

void gen(Program* p, std::ostream& out) {
        emitRuntimeDecls();
        // pass 1: collect signatures
        for (TopDef* td : *p->listtopdef_) {
            if (auto fn = dynamic_cast<FnDef*>(td))
                collectSig(fn);
        }
        // pass 2: emit bodies
        for (TopDef* td : *p->listtopdef_) {
            if (auto fn = dynamic_cast<FnDef*>(td))
                emitFunction(fn);
        }
        out << decls.str() << "\n" << globals.str();
        if (strCount) out << "\n";
        out << funs.str();
    }
};
// ---------- Public API ----------
void generateLLVM(Program* prog, std::ostream& out) {
    CodeGen cg;
    cg.gen(prog, out);
}

