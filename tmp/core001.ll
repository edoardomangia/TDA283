
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "printInt" [(EApp "fac" [(ELitInt 10)] )] )] ), (SExp [(EApp "printInt" [(EApp "rfac" [(ELitInt 10)] )] )] ), (SExp [(EApp "printInt" [(EApp "mfac" [(ELitInt 10)] )] )] ), (SExp [(EApp "printInt" [(EApp "ifac" [(ELitInt 10)] )] )] ), (Decl [Doub] [(NoInit "r")] ), (BStmt [(Block [(Decl [Int] [(Init "n" [(ELitInt 10)])] ), (Decl [Int] [(Init "r" [(ELitInt 1)])] ), (While [(ERel (EVar "n") [GTH] (ELitInt 0))] [(BStmt [(Block [(Ass "r" [(EMul (EVar "r") [Times] (EVar "n"))] ), (Decr "n" )] )])]), (SExp [(EApp "printInt" [(EVar "r")] )] )] )]), (SExp [(EApp "printDouble" [(EApp "dfac" [(ELitDoub 10)] )] )] ), (SExp [(EApp "printString" [(EString "hello */")] )] ), (SExp [(EApp "printString" [(EString "/* world")] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Int] "fac" [(Argument [Int] "a")] [(Block [(Decl [Int] [(NoInit "r")] ), (Decl [Int] [(NoInit "n")] ), (Ass "r" [(ELitInt 1)] ), (Ass "n" [(EVar "a")] ), (While [(ERel (EVar "n") [GTH] (ELitInt 0))] [(BStmt [(Block [(Ass "r" [(EMul (EVar "r") [Times] (EVar "n"))] ), (Ass "n" [(EAdd (EVar "n") [Minus] (ELitInt 1))] )] )])]), (Ret [(EVar "r")] )] )]), (FnDef [Int] "rfac" [(Argument [Int] "n")] [(Block [(CondElse [(ERel (EVar "n") [EQU] (ELitInt 0))] (Ret [(ELitInt 1)] ) (Ret [(EMul (EVar "n") [Times] (EApp "rfac" [(EAdd (EVar "n") [Minus] (ELitInt 1))] ))] ))] )]), (FnDef [Int] "mfac" [(Argument [Int] "n")] [(Block [(CondElse [(ERel (EVar "n") [EQU] (ELitInt 0))] (Ret [(ELitInt 1)] ) (Ret [(EMul (EVar "n") [Times] (EApp "nfac" [(EAdd (EVar "n") [Minus] (ELitInt 1))] ))] ))] )]), (FnDef [Int] "nfac" [(Argument [Int] "n")] [(Block [(CondElse [(ERel (EVar "n") [NE] (ELitInt 0))] (Ret [(EMul (EApp "mfac" [(EAdd (EVar "n") [Minus] (ELitInt 1))] ) [Times] (EVar "n"))] ) (Ret [(ELitInt 1)] ))] )]), (FnDef [Doub] "dfac" [(Argument [Doub] "n")] [(Block [(CondElse [(ERel (EVar "n") [EQU] (ELitDoub 0))] (Ret [(ELitDoub 1)] ) (Ret [(EMul (EVar "n") [Times] (EApp "dfac" [(EAdd (EVar "n") [Minus] (ELitDoub 1))] ))] ))] )]), (FnDef [Int] "ifac" [(Argument [Int] "n")] [(Block [(Ret [(EApp "ifac2f" [(ELitInt 1), (EVar "n")] )] )] )]), (FnDef [Int] "ifac2f" [(Argument [Int] "l"), (Argument [Int] "h")] [(Block [(Cond [(ERel (EVar "l") [EQU] (EVar "h"))] [(Ret [(EVar "l")] )]), (Cond [(ERel (EVar "l") [GTH] (EVar "h"))] [(Ret [(ELitInt 1)] )]), (Decl [Int] [(NoInit "m")] ), (Ass "m" [(EMul (EAdd (EVar "l") [Plus] (EVar "h")) [Div] (ELitInt 2))] ), (Ret [(EMul (EApp "ifac2f" [(EVar "l"), (EVar "m")] ) [Times] (EApp "ifac2f" [(EAdd (EVar "m") [Plus] (ELitInt 1)), (EVar "h")] ))] )] )])])

[Linearized Tree]
int main ()
{
  printInt (fac (10));
  printInt (rfac (10));
  printInt (mfac (10));
  printInt (ifac (10));
  double r;
  {
    int n = 10;
    int r = 1;
    while (n > 0)
    {
      r = r * n;
      n --;
    }
    printInt (r);
  }
  printDouble (dfac (10));
  printString ("hello */");
  printString ("/* world");
  return 0;
}
int fac (int a)
{
  int r;
  int n;
  r = 1;
  n = a;
  while (n > 0)
  {
    r = r * n;
    n = n - 1;
  }
  return r;
}
int rfac (int n)
{
  if (n == 0) return 1;
  else return n * rfac (n - 1);
}
int mfac (int n)
{
  if (n == 0) return 1;
  else return n * nfac (n - 1);
}
int nfac (int n)
{
  if (n != 0) return mfac (n - 1) * n;
  else return 1;
}
double dfac (double n)
{
  if (n == 0) return 1;
  else return n * dfac (n - 1);
}
int ifac (int n)
{
  return ifac2f (1, n);
}
int ifac2f (int l, int h)
{
  if (l == h) return l;
  if (l > h) return 1;
  int m;
  m = (l + h) / 2;
  return ifac2f (l, m) * ifac2f (m + 1, h);
}


