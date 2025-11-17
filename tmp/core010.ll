
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "printInt" [(EApp "fac" [(ELitInt 5)] )] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Int] "fac" [(Argument [Int] "a")] [(Block [(Decl [Int] [(NoInit "r")] ), (Decl [Int] [(NoInit "n")] ), (Ass "r" [(ELitInt 1)] ), (Ass "n" [(EVar "a")] ), (While [(ERel (EVar "n") [GTH] (ELitInt 0))] [(BStmt [(Block [(Ass "r" [(EMul (EVar "r") [Times] (EVar "n"))] ), (Ass "n" [(EAdd (EVar "n") [Minus] (ELitInt 1))] )] )])]), (Ret [(EVar "r")] )] )])])

[Linearized Tree]
int main ()
{
  printInt (fac (5));
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


