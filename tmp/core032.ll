
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "foo" [(Argument [Int] "n")] [(Block [(Cond [(ERel (EVar "n") [LTH] (ELitInt 100))] [(BStmt [(Block [(Ass "n" [(EApp "foo" [(EApp "foo" [(EAdd (EVar "n") [Plus] (ELitInt 11))] )] )] )] )])]), (Ret [(EVar "n")] )] )]), (FnDef [Int] "main" [] [(Block [(SExp [(EApp "printInt" [(EApp "foo" [(ELitInt 1)] )] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int foo (int n)
{
  if (n < 100)
  {
    n = foo (foo (n + 11));
  }
  return n;
}
int main ()
{
  printInt (foo (1));
  return 0;
}


