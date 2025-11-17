
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(NoInit "x")] ), (Decl [Int] [(Init "y" [(ELitInt 56)])] ), (CondElse [(ERel (EAdd (EVar "y") [Plus] (ELitInt 45)) [LE] (ELitInt 2))] (BStmt [(Block [(Ass "x" [(ELitInt 1)] )] )]) (BStmt [(Block [(Ass "x" [(ELitInt 2)] )] )])), (SExp [(EApp "printInt" [(EVar "x")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x;
  int y = 56;
  if (y + 45 <= 2)
  {
    x = 1;
  }
  else
  {
    x = 2;
  }
  printInt (x);
  return 0;
}


