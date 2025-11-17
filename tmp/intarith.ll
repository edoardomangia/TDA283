
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(NoInit "x")] ), (Ass "x" [(ELitInt 5)] ), (While [(ERel (EVar "x") [GTH] (ELitInt 0))] [(BStmt [(Block [(SExp [(EApp "printInt" [(EVar "x")] )] ), (Decr "x" )] )])]), (SExp [(EApp "printInt" [(EVar "x")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x;
  x = 5;
  while (x > 0)
  {
    printInt (x);
    x --;
  }
  printInt (x);
  return 0;
}


