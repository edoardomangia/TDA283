
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(NoInit "x")] ), (BStmt [(Block [(Decl [Int] [(Init "x" [(ELitInt 10)])] ), (While [(ERel (EVar "x") [GTH] (ELitInt 0))] [(BStmt [(Block [(SExp [(EApp "printInt" [(EVar "x")] )] ), (Decr "x" )] )])])] )]), (SExp [(EApp "printInt" [(EVar "x")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x;
  {
    int x = 10;
    while (x > 0)
    {
      printInt (x);
      x --;
    }
  }
  printInt (x);
  return 0;
}


