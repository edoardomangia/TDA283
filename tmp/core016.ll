
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "y" [(ELitInt 17)])] ), (While [(ERel (EVar "y") [GTH] (ELitInt 0))] [(Ass "y" [(EAdd (EVar "y") [Minus] (ELitInt 2))] )]), (CondElse [(ERel (EVar "y") [LTH] (ELitInt 0))] (BStmt [(Block [(SExp [(EApp "printInt" [(ELitInt 0)] )] ), (Ret [(ELitInt 0)] )] )]) (BStmt [(Block [(SExp [(EApp "printInt" [(ELitInt 1)] )] ), (Ret [(ELitInt 0)] )] )]))] )])])

[Linearized Tree]
int main ()
{
  int y = 17;
  while (y > 0) y = y - 2;
  if (y < 0)
  {
    printInt (0);
    return 0;
  }
  else
  {
    printInt (1);
    return 0;
  }
}


