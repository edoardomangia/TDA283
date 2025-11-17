
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "i" [(ELitInt 78)])] ), (BStmt [(Block [(Decl [Int] [(Init "i" [(ELitInt 1)])] ), (SExp [(EApp "printInt" [(EVar "i")] )] )] )]), (SExp [(EApp "printInt" [(EVar "i")] )] ), (While [(ERel (EVar "i") [GTH] (ELitInt 76))] [(BStmt [(Block [(Decr "i" ), (SExp [(EApp "printInt" [(EVar "i")] )] ), (Decl [Int] [(Init "i" [(EAdd (EVar "i") [Plus] (ELitInt 7))])] ), (SExp [(EApp "printInt" [(EVar "i")] )] )] )])]), (SExp [(EApp "printInt" [(EVar "i")] )] ), (CondElse [(ERel (EVar "i") [GTH] (ELitInt 4))] (BStmt [(Block [(Decl [Int] [(Init "i" [(ELitInt 4)])] ), (SExp [(EApp "printInt" [(EVar "i")] )] )] )]) (BStmt [(Block [(SExp [(EApp "printString" [(EString "foo")] )] )] )])), (SExp [(EApp "printInt" [(EVar "i")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int i = 78;
  {
    int i = 1;
    printInt (i);
  }
  printInt (i);
  while (i > 76)
  {
    i --;
    printInt (i);
    int i = i + 7;
    printInt (i);
  }
  printInt (i);
  if (i > 4)
  {
    int i = 4;
    printInt (i);
  }
  else
  {
    printString ("foo");
  }
  printInt (i);
  return 0;
}


