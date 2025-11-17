
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Doub] [(Init "y" [(EApp "readDouble" [] )])] ), (SExp [(EApp "printDouble" [(ELitDoub 101325)] )] ), (SExp [(EApp "printDouble" [(EVar "y")] )] ), (Cond [(EAnd (ERel (ELitDoub 101325) [EQU] (EVar "y")) (EAnd (ERel (ELitDoub 101325) [LE] (EVar "y")) (ERel (ELitDoub 101325) [GE] (EVar "y"))))] [(BStmt [(Block [(SExp [(EApp "printInt" [(ELitInt 1)] )] )] )])]), (CondElse [(EAnd (ERel (ELitDoub 1325) [GTH] (EVar "y")) (EAnd (ERel (EVar "y") [LTH] (ELitDoub 1325)) (ERel (EVar "y") [NE] (ELitDoub 1325))))] (BStmt [(Block [] )]) (BStmt [(Block [(SExp [(EApp "printInt" [(ELitInt 2)] )] )] )])), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  double y = readDouble ();
  printDouble (101325);
  printDouble (y);
  if (101325 == y && 101325 <= y && 101325 >= y)
  {
    printInt (1);
  }
  if (1325 > y && y < 1325 && y != 1325)
  {
  }
  else
  {
    printInt (2);
  }
  return 0;
}


