
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Cond [(EOr (ERel (ELitInt 1) [LTH] (ELitInt 6)) (EApp "no" [] ))] [(BStmt [(Block [(SExp [(EApp "printInt" [(ELitInt 1)] )] )] )])]), (Cond [(EAnd (ERel (EAdd (ELitInt 2) [Plus] (ELitInt 2)) [NE] (ELitInt 4)) (EApp "no" [] ))] [(BStmt [(Block [(SExp [(EApp "printInt" [(ELitInt 2)] )] )] )])]), (Cond [(EOr (ERel (ELitInt 5) [LTH] (ELitInt 5)) (EApp "yes" [] ))] [(BStmt [(Block [(SExp [(EApp "printInt" [(ELitInt 3)] )] )] )])]), (Cond [(EAnd (ERel (ELitDoub 0.4) [GE] (ELitDoub 0.3)) (EApp "yes" [] ))] [(BStmt [(Block [(SExp [(EApp "printInt" [(ELitInt 4)] )] )] )])]), (Ret [(ELitInt 0)] )] )]), (FnDef [Bool] "no" [] [(Block [(SExp [(EApp "printString" [(EString "no")] )] ), (Ret [ELitFalse] )] )]), (FnDef [Bool] "yes" [] [(Block [(SExp [(EApp "printString" [(EString "yes")] )] ), (Ret [ELitTrue] )] )])])

[Linearized Tree]
int main ()
{
  if (1 < 6 || no ())
  {
    printInt (1);
  }
  if (2 + 2 != 4 && no ())
  {
    printInt (2);
  }
  if (5 < 5 || yes ())
  {
    printInt (3);
  }
  if (0.4 >= 0.3 && yes ())
  {
    printInt (4);
  }
  return 0;
}
boolean no ()
{
  printString ("no");
  return false;
}
boolean yes ()
{
  printString ("yes");
  return true;
}


