
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "x" [(ELitInt 4)])] ), (CondElse [(EAnd (ERel (ELitInt 3) [LE] (EVar "x")) (EAnd (ERel (ELitInt 4) [NE] (ELitInt 2)) ELitTrue))] (BStmt [(Block [(SExp [(EApp "printBool" [ELitTrue] )] )] )]) (BStmt [(Block [(SExp [(EApp "printString" [(EString "apa")] )] )] )])), (SExp [(EApp "printBool" [(EOr (ERel ELitTrue [EQU] ELitTrue) (EApp "dontCallMe" [(ELitInt 1)] ))] )] ), (SExp [(EApp "printBool" [(EAnd (ERel (ELitDoub 4) [LTH] (Neg [(ELitDoub 50)])) (EApp "dontCallMe" [(ELitInt 2)] ))] )] ), (SExp [(EApp "printBool" [(EAnd (ERel (ELitInt 4) [EQU] (EVar "x")) (EAnd (ERel ELitTrue [EQU] (Not [ELitFalse])) ELitTrue))] )] ), (SExp [(EApp "printBool" [(EApp "implies" [ELitFalse, ELitFalse] )] )] ), (SExp [(EApp "printBool" [(EApp "implies" [ELitFalse, ELitTrue] )] )] ), (SExp [(EApp "printBool" [(EApp "implies" [ELitTrue, ELitFalse] )] )] ), (SExp [(EApp "printBool" [(EApp "implies" [ELitTrue, ELitTrue] )] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Bool] "dontCallMe" [(Argument [Int] "x")] [(Block [(SExp [(EApp "printInt" [(EVar "x")] )] ), (Ret [ELitTrue] )] )]), (FnDef [Void] "printBool" [(Argument [Bool] "b")] [(Block [(CondElse [(EVar "b")] (BStmt [(Block [(SExp [(EApp "printString" [(EString "true")] )] )] )]) (BStmt [(Block [(SExp [(EApp "printString" [(EString "false")] )] )] )])), VRet] )]), (FnDef [Bool] "implies" [(Argument [Bool] "x"), (Argument [Bool] "y")] [(Block [(Ret [(EOr (Not [(EVar "x")]) (ERel (EVar "x") [EQU] (EVar "y")))] )] )])])

[Linearized Tree]
int main ()
{
  int x = 4;
  if (3 <= x && 4 != 2 && true)
  {
    printBool (true);
  }
  else
  {
    printString ("apa");
  }
  printBool (true == true || dontCallMe (1));
  printBool (4 < - 50 && dontCallMe (2));
  printBool (4 == x && true == ! false && true);
  printBool (implies (false, false));
  printBool (implies (false, true));
  printBool (implies (true, false));
  printBool (implies (true, true));
  return 0;
}
boolean dontCallMe (int x)
{
  printInt (x);
  return true;
}
void printBool (boolean b)
{
  if (b)
  {
    printString ("true");
  }
  else
  {
    printString ("false");
  }
  return;
}
boolean implies (boolean x, boolean y)
{
  return ! x || x == y;
}


