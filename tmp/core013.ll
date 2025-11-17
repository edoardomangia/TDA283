
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "printString" [(EString "&&")] )] ), (SExp [(EApp "printBool" [(EAnd (EApp "test" [(Neg [(ELitInt 1)])] ) (EApp "test" [(ELitInt 0)] ))] )] ), (SExp [(EApp "printBool" [(EAnd (EApp "test" [(Neg [(ELitInt 2)])] ) (EApp "test" [(ELitInt 1)] ))] )] ), (SExp [(EApp "printBool" [(EAnd (EApp "test" [(ELitInt 3)] ) (EApp "test" [(Neg [(ELitInt 5)])] ))] )] ), (SExp [(EApp "printBool" [(EAnd (EApp "test" [(ELitInt 234234)] ) (EApp "test" [(ELitInt 21321)] ))] )] ), (SExp [(EApp "printString" [(EString "||")] )] ), (SExp [(EApp "printBool" [(EOr (EApp "test" [(Neg [(ELitInt 1)])] ) (EApp "test" [(ELitInt 0)] ))] )] ), (SExp [(EApp "printBool" [(EOr (EApp "test" [(Neg [(ELitInt 2)])] ) (EApp "test" [(ELitInt 1)] ))] )] ), (SExp [(EApp "printBool" [(EOr (EApp "test" [(ELitInt 3)] ) (EApp "test" [(Neg [(ELitInt 5)])] ))] )] ), (SExp [(EApp "printBool" [(EOr (EApp "test" [(ELitInt 234234)] ) (EApp "test" [(ELitInt 21321)] ))] )] ), (SExp [(EApp "printString" [(EString "!")] )] ), (SExp [(EApp "printBool" [ELitTrue] )] ), (SExp [(EApp "printBool" [ELitFalse] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Void] "printBool" [(Argument [Bool] "b")] [(Block [(CondElse [(Not [(EVar "b")])] (BStmt [(Block [(SExp [(EApp "printString" [(EString "false")] )] )] )]) (BStmt [(Block [(SExp [(EApp "printString" [(EString "true")] )] )] )])), VRet] )]), (FnDef [Bool] "test" [(Argument [Int] "i")] [(Block [(SExp [(EApp "printInt" [(EVar "i")] )] ), (Ret [(ERel (EVar "i") [GTH] (ELitInt 0))] )] )])])

[Linearized Tree]
int main ()
{
  printString ("&&");
  printBool (test (- 1) && test (0));
  printBool (test (- 2) && test (1));
  printBool (test (3) && test (- 5));
  printBool (test (234234) && test (21321));
  printString ("||");
  printBool (test (- 1) || test (0));
  printBool (test (- 2) || test (1));
  printBool (test (3) || test (- 5));
  printBool (test (234234) || test (21321));
  printString ("!");
  printBool (true);
  printBool (false);
  return 0;
}
void printBool (boolean b)
{
  if (! b)
  {
    printString ("false");
  }
  else
  {
    printString ("true");
  }
  return;
}
boolean test (int i)
{
  printInt (i);
  return i > 0;
}


