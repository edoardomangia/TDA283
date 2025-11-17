
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "x" [(ELitInt 56)])] ), (Decl [Int] [(Init "y" [(Neg [(ELitInt 23)])])] ), (SExp [(EApp "printInt" [(EAdd (EVar "x") [Plus] (EVar "y"))] )] ), (SExp [(EApp "printInt" [(EAdd (EVar "x") [Minus] (EVar "y"))] )] ), (SExp [(EApp "printInt" [(EMul (EVar "x") [Times] (EVar "y"))] )] ), (SExp [(EApp "printInt" [(EMul (ELitInt 45) [Div] (ELitInt 2))] )] ), (SExp [(EApp "printInt" [(EMul (ELitInt 78) [Mod] (ELitInt 3))] )] ), (Decl [Doub] [(Init "z" [(Neg [(ELitDoub 9.3)])])] ), (Decl [Doub] [(Init "w" [(ELitDoub 5.1)])] ), (SExp [(EApp "printBool" [(ERel (EAdd (EVar "z") [Plus] (EVar "w")) [GTH] (EAdd (EVar "z") [Minus] (EVar "w")))] )] ), (SExp [(EApp "printBool" [(ERel (EMul (EVar "z") [Div] (EVar "w")) [LE] (EMul (EVar "z") [Times] (EVar "w")))] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Void] "printBool" [(Argument [Bool] "b")] [(Block [(CondElse [(EVar "b")] (BStmt [(Block [(SExp [(EApp "printString" [(EString "true")] )] ), VRet] )]) (BStmt [(Block [(SExp [(EApp "printString" [(EString "false")] )] ), VRet] )]))] )])])

[Linearized Tree]
int main ()
{
  int x = 56;
  int y = - 23;
  printInt (x + y);
  printInt (x - y);
  printInt (x * y);
  printInt (45 / 2);
  printInt (78 % 3);
  double z = - 9.3;
  double w = 5.1;
  printBool (z + w > z - w);
  printBool (z / w <= z * w);
  return 0;
}
void printBool (boolean b)
{
  if (b)
  {
    printString ("true");
    return;
  }
  else
  {
    printString ("false");
    return;
  }
}


