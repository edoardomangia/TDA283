
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Doub] [(NoInit "dA")] ), (Decl [Doub] [(NoInit "dB")] ), (Ass "dA" [(ELitDoub 0.0014)] ), (Ass "dB" [(ELitDoub 0.0004)] ), (Cond [(ERel (EAdd (EVar "dA") [Minus] (EVar "dB")) [EQU] (ELitDoub 0.001))] [(BStmt [(Block [(SExp [(EApp "printInt" [(ELitInt 99)] )] )] )])]), (Decl [Int] [(NoInit "iA")] ), (Decl [Int] [(NoInit "iB")] ), (Ass "iA" [(ELitInt 342)] ), (Ass "iB" [(ELitInt 5123123)] ), (SExp [(EApp "printInt" [(EAdd (EVar "iA") [Minus] (EVar "iB"))] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  double dA;
  double dB;
  dA = 0.0014;
  dB = 0.0004;
  if (dA - dB == 0.001)
  {
    printInt (99);
  }
  int iA;
  int iB;
  iA = 342;
  iB = 5123123;
  printInt (iA - iB);
  return 0;
}


