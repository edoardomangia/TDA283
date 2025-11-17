
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "many_params" [(ELitInt 1), (ELitInt 2), (ELitInt 3), (ELitInt 4), (ELitDoub 100), (ELitDoub 100), (ELitDoub 100), (ELitDoub 100), (ELitInt 5), (ELitInt 6), (ELitInt 7), (ELitInt 8), (ELitInt 9), (ELitInt 10), (ELitInt 11), (ELitInt 12), (ELitInt 13), (ELitInt 14), (ELitInt 15), (ELitInt 16)] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Void] "many_params" [(Argument [Int] "x1"), (Argument [Int] "x2"), (Argument [Int] "x3"), (Argument [Int] "x4"), (Argument [Doub] "d1"), (Argument [Doub] "d2"), (Argument [Doub] "d3"), (Argument [Doub] "d4"), (Argument [Int] "y1"), (Argument [Int] "y2"), (Argument [Int] "y3"), (Argument [Int] "y4"), (Argument [Int] "z1"), (Argument [Int] "z2"), (Argument [Int] "z3"), (Argument [Int] "z4"), (Argument [Int] "q1"), (Argument [Int] "q2"), (Argument [Int] "q3"), (Argument [Int] "q4")] [(Block [(SExp [(EApp "printInt" [(EVar "x1")] )] ), (SExp [(EApp "printInt" [(EVar "y1")] )] ), (SExp [(EApp "printInt" [(EVar "z1")] )] ), (SExp [(EApp "printInt" [(EVar "q1")] )] ), (SExp [(EApp "printDouble" [(EVar "d1")] )] ), (Cond [(ERel (EVar "x1") [NE] (ELitInt 2))] [(BStmt [(Block [(SExp [(EApp "many_params" [(EVar "q4"), (EVar "x1"), (EVar "x2"), (EVar "x3"), (EMul (EVar "d4") [Div] (ELitDoub 2)), (EMul (EVar "d1") [Times] (ELitDoub 2)), (EAdd (EVar "d2") [Plus] (ELitDoub 1)), (EAdd (EVar "d3") [Minus] (ELitDoub 0)), (EVar "x4"), (EVar "y1"), (EVar "y2"), (EVar "y3"), (EVar "y4"), (EVar "z1"), (EVar "z2"), (EVar "z3"), (EVar "z4"), (EVar "q1"), (EVar "q2"), (EVar "q3")] )] )] )])])] )])])

[Linearized Tree]
int main ()
{
  many_params (1, 2, 3, 4, 100, 100, 100, 100, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  return 0;
}
void many_params (int x1, int x2, int x3, int x4, double d1, double d2, double d3, double d4, int y1, int y2, int y3, int y4, int z1, int z2, int z3, int z4, int q1, int q2, int q3, int q4)
{
  printInt (x1);
  printInt (y1);
  printInt (z1);
  printInt (q1);
  printDouble (d1);
  if (x1 != 2)
  {
    many_params (q4, x1, x2, x3, d4 / 2, d1 * 2, d2 + 1, d3 - 0, x4, y1, y2, y3, y4, z1, z2, z3, z4, q1, q2, q3);
  }
}


