
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "many_params" [(ELitInt 1), (ELitInt 2), (ELitInt 3), (ELitInt 4), (ELitDoub 100), (ELitDoub 100), (ELitDoub 100), (ELitDoub 100), (ELitDoub 200), (ELitDoub 200), (ELitDoub 200), (ELitDoub 200), (ELitDoub 300), (ELitDoub 300), (ELitDoub 300), (ELitDoub 300), (ELitDoub 400), (ELitDoub 400), (ELitDoub 400), (ELitDoub 400), (ELitInt 5), (ELitInt 6), (ELitInt 7), (ELitInt 8), (ELitInt 9), (ELitInt 10), (ELitInt 11), (ELitInt 12), (ELitInt 13), (ELitInt 14), (ELitInt 15)] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Void] "many_params" [(Argument [Int] "x1"), (Argument [Int] "x2"), (Argument [Int] "x3"), (Argument [Int] "x4"), (Argument [Doub] "d01"), (Argument [Doub] "d02"), (Argument [Doub] "d03"), (Argument [Doub] "d04"), (Argument [Doub] "d11"), (Argument [Doub] "d12"), (Argument [Doub] "d13"), (Argument [Doub] "d14"), (Argument [Doub] "d21"), (Argument [Doub] "d22"), (Argument [Doub] "d23"), (Argument [Doub] "d24"), (Argument [Doub] "d31"), (Argument [Doub] "d32"), (Argument [Doub] "d33"), (Argument [Doub] "d34"), (Argument [Int] "y1"), (Argument [Int] "y2"), (Argument [Int] "y3"), (Argument [Int] "y4"), (Argument [Int] "z1"), (Argument [Int] "z2"), (Argument [Int] "z3"), (Argument [Int] "z4"), (Argument [Int] "q1"), (Argument [Int] "q2"), (Argument [Int] "q3")] [(Block [(SExp [(EApp "printInt" [(EVar "x1")] )] ), (SExp [(EApp "printInt" [(EVar "y1")] )] ), (SExp [(EApp "printInt" [(EVar "z1")] )] ), (SExp [(EApp "printInt" [(EVar "q1")] )] ), (SExp [(EApp "printDouble" [(EVar "d01")] )] ), (SExp [(EApp "printDouble" [(EVar "d11")] )] ), (SExp [(EApp "printDouble" [(EVar "d21")] )] ), (SExp [(EApp "printDouble" [(EVar "d31")] )] ), (Cond [(ERel (EVar "x1") [NE] (ELitInt 2))] [(BStmt [(Block [(SExp [(EApp "many_params" [(EVar "q3"), (EVar "x1"), (EVar "x2"), (EVar "x3"), (EMul (EVar "d04") [Div] (ELitDoub 2)), (EMul (EVar "d01") [Times] (ELitDoub 2)), (EAdd (EVar "d02") [Plus] (ELitDoub 1)), (EAdd (EVar "d03") [Minus] (ELitDoub 0)), (EMul (EVar "d14") [Div] (ELitDoub 2)), (EMul (EVar "d11") [Times] (ELitDoub 2)), (EAdd (EVar "d12") [Plus] (ELitDoub 1)), (EAdd (EVar "d13") [Minus] (ELitDoub 0)), (EMul (EVar "d24") [Div] (ELitDoub 2)), (EMul (EVar "d21") [Times] (ELitDoub 2)), (EAdd (EVar "d22") [Plus] (ELitDoub 1)), (EAdd (EVar "d23") [Minus] (ELitDoub 0)), (EMul (EVar "d34") [Div] (ELitDoub 2)), (EMul (EVar "d31") [Times] (ELitDoub 2)), (EAdd (EVar "d32") [Plus] (ELitDoub 1)), (EAdd (EVar "d33") [Minus] (ELitDoub 0)), (EVar "x4"), (EVar "y1"), (EVar "y2"), (EVar "y3"), (EVar "y4"), (EVar "z1"), (EVar "z2"), (EVar "z3"), (EVar "z4"), (EVar "q1"), (EVar "q2")] )] )] )])])] )])])

[Linearized Tree]
int main ()
{
  many_params (1, 2, 3, 4, 100, 100, 100, 100, 200, 200, 200, 200, 300, 300, 300, 300, 400, 400, 400, 400, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  return 0;
}
void many_params (int x1, int x2, int x3, int x4, double d01, double d02, double d03, double d04, double d11, double d12, double d13, double d14, double d21, double d22, double d23, double d24, double d31, double d32, double d33, double d34, int y1, int y2, int y3, int y4, int z1, int z2, int z3, int z4, int q1, int q2, int q3)
{
  printInt (x1);
  printInt (y1);
  printInt (z1);
  printInt (q1);
  printDouble (d01);
  printDouble (d11);
  printDouble (d21);
  printDouble (d31);
  if (x1 != 2)
  {
    many_params (q3, x1, x2, x3, d04 / 2, d01 * 2, d02 + 1, d03 - 0, d14 / 2, d11 * 2, d12 + 1, d13 - 0, d24 / 2, d21 * 2, d22 + 1, d23 - 0, d34 / 2, d31 * 2, d32 + 1, d33 - 0, x4, y1, y2, y3, y4, z1, z2, z3, z4, q1, q2);
  }
}


