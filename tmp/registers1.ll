
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "x01" [(ELitInt 1)])] ), (Decl [Int] [(Init "x02" [(ELitInt 2)])] ), (Decl [Int] [(Init "x03" [(ELitInt 3)])] ), (Decl [Int] [(Init "x04" [(ELitInt 4)])] ), (Decl [Int] [(Init "x05" [(ELitInt 5)])] ), (Decl [Int] [(Init "x06" [(ELitInt 6)])] ), (Decl [Int] [(Init "x07" [(ELitInt 7)])] ), (Decl [Int] [(Init "x08" [(ELitInt 8)])] ), (Decl [Int] [(Init "x09" [(ELitInt 9)])] ), (Decl [Int] [(Init "x10" [(ELitInt 10)])] ), (Decl [Int] [(Init "x11" [(ELitInt 11)])] ), (Decl [Int] [(Init "x12" [(ELitInt 12)])] ), (Decl [Int] [(Init "x13" [(ELitInt 13)])] ), (Decl [Int] [(Init "x14" [(ELitInt 14)])] ), (Decl [Int] [(Init "x15" [(ELitInt 15)])] ), (Decl [Int] [(Init "x16" [(EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EVar "x01") [Plus] (EVar "x02")) [Plus] (EVar "x03")) [Plus] (EVar "x04")) [Plus] (EVar "x05")) [Plus] (EVar "x06")) [Plus] (EVar "x07")) [Plus] (EVar "x08")) [Plus] (EVar "x09")) [Plus] (EVar "x10")) [Plus] (EVar "x11")) [Plus] (EVar "x12")) [Plus] (EVar "x13")) [Plus] (EVar "x14")) [Plus] (EVar "x15"))])] ), (SExp [(EApp "printInt" [(EVar "x01")] )] ), (SExp [(EApp "printInt" [(EVar "x02")] )] ), (SExp [(EApp "printInt" [(EVar "x03")] )] ), (SExp [(EApp "printInt" [(EVar "x04")] )] ), (SExp [(EApp "printInt" [(EVar "x05")] )] ), (SExp [(EApp "printInt" [(EVar "x06")] )] ), (SExp [(EApp "printInt" [(EVar "x07")] )] ), (SExp [(EApp "printInt" [(EVar "x08")] )] ), (SExp [(EApp "printInt" [(EVar "x09")] )] ), (SExp [(EApp "printInt" [(EVar "x10")] )] ), (SExp [(EApp "printInt" [(EVar "x11")] )] ), (SExp [(EApp "printInt" [(EVar "x12")] )] ), (SExp [(EApp "printInt" [(EVar "x13")] )] ), (SExp [(EApp "printInt" [(EVar "x14")] )] ), (SExp [(EApp "printInt" [(EVar "x15")] )] ), (SExp [(EApp "printInt" [(EVar "x16")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x01 = 1;
  int x02 = 2;
  int x03 = 3;
  int x04 = 4;
  int x05 = 5;
  int x06 = 6;
  int x07 = 7;
  int x08 = 8;
  int x09 = 9;
  int x10 = 10;
  int x11 = 11;
  int x12 = 12;
  int x13 = 13;
  int x14 = 14;
  int x15 = 15;
  int x16 = x01 + x02 + x03 + x04 + x05 + x06 + x07 + x08 + x09 + x10 + x11 + x12 + x13 + x14 + x15;
  printInt (x01);
  printInt (x02);
  printInt (x03);
  printInt (x04);
  printInt (x05);
  printInt (x06);
  printInt (x07);
  printInt (x08);
  printInt (x09);
  printInt (x10);
  printInt (x11);
  printInt (x12);
  printInt (x13);
  printInt (x14);
  printInt (x15);
  printInt (x16);
  return 0;
}


