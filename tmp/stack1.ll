
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Void] "foo" [] [(Block [(Decl [Int] [(Init "x01" [(ELitInt 1)])] ), (Decl [Int] [(Init "x02" [(ELitInt 2)])] ), (Decl [Int] [(Init "x03" [(ELitInt 3)])] ), (Decl [Int] [(Init "x04" [(ELitInt 4)])] ), (Decl [Int] [(Init "x05" [(ELitInt 5)])] ), (Decl [Int] [(Init "x06" [(ELitInt 6)])] ), (Decl [Int] [(Init "x07" [(ELitInt 7)])] ), (Decl [Int] [(Init "x08" [(ELitInt 8)])] ), (Decl [Int] [(Init "x09" [(ELitInt 9)])] ), (Decl [Int] [(Init "x10" [(ELitInt 10)])] )] )]), (FnDef [Void] "bar" [] [(Block [(Decl [Int] [(NoInit "x01")] ), (Decl [Int] [(NoInit "x02")] ), (Decl [Int] [(NoInit "x03")] ), (Decl [Int] [(NoInit "x04")] ), (Decl [Int] [(NoInit "x05")] ), (Decl [Int] [(NoInit "x06")] ), (Decl [Int] [(NoInit "x07")] ), (Decl [Int] [(NoInit "x08")] ), (Decl [Int] [(NoInit "x09")] ), (Decl [Int] [(NoInit "x10")] ), (SExp [(EApp "printInt" [(EVar "x01")] )] ), (SExp [(EApp "printInt" [(EVar "x02")] )] ), (SExp [(EApp "printInt" [(EVar "x03")] )] ), (SExp [(EApp "printInt" [(EVar "x04")] )] ), (SExp [(EApp "printInt" [(EVar "x05")] )] ), (SExp [(EApp "printInt" [(EVar "x06")] )] ), (SExp [(EApp "printInt" [(EVar "x07")] )] ), (SExp [(EApp "printInt" [(EVar "x08")] )] ), (SExp [(EApp "printInt" [(EVar "x09")] )] ), (SExp [(EApp "printInt" [(EVar "x10")] )] )] )]), (FnDef [Int] "main" [] [(Block [(SExp [(EApp "foo" [] )] ), (SExp [(EApp "bar" [] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
void foo ()
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
}
void bar ()
{
  int x01;
  int x02;
  int x03;
  int x04;
  int x05;
  int x06;
  int x07;
  int x08;
  int x09;
  int x10;
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
}
int main ()
{
  foo ();
  bar ();
  return 0;
}


