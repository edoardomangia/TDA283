
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(NoInit "x"), (Init "y" [(ELitInt 7)])] ), (Ass "x" [(Neg [(ELitInt 1234234)])] ), (SExp [(EApp "printInt" [(EVar "x")] )] ), (SExp [(EApp "printInt" [(EVar "y")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x, y = 7;
  x = - 1234234;
  printInt (x);
  printInt (y);
  return 0;
}


