
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(NoInit "x"), (NoInit "y")] ), (Ass "x" [(ELitInt 45)] ), (Ass "y" [(Neg [(ELitInt 36)])] ), (SExp [(EApp "printInt" [(EVar "x")] )] ), (SExp [(EApp "printInt" [(EVar "y")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x, y;
  x = 45;
  y = - 36;
  printInt (x);
  printInt (y);
  return 0;
}


