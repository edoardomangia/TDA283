
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(NoInit "x")] ), (Decl [Doub] [(NoInit "y")] ), (SExp [(EApp "printInt" [(EVar "x")] )] ), (SExp [(EApp "printDouble" [(EVar "y")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x;
  double y;
  printInt (x);
  printDouble (y);
  return 0;
}


