
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(NoInit "x")] ), (Decl [Int] [(NoInit "y")] ), (Ass "x" [(ELitInt 10)] ), (Ass "y" [(ELitInt 3)] ), (SExp [(EApp "printInt" [(EAdd (EVar "x") [Plus] (EVar "y"))] )] ), (SExp [(EApp "printInt" [(EAdd (EVar "x") [Minus] (EVar "y"))] )] ), (SExp [(EApp "printInt" [(EMul (EVar "x") [Times] (EVar "y"))] )] ), (SExp [(EApp "printInt" [(EMul (EVar "x") [Div] (EVar "y"))] )] ), (SExp [(EApp "printInt" [(EMul (EVar "x") [Mod] (EVar "y"))] )] ), (SExp [(EApp "printInt" [(Neg [(EVar "x")])] )] ), (Incr "x" ), (SExp [(EApp "printInt" [(EVar "x")] )] ), (Decr "y" ), (SExp [(EApp "printInt" [(EVar "y")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x;
  int y;
  x = 10;
  y = 3;
  printInt (x + y);
  printInt (x - y);
  printInt (x * y);
  printInt (x / y);
  printInt (x % y);
  printInt (- x);
  x ++;
  printInt (x);
  y --;
  printInt (y);
  return 0;
}


