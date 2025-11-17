
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(NoInit "x")] ), (Incr "x" ), (SExp [(EApp "printInt" [(EVar "x")] )] ), (Decr "x" ), (SExp [(EApp "printInt" [(EVar "x")] )] ), (Decr "x" ), (SExp [(EApp "printInt" [(EVar "x")] )] ), (SExp [(EApp "printInt" [(EAdd (EVar "x") [Plus] (EVar "x"))] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x;
  x ++;
  printInt (x);
  x --;
  printInt (x);
  x --;
  printInt (x);
  printInt (x + x);
  return 0;
}


