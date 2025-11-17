
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "x" [(ELitInt 7)])] ), (SExp [(EApp "printInt" [(EVar "x")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x = 7;
  printInt (x);
  return 0;
}


