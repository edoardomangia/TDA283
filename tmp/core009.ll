
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "x" [(EApp "foo" [] )])] ), (SExp [(EApp "printInt" [(EVar "x")] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Int] "foo" [] [(Block [(Ret [(ELitInt 10)] )] )])])

[Linearized Tree]
int main ()
{
  int x = foo ();
  printInt (x);
  return 0;
}
int foo ()
{
  return 10;
}


