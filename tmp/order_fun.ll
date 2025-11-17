
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "printInt" [(EApp "bar" [(EApp "foo" [(ELitInt 1)] ), (EApp "foo" [(ELitInt 2)] ), (EApp "foo" [(ELitInt 3)] )] )] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Int] "foo" [(Argument [Int] "x")] [(Block [(SExp [(EApp "printInt" [(EVar "x")] )] ), (Ret [(EVar "x")] )] )]), (FnDef [Int] "bar" [(Argument [Int] "x"), (Argument [Int] "y"), (Argument [Int] "z")] [(Block [(Ret [(EAdd (EAdd (EVar "x") [Plus] (EVar "y")) [Plus] (EVar "z"))] )] )])])

[Linearized Tree]
int main ()
{
  printInt (bar (foo (1), foo (2), foo (3)));
  return 0;
}
int foo (int x)
{
  printInt (x);
  return x;
}
int bar (int x, int y, int z)
{
  return x + y + z;
}


