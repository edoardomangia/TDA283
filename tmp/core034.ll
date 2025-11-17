
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "printInt" [(Neg [(EApp "add" [(ELitInt 2), (ELitInt 3)] )])] )] ), (SExp [(EApp "printInt" [(EAdd (Neg [(EMul (ELitInt 4) [Times] (Neg [(ELitInt 3)]))]) [Plus] (Neg [(Neg [(ELitInt 2)])]))] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Int] "add" [(Argument [Int] "x"), (Argument [Int] "y")] [(Block [(Ret [(EAdd (EVar "x") [Plus] (EVar "y"))] )] )])])

[Linearized Tree]
int main ()
{
  printInt (- add (2, 3));
  printInt (- (4 * - 3) + - (- 2));
  return 0;
}
int add (int x, int y)
{
  return x + y;
}


