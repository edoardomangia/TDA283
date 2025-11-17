
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(NoInit "x")] ), (Ass "x" [(EAdd (EApp "f" [(ELitInt 1)] ) [Minus] (EApp "f" [(ELitInt 2)] ))] ), (Ass "x" [(EAdd (EApp "f" [(ELitInt 3)] ) [Plus] (EApp "f" [(ELitInt 4)] ))] ), (Ass "x" [(EMul (EApp "f" [(ELitInt 5)] ) [Times] (EApp "f" [(ELitInt 6)] ))] ), (Ass "x" [(EMul (EApp "f" [(ELitInt 7)] ) [Div] (EApp "f" [(ELitInt 8)] ))] ), (Ass "x" [(EMul (EApp "f" [(ELitInt 9)] ) [Mod] (EApp "f" [(ELitInt 2)] ))] ), (SExp [(EApp "printInt" [(EAdd (EApp "f" [(ELitInt 12)] ) [Plus] (EApp "f" [(ELitInt 34)] ))] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Int] "f" [(Argument [Int] "x")] [(Block [(SExp [(EApp "printInt" [(EVar "x")] )] ), (Ret [(EVar "x")] )] )])])

[Linearized Tree]
int main ()
{
  int x;
  x = f (1) - f (2);
  x = f (3) + f (4);
  x = f (5) * f (6);
  x = f (7) / f (8);
  x = f (9) % f (2);
  printInt (f (12) + f (34));
  return 0;
}
int f (int x)
{
  printInt (x);
  return x;
}


