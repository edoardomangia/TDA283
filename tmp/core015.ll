
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "printInt" [(EApp "ev" [(ELitInt 17)] )] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Int] "ev" [(Argument [Int] "y")] [(Block [(CondElse [(ERel (EVar "y") [GTH] (ELitInt 0))] (Ret [(EApp "ev" [(EAdd (EVar "y") [Minus] (ELitInt 2))] )] ) (CondElse [(ERel (EVar "y") [LTH] (ELitInt 0))] (Ret [(ELitInt 0)] ) (Ret [(ELitInt 1)] )))] )])])

[Linearized Tree]
int main ()
{
  printInt (ev (17));
  return 0;
}
int ev (int y)
{
  if (y > 0) return ev (y - 2);
  else if (y < 0) return 0;
  else return 1;
}


