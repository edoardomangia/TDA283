
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "printInt" [(EApp "fact" [(ELitInt 7)] )] )] ), (SExp [(EApp "printInt" [(EApp "factr" [(ELitInt 7)] )] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Int] "fact" [(Argument [Int] "n")] [(Block [(Decl [Int] [(NoInit "i"), (NoInit "r")] ), (Ass "i" [(ELitInt 1)] ), (Ass "r" [(ELitInt 1)] ), (While [(ERel (EVar "i") [LE] (EVar "n"))] [(BStmt [(Block [(Ass "r" [(EMul (EVar "r") [Times] (EVar "i"))] ), (Incr "i" )] )])]), (Ret [(EVar "r")] )] )]), (FnDef [Int] "factr" [(Argument [Int] "n")] [(Block [(CondElse [(ERel (EVar "n") [LTH] (ELitInt 2))] (Ret [(ELitInt 1)] ) (Ret [(EMul (EVar "n") [Times] (EApp "factr" [(EAdd (EVar "n") [Minus] (ELitInt 1))] ))] ))] )])])

[Linearized Tree]
int main ()
{
  printInt (fact (7));
  printInt (factr (7));
  return 0;
}
int fact (int n)
{
  int i, r;
  i = 1;
  r = 1;
  while (i <= n)
  {
    r = r * i;
    i ++;
  }
  return r;
}
int factr (int n)
{
  if (n < 2) return 1;
  else return n * factr (n - 1);
}


