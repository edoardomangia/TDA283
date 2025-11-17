
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(NoInit "lo"), (NoInit "hi"), (NoInit "mx")] ), (Ass "lo" [(ELitInt 1)] ), (Ass "hi" [(EVar "lo")] ), (Ass "mx" [(ELitInt 5000000)] ), (SExp [(EApp "printInt" [(EVar "lo")] )] ), (While [(ERel (EVar "hi") [LTH] (EVar "mx"))] [(BStmt [(Block [(SExp [(EApp "printInt" [(EVar "hi")] )] ), (Ass "hi" [(EAdd (EVar "lo") [Plus] (EVar "hi"))] ), (Ass "lo" [(EAdd (EVar "hi") [Minus] (EVar "lo"))] )] )])]), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int lo, hi, mx;
  lo = 1;
  hi = lo;
  mx = 5000000;
  printInt (lo);
  while (hi < mx)
  {
    printInt (hi);
    hi = lo + hi;
    lo = hi - lo;
  }
  return 0;
}


