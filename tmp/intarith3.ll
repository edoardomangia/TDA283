
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "i" [(ELitInt 0)])] ), (While [(ERel (EVar "i") [LTH] (ELitInt 10))] [(BStmt [(Block [(Cond [(ERel (EMul (EVar "i") [Mod] (ELitInt 2)) [EQU] (ELitInt 0))] [(SExp [(EApp "printInt" [(EVar "i")] )] )]), (Incr "i" )] )])]), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int i = 0;
  while (i < 10)
  {
    if (i % 2 == 0) printInt (i);
    i ++;
  }
  return 0;
}


