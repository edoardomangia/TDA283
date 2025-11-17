
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Cond [(ERel ELitTrue [EQU] ELitTrue)] [(BStmt [(Block [(SExp [(EApp "printInt" [(ELitInt 42)] )] )] )])]), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  if (true == true)
  {
    printInt (42);
  }
  return 0;
}


