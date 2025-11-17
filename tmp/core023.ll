
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Cond [ELitTrue] [(BStmt [(Block [] )])]), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  if (true)
  {
  }
  return 0;
}


