
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Bool] [(Init "b" [ELitFalse])] ), (While [(EVar "b")] [(BStmt [(Block [] )])]), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  boolean b = false;
  while (b)
  {
  }
  return 0;
}


