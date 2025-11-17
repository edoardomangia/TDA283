
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Cond [ELitFalse] [Empty]), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  if (false);
  return 0;
}


