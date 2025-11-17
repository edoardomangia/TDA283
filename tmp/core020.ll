
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "p" [] )] ), (SExp [(EApp "printInt" [(ELitInt 1)] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Void] "p" [] [(Block [] )])])

[Linearized Tree]
int main ()
{
  p ();
  printInt (1);
  return 0;
}
void p ()
{
}


