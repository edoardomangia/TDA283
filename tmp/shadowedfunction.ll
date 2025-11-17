
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Void] "func" [] [(Block [(SExp [(EApp "printString" [(EString "I\'m here!")] )] )] )]), (FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "func" [(ELitInt 0)])] ), (SExp [(EApp "func" [] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
void func ()
{
  printString ("I\'m here!");
}
int main ()
{
  int func = 0;
  func ();
  return 0;
}


