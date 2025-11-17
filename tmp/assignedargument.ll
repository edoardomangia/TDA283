
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "heyo" [(Argument [Int] "greeting")] [(Block [(Ass "greeting" [(ELitInt 5)] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "greeting" [(ELitInt 6)])] ), (Decl [Int] [(Init "b" [(EApp "heyo" [(EVar "greeting")] )])] ), (SExp [(EApp "printInt" [(EVar "greeting")] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int heyo (int greeting)
{
  greeting = 5;
  return 0;
}
int main ()
{
  int greeting = 6;
  int b = heyo (greeting);
  printInt (greeting);
  return 0;
}


