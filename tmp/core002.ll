
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "foo" [] )] ), (Ret [(ELitInt 0)] )] )]), (FnDef [Void] "foo" [] [(Block [(SExp [(EApp "printString" [(EString "foo")] )] ), VRet] )])])

[Linearized Tree]
int main ()
{
  foo ();
  return 0;
}
void foo ()
{
  printString ("foo");
  return;
}


