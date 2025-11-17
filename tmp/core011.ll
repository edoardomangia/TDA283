
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "printInt" [(Neg [(ELitInt 1)])] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  printInt (- 1);
  return 0;
}


