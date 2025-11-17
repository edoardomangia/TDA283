
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(SExp [(EApp "printInt" [(EAdd (EApp "readInt" [] ) [Minus] (EApp "readInt" [] ))] )] ), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  printInt (readInt () - readInt ());
  return 0;
}


