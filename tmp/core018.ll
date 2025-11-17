
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Int] "main" [] [(Block [(Decl [Int] [(Init "x" [(EApp "readInt" [] )])] ), (Decl [Doub] [(Init "y" [(EApp "readDouble" [] )])] ), (SExp [(EApp "printInt" [(EAdd (EVar "x") [Minus] (ELitInt 5))] )] ), (CondElse [(EOr (ERel (EVar "y") [GTH] (ELitDoub 42)) (ERel (EVar "y") [LTH] (ELitDoub 43)))] (SExp [(EApp "printString" [(EString "yay!")] )] ) (SExp [(EApp "printString" [(EString "nay!")] )] )), (Ret [(ELitInt 0)] )] )])])

[Linearized Tree]
int main ()
{
  int x = readInt ();
  double y = readDouble ();
  printInt (x - 5);
  if (y > 42 || y < 43) printString ("yay!");
  else printString ("nay!");
  return 0;
}


