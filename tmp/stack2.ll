
Parse Successful!

[Abstract Syntax]
(Program [(FnDef [Void] "foo" [] [(Block [(Decl [Int] [(Init "x1" [(ELitInt 55555)])] ), (Decl [Int] [(Init "x2" [(ELitInt 66666)])] ), (Decl [Int] [(Init "x3" [(ELitInt 77777)])] ), (Decl [Int] [(Init "x4" [(ELitInt 88888)])] ), (Decl [Int] [(Init "x5" [(ELitInt 99999)])] ), (Decl [Int] [(Init "x6" [(ELitInt 11111)])] ), (Decl [Int] [(Init "x7" [(ELitInt 22222)])] ), (Decl [Int] [(Init "x8" [(ELitInt 33333)])] )] )]), (FnDef [Int] "main" [] [(Block [(SExp [(EApp "foo" [] )] ), (Decl [Int] [(NoInit "x")] ), (BStmt [(Block [(Decl [Int] [(Init "x" [(EVar "x")])] ), (SExp [(EApp "printInt" [(EVar "x")] )] ), (Ret [(ELitInt 0)] )] )])] )])])

[Linearized Tree]
void foo ()
{
  int x1 = 55555;
  int x2 = 66666;
  int x3 = 77777;
  int x4 = 88888;
  int x5 = 99999;
  int x6 = 11111;
  int x7 = 22222;
  int x8 = 33333;
}
int main ()
{
  foo ();
  int x;
  {
    int x = x;
    printInt (x);
    return 0;
  }
}


