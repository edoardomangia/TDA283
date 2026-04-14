declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

@.str.0 = private constant [4 x i8] c"foo\00"

define i32 @main() {
entry:
  call void @foo()
  ret i32 0
}

define void @foo() {
entry:
  %t0 = getelementptr [4 x i8], [4 x i8]* @.str.0, i32 0, i32 0
  call void @printString(i8* %t0)
  ret void
}

