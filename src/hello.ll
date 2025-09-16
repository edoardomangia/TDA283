declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = getelementptr [14 x i8], [14 x i8]* @.str0, i32 0, i32 0
  call void @printString(i8* %t0)
  ret i32 0
}


@.str0 = private unnamed_addr constant [14 x i8] c"Hello, world!\00"
