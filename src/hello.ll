declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  call void @printInt(i32 42)
  ret i32 0
}

