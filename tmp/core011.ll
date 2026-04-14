declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = sub i32 0, 1
  call void @printInt(i32 %t0)
  ret i32 0
}

