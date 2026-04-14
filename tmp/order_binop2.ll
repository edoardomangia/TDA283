declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = call i32 @readInt()
  %t1 = call i32 @readInt()
  %t2 = sub i32 %t0, %t1
  call void @printInt(i32 %t2)
  ret i32 0
}

