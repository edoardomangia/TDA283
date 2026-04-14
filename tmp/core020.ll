declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  call void @p()
  call void @printInt(i32 1)
  ret i32 0
}

define void @p() {
entry:
  ret void
}

