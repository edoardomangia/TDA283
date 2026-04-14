declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = icmp eq i1 1, 1
  br i1 %t0, label %L0, label %L1
L0:
  call void @printInt(i32 42)
  br label %L1
L1:
  ret i32 0
}

