declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  br i1 0, label %L0, label %L1
L0:
  br label %L1
L1:
  ret i32 0
}

