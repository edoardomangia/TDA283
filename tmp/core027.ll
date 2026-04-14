declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca i1
  store i1 0, i1* %t0
  br label %L0
L0:
  %t1 = load i1, i1* %t0
  br i1 %t1, label %L1, label %L2
L1:
  br label %L0
L2:
  ret i32 0
}

