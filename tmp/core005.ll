declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 0, i32* %t0
  %t1 = alloca i32
  store i32 56, i32* %t1
  %t2 = load i32, i32* %t1
  %t3 = add i32 %t2, 45
  %t4 = icmp sle i32 %t3, 2
  br i1 %t4, label %L0, label %L1
L0:
  store i32 1, i32* %t0
  br label %L2
L1:
  store i32 2, i32* %t0
  br label %L2
L2:
  %t5 = load i32, i32* %t0
  call void @printInt(i32 %t5)
  ret i32 0
}

