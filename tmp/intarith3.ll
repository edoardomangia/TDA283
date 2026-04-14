declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 0, i32* %t0
  br label %L0
L0:
  %t1 = load i32, i32* %t0
  %t2 = icmp slt i32 %t1, 10
  br i1 %t2, label %L1, label %L2
L1:
  %t3 = load i32, i32* %t0
  %t4 = srem i32 %t3, 2
  %t5 = icmp eq i32 %t4, 0
  br i1 %t5, label %L3, label %L4
L3:
  %t6 = load i32, i32* %t0
  call void @printInt(i32 %t6)
  br label %L4
L4:
  %t7 = load i32, i32* %t0
  %t8 = add i32 %t7, 1
  store i32 %t8, i32* %t0
  br label %L0
L2:
  ret i32 0
}

