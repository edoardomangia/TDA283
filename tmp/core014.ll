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
  store i32 0, i32* %t1
  %t2 = alloca i32
  store i32 0, i32* %t2
  store i32 1, i32* %t0
  %t3 = load i32, i32* %t0
  store i32 %t3, i32* %t1
  store i32 5000000, i32* %t2
  %t4 = load i32, i32* %t0
  call void @printInt(i32 %t4)
  br label %L0
L0:
  %t5 = load i32, i32* %t1
  %t6 = load i32, i32* %t2
  %t7 = icmp slt i32 %t5, %t6
  br i1 %t7, label %L1, label %L2
L1:
  %t8 = load i32, i32* %t1
  call void @printInt(i32 %t8)
  %t9 = load i32, i32* %t0
  %t10 = load i32, i32* %t1
  %t11 = add i32 %t9, %t10
  store i32 %t11, i32* %t1
  %t12 = load i32, i32* %t1
  %t13 = load i32, i32* %t0
  %t14 = sub i32 %t12, %t13
  store i32 %t14, i32* %t0
  br label %L0
L2:
  ret i32 0
}

