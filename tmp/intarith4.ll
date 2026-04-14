declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = call i32 @fact(i32 7)
  call void @printInt(i32 %t0)
  %t1 = call i32 @factr(i32 7)
  call void @printInt(i32 %t1)
  ret i32 0
}

define i32 @fact(i32 %__p__n) {
entry:
  %t0 = alloca i32
  store i32 %__p__n, i32* %t0
  %t1 = alloca i32
  store i32 0, i32* %t1
  %t2 = alloca i32
  store i32 0, i32* %t2
  store i32 1, i32* %t1
  store i32 1, i32* %t2
  br label %L0
L0:
  %t3 = load i32, i32* %t1
  %t4 = load i32, i32* %t0
  %t5 = icmp sle i32 %t3, %t4
  br i1 %t5, label %L1, label %L2
L1:
  %t6 = load i32, i32* %t2
  %t7 = load i32, i32* %t1
  %t8 = mul i32 %t6, %t7
  store i32 %t8, i32* %t2
  %t9 = load i32, i32* %t1
  %t10 = add i32 %t9, 1
  store i32 %t10, i32* %t1
  br label %L0
L2:
  %t11 = load i32, i32* %t2
  ret i32 %t11
}

define i32 @factr(i32 %__p__n) {
entry:
  %t0 = alloca i32
  store i32 %__p__n, i32* %t0
  %t1 = load i32, i32* %t0
  %t2 = icmp slt i32 %t1, 2
  br i1 %t2, label %L0, label %L1
L0:
  ret i32 1
L1:
  %t3 = load i32, i32* %t0
  %t4 = load i32, i32* %t0
  %t5 = sub i32 %t4, 1
  %t6 = call i32 @factr(i32 %t5)
  %t7 = mul i32 %t3, %t6
  ret i32 %t7
}

