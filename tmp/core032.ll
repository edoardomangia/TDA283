declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @foo(i32 %__p__n) {
entry:
  %t0 = alloca i32
  store i32 %__p__n, i32* %t0
  %t1 = load i32, i32* %t0
  %t2 = icmp slt i32 %t1, 100
  br i1 %t2, label %L0, label %L1
L0:
  %t3 = load i32, i32* %t0
  %t4 = add i32 %t3, 11
  %t5 = call i32 @foo(i32 %t4)
  %t6 = call i32 @foo(i32 %t5)
  store i32 %t6, i32* %t0
  br label %L1
L1:
  %t7 = load i32, i32* %t0
  ret i32 %t7
}

define i32 @main() {
entry:
  %t0 = call i32 @foo(i32 1)
  call void @printInt(i32 %t0)
  ret i32 0
}

