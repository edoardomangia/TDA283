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
  store i32 10, i32* %t1
  br label %L0
L0:
  %t2 = load i32, i32* %t1
  %t3 = icmp sgt i32 %t2, 0
  br i1 %t3, label %L1, label %L2
L1:
  %t4 = load i32, i32* %t1
  call void @printInt(i32 %t4)
  %t5 = load i32, i32* %t1
  %t6 = sub i32 %t5, 1
  store i32 %t6, i32* %t1
  br label %L0
L2:
  %t7 = load i32, i32* %t0
  call void @printInt(i32 %t7)
  ret i32 0
}

