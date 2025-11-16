declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 17, i32* %t0
  br label %L0
L0:
  %t1 = load i32, i32* %t0
  %t2 = icmp sgt i32 %t1, 0
  br i1 %t2, label %L1, label %L2
L1:
  %t3 = load i32, i32* %t0
  %t4 = sub i32 %t3, 2
  store i32 %t4, i32* %t0
  br label %L0
L2:
  %t5 = load i32, i32* %t0
  %t6 = icmp slt i32 %t5, 0
  br i1 %t6, label %L3, label %L4
L3:
  call void @printInt(i32 0)
  ret i32 0
L4:
  call void @printInt(i32 1)
  ret i32 0
L5:
}

