declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = call i32 @ev(i32 17)
  call void @printInt(i32 %t0)
  ret i32 0
}

define i32 @ev(i32 %__p__y) {
entry:
  %t0 = alloca i32
  store i32 %__p__y, i32* %t0
  %t1 = load i32, i32* %t0
  %t2 = icmp sgt i32 %t1, 0
  br i1 %t2, label %L0, label %L1
L0:
  %t3 = load i32, i32* %t0
  %t4 = sub i32 %t3, 2
  %t5 = call i32 @ev(i32 %t4)
  ret i32 %t5
L1:
  %t6 = load i32, i32* %t0
  %t7 = icmp slt i32 %t6, 0
  br i1 %t7, label %L3, label %L4
L3:
  ret i32 0
L4:
  ret i32 1
L5:
  br label %L2
L2:
}

