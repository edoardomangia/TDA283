declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = call i32 @fac(i32 5)
  call void @printInt(i32 %t0)
  ret i32 0
}

define i32 @fac(i32 %__p__a) {
entry:
  %t0 = alloca i32
  store i32 %__p__a, i32* %t0
  %t1 = alloca i32
  store i32 0, i32* %t1
  %t2 = alloca i32
  store i32 0, i32* %t2
  store i32 1, i32* %t1
  %t3 = load i32, i32* %t0
  store i32 %t3, i32* %t2
  br label %L0
L0:
  %t4 = load i32, i32* %t2
  %t5 = icmp sgt i32 %t4, 0
  br i1 %t5, label %L1, label %L2
L1:
  %t6 = load i32, i32* %t1
  %t7 = load i32, i32* %t2
  %t8 = mul i32 %t6, %t7
  store i32 %t8, i32* %t1
  %t9 = load i32, i32* %t2
  %t10 = sub i32 %t9, 1
  store i32 %t10, i32* %t2
  br label %L0
L2:
  %t11 = load i32, i32* %t1
  ret i32 %t11
}

