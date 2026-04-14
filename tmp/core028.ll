declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 0, i32* %t0
  %t1 = load i32, i32* %t0
  %t2 = add i32 %t1, 1
  store i32 %t2, i32* %t0
  %t3 = load i32, i32* %t0
  call void @printInt(i32 %t3)
  %t4 = load i32, i32* %t0
  %t5 = sub i32 %t4, 1
  store i32 %t5, i32* %t0
  %t6 = load i32, i32* %t0
  call void @printInt(i32 %t6)
  %t7 = load i32, i32* %t0
  %t8 = sub i32 %t7, 1
  store i32 %t8, i32* %t0
  %t9 = load i32, i32* %t0
  call void @printInt(i32 %t9)
  %t10 = load i32, i32* %t0
  %t11 = load i32, i32* %t0
  %t12 = add i32 %t10, %t11
  call void @printInt(i32 %t12)
  ret i32 0
}

