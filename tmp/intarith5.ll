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
  store i32 10, i32* %t0
  store i32 3, i32* %t1
  %t2 = load i32, i32* %t0
  %t3 = load i32, i32* %t1
  %t4 = add i32 %t2, %t3
  call void @printInt(i32 %t4)
  %t5 = load i32, i32* %t0
  %t6 = load i32, i32* %t1
  %t7 = sub i32 %t5, %t6
  call void @printInt(i32 %t7)
  %t8 = load i32, i32* %t0
  %t9 = load i32, i32* %t1
  %t10 = mul i32 %t8, %t9
  call void @printInt(i32 %t10)
  %t11 = load i32, i32* %t0
  %t12 = load i32, i32* %t1
  %t13 = sdiv i32 %t11, %t12
  call void @printInt(i32 %t13)
  %t14 = load i32, i32* %t0
  %t15 = load i32, i32* %t1
  %t16 = srem i32 %t14, %t15
  call void @printInt(i32 %t16)
  %t17 = load i32, i32* %t0
  %t18 = sub i32 0, %t17
  call void @printInt(i32 %t18)
  %t19 = load i32, i32* %t0
  %t20 = add i32 %t19, 1
  store i32 %t20, i32* %t0
  %t21 = load i32, i32* %t0
  call void @printInt(i32 %t21)
  %t22 = load i32, i32* %t1
  %t23 = sub i32 %t22, 1
  store i32 %t23, i32* %t1
  %t24 = load i32, i32* %t1
  call void @printInt(i32 %t24)
  ret i32 0
}

