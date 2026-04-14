declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 0, i32* %t0
  %t1 = call i32 @f(i32 1)
  %t2 = call i32 @f(i32 2)
  %t3 = sub i32 %t1, %t2
  store i32 %t3, i32* %t0
  %t4 = call i32 @f(i32 3)
  %t5 = call i32 @f(i32 4)
  %t6 = add i32 %t4, %t5
  store i32 %t6, i32* %t0
  %t7 = call i32 @f(i32 5)
  %t8 = call i32 @f(i32 6)
  %t9 = mul i32 %t7, %t8
  store i32 %t9, i32* %t0
  %t10 = call i32 @f(i32 7)
  %t11 = call i32 @f(i32 8)
  %t12 = sdiv i32 %t10, %t11
  store i32 %t12, i32* %t0
  %t13 = call i32 @f(i32 9)
  %t14 = call i32 @f(i32 2)
  %t15 = srem i32 %t13, %t14
  store i32 %t15, i32* %t0
  %t16 = call i32 @f(i32 12)
  %t17 = call i32 @f(i32 34)
  %t18 = add i32 %t16, %t17
  call void @printInt(i32 %t18)
  ret i32 0
}

define i32 @f(i32 %__p__x) {
entry:
  %t0 = alloca i32
  store i32 %__p__x, i32* %t0
  %t1 = load i32, i32* %t0
  call void @printInt(i32 %t1)
  %t2 = load i32, i32* %t0
  ret i32 %t2
}

