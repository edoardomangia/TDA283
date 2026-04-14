declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = call i32 @add(i32 2, i32 3)
  %t1 = sub i32 0, %t0
  call void @printInt(i32 %t1)
  %t2 = sub i32 0, 3
  %t3 = mul i32 4, %t2
  %t4 = sub i32 0, %t3
  %t5 = sub i32 0, 2
  %t6 = sub i32 0, %t5
  %t7 = add i32 %t4, %t6
  call void @printInt(i32 %t7)
  ret i32 0
}

define i32 @add(i32 %__p__x, i32 %__p__y) {
entry:
  %t0 = alloca i32
  store i32 %__p__x, i32* %t0
  %t1 = alloca i32
  store i32 %__p__y, i32* %t1
  %t2 = load i32, i32* %t0
  %t3 = load i32, i32* %t1
  %t4 = add i32 %t2, %t3
  ret i32 %t4
}

