declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define void @foo() {
entry:
  %t0 = alloca i32
  store i32 1, i32* %t0
  %t1 = alloca i32
  store i32 2, i32* %t1
  %t2 = alloca i32
  store i32 3, i32* %t2
  %t3 = alloca i32
  store i32 4, i32* %t3
  %t4 = alloca i32
  store i32 5, i32* %t4
  %t5 = alloca i32
  store i32 6, i32* %t5
  %t6 = alloca i32
  store i32 7, i32* %t6
  %t7 = alloca i32
  store i32 8, i32* %t7
  %t8 = alloca i32
  store i32 9, i32* %t8
  %t9 = alloca i32
  store i32 10, i32* %t9
  ret void
}

define void @bar() {
entry:
  %t0 = alloca i32
  store i32 0, i32* %t0
  %t1 = alloca i32
  store i32 0, i32* %t1
  %t2 = alloca i32
  store i32 0, i32* %t2
  %t3 = alloca i32
  store i32 0, i32* %t3
  %t4 = alloca i32
  store i32 0, i32* %t4
  %t5 = alloca i32
  store i32 0, i32* %t5
  %t6 = alloca i32
  store i32 0, i32* %t6
  %t7 = alloca i32
  store i32 0, i32* %t7
  %t8 = alloca i32
  store i32 0, i32* %t8
  %t9 = alloca i32
  store i32 0, i32* %t9
  %t10 = load i32, i32* %t0
  call void @printInt(i32 %t10)
  %t11 = load i32, i32* %t1
  call void @printInt(i32 %t11)
  %t12 = load i32, i32* %t2
  call void @printInt(i32 %t12)
  %t13 = load i32, i32* %t3
  call void @printInt(i32 %t13)
  %t14 = load i32, i32* %t4
  call void @printInt(i32 %t14)
  %t15 = load i32, i32* %t5
  call void @printInt(i32 %t15)
  %t16 = load i32, i32* %t6
  call void @printInt(i32 %t16)
  %t17 = load i32, i32* %t7
  call void @printInt(i32 %t17)
  %t18 = load i32, i32* %t8
  call void @printInt(i32 %t18)
  %t19 = load i32, i32* %t9
  call void @printInt(i32 %t19)
  ret void
}

define i32 @main() {
entry:
  call void @foo()
  call void @bar()
  ret i32 0
}

