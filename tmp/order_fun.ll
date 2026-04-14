declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = call i32 @foo(i32 1)
  %t1 = call i32 @foo(i32 2)
  %t2 = call i32 @foo(i32 3)
  %t3 = call i32 @bar(i32 %t0, i32 %t1, i32 %t2)
  call void @printInt(i32 %t3)
  ret i32 0
}

define i32 @foo(i32 %__p__x) {
entry:
  %t0 = alloca i32
  store i32 %__p__x, i32* %t0
  %t1 = load i32, i32* %t0
  call void @printInt(i32 %t1)
  %t2 = load i32, i32* %t0
  ret i32 %t2
}

define i32 @bar(i32 %__p__x, i32 %__p__y, i32 %__p__z) {
entry:
  %t0 = alloca i32
  store i32 %__p__x, i32* %t0
  %t1 = alloca i32
  store i32 %__p__y, i32* %t1
  %t2 = alloca i32
  store i32 %__p__z, i32* %t2
  %t3 = load i32, i32* %t0
  %t4 = load i32, i32* %t1
  %t5 = add i32 %t3, %t4
  %t6 = load i32, i32* %t2
  %t7 = add i32 %t5, %t6
  ret i32 %t7
}

