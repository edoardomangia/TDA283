declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define void @foo() {
entry:
  %t0 = alloca i32
  store i32 55555, i32* %t0
  %t1 = alloca i32
  store i32 66666, i32* %t1
  %t2 = alloca i32
  store i32 77777, i32* %t2
  %t3 = alloca i32
  store i32 88888, i32* %t3
  %t4 = alloca i32
  store i32 99999, i32* %t4
  %t5 = alloca i32
  store i32 11111, i32* %t5
  %t6 = alloca i32
  store i32 22222, i32* %t6
  %t7 = alloca i32
  store i32 33333, i32* %t7
  ret void
}

define i32 @main() {
entry:
  call void @foo()
  %t0 = alloca i32
  store i32 0, i32* %t0
  %t1 = alloca i32
  %t2 = load i32, i32* %t0
  store i32 %t2, i32* %t1
  %t3 = load i32, i32* %t1
  call void @printInt(i32 %t3)
  ret i32 0
}

