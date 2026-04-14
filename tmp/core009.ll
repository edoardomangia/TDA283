declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca i32
  %t1 = call i32 @foo()
  store i32 %t1, i32* %t0
  %t2 = load i32, i32* %t0
  call void @printInt(i32 %t2)
  ret i32 0
}

define i32 @foo() {
entry:
  ret i32 10
}

