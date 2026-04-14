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
  store i32 45, i32* %t0
  %t2 = sub i32 0, 36
  store i32 %t2, i32* %t1
  %t3 = load i32, i32* %t0
  call void @printInt(i32 %t3)
  %t4 = load i32, i32* %t1
  call void @printInt(i32 %t4)
  ret i32 0
}

