declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @heyo(i32 %__p__greeting) {
entry:
  %t0 = alloca i32
  store i32 %__p__greeting, i32* %t0
  store i32 5, i32* %t0
  ret i32 0
}

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 6, i32* %t0
  %t1 = alloca i32
  %t2 = load i32, i32* %t0
  %t3 = call i32 @heyo(i32 %t2)
  store i32 %t3, i32* %t1
  %t4 = load i32, i32* %t0
  call void @printInt(i32 %t4)
  ret i32 0
}

