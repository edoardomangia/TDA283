declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 0, i32* %t0
  %t1 = alloca double
  store double 0.0, double* %t1
  %t2 = load i32, i32* %t0
  call void @printInt(i32 %t2)
  %t3 = load double, double* %t1
  call void @printDouble(double %t3)
  ret i32 0
}

