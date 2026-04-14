declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca double
  store double 0.0, double* %t0
  %t1 = alloca double
  store double 0.0, double* %t1
  store double 0.001400, double* %t0
  store double 0.000400, double* %t1
  %t2 = load double, double* %t0
  %t3 = load double, double* %t1
  %t4 = fsub double %t2, %t3
  %t5 = fcmp oeq double %t4, 0.001000
  br i1 %t5, label %L0, label %L1
L0:
  call void @printInt(i32 99)
  br label %L1
L1:
  %t6 = alloca i32
  store i32 0, i32* %t6
  %t7 = alloca i32
  store i32 0, i32* %t7
  store i32 342, i32* %t6
  store i32 5123123, i32* %t7
  %t8 = load i32, i32* %t6
  %t9 = load i32, i32* %t7
  %t10 = sub i32 %t8, %t9
  call void @printInt(i32 %t10)
  ret i32 0
}

