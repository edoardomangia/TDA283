declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

@.str.0 = private constant [5 x i8] c"true\00"
@.str.1 = private constant [6 x i8] c"false\00"

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 56, i32* %t0
  %t1 = alloca i32
  %t2 = sub i32 0, 23
  store i32 %t2, i32* %t1
  %t3 = load i32, i32* %t0
  %t4 = load i32, i32* %t1
  %t5 = add i32 %t3, %t4
  call void @printInt(i32 %t5)
  %t6 = load i32, i32* %t0
  %t7 = load i32, i32* %t1
  %t8 = sub i32 %t6, %t7
  call void @printInt(i32 %t8)
  %t9 = load i32, i32* %t0
  %t10 = load i32, i32* %t1
  %t11 = mul i32 %t9, %t10
  call void @printInt(i32 %t11)
  %t12 = sdiv i32 45, 2
  call void @printInt(i32 %t12)
  %t13 = srem i32 78, 3
  call void @printInt(i32 %t13)
  %t14 = alloca double
  %t15 = fsub double 0.0, 9.300000
  store double %t15, double* %t14
  %t16 = alloca double
  store double 5.100000, double* %t16
  %t17 = load double, double* %t14
  %t18 = load double, double* %t16
  %t19 = fadd double %t17, %t18
  %t20 = load double, double* %t14
  %t21 = load double, double* %t16
  %t22 = fsub double %t20, %t21
  %t23 = fcmp ogt double %t19, %t22
  call void @printBool(i1 %t23)
  %t24 = load double, double* %t14
  %t25 = load double, double* %t16
  %t26 = fdiv double %t24, %t25
  %t27 = load double, double* %t14
  %t28 = load double, double* %t16
  %t29 = fmul double %t27, %t28
  %t30 = fcmp ole double %t26, %t29
  call void @printBool(i1 %t30)
  ret i32 0
}

define void @printBool(i1 %__p__b) {
entry:
  %t0 = alloca i1
  store i1 %__p__b, i1* %t0
  %t1 = load i1, i1* %t0
  br i1 %t1, label %L0, label %L1
L0:
  %t2 = getelementptr [5 x i8], [5 x i8]* @.str.0, i32 0, i32 0
  call void @printString(i8* %t2)
  ret void
L1:
  %t3 = getelementptr [6 x i8], [6 x i8]* @.str.1, i32 0, i32 0
  call void @printString(i8* %t3)
  ret void
}

