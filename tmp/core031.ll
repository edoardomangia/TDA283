declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  call void @many_params(i32 1, i32 2, i32 3, i32 4, double 100.000000, double 100.000000, double 100.000000, double 100.000000, i32 5, i32 6, i32 7, i32 8, i32 9, i32 10, i32 11, i32 12, i32 13, i32 14, i32 15, i32 16)
  ret i32 0
}

define void @many_params(i32 %__p__x1, i32 %__p__x2, i32 %__p__x3, i32 %__p__x4, double %__p__d1, double %__p__d2, double %__p__d3, double %__p__d4, i32 %__p__y1, i32 %__p__y2, i32 %__p__y3, i32 %__p__y4, i32 %__p__z1, i32 %__p__z2, i32 %__p__z3, i32 %__p__z4, i32 %__p__q1, i32 %__p__q2, i32 %__p__q3, i32 %__p__q4) {
entry:
  %t0 = alloca i32
  store i32 %__p__x1, i32* %t0
  %t1 = alloca i32
  store i32 %__p__x2, i32* %t1
  %t2 = alloca i32
  store i32 %__p__x3, i32* %t2
  %t3 = alloca i32
  store i32 %__p__x4, i32* %t3
  %t4 = alloca double
  store double %__p__d1, double* %t4
  %t5 = alloca double
  store double %__p__d2, double* %t5
  %t6 = alloca double
  store double %__p__d3, double* %t6
  %t7 = alloca double
  store double %__p__d4, double* %t7
  %t8 = alloca i32
  store i32 %__p__y1, i32* %t8
  %t9 = alloca i32
  store i32 %__p__y2, i32* %t9
  %t10 = alloca i32
  store i32 %__p__y3, i32* %t10
  %t11 = alloca i32
  store i32 %__p__y4, i32* %t11
  %t12 = alloca i32
  store i32 %__p__z1, i32* %t12
  %t13 = alloca i32
  store i32 %__p__z2, i32* %t13
  %t14 = alloca i32
  store i32 %__p__z3, i32* %t14
  %t15 = alloca i32
  store i32 %__p__z4, i32* %t15
  %t16 = alloca i32
  store i32 %__p__q1, i32* %t16
  %t17 = alloca i32
  store i32 %__p__q2, i32* %t17
  %t18 = alloca i32
  store i32 %__p__q3, i32* %t18
  %t19 = alloca i32
  store i32 %__p__q4, i32* %t19
  %t20 = load i32, i32* %t0
  call void @printInt(i32 %t20)
  %t21 = load i32, i32* %t8
  call void @printInt(i32 %t21)
  %t22 = load i32, i32* %t12
  call void @printInt(i32 %t22)
  %t23 = load i32, i32* %t16
  call void @printInt(i32 %t23)
  %t24 = load double, double* %t4
  call void @printDouble(double %t24)
  %t25 = load i32, i32* %t0
  %t26 = icmp ne i32 %t25, 2
  br i1 %t26, label %L0, label %L1
L0:
  %t27 = load i32, i32* %t19
  %t28 = load i32, i32* %t0
  %t29 = load i32, i32* %t1
  %t30 = load i32, i32* %t2
  %t31 = load double, double* %t7
  %t32 = fdiv double %t31, 2.000000
  %t33 = load double, double* %t4
  %t34 = fmul double %t33, 2.000000
  %t35 = load double, double* %t5
  %t36 = fadd double %t35, 1.000000
  %t37 = load double, double* %t6
  %t38 = fsub double %t37, 0.000000
  %t39 = load i32, i32* %t3
  %t40 = load i32, i32* %t8
  %t41 = load i32, i32* %t9
  %t42 = load i32, i32* %t10
  %t43 = load i32, i32* %t11
  %t44 = load i32, i32* %t12
  %t45 = load i32, i32* %t13
  %t46 = load i32, i32* %t14
  %t47 = load i32, i32* %t15
  %t48 = load i32, i32* %t16
  %t49 = load i32, i32* %t17
  %t50 = load i32, i32* %t18
  call void @many_params(i32 %t27, i32 %t28, i32 %t29, i32 %t30, double %t32, double %t34, double %t36, double %t38, i32 %t39, i32 %t40, i32 %t41, i32 %t42, i32 %t43, i32 %t44, i32 %t45, i32 %t46, i32 %t47, i32 %t48, i32 %t49, i32 %t50)
  br label %L1
L1:
  ret void
}

