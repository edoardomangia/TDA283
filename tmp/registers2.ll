declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  call void @many_params(i32 1, i32 2, i32 3, i32 4, double 100.000000, double 100.000000, double 100.000000, double 100.000000, double 200.000000, double 200.000000, double 200.000000, double 200.000000, double 300.000000, double 300.000000, double 300.000000, double 300.000000, double 400.000000, double 400.000000, double 400.000000, double 400.000000, i32 5, i32 6, i32 7, i32 8, i32 9, i32 10, i32 11, i32 12, i32 13, i32 14, i32 15)
  ret i32 0
}

define void @many_params(i32 %__p__x1, i32 %__p__x2, i32 %__p__x3, i32 %__p__x4, double %__p__d01, double %__p__d02, double %__p__d03, double %__p__d04, double %__p__d11, double %__p__d12, double %__p__d13, double %__p__d14, double %__p__d21, double %__p__d22, double %__p__d23, double %__p__d24, double %__p__d31, double %__p__d32, double %__p__d33, double %__p__d34, i32 %__p__y1, i32 %__p__y2, i32 %__p__y3, i32 %__p__y4, i32 %__p__z1, i32 %__p__z2, i32 %__p__z3, i32 %__p__z4, i32 %__p__q1, i32 %__p__q2, i32 %__p__q3) {
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
  store double %__p__d01, double* %t4
  %t5 = alloca double
  store double %__p__d02, double* %t5
  %t6 = alloca double
  store double %__p__d03, double* %t6
  %t7 = alloca double
  store double %__p__d04, double* %t7
  %t8 = alloca double
  store double %__p__d11, double* %t8
  %t9 = alloca double
  store double %__p__d12, double* %t9
  %t10 = alloca double
  store double %__p__d13, double* %t10
  %t11 = alloca double
  store double %__p__d14, double* %t11
  %t12 = alloca double
  store double %__p__d21, double* %t12
  %t13 = alloca double
  store double %__p__d22, double* %t13
  %t14 = alloca double
  store double %__p__d23, double* %t14
  %t15 = alloca double
  store double %__p__d24, double* %t15
  %t16 = alloca double
  store double %__p__d31, double* %t16
  %t17 = alloca double
  store double %__p__d32, double* %t17
  %t18 = alloca double
  store double %__p__d33, double* %t18
  %t19 = alloca double
  store double %__p__d34, double* %t19
  %t20 = alloca i32
  store i32 %__p__y1, i32* %t20
  %t21 = alloca i32
  store i32 %__p__y2, i32* %t21
  %t22 = alloca i32
  store i32 %__p__y3, i32* %t22
  %t23 = alloca i32
  store i32 %__p__y4, i32* %t23
  %t24 = alloca i32
  store i32 %__p__z1, i32* %t24
  %t25 = alloca i32
  store i32 %__p__z2, i32* %t25
  %t26 = alloca i32
  store i32 %__p__z3, i32* %t26
  %t27 = alloca i32
  store i32 %__p__z4, i32* %t27
  %t28 = alloca i32
  store i32 %__p__q1, i32* %t28
  %t29 = alloca i32
  store i32 %__p__q2, i32* %t29
  %t30 = alloca i32
  store i32 %__p__q3, i32* %t30
  %t31 = load i32, i32* %t0
  call void @printInt(i32 %t31)
  %t32 = load i32, i32* %t20
  call void @printInt(i32 %t32)
  %t33 = load i32, i32* %t24
  call void @printInt(i32 %t33)
  %t34 = load i32, i32* %t28
  call void @printInt(i32 %t34)
  %t35 = load double, double* %t4
  call void @printDouble(double %t35)
  %t36 = load double, double* %t8
  call void @printDouble(double %t36)
  %t37 = load double, double* %t12
  call void @printDouble(double %t37)
  %t38 = load double, double* %t16
  call void @printDouble(double %t38)
  %t39 = load i32, i32* %t0
  %t40 = icmp ne i32 %t39, 2
  br i1 %t40, label %L0, label %L1
L0:
  %t41 = load i32, i32* %t30
  %t42 = load i32, i32* %t0
  %t43 = load i32, i32* %t1
  %t44 = load i32, i32* %t2
  %t45 = load double, double* %t7
  %t46 = fdiv double %t45, 2.000000
  %t47 = load double, double* %t4
  %t48 = fmul double %t47, 2.000000
  %t49 = load double, double* %t5
  %t50 = fadd double %t49, 1.000000
  %t51 = load double, double* %t6
  %t52 = fsub double %t51, 0.000000
  %t53 = load double, double* %t11
  %t54 = fdiv double %t53, 2.000000
  %t55 = load double, double* %t8
  %t56 = fmul double %t55, 2.000000
  %t57 = load double, double* %t9
  %t58 = fadd double %t57, 1.000000
  %t59 = load double, double* %t10
  %t60 = fsub double %t59, 0.000000
  %t61 = load double, double* %t15
  %t62 = fdiv double %t61, 2.000000
  %t63 = load double, double* %t12
  %t64 = fmul double %t63, 2.000000
  %t65 = load double, double* %t13
  %t66 = fadd double %t65, 1.000000
  %t67 = load double, double* %t14
  %t68 = fsub double %t67, 0.000000
  %t69 = load double, double* %t19
  %t70 = fdiv double %t69, 2.000000
  %t71 = load double, double* %t16
  %t72 = fmul double %t71, 2.000000
  %t73 = load double, double* %t17
  %t74 = fadd double %t73, 1.000000
  %t75 = load double, double* %t18
  %t76 = fsub double %t75, 0.000000
  %t77 = load i32, i32* %t3
  %t78 = load i32, i32* %t20
  %t79 = load i32, i32* %t21
  %t80 = load i32, i32* %t22
  %t81 = load i32, i32* %t23
  %t82 = load i32, i32* %t24
  %t83 = load i32, i32* %t25
  %t84 = load i32, i32* %t26
  %t85 = load i32, i32* %t27
  %t86 = load i32, i32* %t28
  %t87 = load i32, i32* %t29
  call void @many_params(i32 %t41, i32 %t42, i32 %t43, i32 %t44, double %t46, double %t48, double %t50, double %t52, double %t54, double %t56, double %t58, double %t60, double %t62, double %t64, double %t66, double %t68, double %t70, double %t72, double %t74, double %t76, i32 %t77, i32 %t78, i32 %t79, i32 %t80, i32 %t81, i32 %t82, i32 %t83, i32 %t84, i32 %t85, i32 %t86, i32 %t87)
  br label %L1
L1:
  ret void
}

