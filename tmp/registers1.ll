declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 1, i32* %t0
  %t1 = alloca i32
  store i32 2, i32* %t1
  %t2 = alloca i32
  store i32 3, i32* %t2
  %t3 = alloca i32
  store i32 4, i32* %t3
  %t4 = alloca i32
  store i32 5, i32* %t4
  %t5 = alloca i32
  store i32 6, i32* %t5
  %t6 = alloca i32
  store i32 7, i32* %t6
  %t7 = alloca i32
  store i32 8, i32* %t7
  %t8 = alloca i32
  store i32 9, i32* %t8
  %t9 = alloca i32
  store i32 10, i32* %t9
  %t10 = alloca i32
  store i32 11, i32* %t10
  %t11 = alloca i32
  store i32 12, i32* %t11
  %t12 = alloca i32
  store i32 13, i32* %t12
  %t13 = alloca i32
  store i32 14, i32* %t13
  %t14 = alloca i32
  store i32 15, i32* %t14
  %t15 = alloca i32
  %t16 = load i32, i32* %t0
  %t17 = load i32, i32* %t1
  %t18 = add i32 %t16, %t17
  %t19 = load i32, i32* %t2
  %t20 = add i32 %t18, %t19
  %t21 = load i32, i32* %t3
  %t22 = add i32 %t20, %t21
  %t23 = load i32, i32* %t4
  %t24 = add i32 %t22, %t23
  %t25 = load i32, i32* %t5
  %t26 = add i32 %t24, %t25
  %t27 = load i32, i32* %t6
  %t28 = add i32 %t26, %t27
  %t29 = load i32, i32* %t7
  %t30 = add i32 %t28, %t29
  %t31 = load i32, i32* %t8
  %t32 = add i32 %t30, %t31
  %t33 = load i32, i32* %t9
  %t34 = add i32 %t32, %t33
  %t35 = load i32, i32* %t10
  %t36 = add i32 %t34, %t35
  %t37 = load i32, i32* %t11
  %t38 = add i32 %t36, %t37
  %t39 = load i32, i32* %t12
  %t40 = add i32 %t38, %t39
  %t41 = load i32, i32* %t13
  %t42 = add i32 %t40, %t41
  %t43 = load i32, i32* %t14
  %t44 = add i32 %t42, %t43
  store i32 %t44, i32* %t15
  %t45 = load i32, i32* %t0
  call void @printInt(i32 %t45)
  %t46 = load i32, i32* %t1
  call void @printInt(i32 %t46)
  %t47 = load i32, i32* %t2
  call void @printInt(i32 %t47)
  %t48 = load i32, i32* %t3
  call void @printInt(i32 %t48)
  %t49 = load i32, i32* %t4
  call void @printInt(i32 %t49)
  %t50 = load i32, i32* %t5
  call void @printInt(i32 %t50)
  %t51 = load i32, i32* %t6
  call void @printInt(i32 %t51)
  %t52 = load i32, i32* %t7
  call void @printInt(i32 %t52)
  %t53 = load i32, i32* %t8
  call void @printInt(i32 %t53)
  %t54 = load i32, i32* %t9
  call void @printInt(i32 %t54)
  %t55 = load i32, i32* %t10
  call void @printInt(i32 %t55)
  %t56 = load i32, i32* %t11
  call void @printInt(i32 %t56)
  %t57 = load i32, i32* %t12
  call void @printInt(i32 %t57)
  %t58 = load i32, i32* %t13
  call void @printInt(i32 %t58)
  %t59 = load i32, i32* %t14
  call void @printInt(i32 %t59)
  %t60 = load i32, i32* %t15
  call void @printInt(i32 %t60)
  ret i32 0
}

