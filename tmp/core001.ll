declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

@.str.0 = private constant [9 x i8] c"hello */\00"
@.str.1 = private constant [9 x i8] c"/* world\00"

define i32 @main() {
entry:
  %t0 = call i32 @fac(i32 10)
  call void @printInt(i32 %t0)
  %t1 = call i32 @rfac(i32 10)
  call void @printInt(i32 %t1)
  %t2 = call i32 @mfac(i32 10)
  call void @printInt(i32 %t2)
  %t3 = call i32 @ifac(i32 10)
  call void @printInt(i32 %t3)
  %t4 = alloca double
  store double 0.0, double* %t4
  %t5 = alloca i32
  store i32 10, i32* %t5
  %t6 = alloca i32
  store i32 1, i32* %t6
  br label %L0
L0:
  %t7 = load i32, i32* %t5
  %t8 = icmp sgt i32 %t7, 0
  br i1 %t8, label %L1, label %L2
L1:
  %t9 = load i32, i32* %t6
  %t10 = load i32, i32* %t5
  %t11 = mul i32 %t9, %t10
  store i32 %t11, i32* %t6
  %t12 = load i32, i32* %t5
  %t13 = sub i32 %t12, 1
  store i32 %t13, i32* %t5
  br label %L0
L2:
  %t14 = load i32, i32* %t6
  call void @printInt(i32 %t14)
  %t15 = call double @dfac(double 10.000000)
  call void @printDouble(double %t15)
  %t16 = getelementptr [9 x i8], [9 x i8]* @.str.0, i32 0, i32 0
  call void @printString(i8* %t16)
  %t17 = getelementptr [9 x i8], [9 x i8]* @.str.1, i32 0, i32 0
  call void @printString(i8* %t17)
  ret i32 0
}

define i32 @fac(i32 %__p__a) {
entry:
  %t0 = alloca i32
  store i32 %__p__a, i32* %t0
  %t1 = alloca i32
  store i32 0, i32* %t1
  %t2 = alloca i32
  store i32 0, i32* %t2
  store i32 1, i32* %t1
  %t3 = load i32, i32* %t0
  store i32 %t3, i32* %t2
  br label %L0
L0:
  %t4 = load i32, i32* %t2
  %t5 = icmp sgt i32 %t4, 0
  br i1 %t5, label %L1, label %L2
L1:
  %t6 = load i32, i32* %t1
  %t7 = load i32, i32* %t2
  %t8 = mul i32 %t6, %t7
  store i32 %t8, i32* %t1
  %t9 = load i32, i32* %t2
  %t10 = sub i32 %t9, 1
  store i32 %t10, i32* %t2
  br label %L0
L2:
  %t11 = load i32, i32* %t1
  ret i32 %t11
}

define i32 @rfac(i32 %__p__n) {
entry:
  %t0 = alloca i32
  store i32 %__p__n, i32* %t0
  %t1 = load i32, i32* %t0
  %t2 = icmp eq i32 %t1, 0
  br i1 %t2, label %L0, label %L1
L0:
  ret i32 1
L1:
  %t3 = load i32, i32* %t0
  %t4 = load i32, i32* %t0
  %t5 = sub i32 %t4, 1
  %t6 = call i32 @rfac(i32 %t5)
  %t7 = mul i32 %t3, %t6
  ret i32 %t7
L2:
}

define i32 @mfac(i32 %__p__n) {
entry:
  %t0 = alloca i32
  store i32 %__p__n, i32* %t0
  %t1 = load i32, i32* %t0
  %t2 = icmp eq i32 %t1, 0
  br i1 %t2, label %L0, label %L1
L0:
  ret i32 1
L1:
  %t3 = load i32, i32* %t0
  %t4 = load i32, i32* %t0
  %t5 = sub i32 %t4, 1
  %t6 = call i32 @nfac(i32 %t5)
  %t7 = mul i32 %t3, %t6
  ret i32 %t7
L2:
}

define i32 @nfac(i32 %__p__n) {
entry:
  %t0 = alloca i32
  store i32 %__p__n, i32* %t0
  %t1 = load i32, i32* %t0
  %t2 = icmp ne i32 %t1, 0
  br i1 %t2, label %L0, label %L1
L0:
  %t3 = load i32, i32* %t0
  %t4 = sub i32 %t3, 1
  %t5 = call i32 @mfac(i32 %t4)
  %t6 = load i32, i32* %t0
  %t7 = mul i32 %t5, %t6
  ret i32 %t7
L1:
  ret i32 1
L2:
}

define double @dfac(double %__p__n) {
entry:
  %t0 = alloca double
  store double %__p__n, double* %t0
  %t1 = load double, double* %t0
  %t2 = fcmp oeq double %t1, 0.000000
  br i1 %t2, label %L0, label %L1
L0:
  ret double 1.000000
L1:
  %t3 = load double, double* %t0
  %t4 = load double, double* %t0
  %t5 = fsub double %t4, 1.000000
  %t6 = call double @dfac(double %t5)
  %t7 = fmul double %t3, %t6
  ret double %t7
L2:
}

define i32 @ifac(i32 %__p__n) {
entry:
  %t0 = alloca i32
  store i32 %__p__n, i32* %t0
  %t1 = load i32, i32* %t0
  %t2 = call i32 @ifac2f(i32 1, i32 %t1)
  ret i32 %t2
}

define i32 @ifac2f(i32 %__p__l, i32 %__p__h) {
entry:
  %t0 = alloca i32
  store i32 %__p__l, i32* %t0
  %t1 = alloca i32
  store i32 %__p__h, i32* %t1
  %t2 = load i32, i32* %t0
  %t3 = load i32, i32* %t1
  %t4 = icmp eq i32 %t2, %t3
  br i1 %t4, label %L0, label %L1
L0:
  %t5 = load i32, i32* %t0
  ret i32 %t5
L1:
  %t6 = load i32, i32* %t0
  %t7 = load i32, i32* %t1
  %t8 = icmp sgt i32 %t6, %t7
  br i1 %t8, label %L2, label %L3
L2:
  ret i32 1
L3:
  %t9 = alloca i32
  store i32 0, i32* %t9
  %t10 = load i32, i32* %t0
  %t11 = load i32, i32* %t1
  %t12 = add i32 %t10, %t11
  %t13 = sdiv i32 %t12, 2
  store i32 %t13, i32* %t9
  %t14 = load i32, i32* %t0
  %t15 = load i32, i32* %t9
  %t16 = call i32 @ifac2f(i32 %t14, i32 %t15)
  %t17 = load i32, i32* %t9
  %t18 = add i32 %t17, 1
  %t19 = load i32, i32* %t1
  %t20 = call i32 @ifac2f(i32 %t18, i32 %t19)
  %t21 = mul i32 %t16, %t20
  ret i32 %t21
}

