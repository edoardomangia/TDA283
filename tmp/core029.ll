declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main() {
entry:
  %t0 = alloca double
  %t1 = call double @readDouble()
  store double %t1, double* %t0
  call void @printDouble(double 101325.000000)
  %t2 = load double, double* %t0
  call void @printDouble(double %t2)
  %t3 = load double, double* %t0
  %t4 = fcmp oeq double 101325.000000, %t3
  br i1 %t4, label %L2, label %L3
L3:
  br label %L4
L2:
  %t5 = load double, double* %t0
  %t6 = fcmp ole double 101325.000000, %t5
  br i1 %t6, label %L5, label %L6
L6:
  br label %L7
L5:
  %t7 = load double, double* %t0
  %t8 = fcmp oge double 101325.000000, %t7
  br label %L7
L7:
  %t9 = phi i1 [ 0, %L6 ], [ %t8, %L5 ]
  br label %L4
L4:
  %t10 = phi i1 [ 0, %L3 ], [ %t9, %L7 ]
  br i1 %t10, label %L0, label %L1
L0:
  call void @printInt(i32 1)
  br label %L1
L1:
  %t11 = load double, double* %t0
  %t12 = fcmp ogt double 1325.000000, %t11
  br i1 %t12, label %L11, label %L12
L12:
  br label %L13
L11:
  %t13 = load double, double* %t0
  %t14 = fcmp olt double %t13, 1325.000000
  br i1 %t14, label %L14, label %L15
L15:
  br label %L16
L14:
  %t15 = load double, double* %t0
  %t16 = fcmp one double %t15, 1325.000000
  br label %L16
L16:
  %t17 = phi i1 [ 0, %L15 ], [ %t16, %L14 ]
  br label %L13
L13:
  %t18 = phi i1 [ 0, %L12 ], [ %t17, %L16 ]
  br i1 %t18, label %L8, label %L9
L8:
  br label %L10
L9:
  call void @printInt(i32 2)
  br label %L10
L10:
  ret i32 0
}

