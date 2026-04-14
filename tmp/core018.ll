declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

@.str.0 = private constant [5 x i8] c"yay!\00"
@.str.1 = private constant [5 x i8] c"nay!\00"

define i32 @main() {
entry:
  %t0 = alloca i32
  %t1 = call i32 @readInt()
  store i32 %t1, i32* %t0
  %t2 = alloca double
  %t3 = call double @readDouble()
  store double %t3, double* %t2
  %t4 = load i32, i32* %t0
  %t5 = sub i32 %t4, 5
  call void @printInt(i32 %t5)
  %t6 = load double, double* %t2
  %t7 = fcmp ogt double %t6, 42.000000
  br i1 %t7, label %L3, label %L4
L3:
  br label %L5
L4:
  %t8 = load double, double* %t2
  %t9 = fcmp olt double %t8, 43.000000
  br label %L5
L5:
  %t10 = phi i1 [ 1, %L3 ], [ %t9, %L4 ]
  br i1 %t10, label %L0, label %L1
L0:
  %t11 = getelementptr [5 x i8], [5 x i8]* @.str.0, i32 0, i32 0
  call void @printString(i8* %t11)
  br label %L2
L1:
  %t12 = getelementptr [5 x i8], [5 x i8]* @.str.1, i32 0, i32 0
  call void @printString(i8* %t12)
  br label %L2
L2:
  ret i32 0
}

