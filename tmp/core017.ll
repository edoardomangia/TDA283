declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

@.str.0 = private constant [4 x i8] c"apa\00"
@.str.1 = private constant [5 x i8] c"true\00"
@.str.2 = private constant [6 x i8] c"false\00"

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 4, i32* %t0
  %t1 = load i32, i32* %t0
  %t2 = icmp sle i32 3, %t1
  br i1 %t2, label %L3, label %L4
L4:
  br label %L5
L3:
  %t3 = icmp ne i32 4, 2
  br i1 %t3, label %L6, label %L7
L7:
  br label %L8
L6:
  br label %L8
L8:
  %t4 = phi i1 [ 0, %L7 ], [ 1, %L6 ]
  br label %L5
L5:
  %t5 = phi i1 [ 0, %L4 ], [ %t4, %L3 ]
  br i1 %t5, label %L0, label %L1
L0:
  call void @printBool(i1 1)
  br label %L2
L1:
  %t6 = getelementptr [4 x i8], [4 x i8]* @.str.0, i32 0, i32 0
  call void @printString(i8* %t6)
  br label %L2
L2:
  %t7 = icmp eq i1 1, 1
  br i1 %t7, label %L9, label %L10
L9:
  br label %L11
L10:
  %t8 = call i1 @dontCallMe(i32 1)
  br label %L11
L11:
  %t9 = phi i1 [ 1, %L9 ], [ %t8, %L10 ]
  call void @printBool(i1 %t9)
  %t10 = fsub double 0.0, 50.000000
  %t11 = fcmp olt double 4.000000, %t10
  br i1 %t11, label %L12, label %L13
L13:
  br label %L14
L12:
  %t12 = call i1 @dontCallMe(i32 2)
  br label %L14
L14:
  %t13 = phi i1 [ 0, %L13 ], [ %t12, %L12 ]
  call void @printBool(i1 %t13)
  %t14 = load i32, i32* %t0
  %t15 = icmp eq i32 4, %t14
  br i1 %t15, label %L15, label %L16
L16:
  br label %L17
L15:
  %t16 = xor i1 0, 1
  %t17 = icmp eq i1 1, %t16
  br i1 %t17, label %L18, label %L19
L19:
  br label %L20
L18:
  br label %L20
L20:
  %t18 = phi i1 [ 0, %L19 ], [ 1, %L18 ]
  br label %L17
L17:
  %t19 = phi i1 [ 0, %L16 ], [ %t18, %L15 ]
  call void @printBool(i1 %t19)
  %t20 = call i1 @implies(i1 0, i1 0)
  call void @printBool(i1 %t20)
  %t21 = call i1 @implies(i1 0, i1 1)
  call void @printBool(i1 %t21)
  %t22 = call i1 @implies(i1 1, i1 0)
  call void @printBool(i1 %t22)
  %t23 = call i1 @implies(i1 1, i1 1)
  call void @printBool(i1 %t23)
  ret i32 0
}

define i1 @dontCallMe(i32 %__p__x) {
entry:
  %t0 = alloca i32
  store i32 %__p__x, i32* %t0
  %t1 = load i32, i32* %t0
  call void @printInt(i32 %t1)
  ret i1 1
}

define void @printBool(i1 %__p__b) {
entry:
  %t0 = alloca i1
  store i1 %__p__b, i1* %t0
  %t1 = load i1, i1* %t0
  br i1 %t1, label %L0, label %L1
L0:
  %t2 = getelementptr [5 x i8], [5 x i8]* @.str.1, i32 0, i32 0
  call void @printString(i8* %t2)
  br label %L2
L1:
  %t3 = getelementptr [6 x i8], [6 x i8]* @.str.2, i32 0, i32 0
  call void @printString(i8* %t3)
  br label %L2
L2:
  ret void
}

define i1 @implies(i1 %__p__x, i1 %__p__y) {
entry:
  %t0 = alloca i1
  store i1 %__p__x, i1* %t0
  %t1 = alloca i1
  store i1 %__p__y, i1* %t1
  %t2 = load i1, i1* %t0
  %t3 = xor i1 %t2, 1
  br i1 %t3, label %L0, label %L1
L0:
  br label %L2
L1:
  %t4 = load i1, i1* %t0
  %t5 = load i1, i1* %t1
  %t6 = icmp eq i1 %t4, %t5
  br label %L2
L2:
  %t7 = phi i1 [ 1, %L0 ], [ %t6, %L1 ]
  ret i1 %t7
}

