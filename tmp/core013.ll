declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

@.str.0 = private constant [3 x i8] c"&&\00"
@.str.1 = private constant [3 x i8] c"||\00"
@.str.2 = private constant [2 x i8] c"!\00"
@.str.3 = private constant [6 x i8] c"false\00"
@.str.4 = private constant [5 x i8] c"true\00"

define i32 @main() {
entry:
  %t0 = getelementptr [3 x i8], [3 x i8]* @.str.0, i32 0, i32 0
  call void @printString(i8* %t0)
  %t1 = sub i32 0, 1
  %t2 = call i1 @test(i32 %t1)
  br i1 %t2, label %L0, label %L1
L1:
  br label %L2
L0:
  %t3 = call i1 @test(i32 0)
  br label %L2
L2:
  %t4 = phi i1 [ 0, %L1 ], [ %t3, %L0 ]
  call void @printBool(i1 %t4)
  %t5 = sub i32 0, 2
  %t6 = call i1 @test(i32 %t5)
  br i1 %t6, label %L3, label %L4
L4:
  br label %L5
L3:
  %t7 = call i1 @test(i32 1)
  br label %L5
L5:
  %t8 = phi i1 [ 0, %L4 ], [ %t7, %L3 ]
  call void @printBool(i1 %t8)
  %t9 = call i1 @test(i32 3)
  br i1 %t9, label %L6, label %L7
L7:
  br label %L8
L6:
  %t10 = sub i32 0, 5
  %t11 = call i1 @test(i32 %t10)
  br label %L8
L8:
  %t12 = phi i1 [ 0, %L7 ], [ %t11, %L6 ]
  call void @printBool(i1 %t12)
  %t13 = call i1 @test(i32 234234)
  br i1 %t13, label %L9, label %L10
L10:
  br label %L11
L9:
  %t14 = call i1 @test(i32 21321)
  br label %L11
L11:
  %t15 = phi i1 [ 0, %L10 ], [ %t14, %L9 ]
  call void @printBool(i1 %t15)
  %t16 = getelementptr [3 x i8], [3 x i8]* @.str.1, i32 0, i32 0
  call void @printString(i8* %t16)
  %t17 = sub i32 0, 1
  %t18 = call i1 @test(i32 %t17)
  br i1 %t18, label %L12, label %L13
L12:
  br label %L14
L13:
  %t19 = call i1 @test(i32 0)
  br label %L14
L14:
  %t20 = phi i1 [ 1, %L12 ], [ %t19, %L13 ]
  call void @printBool(i1 %t20)
  %t21 = sub i32 0, 2
  %t22 = call i1 @test(i32 %t21)
  br i1 %t22, label %L15, label %L16
L15:
  br label %L17
L16:
  %t23 = call i1 @test(i32 1)
  br label %L17
L17:
  %t24 = phi i1 [ 1, %L15 ], [ %t23, %L16 ]
  call void @printBool(i1 %t24)
  %t25 = call i1 @test(i32 3)
  br i1 %t25, label %L18, label %L19
L18:
  br label %L20
L19:
  %t26 = sub i32 0, 5
  %t27 = call i1 @test(i32 %t26)
  br label %L20
L20:
  %t28 = phi i1 [ 1, %L18 ], [ %t27, %L19 ]
  call void @printBool(i1 %t28)
  %t29 = call i1 @test(i32 234234)
  br i1 %t29, label %L21, label %L22
L21:
  br label %L23
L22:
  %t30 = call i1 @test(i32 21321)
  br label %L23
L23:
  %t31 = phi i1 [ 1, %L21 ], [ %t30, %L22 ]
  call void @printBool(i1 %t31)
  %t32 = getelementptr [2 x i8], [2 x i8]* @.str.2, i32 0, i32 0
  call void @printString(i8* %t32)
  call void @printBool(i1 1)
  call void @printBool(i1 0)
  ret i32 0
}

define void @printBool(i1 %__p__b) {
entry:
  %t0 = alloca i1
  store i1 %__p__b, i1* %t0
  %t1 = load i1, i1* %t0
  %t2 = xor i1 %t1, 1
  br i1 %t2, label %L0, label %L1
L0:
  %t3 = getelementptr [6 x i8], [6 x i8]* @.str.3, i32 0, i32 0
  call void @printString(i8* %t3)
  br label %L2
L1:
  %t4 = getelementptr [5 x i8], [5 x i8]* @.str.4, i32 0, i32 0
  call void @printString(i8* %t4)
  br label %L2
L2:
  ret void
}

define i1 @test(i32 %__p__i) {
entry:
  %t0 = alloca i32
  store i32 %__p__i, i32* %t0
  %t1 = load i32, i32* %t0
  call void @printInt(i32 %t1)
  %t2 = load i32, i32* %t0
  %t3 = icmp sgt i32 %t2, 0
  ret i1 %t3
}

