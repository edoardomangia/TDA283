declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

@.str.0 = private constant [3 x i8] c"no\00"
@.str.1 = private constant [4 x i8] c"yes\00"

define i32 @main() {
entry:
  %t0 = icmp slt i32 1, 6
  br i1 %t0, label %L2, label %L3
L2:
  br label %L4
L3:
  %t1 = call i1 @no()
  br label %L4
L4:
  %t2 = phi i1 [ 1, %L2 ], [ %t1, %L3 ]
  br i1 %t2, label %L0, label %L1
L0:
  call void @printInt(i32 1)
  br label %L1
L1:
  %t3 = add i32 2, 2
  %t4 = icmp ne i32 %t3, 4
  br i1 %t4, label %L7, label %L8
L8:
  br label %L9
L7:
  %t5 = call i1 @no()
  br label %L9
L9:
  %t6 = phi i1 [ 0, %L8 ], [ %t5, %L7 ]
  br i1 %t6, label %L5, label %L6
L5:
  call void @printInt(i32 2)
  br label %L6
L6:
  %t7 = icmp slt i32 5, 5
  br i1 %t7, label %L12, label %L13
L12:
  br label %L14
L13:
  %t8 = call i1 @yes()
  br label %L14
L14:
  %t9 = phi i1 [ 1, %L12 ], [ %t8, %L13 ]
  br i1 %t9, label %L10, label %L11
L10:
  call void @printInt(i32 3)
  br label %L11
L11:
  %t10 = fcmp oge double 0.400000, 0.300000
  br i1 %t10, label %L17, label %L18
L18:
  br label %L19
L17:
  %t11 = call i1 @yes()
  br label %L19
L19:
  %t12 = phi i1 [ 0, %L18 ], [ %t11, %L17 ]
  br i1 %t12, label %L15, label %L16
L15:
  call void @printInt(i32 4)
  br label %L16
L16:
  ret i32 0
}

define i1 @no() {
entry:
  %t0 = getelementptr [3 x i8], [3 x i8]* @.str.0, i32 0, i32 0
  call void @printString(i8* %t0)
  ret i1 0
}

define i1 @yes() {
entry:
  %t0 = getelementptr [4 x i8], [4 x i8]* @.str.1, i32 0, i32 0
  call void @printString(i8* %t0)
  ret i1 1
}

