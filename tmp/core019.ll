declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

@.str.0 = private constant [4 x i8] c"foo\00"

define i32 @main() {
entry:
  %t0 = alloca i32
  store i32 78, i32* %t0
  %t1 = alloca i32
  store i32 1, i32* %t1
  %t2 = load i32, i32* %t1
  call void @printInt(i32 %t2)
  %t3 = load i32, i32* %t0
  call void @printInt(i32 %t3)
  br label %L0
L0:
  %t4 = load i32, i32* %t0
  %t5 = icmp sgt i32 %t4, 76
  br i1 %t5, label %L1, label %L2
L1:
  %t6 = load i32, i32* %t0
  %t7 = sub i32 %t6, 1
  store i32 %t7, i32* %t0
  %t8 = load i32, i32* %t0
  call void @printInt(i32 %t8)
  %t9 = alloca i32
  %t10 = load i32, i32* %t0
  %t11 = add i32 %t10, 7
  store i32 %t11, i32* %t9
  %t12 = load i32, i32* %t9
  call void @printInt(i32 %t12)
  br label %L0
L2:
  %t13 = load i32, i32* %t0
  call void @printInt(i32 %t13)
  %t14 = load i32, i32* %t0
  %t15 = icmp sgt i32 %t14, 4
  br i1 %t15, label %L3, label %L4
L3:
  %t16 = alloca i32
  store i32 4, i32* %t16
  %t17 = load i32, i32* %t16
  call void @printInt(i32 %t17)
  br label %L5
L4:
  %t18 = getelementptr [4 x i8], [4 x i8]* @.str.0, i32 0, i32 0
  call void @printString(i8* %t18)
  br label %L5
L5:
  %t19 = load i32, i32* %t0
  call void @printInt(i32 %t19)
  ret i32 0
}

