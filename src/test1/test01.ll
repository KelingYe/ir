declare i32 @getch( )
declare i32 @getint( )
declare void @putch( i32 )
declare void @putint( i32 )
declare void @putarray( i32, i32* )
declare void @_sysy_starttime( i32 )
declare void @_sysy_stoptime( i32 )
@a = global [ 10 x i32 ] zeroinitializer
@b = global i32 27
@c = global i32 1
define i32 @main( ) {
main:
  %r100 = alloca i32
  %r101 = alloca i32
  call void @_sysy_starttime(i32 6)
  store i32 0, i32* %r100
  store i32 0, i32* %r101
  br label %bb1

bb1:
  %r102 = load i32, i32* %r100
  %r103 = icmp slt i32 %r102, 10
  br i1 %r103, label %bb2, label %bb3

bb2:
  %r104 = load i32, i32* %r100
  %r105 = load i32, i32* %r100
  %r106 = getelementptr [10 x i32 ], [10 x i32 ]* @a, i32 0, i32 %r105
  store i32 %r104, i32* %r106
  %r107 = load i32, i32* %r100
  %r108 = add i32 %r107, 1
  store i32 %r108, i32* %r100
  br label %bb1

bb3:
  store i32 0, i32* %r100
  br label %bb4

bb4:
  %r109 = load i32, i32* %r100
  %r110 = icmp slt i32 %r109, 10
  br i1 %r110, label %bb5, label %bb6

bb5:
  %r111 = load i32, i32* %r101
  %r112 = load i32, i32* %r100
  %r114 = getelementptr [10 x i32 ], [10 x i32 ]* @a, i32 0, i32 %r112
  %r113 = load i32, i32* %r114
  %r115 = add i32 %r111, %r113
  store i32 %r115, i32* %r101
  %r116 = load i32, i32* %r100
  %r117 = add i32 %r116, 1
  store i32 %r117, i32* %r100
  br label %bb4

bb6:
  %r118 = load i32, i32* @b
  call void @putint(i32 %r118)
  %r119 = load i32, i32* @c
  call void @putint(i32 %r119)
  %r120 = load i32, i32* %r101
  call void @putint(i32 %r120)
  call void @_sysy_stoptime(i32 23)
  ret i32 0
}

