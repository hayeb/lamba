@.str = private constant [16 x i8] c"Fibonacci done\0A\00"
declare i32 @puts(i8* nocapture) nounwind

define i64 @fib(i64 %arg0) {
match0arg0:
	%loc0 = icmp eq i64 %arg0, 0
	br i1 %loc0, label %body0, label %match1arg0
body0:
	ret i64 1
match1arg0:
	%loc2 = icmp eq i64 %arg0, 1
	br i1 %loc2, label %body1, label %match2arg0
body1:
	ret i64 1
match2arg0:
	%loc4 = add i1 0, 1
	br i1 %loc4, label %body2, label %noMatchLabel
body2:
	%loc6 = sub i64 %arg0, 1
	%loc7 = tail call ccc i64 @fib(i64 %loc6)
	%loc9 = sub i64 %arg0, 2
	%loc10 = tail call ccc i64 @fib(i64 %loc9)
	%loc11 = add i64 %loc7, %loc10
	ret i64 %loc11
noMatchLabel:
	ret i64 -1
}
define i64 @test(i64 %arg0, i64 %arg1) {
match0arg0:
	%loc12 = icmp eq i64 %arg0, 0
	br i1 %loc12, label %match0arg1, label %match1arg0
match0arg1:
	%loc13 = icmp eq i64 %arg1, 0
	br i1 %loc13, label %body0, label %match1arg0
body0:
	ret i64 1
match1arg0:
	%loc15 = icmp eq i64 %arg0, 0
	br i1 %loc15, label %match1arg1, label %noMatchLabel
match1arg1:
	%loc16 = icmp eq i64 %arg1, 1
	br i1 %loc16, label %body1, label %noMatchLabel
body1:
	ret i64 2
noMatchLabel:
	ret i64 -1
}
define i64 @main() {
	br label %body0
body0:
	%loc19 = tail call ccc i64 @fib(i64 15)
	%cast210 = getelementptr [16 x i8],[16 x i8]* @.str, i64 0, i64 0
	%huh = call i32 @puts(i8* %cast210)
	ret i64 %loc19
noMatchLabel:
	ret i64 -1
}
