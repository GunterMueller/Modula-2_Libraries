$ Conformance test 1. 
$ "Weird" cases for the RealConvert - syntax parser. 
$
#test1
00000000000000000000000000000000000000000000000000000000000000000000.0
floating
long
9
7
#test1
00000000000000000000000000000000000.0000000000000000000000000000000000
floating
long
18
16
#test1
0.00000000000000000000000000000000000000000000000000000000000000000000
floating
long
3
1
#test1
00000000000000000000000000000000000000000000000000000000000000000000.1
floating
long
5
1
#test1
-0000000000000000000000000000000000000000000000000000000000000000000.1
floating
long
70
1
#test1
+666.666
floating
long
9
4
#test1
1E+5
floating
long
10
1
#test1
1E-5
floating
long
10
6
#test1
1E0000000000000000000000000000000000000000000000000000000000000000005
floating
long
10
1
#test1
1E-000000000000000000000000000000000000000000000000000000000000000005
floating
long
10
6
#test1
000000000000000000000000000000000000000000000000000000000000000001E05
floating
long
10
1
