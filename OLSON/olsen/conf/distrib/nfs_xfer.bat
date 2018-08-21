echo off
if not %1. == . goto Loop
shift
goto Disk1
:Loop
shift
if %0. == . goto End
goto Disk%0
:Disk1
echo Please insert disk 1
pause
rem make sure copyright gets on there first!
copy copying a:
copy readme a:
copy header a:
copy bugs a:
copy todo a:
echo hello > a:disk1
copy conf\distrib\pc_xfer.bat a:xfer.bat
mkdir a:conf
mkdir a:conf\distrib
mkdir a:conf\lg2
mkdir a:conf\lg3
mkdir a:conf\pc
mkdir a:conf\sun
mkdir a:porting
mkdir a:misc
mkdir a:examples
copy conf\distrib 	a:conf\distrib
copy conf\lg2 		a:conf\lg2
copy conf\lg3 		a:conf\lg3
copy conf\pc 		a:conf\pc
copy conf\sun 		a:conf\sun
copy porting 		a:porting
copy misc 		a:misc
copy examples 		a:examples
if not %0. == . goto Loop

:Disk2
echo Please insert disk 2
pause
copy copying a:
echo hello > a:disk2
mkdir a:gen
mkdir a:gen\shared.src
mkdir a:gen\sun.src
mkdir a:gen\pc.src
mkdir a:gen\lg2.src
mkdir a:gen\lg3.src
mkdir a:gen\lwp.src
copy gen\shared.src  	a:gen\shared.src
copy gen\sun.src	a:gen\sun.src
copy gen\pc.src		a:gen\pc.src
copy gen\lg2.src	a:gen\lg2.src
copy gen\lg3.src	a:gen\lg3.src
copy gen\lwp.src	a:gen\lwp.src
if not %0. == . goto Loop

:Disk3
echo Please insert disk 3
pause
copy copying a:
echo hello > a:disk3
mkdir a:gen
mkdir a:gen\pc_lwp.src
mkdir a:gen\sun_lwp.src
mkdir a:gen\test.src
mkdir a:gen\test_pc.src
mkdir a:gen\test_sun.src
mkdir a:gen\test_lwp.src
mkdir a:gen\testpclw.src
mkdir a:gen\bin
mkdir a:gen\lwp.bin
mkdir a:gen\test.bin
mkdir a:gen\test_lwp.bin
copy gen\pc_lwp.src 	 a:gen\pc_lwp.src
copy gen\sun_lwp.src 	 a:gen\sun_lwp.src
copy gen\test.src	 a:gen\test.src
copy gen\test_pc.src 	 a:gen\test_pc.src
copy gen\test_sun.src 	 a:gen\test_sun.src
copy gen\test_lwp.src 	 a:gen\test_lwp.src
copy gen\testpclw.src 	 a:gen\testpclw.src
copy gen\bin 		 a:gen\bin
copy gen\lwp.bin 	 a:gen\lwp.bin
copy gen\test.bin 	 a:gen\test.bin
copy gen\test_lwp.bin 	 a:gen\test_lwp.bin
if not %0. == . goto Loop

:Disk4
echo Please insert disk 4
pause
copy copying a:
echo hello > a:disk4
mkdir a:io
mkdir a:io\bin
mkdir a:io\lwp.bin
mkdir a:io\lwp.src
mkdir a:io\pc.src
mkdir a:io\shared.src
mkdir a:io\sun.src
mkdir a:io\sun_lwp.src
mkdir a:io\test.bin
mkdir a:io\test.src
mkdir a:io\test_lwp.bin
mkdir a:io\test_pc.src
mkdir a:io\test_sun.src
mkdir a:io\testsunl.src
copy io\bin		a:io\bin
copy io\lwp.bin		a:io\lwp.bin
copy io\lwp.src		a:io\lwp.src
copy io\pc.src		a:io\pc.src
copy io\shared.src	a:io\shared.src
copy io\sun.src		a:io\sun.src
copy io\sun_lwp.src	a:io\sun_lwp.src
copy io\test.bin	a:io\test.bin
copy io\test.src	a:io\test.src
copy io\test_lwp.bin	a:io\test_lwp.bin
copy io\test_pc.src	a:io\test_pc.src
copy io\test_sun.src	a:io\test_sun.src
copy io\testsunl.src	a:io\testsunl.src
if not %0. == . goto Loop

:Disk5
echo Please insert disk 5
pause
copy copying a:
echo hello > a:disk5
mkdir a:fio
mkdir a:fio\bin
mkdir a:fio\lwp.bin
mkdir a:fio\shared.src
mkdir a:fio\test.bin
mkdir a:fio\test.dat
mkdir a:fio\test.src
mkdir a:fio\test_lwp.bin
mkdir a:tools
mkdir a:tools\shared.src
mkdir a:tools\sun.src
mkdir a:tools\bin
mkdir a:demo
mkdir a:demo\lwp.src
mkdir a:demo\pc_lwp.src
mkdir a:demo\lwp.bin
mkdir a:doc
copy fio\bin		a:fio\bin
copy fio\lwp.bin	a:fio\lwp.bin
copy fio\shared.src	a:fio\shared.src
copy fio\test.bin	a:fio\test.bin
copy fio\test.dat	a:fio\test.dat
copy fio\test.src	a:fio\test.src
copy fio\test_lwp.bin	a:fio\test_lwp.bin
copy tools\shared.src	a:tools\shared.src
copy tools\sun.src	a:tools\sun.src
copy tools\bin		a:tools\bin
copy demo\lwp.src	a:demo\lwp.src
copy demo\pc_lwp.src	a:demo\pc_lwp.src
copy demo\lwp.bin	a:demo\lwp.bin
copy doc		a:doc
:End
