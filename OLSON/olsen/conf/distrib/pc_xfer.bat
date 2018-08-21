echo off
if %1. == . goto Usage
if %2. == . goto Usage
rem If already copied to dest, do the script.
if %3. == ONDEST. goto Disk1
copy %1:%0.bat %2:%0.bat
if not exist %2:%0.bat goto OnDestError
rem Switch drives and do the copy
%2:
%0 %1 %2 ONDEST
:Usage
    echo Usage: %0 source-drive dest-drive
    echo        The dest-drive must be cd'd to the library root directory.
    echo        See the installation manual for more details.
    goto End

:OnDestError
    echo The file %1%0 does not exist or %2%0 could not be created
    echo Note that %1 and %2 must be DOS drive letters e.g. "a" and "c"
    goto Usage

rem here is the go
:Disk1
if exist %1:disk1 goto Disk1Copy
    echo Please insert disk 1
    pause
    goto Disk1

:Disk1Copy
rem make sure copyright gets on there first!
copy %1:copying
copy %1:readme
copy %1:header
copy %1:todo
copy %1:bugs
mkdir conf
mkdir conf\distrib
mkdir conf\lg2
mkdir conf\lg3
mkdir conf\pc
mkdir conf\sun
mkdir porting
mkdir misc
mkdir examples
copy %1:conf\distrib 	conf\distrib
copy %1:conf\lg2 	conf\lg2
copy %1:conf\lg3 	conf\lg3
copy %1:conf\pc 	conf\pc
copy %1:conf\sun 	conf\sun
copy %1:porting 	porting
copy %1:misc 		misc
copy %1:examples 	examples

:Disk2
if exist %1:disk2 goto Disk2Copy
    echo Please insert disk 2
    pause
    goto Disk2

:Disk2Copy
mkdir gen
mkdir gen\shared.src
mkdir gen\sun.src
mkdir gen\pc.src
mkdir gen\lg2.src
mkdir gen\lg3.src
mkdir gen\lwp.src
copy %1:gen\shared.src  gen\shared.src
copy %1:gen\sun.src	gen\sun.src
copy %1:gen\pc.src	gen\pc.src
copy %1:gen\lg2.src	gen\lg2.src
copy %1:gen\lg3.src	gen\lg3.src
copy %1:gen\lwp.src	gen\lwp.src

:Disk3
if exist %1:disk3 goto Disk3Copy
    echo Please insert disk 3
    pause
    goto Disk3

:Disk3Copy
echo May get unable to create message here.
mkdir gen
mkdir gen\pc_lwp.src
mkdir gen\sun_lwp.src
mkdir gen\test.src
mkdir gen\test_pc.src
mkdir gen\test_sun.src
mkdir gen\test_lwp.src
mkdir gen\testpclw.src
mkdir gen\bin
mkdir gen\lwp.bin
mkdir gen\test.bin
mkdir gen\test_lwp.bin
copy %1:gen\pc_lwp.src 		gen\pc_lwp.src
copy %1:gen\sun_lwp.src 	gen\sun_lwp.src
copy %1:gen\test.src	 	gen\test.src
copy %1:gen\test_pc.src 	gen\test_pc.src
copy %1:gen\test_sun.src 	gen\test_sun.src
copy %1:gen\test_lwp.src 	gen\test_lwp.src
copy %1:gen\testpclw.src 	gen\testpclw.src
copy %1:gen\bin 		gen\bin
copy %1:gen\lwp.bin 		gen\lwp.bin
copy %1:gen\test.bin 		gen\test.bin
copy %1:gen\test_lwp.bin 	gen\test_lwp.bin

:Disk4
if exist %1:disk4 goto Disk4Copy
    echo Please insert disk 4
    pause
    goto Disk4

:Disk4Copy
mkdir io
mkdir io\bin
mkdir io\lwp.bin
mkdir io\lwp.src
mkdir io\pc.src
mkdir io\shared.src
mkdir io\sun.src
mkdir io\sun_lwp.src
mkdir io\test.bin
mkdir io\test.src
mkdir io\test_lwp.bin
mkdir io\test_pc.src
mkdir io\test_sun.src
mkdir io\testsunl.src
copy %1:io\bin		io\bin
copy %1:io\lwp.bin	io\lwp.bin
copy %1:io\lwp.src	io\lwp.src
copy %1:io\pc.src	io\pc.src
copy %1:io\shared.src	io\shared.src
copy %1:io\sun.src	io\sun.src
copy %1:io\sun_lwp.src	io\sun_lwp.src
copy %1:io\test.bin	io\test.bin
copy %1:io\test.src	io\test.src
copy %1:io\test_lwp.bin	io\test_lwp.bin
copy %1:io\test_pc.src	io\test_pc.src
copy %1:io\testsunl.src	io\testsunl.src

:Disk5
if exist %1:disk5 goto Disk5Copy
    echo Please insert disk 5
    pause
    goto Disk5

:Disk5Copy
mkdir fio
mkdir fio\bin
mkdir fio\lwp.bin
mkdir fio\shared.src
mkdir fio\test.bin
mkdir fio\test.dat
mkdir fio\test.src
mkdir fio\test_lwp.bin
mkdir tools
mkdir tools\shared.src
mkdir tools\sun.src
mkdir tools\bin
mkdir demo
mkdir demo\lwp.src
mkdir demo\pc_lwp.src
mkdir demo\lwp.bin
mkdir doc

copy %1:fio\bin			fio\bin
copy %1:fio\lwp.bin		fio\lwp.bin
copy %1:fio\shared.src		fio\shared.src
copy %1:fio\test.bin		fio\test.bin
copy %1:fio\test.dat		fio\test.dat
copy %1:fio\test.src		fio\test.src
copy %1:fio\test_lwp.bin	fio\test_lwp.bin
copy %1:tools\shared.src	tools\shared.src
copy %1:tools\sun.src		tools\sun.src
copy %1:tools\bin		tools\bin
copy %1:demo\lwp.src		demo\lwp.src
copy %1:demo\pc_lwp.src		demo\pc_lwp.src
copy %1:demo\lwp.bin		demo\lwp.bin
copy %1:doc			doc

echo Copying complete.  Now proceed to the xxx_inst procedure.
del %2:%0.bat

:End

