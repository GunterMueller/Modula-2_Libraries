echo off
rem directory level makefile.
shift
    rem This list is sorted in dependency order.
    rem The first argument should be understood by the makefiles.
cd gen\bin
make %0
if errorlevel 1 goto Error
cd ..\lwp.bin
make %0
if errorlevel 1 goto Error
:TEST cd ..\test.bin
:TEST make %0
:TEST if errorlevel 1 goto Error
:TEST cd ..\test_lwp.bin
:TEST make %0
:TEST if errorlevel 1 goto Error
cd ..\..\io\bin
make %0
if errorlevel 1 goto Error
cd ..\lwp.bin
make %0
if errorlevel 1 goto Error
:TEST cd ..\test.bin
:TEST make %0
:TEST if errorlevel 1 goto Error
:TEST cd ..\test_lwp.bin
:TEST make %0
:TEST if errorlevel 1 goto Error
cd ..\..\fio\bin
make %0
if errorlevel 1 goto Error
cd ..\lwp.bin
make %0
if errorlevel 1 goto Error
:TEST cd ..\test.bin
:TEST make %0
:TEST if errorlevel 1 goto Error
:TEST cd ..\test_lwp.bin
:TEST make %0
:TEST if errorlevel 1 goto Error
cd ..\..\tools\bin
make %0
if errorlevel 1 goto Error
cd ..\..\demo\lwp.bin
make %0
if errorlevel 1 goto Error

goto End

:Error
rem print the name of the directory
echo Error in the following directory:
cd 
:End

rem Return to where we started
cd ..\..
