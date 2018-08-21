echo off
echo Deleting all files not related to Logitech 3.0
echo y | del conf\lg2
rmdir conf\lg2
echo y | del conf\sun
rmdir conf\sun
del fio\bin\*.lg2 
del fio\bin\*.sun 
del fio\test.bin\*.lg2
del fio\test.bin\*.sun
del fio\test_lwp.bin\*.lg2
del fio\test_lwp.bin\*.sun
del fio\lwp.bin\*.lg2
del fio\lwp.bin\*.sun 
echo y | del gen\lg2.src
rmdir gen\lg2.src
echo y | del gen\sun.src
rmdir gen\sun.src
echo y | del gen\sun_lwp.src
rmdir gen\sun_lwp.src
echo y | del gen\test_sun.src
rmdir gen\test_sun.src
del gen\test.bin\*.lg2
del gen\test.bin\*.sun
del gen\bin\*.lg2
del gen\bin\*.sun
del gen\test_lwp.bin\*.lg2
del gen\test_lwp.bin\*.sun
del gen\lwp.bin\*.lg2
del gen\lwp.bin\*.sun
del tools\bin\*.lg2
del tools\bin\*.sun
del tools\bin\*.lg2
del tools\bin\*.sun
del demo\lwp.bin\*.lg2
del demo\lwp.bin\*.sun
echo y | del io\sun.src
rmdir io\sun.src
echo y | del io\sun_lwp.src
rmdir io\sun_lwp.src
echo y | del io\test_sun.src
rmdir io\test_sun.src
echo y | del io\testsunl.src
rmdir io\testsunl.src
del io\bin\*.lg2
del io\bin\*.sun
del io\test_lwp.bin\*.lg2
del io\test_lwp.bin\*.sun
del io\test.bin\*.lg2
del io\test.bin\*.sun
del io\lwp.bin\*.lg2
del io\lwp.bin\*.sun
