echo off
mkdir lib
mkdir lib\def
mkdir lib\mod
mkdir lib\sym
mkdir lib\obj
mkdir lib\ref
mkdir lwp.lib
mkdir lwp.lib\def
mkdir lwp.lib\mod
mkdir lwp.lib\sym
mkdir lwp.lib\obj
mkdir lwp.lib\ref
mkdir bin
copy conf\lg3 bin
copy conf\pc\mainmake.bat make.bat
copy conf\pc\mk_inst.bat bin
copy conf\pc\*.exe bin
rem The following is counter-intuitive, but it works.
ren fio\bin\depend.lg3 depend.mak
ren fio\bin\environ.lg3 environ.mak
ren fio\bin\makefile.lg3 makefile
ren fio\bin\m2depend.lg3 m2depend.mak
ren fio\bin\m2source.lg3 m2source.mak
ren fio\test.bin\depend.lg3 depend.mak
ren fio\test.bin\environ.lg3 environ.mak
ren fio\test.bin\makefile.lg3 makefile
ren fio\test.bin\m2depend.lg3 m2depend.mak
ren fio\test.bin\m2source.lg3 m2source.mak
ren fio\test_lwp.bin\depend.lg3 depend.mak
ren fio\test_lwp.bin\environ.lg3 environ.mak
ren fio\test_lwp.bin\makefile.lg3 makefile
ren fio\test_lwp.bin\m2depend.lg3 m2depend.mak
ren fio\test_lwp.bin\m2source.lg3 m2source.mak
ren fio\lwp.bin\depend.lg3 depend.mak
ren fio\lwp.bin\environ.lg3 environ.mak
ren fio\lwp.bin\makefile.lg3 makefile
ren fio\lwp.bin\m2depend.lg3 m2depend.mak
ren fio\lwp.bin\m2source.lg3 m2source.mak
ren gen\test.bin\depend.lg3 depend.mak
ren gen\test.bin\environ.lg3 environ.mak
ren gen\test.bin\makefile.lg3 makefile
ren gen\test.bin\m2depend.lg3 m2depend.mak
ren gen\test.bin\m2source.lg3 m2source.mak
ren gen\bin\depend.lg3 depend.mak
ren gen\bin\environ.lg3 environ.mak
ren gen\bin\makefile.lg3 makefile
ren gen\bin\m2depend.lg3 m2depend.mak
ren gen\bin\m2source.lg3 m2source.mak
ren gen\test_lwp.bin\depend.lg3 depend.mak
ren gen\test_lwp.bin\environ.lg3 environ.mak
ren gen\test_lwp.bin\makefile.lg3 makefile
ren gen\test_lwp.bin\m2depend.lg3 m2depend.mak
ren gen\test_lwp.bin\m2source.lg3 m2source.mak
ren gen\lwp.bin\depend.lg3 depend.mak
ren gen\lwp.bin\environ.lg3 environ.mak
ren gen\lwp.bin\makefile.lg3 makefile
ren gen\lwp.bin\m2source.lg3 m2source.mak
ren gen\lwp.bin\m2depend.lg3 m2depend.mak
ren tools\bin\depend.lg3 depend.mak
ren tools\bin\environ.lg3 environ.mak
ren tools\bin\makefile.lg3 makefile
ren tools\bin\m2depend.lg3 m2depend.mak
ren tools\bin\m2source.lg3 m2source.mak
ren demo\lwp.bin\depend.lg3 depend.mak
ren demo\lwp.bin\environ.lg3 environ.mak
ren demo\lwp.bin\makefile.lg3 makefile
ren demo\lwp.bin\m2depend.lg3 m2depend.mak
ren demo\lwp.bin\m2source.lg3 m2source.mak
ren io\bin\depend.lg3 depend.mak
ren io\bin\environ.lg3 environ.mak
ren io\bin\makefile.lg3 makefile
ren io\bin\m2depend.lg3 m2depend.mak
ren io\bin\m2source.lg3 m2source.mak
ren io\test_lwp.bin\depend.lg3 depend.mak
ren io\test_lwp.bin\environ.lg3 environ.mak
ren io\test_lwp.bin\makefile.lg3 makefile
ren io\test_lwp.bin\m2depend.lg3 m2depend.mak
ren io\test_lwp.bin\m2source.lg3 m2source.mak
ren io\test.bin\depend.lg3 depend.mak
ren io\test.bin\environ.lg3 environ.mak
ren io\test.bin\makefile.lg3 makefile
ren io\test.bin\m2depend.lg3 m2depend.mak
ren io\test.bin\m2source.lg3 m2source.mak
ren io\lwp.bin\depend.lg3 depend.mak
ren io\lwp.bin\environ.lg3 environ.mak
ren io\lwp.bin\makefile.lg3 makefile
ren io\lwp.bin\m2depend.lg3 m2depend.mak
ren io\lwp.bin\m2source.lg3 m2source.mak
