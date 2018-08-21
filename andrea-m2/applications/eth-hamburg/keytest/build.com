$! build.com  - build the keytest program
$ modula /log /nocheck /nodebug keytest
$ link keytest + mod$system:modula.olb/lib
$ delete /nolog *.obj;*
$ set noon
$ purge /nolog keytest.exe
$ ren keytest.exe [-]*/lo
$ purge /nolog [-]keytest.exe
$ set prot=w:e [-]keytest.exe
$ exit
