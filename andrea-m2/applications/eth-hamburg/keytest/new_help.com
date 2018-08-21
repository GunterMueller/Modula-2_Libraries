$! new_help.com
$! create or modify the help library
$!
$ lib = "[-]keytest.hlb"
$!
$ if f$search( lib ) .eqs. ""
$ then
$   !--create the library
$   library /log /create /help 'lib'
$   set prot=w:r 'lib'
$ endif
$!
$ library /log /replace /help 'lib' keytest.hlp
$!
$ exit
