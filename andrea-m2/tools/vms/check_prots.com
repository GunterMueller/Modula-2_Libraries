$! CHECK_PROT.COM
$! check protections on the files in the mod$library directory,
$! the ones checked here should have world read
$!
$! V1.0, J. Andrea, Mar.30/92, vms 5.4-2
$!
$ say = "write sys$output"
$!
$ say " "
$ say "checking DEF files"
$ type = "*.def;0"
$ gosub files
$!
$ say " "
$ say "checking MOD files"
$ type = "*.mod;0"
$ gosub files
$!
$ say " "
$ say "checking SYM files"
$ type = "*.sym;0"
$ gosub files
$!
$ say " "
$ say "checking mini-man files"
$ type = "miniman.*;0"
$ gosub files
$!
$ exit
$!
$!-------------------------------------------subroutine
$!
$ files:
$ ok = "Y"
$ loop:
$  file = f$search( type )
$  if file .eqs. "" then goto end_loop
$    gosub check
$    goto loop
$ end_loop:
$ if ok then say "all ok"
$ return
$!
$!-------------------------------------------subroutine
$ check:
$   !....protections must contain at least world=read
$   prot1 = f$file_attr( file, "pro" )
$   i     = f$locate( "WORLD", prot1 ) + 6
$   l     = f$length( prot1 )
$   prot2 = f$extract( i, l-i, prot1 )
$   test  = prot2 - "D" - "E" - "W"
$   if test .nes. "R"
$   then
$     ok = "N"
$     file = f$parse( file,,,"NAME" ) + f$parse( file,,, "TYPE" )
$     say "     ", file, "  word prot=", prot2, " doesnt have READ protection"
$   endif
$ return
