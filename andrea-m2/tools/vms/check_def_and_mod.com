$! CHECK_DEF_AND_MOD.COM
$! check that *.DEF files have matching .MOD files, and verse visa
$!
$! For *.DEF files, foreign definition files are skipped.
$! For *.MOD files, only implementation modules are checked.
$!
$! V1.0, Jaa, Aug.15/91
$!
$ say = "write sys$output"
$!
$ say " "
$!
$ i = 0
$ def_loop:
$   def = f$search( "*.def;0" )
$   if def .eqs. "" then goto end_def_loop
$     name = f$parse( def,,,"NAME" )
$     i = i + 1
$     set noon
$     define /user_mode sys$output nla0:
$     define /user_mode sys$error  nla0:
$     search 'def' "%FOREIGN","DEFINITION","MODULE" /match=and /exact
$     status = $status
$     set on
$     if status .ne. 1
$     then
$       mod  = name + ".mod"
$       if f$search( mod, 2 ) .eqs. "" then -
          say "no matching .MOD file for ", name, ".DEF"
$     endif
$     goto def_loop
$ end_def_loop:
$!
$ say " "
$ say "     all DEF files checked ", f$string(i)
$!
$ say " "
$ i = 0
$ mod_loop:
$   mod = f$search( "*.mod;0" )
$   if mod .eqs. "" then goto end_mod_loop
$     name = f$parse( mod,,,"NAME" )
$     i = i + 1
$     set noon
$     define /user_mode sys$output nla0:
$     define /user_mode sys$error  nla0:
$     search 'mod' "IMPLEMENTATION","MODULE" /match=and /exact
$     status = $status
$     set on
$     if status .eq. 1
$     then
$       def  = name + ".def"
$       if f$search( def, 2 ) .eqs. "" then -
           say "no matching .DEF file for ", name, ".MOD"
$     endif
$     goto mod_loop
$ end_mod_loop:
$!
$ say " "
$ say "     all MOD files checked ", f$string(i)
$!
$ exit
