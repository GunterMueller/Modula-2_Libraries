$!
$! Use this procedure to update all the fortran routines
$! which are called by foreign definitions and are part of the
$! modula-2 library.
$!
$! V1.0, J. Andrea, Aug.9/91, VMS 5.4-2
$!
$ say = "write sys$output"
$!
$ loop:
$    file = f$search( "*.for;0", 2 )
$    if file .eqs. "" then goto end_loop
$      !
$      name = f$parse( file,,,"NAME" )
$      obj  = name + ".obj"
$      !
$      if f$search( obj ) .nes. "" then delete /nolog 'obj';*
$      !
$      say " "
$      say "compiling ", name, ".for"
$      say " "
$      !
$      set noon
$      fortran /noopt 'name'
$      status = $status
$      set on
$      if status
$      then
$        library /log /object /replace mod$system:modula  'obj'
$      else
$        say " --> error compiling ", name, ".for"
$      endif
$      if f$search( obj ) .nes. "" then delete /nolog 'obj';*
$!
$      goto loop
$ end_loop:
$!
$ say " "
$ say "done"
$!
$ exit
