$! M2.COM - history at end
$! compile, link, and run a MODULA_2 program
$! no link list, this is only for self-contained programs + standard-library
$!
$! p1 = program module file name
$! p2 = listing option, default = "T"
$!      N= none, T=terminal, F=save list file, S=print short list,
$!      P=print list, C=syntax check only with no list,
$!      Q=print list and program output,
$!      R=print list and program output on narrow printer
$!
$ say = "write sys$output"
$!
$ p1 = f$edit( p1, "TRIM" )
$ if p1 .nes. "" then goto got_p1
$    say " "
$    say "no file name given !"
$    exit %X10000000  ! bad completion status
$ got_p1:
$ p2 = f$edit( p2, "TRIM" )
$ if p2 .eqs. "" then p2 = "T"
$!
$ device    = f$parse( p1,,, "DEVICE" )
$ directory = f$parse( p1,,, "DIRECTORY" )
$!
$ ftype = f$parse( p1,,, "TYPE", "SYNTAX_ONLY" )
$ if ftype .eqs. "." then ftype = ".MOD"
$ if ftype .eqs. ".MOD" then goto mod_ok
$    if ftype .eqs. ".DEF" then goto mod_ok
$       say " "
$       say p1, " is not a Modula source file, the type must be .MOD or .DEF !"
$       exit %X10000000  ! bad completion status
$ mod_ok:
$!
$ fname = f$parse( p1,,, "NAME", "SYNTAX_ONLY" )
$ if fname .nes. "" then goto got_name
$    say " "
$    say "no name for file !"
$    exit %X10000000  ! bad completion status
$ got_name:
$!
$ wanted_file = device + directory + fname + ftype
$ file = f$search( wanted_file )
$ if file .nes. "" then goto got_the_file
$    say " "
$    say wanted_file, " doesnt exist !"
$    exit %X10000000  ! bad completion status
$ got_the_file:
$ fname = f$parse( file,,, "NAME" )
$!
$ obj = fname + ".OBJ;"
$ lis = fname + ".LIS;"
$!
$ dolist := "/nolist"  !--no list for an invalid option
$ if p2 .eqs. "C" then dolist = "/nolist/noobject"
$ if p2 .eqs. "T" then dolist = "/list=sys$output"
$ if p2 .eqs. "N" then dolist = "/nolist"
$ if p2 .eqs. "F" then dolist = "/list=" + lis
$ if p2 .eqs. "S" then dolist = "/nocross/list=" + lis
$ if p2 .eqs. "P" then dolist = "/list=" + lis
$ if p2 .eqs. "Q" then dolist = "/nocross/list=" + lis
$ if p2 .eqs. "R" then dolist = "/nocross/list=" + lis
$ quick = p2 .eqs. "Q" .or. p2 .eqs. "R"
$!
$ set noon
$!
$ if f$search( obj ) .nes. "" then delete 'obj'*
$ if f$search( lis ) .nes. "" then delete 'lis'*
$ if quick
$ then 
$   out = fname + "$OUTPUT.TMP;"
$   if f$search( out ) .nes. "" then delete 'out'*
$ endif
$!
$ modula  'dolist'  'file'
$ status = $status
$!
$!--- check the printer option
$ if p2 .eqs. "P" .or. p2 .eqs. "S"
$ then
$   if f$search( lis ) .nes. "" then print 'lis' /delete/nofeed
$ endif
$!
$ if status .and. ( f$search( obj ) .nes. "" )
$ then
$    say " "
$    say "compile complete"
$    !
$    if ftype .nes. ".DEF"
$    then
$      link /nomap 'fname' + mod$system:modula/library
$      status = $status
$      if status
$      then
$         say " "
$         say "link complete"
$         delete 'obj'*
$         purge /nolog 'fname'.exe
$         !
$         say " "
$         say "program now executing"
$         say " "
$         define/user_mode sys$input sys$command
$         if quick then define /user_mode sys$output 'out'
$         run 'fname'
$         !
$         if quick
$         then
$            if p2 .eqs. "Q" then print /delete/nofeed/flag=one  'lis','out'
$            if p2 .eqs. "R" then print /delete/nofeed/flag=one  'lis','out' /queue=narrow$print
$         endif
$     endif
$   endif
$ endif
$!
$ exit
$----------------------------------------------------------------history
$! compile , link , and run a MODULA_2 program
$! .. intended for a student environment      ... see compile options
$!
$! no link list, this is only for self-contained programs + standard-library
$!
$! V2.4, Jaa, Feb.14/92 VMS 5.4-2
$!     - add options C and R
$! V2.3, Jaa, Feb.7/92 VMS 5.4-2
$!     - correct deletion of temp output file
$! V2.2, Jaa, Sep.5/91, VMS 5.4-1
$!     - make S option shorter output than P option
$!     - remove verify on/off
$!     - remove control_y trap
$!     - have Q output use /nocross
$!     - general cleanup
$! v2.1, Jaa, Apr.87, VMS4.4
$!    - exit with bad status if compile/link error
$! v2.0, Sept 1986  - add wildcard filename input, Jaa
$! Mar 10/86  - add the new object library, Jaa
$! Jan 20/86  - add the Q print option, Jaa
$! Apr 2 / 85 - fix to print list files properly , VMS 3.7
$! J. Andrea, Nov. 1984 , VMS 3.7
$! V1.0, J. Andrea, VAX/VMS 3.4
$! This code may be freely used and distributed, it may not be sold.
