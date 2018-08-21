$! MOD2.COM - histroy at end
$! compile, link, and run a MODULA_2 program --- advanced users
$!
$! p1 = name of file to compile
$! p2 = compile/print option, default = "N"
$!      N= none, T=terminal, F=save list file, S=print short list,
$!      P=print list, Q=print list and program output
$!      X=no list and turn off run time checks
$!
$ run_ok    = "N"
$ file_open = "N"
$ say       = "write sys$output"
$ on control_y then goto all_done            ! someone could stop a compile
$!
$ p1 = f$edit( p1, "TRIM" )
$ if p1 .nes. "" then goto got_p1
$    say " "
$    say "no file name given"
$    exit %X10000000  ! bad completion status
$ got_p1:
$ p2 = f$edit( p2, "TRIM" )
$ if p2 .eqs. "" then p2 = "N"
$!
$ device    = f$parse( p1,,, "DEVICE", "SYNTAX_ONLY" )
$ directory = f$parse( p1,,, "DIRECTORY", "SYNTAX_ONLY" )
$!
$ ftype = f$parse( p1,,, "TYPE", "SYNTAX_ONLY" )
$ if ftype .eqs. "." then ftype = ".MOD"
$ if ftype .eqs. ".MOD" then goto mod_ok
$    if ftype .eqs. ".DEF" then goto mod_ok
$       say " "
$       say p1, " is not a Modula source file, the type must be .MOD or .DEF"
$       exit %X10000000  ! bad completion status
$ mod_ok:
$!
$ fname = f$parse( p1,,, "NAME", "SYNTAX_ONLY" )
$ if fname .nes. "" then goto got_name
$    say " "
$    say "no name for file"
$    exit %X10000000  ! bad completion status
$ got_name:
$!
$ wanted_file = device + directory + fname + ftype
$ file = f$search( wanted_file )
$ if file .nes. "" then goto got_the_file
$    say " "
$    say wanted_file, " doesnt exist"
$    exit %X10000000  ! bad completion status
$ got_the_file:
$ fname = f$parse( file,,, "NAME" )
$!
$ obj = fname + ".OBJ;"
$ sym = fname + ".SYM;"
$ lis = fname + ".LIS;"
$!
$!-- now check the listing options
$!
$ dolist = "/nolist"   !--an invalid option produces no listing
$ if p2 .eqs. "T" then dolist = "/list=sys$output/nocross"
$ if p2 .eqs. "F" .or. p2 .eqs. "P" then dolist = "/list=" + lis
$ if p2 .eqs. "S" .or. p2 .eqs. "Q" then dolist = "/list=" + lis + "/nocross"
$ if p2 .eqs. "X" then dolist = "/nocheck/nolist"
$!
$ set noon
$!
$ if f$search( lis ) .nes. "" then delete 'lis'*
$!--- always delete the last object
$!    even if this current compile is a definition the previous object
$!    must be recreated anyway
$ if f$search( obj ) .nes. "" then delete 'obj'*
$!-- now take care of the definition module case
$ if ftype .eqs. ".DEF" then if f$search( sym ) .nes. "" then delete 'sym'*
$!
$ modula /log  'dolist'  'file'
$ status = $status
$!
$ if p2 .eqs. "P" .or. p2 .eqs. "S" then print /delete/nofeed 'lis'
$!
$!--- was a new object created ?
$ if status .and. ( f$search( obj ) .nes. "" )
$ then
$    say " "
$    say "compile complete"
$    !
$    if ftype .eqs. ".DEF"
$    then
$      say "definition module - no link required"
$    else
$      !--- is it an implementation module
$      open/read compile_file 'file'
$      file_open = "Y"
$      read_first_line_loop:
$          read  compile_file  first_line
$          first_line = f$extract(0,14,f$edit(first_line,"COLLAPSE"))
$          !--- try again on blank lines
$          if first_line .eqs. "" then goto read_first_line_loop
$      end_read_first_line_loop:
$      close compile_file
$      file_open = "N"
$      if first_line .eqs. "IMPLEMENTATION"
$      then
$        say "implementation module - no link required"
$      else
$        @pub$coms:mod_link  'fname'   'p2'
$        if $status then run_ok = "Y"
$      endif
$   endif
$ endif
$ all_done:
$!--- did someone hit control-Y and leave the file open
$ if file_open .eqs. "Y" then close compile_file
$ if run_ok .eqs. "N" then exit %X10000000 ! bad completion status
$ exit
$-------------------------------------------------------------history
$!
$! v2.2, Jaa, Sep.5/91, VMS 5.4-2
$!     - add X compile option
$!     - remove verify on/off
$!     - general cleanup
$! v2.1, Jaa, Apr.87, VMS4.4 - exit with bad status if compile/link error
$! Sept 5 1986 - VMS 4.4
$!             - allow wildcard name input
$! May 17 1985 - delete previous symbol file
$!             - open file to see its an implementation
$!             - separate linking in another .com
$! May. 1985 - delete the object before a run
$! Apr. 1985 - print lists properly
$! Feb. 1985 - look for object libraries
$! J. Andrea, Nov. 1984, VMS 3.7
$! This code may be freely used and distributed, it may not be sold.
