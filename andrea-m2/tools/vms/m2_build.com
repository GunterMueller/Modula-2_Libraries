$! M2_BUILD.COM
$! build a modula2 module set
$!
$! p1 = main module name
$! p2 = list of submodules, in order of calling sequence, separated by commas
$!      order is like this: first is called by second, second called by third...
$! p3 = list of other modules to link with, optional parameter
$! p4 = compile options, optional parameter
$!
$! V1.2, J. Andrea, Jun.15/93
$!     - top level relink only if new objects, not compile too
$!     - and don't delete top level object
$! V1.1, J. Andrea, May.30/91
$!     - truncate module names to 9 characters
$!     - add compile options
$! V1.0, J. Andrea, May.13/91
$! This code may be freely used and distributed, it may not be sold.
$!
$ say := write sys$output
$!
$ p1 = f$edit( p1, "TRIM" )
$ if p1 .nes. "" then goto got_p1
$   say "Main module name missing !"
$   exit %X10000000 ! bad completion status
$ got_p1:
$ p2 = f$edit( p2, "TRIM" )
$ if p2 .nes. "" then goto got_p2
$   say "List of modules missing !"
$   exit %X10000000 ! bad completion status
$ got_p2:
$ p3 = f$edit( p3, "TRIM" )
$ p4 = f$edit( p4, "TRIM" )
$!
$!....first, see if any of the submodules need to have their
$!.....DEF files recompiled
$!
$ i = 0
$ def_loop:
$    name = f$element( i, ",", p2 )
$    if name .eqs. "," then goto end_def_loop
$      i = i + 1
$      gosub re_def
$      goto def_loop
$ end_def_loop:
$!
$!....second, see if any of the submodules need to have their
$!.....implementation files recompiled
$!
$ i = 0
$ imp_loop:
$    name = f$element( i, ",", p2 )
$    if name .eqs. "," then goto end_imp_loop
$      i = i + 1
$      gosub re_imp
$      goto imp_loop
$ end_imp_loop:
$!
$!...and lastly, decide is the main module must be recompiled
$!
$ gosub re_top
$!
$ exit
$!
$!--------------------------------------------------subroutine
$ re_def:
$   if f$length( name ) .gt. 9 then name = f$extract( 0, 9, name )
$   deffile = name + ".DEF"
$   if f$search( deffile ) .eqs. ""
$   then
$     say "Definition module ", deffile, " does not exist"
$   else
$     symfile = name + ".SYM"
$     if f$search( symfile ) .eqs. ""
$     then
$        say "No symbol file ", symfile, " so creating it"
$        !..ok, recompile the definition module
$        modula /log 'p4'  'deffile'
$     else
$        !..check dates of .DEF and .SYM
$        date1 = f$file_attr( deffile, "RDT" )
$        date2 = f$file_attr( symfile, "RDT" )
$        if f$cvtime( date1, "COMPARISON" ) .gts. -
            f$cvtime( date2, "COMPARISON" )
$        then
$          say deffile, " is newer than symbol file, recompiling definition"
$          !...definition is newer than symbol file, so recomile
$          modula /log 'p4' 'deffile'
$        endif
$     endif
$   endif
$ return
$!
$!--------------------------------------------------subroutine
$ re_imp:
$   if f$length( name ) .gt. 9 then name = f$extract( 0, 9, name )
$   impfile = name + ".MOD"
$   if f$search( impfile ) .eqs. ""
$   then
$     say "Implementation module ", impfile, " does not exist"
$   else
$     objfile = name + ".OBJ"
$     if f$search( objfile ) .eqs. ""
$     then
$        say "No object file ", objfile, " so create it"
$        !..ok, recompile the implementation module
$        modula /log 'p4' 'impfile'
$     else
$        !..check dates of .MOD and .OBJ
$        date1 = f$file_attr( impfile, "RDT" )
$        date2 = f$file_attr( objfile, "RDT" )
$        if f$cvtime( date1, "COMPARISON" ) .gts. -
            f$cvtime( date2, "COMPARISON" )
$        then
$          say impfile, " is newer than object file, recompile"
$          !...source is newer than object file, so recomile
$          modula /log 'p4' 'impfile'
$        else
$          symfile = name + ".SYM"
$          !..check dates of .SYM and .OBJ
$          date1 = f$file_attr( symfile, "RDT" )
$          date2 = f$file_attr( objfile, "RDT" )
$          if f$cvtime( date1, "COMPARISON" ) .gts. -
              f$cvtime( date2, "COMPARISON" )
$          then
$            say "symbol file ", symfile, " is newer than object file, recompile"
$            !...symbol file is newer than object file, so recomile
$            modula /log 'p4'  'impfile'
$          endif
$        endif
$     endif
$   endif
$ return
$!
$!--------------------------------------------------subroutine
$ re_top:
$     exefile = p1 + ".EXE"
$     if f$search( exefile ) .eqs. ""
$     then
$       say "No executable ", exefile, " recreate it"
$       modula /log 'p4'  'p1'
$       gosub do_link
$     else
$       new_obj = "N"
$       i := 0
$       top_obj_loop:
$       if new_obj then goto end_top_obj_loop
$         name = f$element( i, ",", p2 )
$         if name .eqs. "," then goto end_top_obj_loop
$           if f$length( name ) .gt. 9 then name = f$extract( 0, 9, name )
$           objfile = name + ".OBJ"
$           date1 = f$file_attr( objfile, "RDT" )
$           date2 = f$file_attr( exefile, "RDT" )
$           if f$cvtime( date1, "COMPARISON" ) .gts. -
              f$cvtime( date2, "COMPARISON" ) then new_obj = "Y"
$           i = i + 1
$           goto top_obj_loop
$       end_top_obj_loop:
$       if new_obj
$       then
$         topfile = p1 + ".MOD"
$         date1 = f$file_attr( topfile, "RDT" )
$         date2 = f$file_attr( exefile, "RDT" )
$         if f$cvtime( date1, "COMPARISON" ) .gts. -
             f$cvtime( date2, "COMPARISON" )
$         then
$           say "Source file ", topfile, " is newer than executable, recompile"
$           modula /log 'p4'  'p1'
$         else
$           say "New object files force a relink of the top level"
$         endif
$         gosub do_link
$      endif
$   endif
$ return
$!
$!--------------------------------------------------subroutine
$ do_link:
$   link_list = ""
$!
$   i = 0
$   link_loop:
$   name = f$element( i, ",", p2 )
$   if name .eqs. "," then goto end_link_loop
$     if f$length( name ) .gt. 9 then name = f$extract( 0, 9, name )
$     i = i + 1
$     link_list = link_list + "+" + name
$     goto link_loop
$   end_link_loop:
$   if p3 .nes. "" then link_list = link_list + "+" + p3
$   link_list = link_list + "+mod$system:modula/lib"
$!
$   say "relinking ", p1, " ", link_list
$   link 'p1' 'link_list'
$!
$ return
