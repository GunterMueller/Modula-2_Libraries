$! MODULA_CALL_SEQ.COM
$! generate a calling sequence of a Modula-2 module
$! the list of called modules will go to standard output
$!
$! p1 = file name
$! p2 = system scan, default = N, if set to Y then system modules will
$!      be listed.
$!
$! How: this procedure works by recompiling, then looking into the listing
$!      file to get the listed module names, then compiling them too, etc.
$! Bug: if a definition module does an import, then that doesn't get listed here
$!
$! V1.0, J. Andrea, Aug.19/91
$!
$ max_level = 0
$!
$ say = "write sys$output"
$!
$ on control_y then goto trap
$ on error     then goto trap
$!
$ proc = f$environment( "procedure" )
$ proc = f$parse( proc,,,"NAME" )
$!
$ p1 = f$edit( p1, "trim" )
$ if p1 .nes. "" then goto got_p1
$   say "no parameter given !"
$   exit %X10000000 ! bad completion status
$ got_p1:
$!
$ p2 = f$edit( p2, "trim" )
$ if p2 .eqs. ""
$ then
$   system_scan = "N"
$ else
$   system_scan = p2
$ endif
$ if system_scan
$ then
$   system_dir = ""
$ else
$   system_dir = f$trnlnm( "mod$system" )
$ endif
$!
$!-- if the file type is missing, then assume its .mod
$!
$ ftype = f$parse( p1,,,"TYPE", "syntax_only" )
$ if ftype .eqs. "." then ftype = ".MOD"
$!
$ in_file = f$parse( p1,,,"DEVICE", "syntax_only" )
$ in_file = in_file + f$parse( p1,,,"DIRECTORY", "syntax_only" )
$ in_file = in_file + f$parse( p1,,,"NAME", "syntax_only" ) + ftype
$!
$ if f$search( in_file ) .nes. "" then goto got_file
$   say "no such file: ", in_file
$   exit %X10000000 ! bad completion status
$ got_file:
$!
$ unique = f$time() - " " - " " - ":" - ":" - "." - "-" - "-"
$ unique = "sys$login:" + proc + "-" + unique
$!
$ n_modules = 0
$!
$ tmp      = ""
$ tmp_file = ""
$ level    = 0
$ say in_file
$ do_file = in_file
$ gosub list_file
$!
$ exit
$!
$!-----------------------------------------------------------
$ trap:
$   on control_y then continue
$   on error     then continue
$   i = 1
$   trap_loop:
$     if i .gt. max_level then goto end_trap_loop
$       file = "tmp_file_" + f$string(i)
$       close 'file'
$       i = i + 1
$       goto trap_loop
$   end_trap_loop:
$   file = unique + "*.tmp"
$   if f$search( file ) .nes. "" then delete /nolog 'file';*
$ exit %X10000000 ! bad completion status
$!
$!-------------------------------------------------------subroutine
$ list_file:
$   level = level + 1
$   if level .gt. max_level then max_level = level
$!
$   save_tmp_'level'      = tmp
$   save_tmp_file_'level' = tmp_file
$!
$   tmp = unique + "-" + f$string( level ) + ".tmp" 
$   if f$search( tmp ) .nes. "" then delete /nolog 'tmp';*
$!
$   set noon
$   modula /list='tmp' /cross /noobj 'do_file'
$   define /user_mode sys$output nla0:
$   define /user_mode sys$error  nla0:
$   search 'tmp';1 "module, imported" /out='tmp';2
$   status = $status
$   set on
$   if status .eq. 1
$   then
$     sort /nodup 'tmp';2  'tmp';3
$!
$     purge/nolog 'tmp'
$!
$     tmp_file = "tmp_file_" + f$string(level)
$     open /read 'tmp_file' 'tmp'
$!
$     sym_loop:
$        read /end=end_sym_loop 'tmp_file' line
$        line = f$edit( line, "COLLAPSE" )
$        p = f$locate( ":", line )
$        module  = f$extract( 0, p, line )
$        symfile = line - module - ":module,imported;symbolfile:"
$        !
$        out_line = ""
$        i = 0
$        space_loop:
$          if i .eq. level then goto end_space_loop
$            out_line = out_line + "...."
$            i = i + 1
$            goto space_loop
$        end_space_loop:
$        !
$        gosub check_module_list
$        if status
$        then
$          say out_line, module
$          modfile = symfile - ".SYM" - f$parse( symfile,,,"VERSION" ) + ".MOD"
$          modfile = f$search( modfile )
$          if modfile .nes. ""
$          then
$             do_file = modfile
$             moddir = f$parse( modfile,,,"DEVICE" ) + f$parse( modfile,,,"DIRECTORY" )
$             if moddir .nes. system_dir
$             then
$                gosub list_file
$             endif
$          endif
$        else
$          say out_line, module, "  (previously scanned)"
$        endif
$        goto sym_loop
$     end_sym_loop:
$!
$     close 'tmp_file'
$   endif
$   delete /nolog 'tmp';*
$!
$   tmp      = save_tmp_'level'
$   tmp_file = save_tmp_file_'level'
$   level    = level - 1
$   return
$!
$!----------------------------------------------------------subroutine
$ check_module_list:
$   i = 1
$   found = "N"
$   check_module_loop:
$   if found .or. i .gt. n_modules then goto end_check_module_loop
$     found = module_'i' .eqs. module 
$     i = i + 1
$     goto check_module_loop
$   end_check_module_loop:
$   if found
$   then
$     status = "N"
$   else
$     status = "Y"
$     n_modules = n_modules + 1
$     module_'n_modules' = module
$   endif
$   return
