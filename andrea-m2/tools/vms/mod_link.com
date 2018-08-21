$! MOD_LINK.COM - history and description at end
$! finish linking and running a modula object file
$!
$! p1 = file name of object file
$! p2 = print options
$!
$ run_ok = "N"
$ say    = "write sys$output"
$!
$ on control_y then goto all_done            ! someone could stop a link
$ on severe_error then goto all_done
$ on error then continue
$!
$ p1 = f$edit( p1, "TRIM" )
$ if p1 .nes. "" then goto got_p1
$    say " "
$    say "no file name given"
$    exit %X10000000  ! bad completion status
$ got_p1:
$ p2 = f$edit( p2, "TRIM" )
$!
$ ftype = f$parse( p1,,, "TYPE", "SYNTAX_ONLY" )
$ if ftype .eqs. "." then ftype = ".OBJ"
$    if ftype .eqs. ".OBJ" then goto obj_ok
$       say " "
$       say "non an object file passed to MOD_LINK: ", p1
$       exit %X10000000  ! bad completion status
$ obj_ok:
$!
$ fname = f$parse( p1,,, "NAME", "SYNTAX_ONLY" )
$ if fname .nes. "" then goto got_name
$    say " "
$    say "no name given to MOD_LINK"
$    exit %X10000000  ! bad completion status
$ got_name:
$!
$ file = fname + ftype 
$ if f$search( file ) .nes. "" then goto got_file
$    say " "
$    say file, " file doesnt exist !"
$    exit %X10000000  ! bad completion status
$ got_file:
$!
$ obj = file + ";"
$ if p2 .eqs. "Q"
$ then
$   quick = "Y"
$   lis = fname + ".LIS;"
$   out = fname + "$OUTPUT.TMP;"
$   if f$search( out ) then delete /nolog 'out';*
$ else
$   quick = "N"
$ endif
$!
$ link_list = ""
$ say " "
$! 
$!-- now we're about to do the link, lets build a search list
$!
$ n_search_dirs = 12
$!
$ logical_dir_1 := []                       ! the default directory
$! use colons up here, in assignments and logical names
$ logical_dir_2  := MOD$LIBRARY:            ! system wide
$ logical_dir_3  := MOD$LIBRARY_1:          ! user defined
$ logical_dir_4  := MOD$LIBRARY_2:          ! user defined
$ logical_dir_5  := MOD$LIBRARY_3:          ! user defined
$ logical_dir_6  := MOD$LIBRARY_4:          ! user defined
$ logical_dir_7  := MOD$LIBRARY_5:          ! user defined
$ logical_dir_8  := MOD$LIBRARY_6:          ! user defined
$ logical_dir_9  := MOD$LIBRARY_7:          ! user defined
$ logical_dir_10 := MOD$LIBRARY_8:          ! user defined
$ logical_dir_11 := MOD$LIBRARY_9:          ! user defined
$ logical_dir_12 := MOD$SYSTEM:             ! system wide
$!
$ search_dir_1 := []
$! don't use colons down here, in assignments nor logical names
$ search_dir_2  = f$logical("MOD$LIBRARY")
$ search_dir_3  = f$logical("MOD$LIBRARY_1")
$ search_dir_4  = f$logical("MOD$LIBRARY_2")
$ search_dir_5  = f$logical("MOD$LIBRARY_3")
$ search_dir_6  = f$logical("MOD$LIBRARY_4")
$ search_dir_7  = f$logical("MOD$LIBRARY_5")
$ search_dir_8  = f$logical("MOD$LIBRARY_6")
$ search_dir_9  = f$logical("MOD$LIBRARY_7")
$ search_dir_10 = f$logical("MOD$LIBRARY_8")
$ search_dir_11 = f$logical("MOD$LIBRARY_9")
$ search_dir_12 = f$logical("MOD$SYSTEM")
$!
$!--this loop is tricky stuff, be careful if you touch it
$!
$ link_loop:
$          inquire module_name "link with ? (CR to end, \ for no link)"
$          !--if name is a "\" then don't link or compile, its an implementation
$          if module_name .eqs. ""  then goto end_link_loop
$          if module_name .eqs. "\" then goto end_link_loop
$             !
$             !-- get file name from module name
$             if f$length(module_name) .le. 9 then goto le9
$                module_name = f$extract(0,9,module_name)
$             le9:
$             !
$             !--- make sure the object is here
$             !--- by looking in each of the search directories, if they exist
$             search_count = 0
$ search_loop:
$             search_count = search_count + 1
$             if search_count .gt. n_search_dirs then goto end_search_loop
$                !--- fake the logical name
$                !
$                !--- see if the logical name has been defined by the user
$                if search_dir_'search_count' .eqs. "" then goto no_log_name
$                   link_name = logical_dir_'search_count' + module_name
$                   !
$                   !--- look for the object in this directory
$                   link_search = link_name + ".OBJ"
$                   link_search = f$search(link_search)
$                   !--- does the object exist here
$                   if link_search .nes. "" then -
                              search_count = n_search_dirs + 1 ! end the loop
$                   if link_search .nes. "" then goto found
$                      !--- no ,the object file wasn't found there
$                      !
$                      !--- try a library
$                      link_search = link_name + ".OLB"
$                      link_search = f$search(link_search)
$                      if link_search .eqs. "" then goto found
$                         !--- found a library match
$                         search_count = n_search_dirs + 1 ! to end the loop
$                         link_name = link_name + "/LIB"
$                   found: !--- maybe, maybe not
$                no_log_name:
$                !
$                goto search_loop
$ end_search_loop:
$            !
$            if link_search .eqs. "" then -
                     say module_name , " not found in search "
$            if link_search .nes. "" then -
                     link_list = link_list + " +" + link_name
$            goto link_loop
$ end_link_loop:
$!
$ if module_name .nes. "\"
$ then
$   if link_list .nes. "" then say "link_list=", link_list
$   set noon
$   link /nomap 'fname' 'link_list'  + mod$system:modula/lib 
$   status = $status
$   if status
$   then
$     say " "
$     say "link complete"
$     delete 'obj'*
$!
$     say " "
$     say "program now executing"
$     run_ok = "Y"
$     say " "
$     define/user_mode sys$input sys$command !-this command fails for batch jobs
$     if quick then define/user_mode sys$output 'out'
$     run 'fname'
$     if quick then print/delete/nofeed/flag=one 'lis','out'
$     purge /nolog 'fname'.exe
$   endif
$ endif
$!
$ all_done:
$ if run_ok then exit
$ exit %X10000000 ! bad completion status
$!-------------------------------------------------------------history
$! this version has a directory-search-list the same as the compiler
$! THE-CURRENT-DIRECTORY , MOD$LIBRARY , then
$! MOD$LIBRARY_1 .. MOD$LIBRARY_9 , and last MOD$SYSTEM
$!
$! if a module name is given as a "\" then it assumes that an
$!  implementation module is being compiled, and no link or run is done
$!
$! this search list is used to look for the extra modules during the link
$!
$! the logical names MOD$LIBRARY_1 to MOD$LIBRARY_9 must be
$!  assigned by the user
$!
$! the search process looks for a .OBJ or a .OLB
$!    if the library is found then a /LIB is appended to the name
$!
$! v2.0, Jaa, Sep.5/91 
$!      - remove verify on/off
$!      - name change from modlink to MOD_LINK
$!      - general cleanup
$! v1.3, Jaa, July.87, VMS4.5 - make big library last in the link list
$! v1.2, Jaa, Apr.87, VMS4.4 - make sure proper status is passed back
$! v1.1, Jaa, Mar 10/86 - add new object library
$! v1.0, J. Andrea, May 1985, VMS 3.7
$! This code may be freely used and distributed, it may not be sold.
