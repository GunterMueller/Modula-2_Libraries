$! update_library.com
$! use this routine to put object files into the modula object library
$!
$! NOT for public use, for system manager only
$!
$! V1.0, J. Andrea. Aug.8/91
$!
$! Most routines in this directory (MOD$LIBRARY) should be compiled
$! with the options /NOCHECK /NODEBUG then the objects placed in the
$! library with this procedure.
$! Be sure to check for the proper module dependancies when rebuilding
$! modules from this directory.
$! 
$ library/replace/log mod$system:modula.olb *.obj
$ exit
