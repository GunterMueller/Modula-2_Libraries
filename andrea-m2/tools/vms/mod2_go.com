$! modula-2 the current file
$!
$! V1.1, J. Andrea, Mar. 1985 - provide an alternate output parameter
$! V1.0, J. Andrea, Nov. 1984, VMS 3.7
$! This code may be freely used and distributed, it may not be sold.
$!
$!
$ if p1 .eqs. "" then p1 = "N"   ! NO OUTPUT as the default
$!
$ @pub$coms:mod2 'ed_name'   'p1'
$!
$ exit
