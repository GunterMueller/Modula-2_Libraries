$ Conformant cases
$
$ no place for pattern, right justification, static length = 41:
$
justify
normal
/////////////////////////////////////////
short
\\\
right
41
normal
/////////////////////////////////////////
$ ------------------------------------------------------ $
$ 
$ no place for pattern, center justification, static length = 41:
$
justify
normal
/////////////////////////////////////////
short
\\\
center
41
normal
/////////////////////////////////////////
$ ------------------------------------------------------ $
$ 
$ no place for pattern, left justification, static length = 41:
$
justify
normal
/////////////////////////////////////////
short
\\\
left
41
normal
/////////////////////////////////////////
$ ------------------------------------------------------ $
$ 
$ no place for pattern, rigth justification, static length = 81:
$
justify
long
/////////////////////////////////////////////////////////////////////////////////
normal
/////////////////////////////////////////
right
81
long
/////////////////////////////////////////////////////////////////////////////////
$ ------------------------------------------------------ $
$ 
$ no place for pattern, center justification, static length = 81:
$
justify
long
/////////////////////////////////////////////////////////////////////////////////
normal
/////////////////////////////////////////
center
81
long
/////////////////////////////////////////////////////////////////////////////////
$ ------------------------------------------------------ $
$ 
$ no place for pattern, left justification, static length = 81:
$
justify
long
/////////////////////////////////////////////////////////////////////////////////
normal
/////////////////////////////////////////
left
81
long
/////////////////////////////////////////////////////////////////////////////////
$ ------------------------------------------------------ $
$
$ null string all filled by pattern, right justification, 
$ static length = 41:
$
justify
normal

normal
\\\
right
41
normal
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
$ ------------------------------------------------------ $
$
$ null string all filled by pattern, center justification, 
$ static length = 41:
$
justify
normal

normal
\\\
center
41
normal
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
$ ------------------------------------------------------ $
$
$ null string all filled by pattern, left justification, 
$ static length = 41:
$
justify
normal

normal
\\\
left
41
normal
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
$ ------------------------------------------------------ $
$
$ "common" cases:
$
justify
normal
//////////
normal
\\\\\\\\\\
right
41
normal
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//////////
$ ------------------------------------------------------ $
justify
normal
//////////
normal
\\\\\\\\\\
center
41
normal
\\\\\\\\\\\\\\\//////////\\\\\\\\\\\\\\\\
$ ------------------------------------------------------ $
justify
normal
//////////
normal
\\\\\\\\\\
left
41
normal
//////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
$ ------------------------------------------------------ $
$
$ Dynamic length of pattern greater then final length
$
justify
normal
\\\\\\\\\\
normal
//////////
right
30
normal
////////////////////\\\\\\\\\\
$ ------------------------------------------------------ $
