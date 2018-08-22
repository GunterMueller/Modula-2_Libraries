MODULE InitTest;

	(********************************************************)
	(*							*)
	(*		Test of module initialisation		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	23 March 1995			*)
	(*  Status:		Problems, see below		*)
	(*							*)
	(*	This module doesn't actually do anything.	*)
	(*	All the action is in the initialisation		*)
	(*	code of the imported modules.			*)
	(*							*)
	(*	Bug: although this program works normally	*)
	(*	when we press a key to terminate (as intended),	*)
	(*	it can crash if Ctrl/Alt/Del is used to		*)
	(*	terminate it.  As far as I can work out, the	*)
	(*	problem occurs only if the INT33 mouse module	*)
	(*	is used, and the installed mouse driver is a	*)
	(*	Microsoft mouse driver.				*)
	(*							*)
	(********************************************************)

FROM Keyboard IMPORT KeyPressed;
FROM Trace IMPORT Pause;
IMPORT TimeOfDay;
IMPORT Floppy;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

VAR j: INTEGER;

BEGIN
    Pause;
(*
    REPEAT
	j := -j;
    UNTIL KeyPressed();
*)
END InitTest.
