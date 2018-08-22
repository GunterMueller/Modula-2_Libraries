MODULE KBtest;

	(****************************************************************)
	(*								*)
	(*			Test of keyboard driver.		*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	24 July 1994				*)
	(*  Status:		Working					*)
	(*								*)
	(****************************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE;

FROM GlassTTY IMPORT
    (* proc *)	WriteString, WriteLn, WriteHexByte, SetCursor;

FROM KTrace IMPORT
    (* proc *)	InTrace, OutTrace, TraceOn;

FROM KBdriver IMPORT
    (* proc *)	GetScanCode;
(*
IMPORT SpuriousInterrupts;
*)

IMPORT Timer;


(************************************************************************)

PROCEDURE RunTheTest;

    (* Keeps reading scan codes, and writing them on the screen in	*)
    (* hexadecimal, until an Esc character is detected.			*)

    VAR code: BYTE;

    BEGIN
	InTrace ("RunTheTest");
	SetCursor (0,0);
	WriteString ("Test of keyboard driver.");
	WriteString ("  To exit, use the Esc key.");
	WriteLn;
	REPEAT
	    code := GetScanCode ();
	    WriteString ("Scan code = ");
	    WriteHexByte (code);
	    WriteLn;
	UNTIL code = BYTE(081H);

	(* Remark: 081H is the code for releasing the Esc key.	*)

	WriteString ("End of test.");
	WriteLn;
	OutTrace ("RunTheTest");
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

    BEGIN
(*	TraceOn; *)
	RunTheTest;
    END KBtest.
