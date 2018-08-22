MODULE AsciiTst;

	(********************************************************)
	(*							*)
	(*	For discovering the ASCII codes returned by	*)
	(*		various special keys.			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	10 October 1992			*)
	(*  Status:		Working.			*)
	(*							*)
	(********************************************************)

FROM Keyboard IMPORT
    (* proc *)	InKey;

FROM GlassTTY IMPORT
    (* proc *)	WriteChar, WriteString, WriteLn, WriteCard;

(************************************************************************)

PROCEDURE RunTheTest;

    CONST ControlZ = CHR(26);

    VAR char: CHAR;

    BEGIN
	WriteString ("Type ^Z to exit");  WriteLn;
	REPEAT
	    char := InKey ();
	    WriteString ("Character is ");  WriteChar (char);
	    WriteString ("  Code is ");  WriteCard (ORD(char));
	    WriteLn;
	UNTIL char = ControlZ;
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    RunTheTest;
END AsciiTst.
