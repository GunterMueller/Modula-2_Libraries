MODULE KeyTest;

	(********************************************************)
	(*							*)
	(*		Test of keyboard driver.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	24 July 1994			*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

FROM GlassTTY IMPORT
    (* proc *)	WriteString, WriteLn, WriteChar, SetCursor;

FROM KTrace IMPORT
    (* proc *)	InTrace, OutTrace, TraceOn;

FROM Keyboard IMPORT
    (* proc *)	InKey;
(*
IMPORT SpuriousInterrupts;
*)
IMPORT Timer;

(************************************************************************)

PROCEDURE RunTheTest;

    (* Keeps reading characters, and writing them on the screen, until	*)
    (* the Esc key has been typed twice in succession.			*)

    CONST Esc = CHR(27);

    VAR code: CHAR;  count: CARDINAL;  EscDetected: BOOLEAN;

    BEGIN
	InTrace ("RunTheTest");
	SetCursor (0,0);  WriteString ("Test of keyboard driver.");
	WriteString ("  To exit, hit the Esc key TWICE.");
	WriteLn;
	count := 0;
	LOOP
	    code := InKey();
	    IF code = Esc THEN
		IF EscDetected THEN EXIT(*LOOP*)
		ELSE EscDetected := TRUE
		END (*IF*);
	    ELSE EscDetected := FALSE;
	    END (*IF*);
	    IF code = CHR(13) THEN
		WriteLn;  count := 0;
	    ELSE
		WriteChar (code);
		INC (count);
		IF count = 80 THEN
		    WriteLn; count := 0;
		END (*IF*);
	    END(*IF*);
	END (*LOOP*);

	WriteLn;
	WriteString ("End of test.");
	WriteLn;
	OutTrace ("RunTheTest");
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

    BEGIN
(*	TraceOn (15, 24, 0, 79);*)
	RunTheTest;
    END KeyTest.
