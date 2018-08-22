MODULE PrinterTest;

	(********************************************************)
	(*							*)
	(*		Test of printer driver.			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	26 February 1993		*)
	(*  Status:						*)
	(*	Basic tests complete.				*)
	(*							*)
	(********************************************************)

FROM GlassTTY IMPORT
    (* proc *)	WriteString, WriteLn;

FROM KTrace IMPORT
    (* proc *)	InTrace, OutTrace, TraceOn, NYI;

FROM Printer IMPORT
    (* proc *)	PrintChar;

(************************************************************************)

PROCEDURE PrintString (text: ARRAY OF CHAR);

    (* Prints from array text until NUL or end of array.	*)

    VAR j: CARDINAL;

    BEGIN
	InTrace ("PrintString");
	j := 0;
	LOOP
	    IF text[j] = CHR(0) THEN EXIT(*LOOP*) END (*IF*);
	    PrintChar (text[j]);
	    IF j = HIGH(text) THEN EXIT(*LOOP*) END (*IF*);
	    INC (j);
	END (*LOOP*);
	OutTrace ("PrintString");
    END PrintString;

(************************************************************************)

PROCEDURE PrintEOL;

    (* Sends a carriage return and line feed to the printer.  Note that	*)
    (* the printer might not print unterminated lines, because of its	*)
    (* internal buffering.						*)

    BEGIN
	PrintChar (CHR(13));  PrintChar(CHR(10));
    END PrintEOL;

(************************************************************************)

PROCEDURE RunTheTest;

    (* Prints a short test message, and then runs through every		*)
    (* printable character code.					*)
    (* Codes CHR(0) to CHR(31) are not tested, since it is hard in such	*)
    (* a test, because of possible sequence dependence, to interpret	*)
    (* the results.							*)

    VAR ch: CHAR;

    BEGIN
	InTrace ("RunTheTest");
	WriteString ("Test of printer driver.");
	WriteLn;

	PrintString ("Test message.");
	PrintEOL;
	PrintString ("A somewhat longer line, to check the buffering.");
	PrintEOL;
	PrintString ("Third line of test message.");
	PrintEOL;
	PrintString ("ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 .,;");
	PrintEOL;

	FOR ch := CHR(32) TO CHR(63) DO
	    PrintChar (ch);
	END (*FOR*);
	PrintEOL;

	FOR ch := CHR(64) TO CHR(127) DO
	    PrintChar (ch);
	END (*FOR*);
	PrintEOL;

	FOR ch := CHR(128) TO CHR(191) DO
	    PrintChar (ch);
	END (*FOR*);
	PrintEOL;

	FOR ch := CHR(192) TO CHR(255) DO
	    PrintChar (ch);
	END (*FOR*);
	PrintEOL;

	WriteString ("End of test.");
	WriteLn;
	OutTrace ("RunTheTest");
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

    BEGIN
(*	TraceOn;*)
	RunTheTest;
    END PrinterTest.
