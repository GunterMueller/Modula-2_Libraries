MODULE PlayMake;

	(********************************************************)
	(*							*)
	(*	Program to make some tables needed for		*)
	(*	playing music.  The data file produced		*)
	(*	by this program can be incorporated into	*)
	(*	an assembly language source file.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	15 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM FileSys IMPORT
    (* type *)	File,
    (* proc *)	OpenFile, CloseFile, WriteRecord;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode,
    (* proc *)	TranslateErrorCode;

FROM Conversions IMPORT
    (* proc *)	ShortCardToString, CardinalToString;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteLn,
		ReadChar, PressAnyKey;

FROM MATHLIB IMPORT
    (* proc *)	Exp, Log, Sin;

(************************************************************************)

TYPE Note = SHORTCARD [0..63];

VAR
    log: Window;
    linecount: CARDINAL;
    CRLF: ARRAY [0..1] OF CHAR;

(************************************************************************)

PROCEDURE SendByteToFile (f: File;  value: SHORTCARD);

    (* Writes one value to the file, with a new line every eight values.*)

    VAR buffer: ARRAY [0..31] OF CHAR;
	status: ErrorCode;

    BEGIN
	status := OK;
	IF linecount = 8 THEN
	    buffer := "   db ";
	    buffer[0] := CHR(13);  buffer[1] := CHR(10);
	    buffer[2] := CHR(9);  buffer[5] := CHR(9);
	    status := WriteRecord (f, ADR(buffer), 6);
	    linecount := 0;
	ELSE
	    buffer := ",";
	    status := WriteRecord (f, ADR(buffer), 1);
	END (*IF*);
	IF status = OK THEN
	    ShortCardToString (value, buffer, 4);
	    status := WriteRecord (f, ADR(buffer), 4);
	END (*IF*);
	INC (linecount);
	IF status <> OK THEN
	    TranslateErrorCode (status, buffer);
	    WriteString (log, buffer);
	END (*IF*);
    END SendByteToFile;

(************************************************************************)

PROCEDURE SendWordToFile (f: File;  value: CARDINAL);

    (* Writes one value to the file, with a new line every eight values.*)


    VAR buffer: ARRAY [0..31] OF CHAR;
	status: ErrorCode;

    BEGIN
	status := OK;
	IF linecount = 8 THEN
	    buffer := "   dw ";
	    buffer[0] := CHR(13);  buffer[1] := CHR(10);
	    buffer[2] := CHR(9);  buffer[5] := CHR(9);
	    status := WriteRecord (f, ADR(buffer), 6);
	    linecount := 0;
	ELSE
	    buffer := ",";
	    status := WriteRecord (f, ADR(buffer), 1);
	END (*IF*);
	IF status = OK THEN
	    CardinalToString (value, buffer, 6);
	    status := WriteRecord (f, ADR(buffer), 6);
	END (*IF*);
	INC (linecount);
	IF status <> OK THEN
	    TranslateErrorCode (status, buffer);
	    WriteString (log, buffer);
	END (*IF*);
    END SendWordToFile;

(************************************************************************)

PROCEDURE MakeDataFile;

    (* Initialises the global data. *)

    CONST PI =  3.141592653589793240;

    VAR step, scale: LONGREAL;
	j, value: SHORTCARD;  note: Note;
	ch: CHAR;
	f: File;  status: ErrorCode;
	buffer: ARRAY [0..31] OF CHAR;

    BEGIN
	(* Open the data file. *)

	status := OpenFile (f, "PLAYDATA.TXT", TRUE);
	IF status = DuplicateFileName THEN
	    WriteString (log, "PLAYDATA.TXT already exists.  Overwrite? ");
	    ReadChar (log, ch);
	    WriteLn (log);
	    IF CAP(ch) = 'Y' THEN
		status := OpenFile (f, "PLAYDATA.TXT", FALSE);
	    END (*IF*);
	END (*IF*);
	IF status <> OK THEN
	    TranslateErrorCode (status, buffer);
	    WriteString (log, buffer);
	    PressAnyKey (log);
	    RETURN;
	END (*IF*);

	(* Create the note-to-interval map. *)

	buffer := "    StepTable:";
	status := WriteRecord (f, ADR(buffer), 14);
	IF status <> OK THEN
	    TranslateErrorCode (status, buffer);
	    WriteString (log, buffer);
	    PressAnyKey (log);
	    RETURN;
	END (*IF*);

	linecount := 8;

	scale := Exp(Log(2.0)/12.0);
	step := 110.0;
	SendWordToFile (f, 0);
	FOR note := 1 TO MAX(Note) DO
	    SendWordToFile (f, VAL(CARDINAL, step));
	    step := step*scale;
	END (*FOR*);

	(* Create the waveform array. *)

	status := WriteRecord (f, ADR(CRLF), 2);
	IF status = OK THEN
	    status := WriteRecord (f, ADR(CRLF), 2);
	END (*IF*);
	IF status = OK THEN
	    buffer := "    Wave:";
	    status := WriteRecord (f, ADR(buffer), 9);
	END (*IF*);
	IF status <> OK THEN
	    TranslateErrorCode (status, buffer);
	    WriteString (log, buffer);
	    PressAnyKey (log);
	    RETURN;
	END (*IF*);

	linecount := 8;

	FOR j := 0 TO 255 DO
	    value := VAL(SHORTCARD,
			42.0*(1.0 + Sin(VAL(LONGREAL,j)*PI/128.0)));
	    SendByteToFile (f, value);
	END (*FOR*);
	status := WriteRecord (f, ADR(CRLF), 2);

	CloseFile (f);

    END MakeDataFile;

(************************************************************************)

BEGIN
    CRLF[0] := CHR(13);  CRLF[1] := CHR(10);
    OpenWindow (log, blue, cyan, 2,22, 2,77, simpleframe, nodivider);
    MakeDataFile;
    CloseWindow (log);
END PlayMake.
