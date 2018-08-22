MODULE Sort3;

	(********************************************************)
	(*							*)
	(*		    File sort program			*)
	(*							*)
	(*	This is a test of the FileSort module.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	11 May 1994			*)
	(*  Status:		Working				*)
	(*	FileSort still not as efficient as I want.	*)
	(*							*)
	(********************************************************)

FROM FileSort IMPORT
    (* proc *)	InplaceSort;

FROM FileSys IMPORT
    (* type *)	File,
    (* proc *)	OpenFile, CloseFile, FileSize;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, Write, WriteString, WriteLn,
		EditString, EditAborted, PressAnyKey;

FROM NumericIO IMPORT
    (* proc *)	EditCardinal;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode,
    (* proc *)	TranslateErrorCode;

(************************************************************************)

CONST testing = TRUE;

TYPE BufferPointer = POINTER TO ARRAY [0..0] OF CHAR;

VAR debug: Window;
    RecordSize: CARDINAL;

(************************************************************************)

PROCEDURE GEproc (first, second: BufferPointer): BOOLEAN;

    (* Tests for first^ >= second^.	*)

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	LOOP
	    (*# save, check(index=>off) *)
	    IF first^[j] > second^[j] THEN RETURN TRUE END(*IF*);
	    IF first^[j] < second^[j] THEN RETURN FALSE END(*IF*);
	    (*# restore *)
	    INC (j);
	    IF j >= RecordSize THEN RETURN TRUE END(*IF*);
	END (*LOOP*);
    END GEproc;

(************************************************************************)

PROCEDURE DoTheSort;

    (* Opens the data file, and uses the FileSort module to sort it.	*)

    VAR datafile: File;  log, w: Window;
	name: ARRAY [0..40] OF CHAR;
	status: ErrorCode;

    BEGIN
	RecordSize := 20;
	OpenWindow (w, white, black, 0, 16, 0, 79,
					noframe, nodivider);
	OpenWindow (log, white, black, 17, 24, 0, 79,
					simpleframe, nodivider);
	WriteString (log, "Test of in-place file sort algorithm");
	WriteLn (log);
	WriteString (log, "File name (full path name): ");
	name := "B:TEST.DAT";
	EditString (log, name, SIZE(name));
	IF EditAborted() THEN status := OperationAborted
	ELSE status := OpenFile (datafile, name, FALSE);
	END (*IF*);
	IF status = OK THEN
	    WriteLn (log);  WriteString (log, "Record size: ");
	    EditCardinal (log, RecordSize, 4);
	    InplaceSort (datafile, 0,
			(FileSize(datafile) DIV VAL(LONGCARD,RecordSize)) - 1,
			RecordSize, 0, GEproc);
	ELSE
	    WriteLn (log);  WriteString (log, "Could not open data file");
	    WriteLn (log);  WriteString (log, "Error code ");
	    TranslateErrorCode (status, name);
	    WriteString (log, name);
	END (*IF successfully opened file*);

	IF testing THEN
	    WriteLn (debug);  WriteString (debug, "Calling CloseFile");
	END (*IF*);
	CloseFile (datafile);
	WriteLn (log);  WriteString (log, "End of Sort");
	PressAnyKey (log);
	CloseWindow (w);  CloseWindow (log);

    END DoTheSort;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    IF testing THEN
	OpenWindow (debug, white, black, 12, 16, 0, 79,
					simpleframe, nodivider);
    END (*IF*);
    DoTheSort;
END Sort3.
