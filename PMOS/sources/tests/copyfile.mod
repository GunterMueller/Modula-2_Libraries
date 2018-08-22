MODULE CopyFile;

	(****************************************************************)
	(*								*)
	(*		Program to copy a file and list it.		*)
	(*								*)
	(*	This is a test program, whose only function is to help	*)
	(*	in defining and testing the Files module.		*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	4 March 1995				*)
	(*  Status:		Working					*)
	(*								*)
	(*	There is however a persistent problem caused by the	*)
	(*	directory cacheing done by MS-DOS: after a file is	*)
	(*	created on the hard disk by the PMOS file system,	*)
	(*	MS-DOS will remain ignorant of its existence, and may	*)
	(*	even corrupt the disk structure by continuing to rely	*)
	(*	on its obsolete copies of the directory and FAT.	*)
	(*	For a floppy this problem is solved by typing ^C to	*)
	(*	the DOS command prompt, but for hard disk files the	*)
	(*	only solutions I've found are either to run Windows	*)
	(*	or to re-boot the machine immediately after running	*)
	(*	the PMOS program.					*)
	(*								*)
	(****************************************************************)

FROM Trace IMPORT
    (* proc *)	TraceOn, InTrace, OutTrace;

FROM Files IMPORT
    (* type *)	File,
    (* proc *)	OpenFile, CloseFile, EOF, ReadByte, WriteByte;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode,
    (* proc *)	TranslateErrorCode;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType, RowRange, ColumnRange,
    (* proc *)	OpenWindow, CloseWindow, ChangeScrollingRegion,
		WriteString, EditString, WriteLn, Write,
		SaveCursor, SetCursor, ReadCharWithoutEcho, PressAnyKey;

FROM NumericIO IMPORT
    (* proc *)	WriteCard;

FROM Keyboard IMPORT
    (* proc *)	InKey, PutBack;

IMPORT Floppy;

(*IMPORT HardDisk;*)

(************************************************************************)

PROCEDURE CopyAndList (w: Window;  source, destination: File);

    (* Copies file "source" to new file "destination", also writing the	*)
    (* contents to window w.						*)

    CONST CR = CHR(13);

    VAR ch: CHAR;  linecount: CARDINAL;  status: ErrorCode;
	errbuf: ARRAY [0..39] OF CHAR;

    BEGIN
	InTrace ("CopyAndList");
	linecount := 0;  status := OK;
	WHILE (status = OK) AND NOT EOF(source) DO
	    ch := ReadByte (source);
	    status := WriteByte (destination, ch);
	    IF status <> OK THEN
		WriteLn (w);
		WriteString (w, ">>>ERROR: Write failure.  Status = ");
		TranslateErrorCode (status, errbuf);
		WriteString (w, errbuf);
		WriteLn (w);
	    ELSIF ch = CR THEN
		WriteLn(w);
		INC (linecount);
		IF linecount = 22 THEN
		    WriteString (w, "                     -- More --");
		    ReadCharWithoutEcho (w, ch);
		    WriteLn (w);
		    linecount := 0;
		END (*IF*);
	    ELSE Write (w, ch)
	    END (*IF*);
	END (*WHILE*);
	WriteString (w, "----------------- End of File -----------------");
	ReadCharWithoutEcho (w, ch);
	OutTrace ("CopyAndList");
    END CopyAndList;

(************************************************************************)

PROCEDURE OpenAFile (w: Window;  VAR (*OUT*) f: File;  newfile: BOOLEAN)
								: BOOLEAN;

    (* Prompts for a file name from the keyboard, opens that file for	*)
    (* input.  Returns TRUE if successful.				*)

    CONST CR = CHR(13);  Esc = CHR(27);

    VAR FileName: ARRAY [0..79] OF CHAR;
	ch: CHAR;  status: ErrorCode;
	oldrow, oldcol: CARDINAL;
	errbuf: ARRAY [0..39] OF CHAR;

    BEGIN
	InTrace ("OpenAFile");
	SaveCursor (w, oldrow, oldcol);
	FileName := "";
	LOOP
	    SetCursor (w, oldrow, oldcol);
	    WriteString (w, "file: ");
	    EditString (w, FileName, 79);
	    ch := InKey();
	    IF ch = Esc THEN RETURN FALSE;
	    ELSIF ch <> CR THEN PutBack(ch);
	    END (*IF*);
	    status := OpenFile (f, FileName, newfile);
	    IF status = OK THEN
		RETURN TRUE;
	    END (*IF*);
	    SetCursor (w, oldrow+1, oldcol);
	    TranslateErrorCode (status, errbuf);
	    WriteString (w, errbuf);
	END (*LOOP*);
	OutTrace ("OpenAFile");
    END OpenAFile;

(************************************************************************)

PROCEDURE RunTheTest;

    VAR w: Window;  source, destination: File;

    BEGIN
	InTrace ("RunTheTest");
	OpenWindow (w, green, brown, 0, 24, 0, 79, noframe, doubledivider);
	WriteString (w, "Copy from ");
	IF OpenAFile (w, source, FALSE) THEN
	    WriteLn (w);
	    WriteString (w, " to ");
	    IF OpenAFile (w, destination, TRUE) THEN
		WriteLn (w);
		ChangeScrollingRegion (w, 2, 24);
		CopyAndList (w, source, destination);
		PressAnyKey (w);
		CloseFile (destination);
	    END (*IF*);
	    CloseFile (source);
	END (*IF*);
	PressAnyKey (w);
	CloseWindow (w);
	OutTrace ("RunTheTest");
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
(*    TraceOn (18,24,0,39); *)
    RunTheTest;
END CopyFile.
