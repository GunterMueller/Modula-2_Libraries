MODULE Sort2;

	(********************************************************)
	(*							*)
	(*		    File sort program			*)
	(*							*)
	(*	This is a test of the PMOS file system, so	*)
	(*	we're not particularly concerned about the	*)
	(*	efficiency of the sorting algorithm.		*)
	(*	However, it's better than Sort1.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	17 March 1995			*)
	(*  Status:		Working, but a bit slow		*)
	(*	Runs a lot faster when module Files replaced	*)
	(*	by FileSys.  This is certainly due to		*)
	(*	insufficient cacheing; most probably the main	*)
	(*	problem is the conservative FAT updating in	*)
	(*	module Directories.				*)
	(*							*)
	(********************************************************)

FROM Files IMPORT
    (* type *)	File,
    (* proc *)	OpenFile, CloseFile, ReadRecord, WriteRecord, EOF,
		FileSize, SetPosition;

IMPORT (*HardDisk,*) Floppy;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, Write, WriteString, WriteLn,
		EditString, EditAborted, PressAnyKey;

FROM NumericIO IMPORT
    (* proc *)	ReadBufferedCardinal;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode,
    (* proc *)	TranslateErrorCode;

(************************************************************************)

CONST testing = TRUE;

TYPE BufferPointer = POINTER TO ARRAY [0..0] OF CHAR;

VAR debug: Window;

(************************************************************************)

PROCEDURE less (first, second: BufferPointer;  recordsize: CARDINAL)
								: BOOLEAN;

    (* Tests for first^ < second^.	*)

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	LOOP
	    (*# save, check(index=>off) *)
	    IF first^[j] > second^[j] THEN RETURN FALSE END(*IF*);
	    IF first^[j] < second^[j] THEN RETURN TRUE END(*IF*);
	    (*# restore *)
	    INC (j);
	    IF j >= recordsize THEN RETURN FALSE END(*IF*);
	END (*LOOP*);
    END less;

(************************************************************************)

PROCEDURE FindInsertionPoint (f: File;  dataptr: BufferPointer;
		recsize: CARDINAL;  VAR (*OUT*) place: LONGCARD): ErrorCode;

    (* Returns (assuming result is OK) with place equal to the point in	*)
    (* file f where dataptr^ should be inserted.			*)

    VAR low, mid, high, recordsize: LONGCARD;  count: CARDINAL;
	bufptr: BufferPointer;
	status: ErrorCode;

    BEGIN
	ALLOCATE (bufptr, recsize);
	status := OK;
	recordsize := LONGCARD(recsize);

	low := 0;  high := FileSize(f) DIV recordsize;
	LOOP
	    IF high = low THEN EXIT (*LOOP*) END(*IF*);
	    mid := (low+high) DIV 2;
	    status := SetPosition (f, recordsize*mid);
	    IF status = OK THEN
		status := ReadRecord (f, bufptr, recsize, count);
	    END (*IF*);
	    IF status <> OK THEN EXIT(*LOOP*) END (*IF*);
	    IF less (bufptr, dataptr, recsize) THEN
		low := mid + 1;
	    ELSE
		high := mid;
	    END (*IF*);
	END (*LOOP*);

	place := recordsize*low;
	DEALLOCATE (bufptr, recsize);
	RETURN status;

    END FindInsertionPoint;

(************************************************************************)

PROCEDURE ShiftData (f: File;  place: LONGCARD;  amount: CARDINAL): ErrorCode;

    (* Creates a gap in file f, by shifting the contents from position	*)
    (* place onwards by amount bytes.  It is assumed (because of the	*)
    (* logic of the rest of the program) that the size of f is already	*)
    (* an integral multiple of amount.					*)

    CONST buffersize = 4096;

    VAR bufptr: BufferPointer;  oldpos, remaining: LONGCARD;
	status: ErrorCode;  dummy: CARDINAL;

    BEGIN
	ALLOCATE (bufptr, buffersize);
	oldpos := FileSize (f);
	remaining := oldpos - place;
	status := OK;

	LOOP
	    IF remaining < buffersize THEN EXIT(*LOOP*) END(*IF*);
	    DEC (oldpos, buffersize);
	    status := SetPosition (f, oldpos);
	    IF status = OK THEN
		status := ReadRecord (f, bufptr, buffersize, dummy);
	    END (*IF*);
	    IF status <> OK THEN EXIT(*LOOP*) END (*IF*);
	    status := SetPosition (f, oldpos+LONGCARD(amount));
	    IF status = OK THEN
		status := WriteRecord (f, bufptr, buffersize);
	    END (*IF*);
	    IF status <> OK THEN EXIT(*LOOP*) END (*IF*);
	    DEC (remaining, buffersize);
	END (*LOOP*);

	(* We still have to move a chunk at the beginning.	*)

	IF (status = OK) AND (remaining > 0) THEN
	    status := SetPosition (f, place);
	    IF status = OK THEN
		status := ReadRecord (f, bufptr, CARDINAL(remaining), dummy);
	    END (*IF*);
	    IF status = OK THEN
		status := SetPosition (f, place+LONGCARD(amount));
	    END (*IF*);
	    IF status = OK THEN
		status := WriteRecord (f, bufptr, CARDINAL(remaining));
	    END (*IF*);
	END (*IF*);

	DEALLOCATE (bufptr, buffersize);
	RETURN status;

    END ShiftData;

(************************************************************************)

PROCEDURE InsertRecord (f: File;  dataptr: BufferPointer;
					recordsize: CARDINAL): ErrorCode;

    (* Inserts a new record into file f. *)

    VAR place: LONGCARD;
	status: ErrorCode;

    BEGIN
	status := FindInsertionPoint (f, dataptr, recordsize, place);
	IF status = OK THEN
	    status := ShiftData (f, place, recordsize);
	END (*IF*);
	IF status = OK THEN
	    status := SetPosition (f, place);
	END (*IF*);
	IF status = OK THEN
	    status := WriteRecord (f, dataptr, recordsize);
	END (*IF*);
	RETURN status;
    END InsertRecord;

(************************************************************************)

PROCEDURE InsertionSort;

    (* Reads a file sequentially, creates a sorted version in a new	*)
    (* file.  For now, we are using an insertion sort method.		*)

    VAR infile, outfile: File;  log, w: Window;
	j: CARDINAL;  count: CARDINAL;
	name, errbuf: ARRAY [0..40] OF CHAR;
	status: ErrorCode;
	BufSize: CARDINAL;
	Bufptr: BufferPointer;

    BEGIN
	BufSize := 20;
	OpenWindow (w, white, black, 0, 16, 0, 79,
					noframe, nodivider);
	OpenWindow (log, white, black, 17, 24, 0, 79,
					simpleframe, nodivider);
	WriteString (log, "I/O Test Program: should produce a sorted file");
	WriteLn (log);
	WriteString (log, "Source file name (full path name): ");
	name := "C:\PMOS\TESTS\UNSORTED.DAT";
	EditString (log, name, SIZE(name));
	IF EditAborted() THEN status := OperationAborted
	ELSE status := OpenFile (infile, name, FALSE);
	END (*IF*);
	IF status = OK THEN
	    WriteLn (log);
	    WriteString (log, "Copy to file (full path name): ");
	    name := "A:TEST1.TMP";
	    EditString (log, name, SIZE(name));
	    IF EditAborted() THEN status := OperationAborted
	    ELSE status := OpenFile (outfile, name, TRUE);
	    END (*IF*);
	    IF status = OK THEN
		WriteLn (log);  WriteString (log, "Record size: ");
		BufSize := ReadBufferedCardinal (log, 4);
		ALLOCATE (Bufptr, BufSize);
		WHILE NOT EOF(infile) DO
		    IF testing THEN
			WriteLn (debug);
			WriteString (debug, "About to read a record");
		    END (*IF*);
		    status := ReadRecord (infile, Bufptr, BufSize, count);
		    IF testing THEN
			WriteLn (debug);
			WriteString (debug, "Read one record");
		    END (*IF*);
		    IF status <> OK THEN
			WriteLn (log);  WriteString (log, "Read Error!");
		    ELSIF count > 0 THEN
			FOR j := 0 TO count-1 DO
			    (*# save, check(index=>off) *)
			    Write (w, Bufptr^[j]);
			    (*# restore *)
			END (*FOR*);
			status := InsertRecord (outfile, Bufptr, BufSize);
			IF status <> OK THEN
			    WriteLn (log);  WriteString (log, "Write Error!");
			END (*IF*);
		    END (*IF*);
		END (*WHILE*);
		IF testing THEN
		    WriteLn (debug);
		    WriteString (debug, "Have written last record");
		END (*IF*);
		DEALLOCATE (Bufptr, BufSize);
	    ELSE
		WriteLn (log);  WriteString (log, "Could not open output file");
		WriteLn (log);  WriteString (log, "Error code ");
		TranslateErrorCode (status, errbuf);
		WriteString (log, errbuf);
	    END (*IF successfully opened output file*);

	    IF testing THEN
		WriteLn (debug);  WriteString (debug, "Calling CloseFile");
	    END (*IF*);
	    CloseFile (outfile);
	ELSE
	    WriteLn (log);  WriteString (log, "Could not open input file");
	    WriteLn (log);  WriteString (log, "Error code ");
	    TranslateErrorCode (status, errbuf);
	    WriteString (log, errbuf);
	END (*IF successfully opened input file*);
	CloseFile (infile);
	WriteLn (log);  WriteString (log, "End of Sort");
	PressAnyKey (log);
	CloseWindow (w);  CloseWindow (log);
    END InsertionSort;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    IF testing THEN
	OpenWindow (debug, white, black, 12, 16, 0, 79,
					simpleframe, nodivider);
    END (*IF*);
    InsertionSort;
END Sort2.
