MODULE Sort1;

	(********************************************************)
	(*							*)
	(*		    File sort program			*)
	(*							*)
	(*	This is a test of the PMOS file system, so	*)
	(*	we're not particularly concerned about the	*)
	(*	efficiency of the sorting algorithm.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	17 March 1995			*)
	(*  Status:		Working, but incredibly slow	*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* type *)	ADDRESS,
    (* proc *)	ADR;

FROM Trace IMPORT
    (* proc *)	NYI;

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
    (* proc *)	ReadBufferedCardinal, WriteAddress, WriteLongCard;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode,
    (* proc *)	TranslateErrorCode;

(************************************************************************)

CONST testing = TRUE;
      testingSP = FALSE;

TYPE BufferPointer = POINTER TO ARRAY [0..MAX(CARDINAL)-1] OF CHAR;

VAR debug: Window;

(************************************************************************)

PROCEDURE greater (first, second: BufferPointer;  recordsize: CARDINAL)
								: BOOLEAN;

    (* Tests for first^ > second^.	*)

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	LOOP
	    IF first^[j] > second^[j] THEN RETURN TRUE END(*IF*);
	    IF first^[j] < second^[j] THEN RETURN FALSE END(*IF*);
	    INC (j);
	    IF j >= recordsize THEN RETURN FALSE END(*IF*);
	END (*LOOP*);
    END greater;

(************************************************************************)

PROCEDURE PutRecord (f: File;  dataptr: BufferPointer;  recordsize: CARDINAL)
								: ErrorCode;

    (* Inserts a new record into file f. *)

    VAR limit, place: LONGCARD;  count: CARDINAL;
	OriginalBufptr, bufptr, temp: BufferPointer;
	status: ErrorCode;

    BEGIN
	limit := FileSize(f);  place := 0;
	ALLOCATE (OriginalBufptr, recordsize);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Allocated buffer at ");
	    WriteAddress (debug, OriginalBufptr);
	END (*IF*);
	bufptr := OriginalBufptr;
	status := OK;

	(* Work out where to do the insertion. *)

	LOOP
	    IF place >= limit THEN EXIT(*LOOP*) END(*IF*);
	    IF testingSP THEN
		WriteLn (debug);  WriteString (debug, "Setting position to ");
		WriteLongCard (debug, place);
		PressAnyKey (debug);
	    END (*IF*);
	    status := SetPosition (f, place);
(*
	    IF testingSP THEN
		WriteLn (debug);  WriteString (debug, "Return from SetPosition");
		PressAnyKey (debug);
	    END (*IF*);
*)
	    IF status = OK THEN
		status := ReadRecord (f, bufptr, recordsize, count);
	    END (*IF*);
	    IF status <> OK THEN EXIT(*LOOP*) END (*IF*);
	    IF greater (bufptr, dataptr, recordsize) THEN
		EXIT (*LOOP*);
	    END (*IF*);
	    INC (place, LONGCARD(recordsize));
	END (*LOOP*);

	IF testing THEN
	    WriteLn (debug);  WriteString (debug, "Inserting record at ");
	    WriteLongCard (debug, place);
	END (*IF*);

	IF status <> OK THEN
	    DEALLOCATE (OriginalBufptr, recordsize);
	    RETURN status;
	END(*IF*);

	(* Shift all the existing records, and insert the new one. *)

	LOOP
	    (* dataptr^ is the record to be inserted at the current	*)
	    (* file position, and bufptr^ is the record which it has	*)
	    (* displaced.						*)

	    IF testingSP THEN
		WriteLn (debug);  WriteString (debug, "Setting position to ");
		WriteLongCard (debug, place);
		PressAnyKey (debug);
	    END (*IF*);
	    status := SetPosition (f, place);
(*
	    IF testingSP THEN
		WriteLn (debug);  WriteString (debug, "Return from SetPosition");
		PressAnyKey (debug);
	    END (*IF*);
*)
	    IF status = OK THEN
		status := WriteRecord (f, dataptr, recordsize);
	    END (*IF*);
	    IF status <> OK THEN EXIT(*LOOP*) END (*IF*);
	    IF place >= limit THEN EXIT(*LOOP*) END(*IF*);

	    (* Swap the buffer pointers, so that dataptr^ will then be	*)
	    (* the last record which we read and which still has to	*)
	    (* be re-inserted.						*)

	    temp := dataptr;  dataptr := bufptr;  bufptr := temp;
	    INC (place, LONGCARD(recordsize));
	    IF testingSP THEN
		WriteLn (debug);  WriteString (debug, "Setting position to ");
		WriteLongCard (debug, place);
		PressAnyKey (debug);
	    END (*IF*);
	    status := SetPosition (f, place);
(*
	    IF testingSP THEN
		WriteLn (debug);  WriteString (debug, "Return from SetPosition");
		PressAnyKey (debug);
	    END (*IF*);
*)
	    IF status = OK THEN
		status := ReadRecord (f, bufptr, recordsize, count);
	    END (*IF*);
	    IF status <> OK THEN EXIT(*LOOP*) END (*IF*);
	END (*LOOP*);

	DEALLOCATE (OriginalBufptr, recordsize);
	RETURN status;
    END PutRecord;

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
	WriteString (log, "I/O Test Program (slow!): should produce a sorted file");
	WriteLn (log);
	WriteString (log, "Source file name (full path name): ");
	name := "A:\UNSORTED.DAT";
	EditString (log, name, SIZE(name));
	IF EditAborted() THEN
	    WriteLn (log);
	    WriteString (log, "Operation aborted at user request");
	ELSE
	    status := OpenFile (infile, name, FALSE);
	    IF status = OK THEN
		WriteLn (log);
		WriteString (log, "Copy to file (full path name): ");
		name := "A:TEST1.TMP";
		EditString (log, name, SIZE(name));
		IF EditAborted() THEN
		    WriteLn (log);
		    WriteString (log, "Operation aborted at user request");
		ELSE
		    status := OpenFile (outfile, name, TRUE);
		    IF status = OK THEN
			WriteLn (log);  WriteString (log, "Record size: ");
			BufSize := ReadBufferedCardinal (log, 4);
			ALLOCATE (Bufptr, BufSize);
			WHILE NOT EOF(infile) DO
			    status := ReadRecord (infile, Bufptr, BufSize, count);
			    IF status <> OK THEN
				WriteLn (log);  WriteString (log, "Read Error!");
			    ELSIF count > 0 THEN
				FOR j := 0 TO count-1 DO
				    Write (w, Bufptr^[j]);
				END (*FOR*);
				status := PutRecord (outfile, Bufptr, BufSize);
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
		END (*IF EditAborted*);
	    ELSE
		WriteLn (log);  WriteString (log, "Could not open input file");
		WriteLn (log);  WriteString (log, "Error code ");
		TranslateErrorCode (status, errbuf);
		WriteString (log, errbuf);
	    END (*IF successfully opened input file*);
	    CloseFile (infile);
	    WriteLn (log);  WriteString (log, "End of Sort");
	    PressAnyKey (log);
	END (*IF EditAborted*);
	CloseWindow (w);  CloseWindow (log);
    END InsertionSort;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    IF testing OR testingSP THEN
	OpenWindow (debug, white, black, 12, 16, 0, 79,
					simpleframe, nodivider);
    END (*IF*);
    InsertionSort;
END Sort1.
