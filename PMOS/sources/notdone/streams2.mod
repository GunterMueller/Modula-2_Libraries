IMPLEMENTATION MODULE Streams2;

	(********************************************************)
	(*							*)
	(*	   I/O module: PMOS replacement for the		*)
	(*	     Streams module in the FTL library		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	16 September 1991		*)
	(*  Status:		Being tested			*)
	(*							*)
	(*	Most operations are working.			*)
	(*	Random-access operations not yet tested.	*)
	(*	RenameFile not yet implemented.			*)
	(*	DeleteFile not yet implemented.			*)
	(*							*)
	(*	The immediate goal is to use this module as	*)
	(*	a test bed for working out what features	*)
	(*	should be added to the Files module.		*)
	(*							*)
	(*	In the long term, this module is likely to	*)
	(*	become obsolete (except as an interface for	*)
	(*	programs which were using the FTL library	*)
	(*	module), since it is probable that all desired	*)
	(*	features will be included in module Files.	*)
	(*							*)
	(********************************************************)

FROM Trace IMPORT
    (* proc *)	NYI;

FROM SYSTEM IMPORT
    (* proc *)	ADR;

FROM Files IMPORT
    (* type *)	File,
    (* proc *)	OpenFile, CloseFile, EOF, ReadByte, ReadRecord,
		WriteByte, WriteRecord, SetPosition, SavePosition,
		FileSize;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode;

FROM Logic IMPORT
    (* proc *)	MakeLong, HighWord, LowWord;

IMPORT HardDisk, Floppy;

(************************************************************************)

TYPE
    STREAM = File;

(************************************************************************)
(*			OPENING AND CLOSING FILES			*)
(************************************************************************)

PROCEDURE Connect (VAR (*OUT*) s:STREAM;  name: ARRAY OF CHAR;
						dir: Direction): CARDINAL;

    (* Opens a file.  The function result is zero if successful, or	*)
    (* nonzero to indicate that the operation failed.			*)

    BEGIN
	RETURN ORD (OpenFile (s, name, dir=output));
    END Connect;

(************************************************************************)

PROCEDURE Disconnect (VAR (*INOUT*) s:STREAM;  closefile:BOOLEAN);

    (* Closes a file, if closefile is TRUE.  The action when closefile	*)
    (* is FALSE has not yet been defined.				*)

    BEGIN
	IF closefile THEN
	    CloseFile (s);
	END (*IF*);
    END Disconnect;

(************************************************************************)
(*				INPUT					*)
(************************************************************************)

PROCEDURE ReadChar (s: STREAM;  VAR (*OUT*) ch:CHAR);

    (* Read one byte.	*)

    BEGIN
	ch := ReadByte (s);
    END ReadChar;

(************************************************************************)

PROCEDURE ReadWord (s: STREAM;  VAR (*OUT*) w: WORD);

    (* Read two sequential bytes.	*)

    VAR reply: INTEGER;

    BEGIN
	ReadRec (s, ADR(w), 2, reply);
    END ReadWord;

(************************************************************************)

PROCEDURE BufferAhead (s: STREAM;  force: BOOLEAN);

    (* So far, has no effect.  In the long term the intention is to	*)
    (* allow line editing for keyboard input.				*)

    BEGIN
    END BufferAhead;

(************************************************************************)

PROCEDURE ReadRec (s: STREAM;  buffaddr: ADDRESS;  count: CARDINAL;
					VAR (*OUT*) reply: INTEGER);

    (* Reads count bytes to memory location buffaddr.  The last		*)
    (* parameter returns the number of bytes actually read.   A reply	*)
    (* of -1 indicates an error, in which case an error code may be	*)
    (* found in variable IOResult.					*)

    VAR actualcount: CARDINAL;  status: ErrorCode;

    BEGIN
	status := ReadRecord (s, buffaddr, count, actualcount);
	IOResult := ORD(status);
	IF status = OK THEN
	    reply := actualcount;
	ELSE
	    reply := -1;
	END (*IF*);
    END ReadRec;

(************************************************************************)
(*				OUTPUT					*)
(************************************************************************)

PROCEDURE WriteWord (s: STREAM;  w: WORD);

    (* Output two bytes.	*)

    VAR dummy: INTEGER;

    BEGIN
	WriteRec (s, ADR(w), 2, dummy);
    END WriteWord;

(************************************************************************)

PROCEDURE WriteChar (s: STREAM;  ch: CHAR);

    (* Output one byte.	*)

    BEGIN
	IOResult := ORD (WriteByte (s, ch));
    END WriteChar;

(************************************************************************)

PROCEDURE WriteRec (s:STREAM;  buffaddr: ADDRESS;  count: CARDINAL;
					VAR (*OUT*) reply: INTEGER);

    (* Writes count bytes from memory location buffaddr.  The last	*)
    (* parameter returns the number of bytes actually written.		*)
    (* A reply of -1 indicates an error, in which case an error code	*)
    (* may be found in variable IOResult.				*)

    VAR status: ErrorCode;

    BEGIN
	status := WriteRecord (s, buffaddr, count);
	IOResult := ORD(status);
	IF status = OK THEN
	    reply := count;
	ELSE
	    reply := -1;
	END (*IF*);
    END WriteRec;

(************************************************************************)
(*			    END-OF-FILE TEST				*)
(************************************************************************)

PROCEDURE EOS (s: STREAM): BOOLEAN;

    (* Returns TRUE iff at end of file. *)

    BEGIN
	RETURN EOF (s);
    END EOS;

(************************************************************************)
(*				RANDOM ACCESS				*)
(************************************************************************)

PROCEDURE Reset (s: STREAM);

    (* Goes to beginning of file.  On list device, starts new page.	*)

    BEGIN
	IOResult := ORD (SetPosition (s, 0));
    END Reset;

(************************************************************************)

PROCEDURE SetLongPos (s: STREAM;  Pos: LONGCARD);

    (* Goes to byte position Pos in file.  Start of file is position 0.	*)

    BEGIN
	IOResult := ORD (SetPosition (s, Pos));
    END SetLongPos;

(************************************************************************)

PROCEDURE SetPos (s: STREAM;  high, low: CARDINAL);

    (* Same as SetLongPos (s, MakeLong(high, low))	*)

    BEGIN
	IOResult := ORD (SetPosition (s, MakeLong (high, low)));
    END SetPos;

(************************************************************************)

PROCEDURE GetLongPos (s: STREAM;  VAR (*OUT*) Pos: LONGCARD);

    (* Returns the current position, i.e. the byte number of the next	*)
    (* byte to be read or written.					*)

    BEGIN
	Pos := SavePosition (s);
    END GetLongPos;

(************************************************************************)

PROCEDURE GetPos (s: STREAM;  VAR (*OUT*) high, low: CARDINAL);

    (* Same as GetLongPos, but decomposes the result into two words.	*)

    VAR position: LONGCARD;

    BEGIN
	position := SavePosition (s);
	high := HighWord (position);  low := LowWord (position);
    END GetPos;

(************************************************************************)

PROCEDURE StreamLength (s: STREAM): LONGCARD;

    (* Returns the length of the file in bytes.	*)

    BEGIN
	RETURN FileSize (s);
    END StreamLength;

(************************************************************************)
(*			OPERATIONS ON AN ENTIRE FILE			*)
(************************************************************************)

PROCEDURE RenameFile (From, To: ARRAY OF CHAR): CARDINAL;

    (* Changes the name of a file.  A full path name is expected.	*)
    (* Do not specify different devices.				*)

    BEGIN
	NYI ("RenameFile");
    END RenameFile;

(************************************************************************)

PROCEDURE DeleteFile (name: ARRAY OF CHAR): CARDINAL;

    (* Deletes a file.  A full path name is expected.			*)

    BEGIN
	NYI ("DeleteFile");
    END DeleteFile;

(************************************************************************)

PROCEDURE BufferStream (s: STREAM;  BufferSize:CARDINAL);

    (* A no-op at this stage.	*)

    BEGIN
    END BufferStream;

(************************************************************************)

END Streams2.
