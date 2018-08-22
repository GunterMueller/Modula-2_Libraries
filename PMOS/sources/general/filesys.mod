IMPLEMENTATION MODULE FileSys;

	(****************************************************************)
	(*								*)
	(*			File operations.			*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	6 September 1993			*)
	(*  Status:		Working					*)
	(*								*)
	(****************************************************************)

IMPORT FIO;

FROM Str IMPORT Caps, Compare;

(************************************************************************)
(*		    TRANSLATING THE LIBRARY ERROR CODES			*)
(************************************************************************)

PROCEDURE IOstatus(): ErrorCode;

    (* Returns the status of the last I/O operation.  We do a "best	*)
    (* guess" approximation to the error code.				*)

    BEGIN
	CASE FIO.IOresult() OF
	  0:	RETURN OK;
	 |
	  0A0H:	RETURN FileNotOpen;
	 |
	  0A2H:	RETURN NameNotFound;
	 |
	  0A5H:	RETURN DeviceFull;
	 |
	ELSE
		RETURN UndiagnosedFailure;
	END (*CASE*);
    END IOstatus;

(************************************************************************)
(*			     OPENING A FILE				*)
(************************************************************************)

PROCEDURE OpenFile (VAR (*OUT*) f: File;  name: ARRAY OF CHAR;
					newfile: BOOLEAN): ErrorCode;

    (* Opens the file named by the given character string, and returns	*)
    (* f as the identification to be used when specifying this file in	*)
    (* future.  If newfile is TRUE, a new file is created.  If newfile	*)
    (* is FALSE, the file must already exist.				*)

    VAR FileExists: BOOLEAN;

    BEGIN
	Caps (name);
	IF Compare (name, "PRN") = 0 THEN
	    f := FIO.PrinterDevice;
	    RETURN OK;
	END (*IF*);
	FileExists := FIO.Exists(name);
	IF newfile THEN
	    IF FileExists THEN RETURN DuplicateFileName;
	    ELSE
		f := FIO.Create(name);
		RETURN IOstatus();
	    END (*IF*);
	ELSIF FileExists THEN
	    f := FIO.Open(name);
	    IF f = MAX(CARDINAL) THEN RETURN NameNotFound
	    ELSE RETURN IOstatus();
	    END (*IF*);
	ELSE
	    RETURN NameNotFound;
	END (*IF*);
    END OpenFile;

(************************************************************************)
(*			     CLOSING A FILE				*)
(************************************************************************)

PROCEDURE CloseFile (VAR (*INOUT*) f: File);

    (* Closes file f. *)

    VAR dummy: CARDINAL;

    BEGIN
	FIO.Close(f);
	dummy := FIO.IOresult();
    END CloseFile;

(************************************************************************)
(*			    END-OF-FILE TEST				*)
(************************************************************************)

PROCEDURE EOF (f: File): BOOLEAN;

    (* Returns TRUE iff we are currently at the end of file f.		*)
    (* Remark: the library procedure FIO.EOF is inadequate for this	*)
    (* purpose, because (a) it is only updated after a read, and	*)
    (* (b) there is no feedback about WHICH file has reached its end.	*)

    BEGIN
	RETURN FIO.GetPos(f) >= FIO.Size(f);
    END EOF;

(************************************************************************)
(*				WRITING					*)
(************************************************************************)

PROCEDURE WriteByte (f: File;  value: BYTE): ErrorCode;

    (* Writes one byte to the file.  The returned value is an error	*)
    (* code (OK if no error).						*)

    BEGIN
	FIO.WrChar (f, value);
	RETURN IOstatus();
    END WriteByte;

(************************************************************************)

PROCEDURE WriteRecord (f: File;  buffaddr: ADDRESS;
					count: CARDINAL): ErrorCode;

    (* Writes count bytes from memory location buffaddr.	*)

    BEGIN
	FIO.WrBin (f, buffaddr^, count);
	RETURN IOstatus();
    END WriteRecord;

(************************************************************************)
(*				READING					*)
(************************************************************************)

PROCEDURE ReadByte (f: File): BYTE;

    (* Returns the next byte from the file.	*)

    BEGIN
	RETURN FIO.RdChar(f);
    END ReadByte;

(************************************************************************)

PROCEDURE ReadRecord (f: File;  buffaddr: ADDRESS;  desired: CARDINAL;
				VAR (*OUT*) actual: CARDINAL): ErrorCode;

    (* Reads up to "desired" bytes from file f to memory location	*)
    (* "buffaddr".  On return, "actual" gives the number of bytes read.	*)

    BEGIN
	actual := FIO.RdBin (f, buffaddr^, desired);
	RETURN IOstatus();
    END ReadRecord;

(************************************************************************)
(*				RANDOM ACCESS				*)
(************************************************************************)

PROCEDURE SetPosition (f: File;  position: LONGCARD): ErrorCode;

    (* Ensures that the next read or write on this file will be at	*)
    (* byte number position in the file.  (The first byte in the file	*)
    (* is byte number 0.)  If a position greater than the file size	*)
    (* is specified, the length of the file will increase.		*)

    VAR size, amount: LONGCARD;

    BEGIN
	size := FIO.Size(f);
	IF position > size THEN
	    FIO.Seek (f, size);
	    IF IOstatus() <> OK THEN
		RETURN SeekFailure;
	    END (*IF*);
	    amount := position - size;
	    WHILE amount > MAX(CARDINAL) DO
		FIO.WrCharRep (f, CHR(0), MAX(CARDINAL));
		DEC (amount, VAL(LONGCARD, MAX(CARDINAL)));	
	    END (*WHILE*);
	    FIO.WrCharRep (f, CHR(0), VAL(CARDINAL,amount));	
	END (*IF*);
	FIO.Seek (f, position);
	IF IOstatus() <> OK THEN RETURN SeekFailure;
	ELSE RETURN OK;
	END (*IF*);
    END SetPosition;

(************************************************************************)

PROCEDURE SavePosition (f: File): LONGCARD;

    (* Returns the current byte number in file f.	*)

    BEGIN
	RETURN FIO.GetPos(f);
    END SavePosition;

(************************************************************************)

PROCEDURE FileSize (f: File): LONGCARD;

    (* Returns the length of the file in bytes.	*)

    BEGIN
	RETURN FIO.Size(f);
    END FileSize;

(************************************************************************)

BEGIN
    FIO.IOcheck := FALSE;
END FileSys.
