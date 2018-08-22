IMPLEMENTATION MODULE Files;

	(****************************************************************)
	(*								*)
	(*			File operations.			*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	27 February 1995			*)
	(*  Status:		OK					*)
	(*								*)
	(*	SetPosition not yet tested.				*)
	(*								*)
	(*	To avoid complications in FAT updating, the first	*)
	(*	cluster of a new file is pre-allocated.  Should we	*)
	(*	de-allocate it again (probably in module Directories)	*)
	(*	if the file size is 0 when the file is closed?		*)
	(*	A related question: what should we do about opening	*)
	(*	an existing file whose file size is 0?  Possibly we	*)
	(*	should be pre-allocating a block in that case too,	*)
	(*	but that could cause problems if the disk is full	*)
	(*	or write-protected.					*)
	(*								*)
	(*	A DeleteFile operation will no doubt be needed		*)
	(*	at some stage.						*)
	(*								*)
	(*	Writing is double-buffered, but so far reading is	*)
	(*	not.  At the present stage of the design, I lean	*)
	(*	towards keeping the current scheme, on the grounds	*)
	(*	that reading ahead is worthwhile only if one can be	*)
	(*	certain that access will be strictly sequential.	*)
	(*	Unlike many operating systems, PMOS does not have a	*)
	(*	bias towards serial streams.				*)
	(*								*)
	(****************************************************************)

FROM SYSTEM IMPORT
    (* proc *)	ADR;

FROM Devices IMPORT
    (* type *)	Device, BlockNumberType, RequestBlock, OperationType,
    (* proc *)	IOrequest;

FROM Directories IMPORT
    (* type *)	Cluster, Handle,
    (* const*)	NoSuchBlock,
    (* proc *)	Lookup, NextBlockNumber, AllocateBlock, UpdateFileSize,
		FindRelativeCluster, DiscardHandle;

FROM Semaphores IMPORT
    (* proc *)	CreateSemaphore, DestroySemaphore, Wait, Signal;

FROM LowLevel IMPORT
    (* proc *)	Copy, AddOffset;

FROM MaintenancePages IMPORT	(* for testing *)
    (* type *)	MaintenancePage,
    (* proc *)	CreateMaintenancePage, Associate;

FROM Windows IMPORT		(* for testing *)
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, ChangeScrollingRegion, SetCursor,
		WriteString, WriteChar, WriteLn;

FROM NumericIO IMPORT
    (* proc *)	WriteLongCard;

FROM IOErrorCodes IMPORT
    (* proc *)	TranslateErrorCode;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM DMA IMPORT
    (* proc *)	AllocateDMABuffer;

(************************************************************************)
(* General remark: a potential cause for confusion in reading this code	*)
(* is the distinction between disk blocks and clusters.  A cluster is	*)
(* some integral number of adjacent blocks, the exact number being held	*)
(* in the boot block of the disk.  Module Directories works internally	*)
(* in terms of clusters, but module Devices (which does not know about	*)
(* the boot block) works in terms of block numbers.  The present module	*)
(* buffers data a cluster at a time, but must talk to module Devices in	*)
(* terms of blocks.  We resolve this conflict by using block numbers	*)
(* to identify the clusters.  Every reference to a block number in this	*)
(* module is in fact a reference to the block number of the first block	*)
(* in a cluster.							*)
(************************************************************************)

CONST testing = TRUE;

CONST GuardConst = 9999H;

TYPE

    (* The size of a data buffer has to be determined when the file is	*)
    (* opened, because different disks have different numbers of bytes	*)
    (* per cluster, and we read or write one cluster at a time.		*)

    DataBufferPointer = POINTER TO ARRAY [0..MAX(CARDINAL)-1] OF BYTE;

    (* A file is identified by pointing to its FileData record.		*)

    File = POINTER TO FileData;

    (********************************************************************)
    (* We keep a FileData record for each open file.  The information	*)
    (* stored is:							*)
    (*									*)
    (*	  guard		a constant, to let us detect dangling pointers	*)
    (*	  locator	information needed by module Directories to	*)
    (*			 identify the file				*)
    (*	  EndBlock	block number of the last cluster in the file;	*)
    (*			 not necessarily accurate at all times, but	*)
    (*			 must be accurate at the time we extend the	*)
    (*			 file.						*)
    (*	  current	byte number within the current buffer		*)
    (*	  ByteNumber	byte number within the file as a whole		*)
    (*	  FileSize	number of bytes in the file			*)
    (*	  BufferSize	number of bytes in the buffer			*)
    (*	  ReadInProgress TRUE if the current data buffer is busy	*)
    (*			 with a read operation				*)
    (*	  AtEOF		TRUE iff we are now at the end of the file	*)
    (*	  BufferModified TRUE if the current contents of the data	*)
    (*			buffer are different from the disk copy		*)
    (*	  LengthChanged	TRUE iff the file size has been altered since	*)
    (*			the file was opened				*)
    (*	  BufferNumber	identifies which I/O request block, and		*)
    (*			therefore which data buffer, is currently active*)
    (*	  BufferHeader	a pair of I/O request blocks; we have two to	*)
    (*			permit double buffering on output (at present	*)
    (*			there is no provision for read-ahead on input)	*)
    (*	  Bufptr	pointer to currently active data buffer		*)
    (*									*)
    (* When writing to a file, a block number is normally not allocated	*)
    (* until the buffer has been filled and we are about to write out	*)
    (* the buffer, so usually Block = NoSuchBlock.  There are in fact	*)
    (* several different cases to consider:				*)
    (*	 (a)	Rewriting an existing cluster, i.e. writing to a	*)
    (*		cluster after reading it.  (This includes the case	*)
    (*		where some data have been appended to the end of the	*)
    (*		file, but not yet enough to require a new cluster to be	*)
    (*		allocated.)  Here Block is the block number of the	*)
    (*		first block in the cluster.				*)
    (*	 (b)	Appending a new cluster at the tail of the file, when	*)
    (*		the file already contains one or more clusters.  In	*)
    (*		this case Block = NoSuchBlock.				*)
    (*	 (c)	Writing the first cluster of a new file.  This case	*)
    (*		could cause complications in terms of updating the	*)
    (*		directory.  To avoid those complications, we arrange	*)
    (*		for the first cluster to be pre-allocated when the file	*)
    (*		is opened, so that this case is actually the same as	*)
    (*		case (a).						*)
    (*									*)
    (* The value of EndBlock is not always up-to-date, since we cannot	*)
    (* know the value until we have read the last cluster of the file.	*)
    (* Fortunately, we need it only when writing to a cluster which has	*)
    (* not yet been allocated (see procedure WriteCurrentBuffer), and	*)
    (* that can happen only during sequential file operations.  In such	*)
    (* cases, EndBlock is correctly updated as we reach the end of the	*)
    (* file.  If we jump to or beyond the current end of the file via	*)
    (* procedure SetPosition, module Directories looks after allocating	*)
    (* the clusters.							*)
    (*									*)
    (********************************************************************)

    FileData =	RECORD
		    guard: CARDINAL;
		    locator: Handle;
		    EndBlock: BlockNumberType;
		    current: CARDINAL;
		    ByteNumber, FileSize: LONGCARD;
		    BufferSize: CARDINAL;
		    ReadInProgress, AtEOF,
			BufferModified, LengthChanged: BOOLEAN;
		    BufferNumber: [0..1];
		    BufferHeader: ARRAY [0..1] OF RequestBlock;
		    Bufptr: DataBufferPointer;
		END (*RECORD*);

(************************************************************************)
(*			MAINTENANCE PAGE VARIABLES			*)
(************************************************************************)

VAR Mpage: MaintenancePage;
    debug: Window;

(************************************************************************)
(*			BLOCK READ/WRITE OPERATIONS			*)
(************************************************************************)

PROCEDURE WriteCurrentBuffer (f: File): BlockNumberType;

    (* Writes out the contents of the file's data buffer.  If this is a	*)
    (* write to an existing block, we simply overwrite the old block.	*)
    (* Otherwise, a new block is allocated.  We can tell the difference	*)
    (* between the two cases by testing whether NoSuchBlock is recorded	*)
    (* as the block number for this buffer.				*)
    (* Note that this is an asynchronous procedure, in that it returns	*)
    (* without waiting for the device driver to complete the write.	*)
    (* The function result is the block number of the following block.	*)

    VAR	Block, following: BlockNumberType;

    BEGIN
	WITH f^ DO
	    Block := BufferHeader[BufferNumber].BlockNumber;
	    IF Block = NoSuchBlock THEN

		(* We have run off the end of the file (which is normal	*)
		(* for a file being written), so allocate a new block.	*)

		Block := AllocateBlock (locator, EndBlock);
		EndBlock := Block;
		following := NoSuchBlock;

	    ELSE

		following := NextBlockNumber (locator, Block);
		IF following = NoSuchBlock THEN
		    EndBlock := Block;
		END (*IF*);

	    END (*IF*);

	    IF testing THEN
		WriteString (debug, "Writing block number ");
		WriteLongCard (debug, Block);  WriteLn (debug);
	    END (*IF*);

	    (* Write out the current block. *)

	    WITH BufferHeader[BufferNumber] DO
		IF Block = NoSuchBlock THEN
		    Status := DeviceFull;  Signal (DoneSemaphorePointer^);
		ELSE
		    operation := write;
		    BlockNumber := Block;
		    IOrequest (BufferHeader[BufferNumber]);
		END (*IF*);
	    END (*WITH*);

	END (*WITH*);
	RETURN following;
    END WriteCurrentBuffer;

(************************************************************************)

PROCEDURE AllocateBuffer (f: File;  B: BlockNumberType): ErrorCode;

    (* Waits until an I/O buffer is available, returns with		*)
    (* f^.BufferHeader[f^.BufferNumber] set up ready for a read of	*)
    (* block B.  As a side-effect, we return the status of the most	*)
    (* recent I/O operation on this buffer.  This version uses double	*)
    (* buffering.							*)

    BEGIN
	WITH f^ DO
	    BufferNumber := 1 - BufferNumber;
	    BufferModified := FALSE;
	    WITH BufferHeader[BufferNumber] DO
		Wait (DoneSemaphorePointer^);
		Bufptr := BufferAddress;
		operation := read;
		BlockNumber := B;
		RETURN Status;
	    END (*WITH*);
	END (*WITH*);
    END AllocateBuffer;

(************************************************************************)

PROCEDURE WaitForReadComplete (f: File): ErrorCode;

    (* Waits for completion of the most recent I/O operation on the	*)
    (* current buffer.  Updates AtEOF, ReadInProgress.			*)

    VAR status: ErrorCode;

    BEGIN
	WITH f^ DO
	    WITH BufferHeader[BufferNumber] DO
		Wait (DoneSemaphorePointer^);
		status := Status;
		AtEOF := BlockNumber = NoSuchBlock;
	    END (*WITH*);
	    ReadInProgress := FALSE;
	END (*WITH*);
	RETURN status;
    END WaitForReadComplete;

(************************************************************************)

PROCEDURE ReleaseCurrentBuffer (f: File): BlockNumberType;

    (* Flushes out the current buffer, writing out the contents if	*)
    (* necessary.  As a side-effect, we return the block number of the	*)
    (* next sequential cluster in the file.  EndBlock is updated if we	*)
    (* discover that we are at the last block in the file.		*)
    (* If the buffer does not need to be written out, and there is no	*)
    (* read in progress, the buffer immediately becomes available to be	*)
    (* grabbed by AllocateBuffer.  Otherwise, the buffer will become	*)
    (* available when the device driver signals that the current I/O	*)
    (* operation on this buffer is complete.				*)

    VAR NextBlock: BlockNumberType;

    BEGIN
	WITH f^ DO
	    IF BufferModified THEN
		NextBlock := WriteCurrentBuffer(f);
	    ELSE
		WITH BufferHeader[BufferNumber] DO
		    NextBlock := NextBlockNumber (locator, BlockNumber);
		    IF NextBlock = NoSuchBlock THEN
			EndBlock := BlockNumber;
		    END (*IF*);
		    IF NOT ReadInProgress THEN
			Signal (DoneSemaphorePointer^);
		    END (*IF*);
		END (*WITH*);
	    END (*IF*);
	END (*WITH*);
	RETURN NextBlock;
    END ReleaseCurrentBuffer;

(************************************************************************)

PROCEDURE StartRead (f: File;  B: BlockNumberType): ErrorCode;

    (* Allocates a buffer for input, and starts a read operation.  We	*)
    (* always allocate a fresh buffer so that we can overlap with the	*)
    (* last write if any, or with any read already in progress.  (If we	*)
    (* are starting a read when a read is already in progress it means	*)
    (* that we no longer care about the outcome of the previous read;	*)
    (* even so, it continues to lock up the buffer until it is done.)	*)

    VAR status: ErrorCode;

    BEGIN
	WITH f^ DO
	    status := AllocateBuffer (f, B);
	    IF status <> OK THEN
		RETURN status;
	    END (*IF*);

	    ReadInProgress := TRUE;
	    IF B = NoSuchBlock THEN
		Signal (BufferHeader[BufferNumber].DoneSemaphorePointer^);
	    ELSE
		IOrequest (BufferHeader[BufferNumber]);
	    END (*IF*);
	END (*WITH*);
	RETURN OK;

    END StartRead;

(************************************************************************)

PROCEDURE StartNextRead (f: File): ErrorCode;

    (* Writes out the current block if necessary, then initiates a read	*)
    (* of the following block in the file.  If we have already reached	*)
    (* the last block in the file, a Signal is immediately performed	*)
    (* to indicate that the operation is complete.			*)
    (* Otherwise, this procedures returns after the read operation has	*)
    (* been initiated, but before the read is complete.			*)

    BEGIN
	RETURN StartRead (f, ReleaseCurrentBuffer(f));
    END StartNextRead;

(************************************************************************)
(*			     OPENING A FILE				*)
(************************************************************************)

PROCEDURE OpenFile (VAR (*OUT*) f: File;  name: ARRAY OF CHAR;
					newfile: BOOLEAN): ErrorCode;

    (* Opens the file named by the given character string, and returns	*)
    (* f as the identification to be used when specifying this file in	*)
    (* future.  If newfile is TRUE, a new file is created.  If newfile	*)
    (* is FALSE, the file must already exist.				*)

    VAR dev: Device;  unitno: CARDINAL;  j: [0..1];
	fileid: Handle;
	BytesPerCluster: CARDINAL;
	StartingBlock: BlockNumberType;
	status: ErrorCode;
	BytesInFile: LONGCARD;
	string: ARRAY [0..31] OF CHAR;

    BEGIN
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "OpenFile: about to call Lookup");
	END (*IF*);
	status := Lookup (newfile, name, dev, unitno, fileid,
				StartingBlock, BytesPerCluster, BytesInFile);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Returned from Lookup, status is ");
	    TranslateErrorCode (status, string);
	    WriteString (debug, string);
	END (*IF*);
	IF status = OK THEN
	    NEW (f);
	    WITH f^ DO
		guard := GuardConst;
		locator := fileid;
		EndBlock := StartingBlock;
		current := 0;  ByteNumber := 0;
		FileSize := BytesInFile;
		BufferSize := BytesPerCluster;
		ReadInProgress := FALSE;
		AtEOF := FileSize = 0;
		BufferModified := FALSE;  LengthChanged := newfile;
		FOR j := 0 TO 1 DO
		    WITH BufferHeader[j] DO
			Status := OK;
			device := dev;
			unit := unitno;
			BlockNumber := NoSuchBlock;
			IF testing THEN
			    WriteLn (debug);
			    WriteString (debug, "About to allocate DMA buffer.");
			END (*IF*);
			AllocateDMABuffer (BufferAddress, BufferSize);
			IF testing THEN
			    WriteLn (debug);
			    WriteString (debug, "Have allocated DMA buffer.");
			END (*IF*);
			ByteCount := BufferSize;
			NEW (DoneSemaphorePointer);
			CreateSemaphore (DoneSemaphorePointer^, 1);
		    END (*WITH*);
		END (*FOR*);
		BufferNumber := 0;
		status := AllocateBuffer (f, StartingBlock);
		IF NOT newfile THEN
		    ReadInProgress := TRUE;
		    IOrequest (BufferHeader[BufferNumber]);
		END (*IF*);
	    END (*WITH*);
	ELSE	(* status <> OK *)
	    f := NIL;
	END (*IF*);

	IF testing THEN
	    IF status <> OK THEN
		WriteLn (debug);
		WriteString (debug, "OpenFile failure, status = ");
		TranslateErrorCode (status, string);
		WriteString (debug, string);
	    END (*IF*);
	END (*IF*);

	RETURN status;
    END OpenFile;

(************************************************************************)
(*			     CLOSING A FILE				*)
(************************************************************************)

PROCEDURE CloseFile (VAR (*INOUT*) f: File);

    (* Closes file f. *)

    VAR j: [0..1];  status: ErrorCode;
	dummy: BlockNumberType;

    BEGIN

	(* Note: we have to check for f = NIL, and also check the guard	*)
	(* field, because it is possible that this is a file which was	*)
	(* never successfully opened.					*)

	IF f <> NIL THEN
	    WITH f^ DO
		IF guard <> GuardConst THEN RETURN END (*IF*);

		(* If we have been writing to the file, then there may	*)
		(* be some data in the buffer which have not yet been	*)
		(* written.  It is also possible that the directory has	*)
		(* to be updated to show a revised file size.		*)

		dummy := ReleaseCurrentBuffer (f);
		IF LengthChanged THEN
		    UpdateFileSize (locator, FileSize);
		END (*IF*);
		DiscardHandle (locator);
		FOR j := 0 TO 1 DO
		    WITH BufferHeader[j] DO
			Wait (DoneSemaphorePointer^);
			DestroySemaphore (DoneSemaphorePointer^);
			DISPOSE (DoneSemaphorePointer);
			DEALLOCATE (BufferAddress, BufferSize);
		    END (*WITH*);
		END (*FOR*);
		guard := 0;
		DISPOSE (f);
	    END (*WITH*);
	END (*IF*);

    END CloseFile;

(************************************************************************)
(*			    END-OF-FILE TEST				*)
(************************************************************************)

PROCEDURE EOF (f: File): BOOLEAN;

    (* Returns TRUE iff we are currently at the end of file f.	*)

    BEGIN
	RETURN f^.AtEOF;
    END EOF;

(************************************************************************)
(*				WRITING					*)
(************************************************************************)

PROCEDURE SendToCurrentBuffer (f: File;  buffaddr: ADDRESS;
				VAR (*INOUT*) count: CARDINAL): ErrorCode;

    (* Writes up to count bytes from memory location buffaddr.		*)
    (* On return, count has been decremented by the number of bytes	*)
    (* actually written.  Unlike WriteRecord (below), this procedure	*)
    (* transfers at most one buffer-full of data.			*)

    VAR status: ErrorCode;  amount: CARDINAL;

    BEGIN
	status := OK;
	WITH f^ DO

	    (* Check whether we are still waiting for the	*)
	    (* buffer to become available.			*)

	    IF ReadInProgress THEN
		status := WaitForReadComplete (f);
		IF status <> OK THEN
		    RETURN status;
		END (*IF*);
	    END (*IF*);

	    (* Work out how much data to transfer.	*)

	    amount := BufferSize - current;
	    IF amount > count THEN
		amount := count;
	    END (*IF*);
	    Copy (buffaddr, ADR(Bufptr^[current]), amount);

	    (* Update counts, etc.	*)

	    BufferModified := TRUE;
	    DEC (count, amount);
	    INC (ByteNumber, LONGCARD(amount));  INC (current, amount);
	    AtEOF := ByteNumber >= FileSize;
	    IF ByteNumber > FileSize THEN
		FileSize := ByteNumber;
		LengthChanged := TRUE;
	    END (*IF*);

	    (* If we have just filled the buffer, write it out and read	*)
	    (* a new buffer-full.  Note that procedure StartNextRead	*)
	    (* writes out the current buffer if necessary, before	*)
	    (* reading the next block.  It is possible that there is no	*)
	    (* next block, but in the present situation that is not an	*)
	    (* error.							*)

	    IF current = BufferSize THEN
		status := StartNextRead (f);
		current := 0;
	    END (*IF*);

	END (*WITH*);
	RETURN status;
    END SendToCurrentBuffer;

(************************************************************************)

PROCEDURE WriteByte (f: File;  value: BYTE): ErrorCode;

    (* Writes one byte to the file.  The returned value is an error	*)
    (* code (OK if no error).						*)

    VAR count: CARDINAL;

    BEGIN
	IF (f = NIL) OR (f^.guard <> GuardConst) THEN
	    RETURN FileNotOpen;
	END (*IF*);
	count := 1;
	RETURN SendToCurrentBuffer (f, ADR(value), count);
    END WriteByte;

(************************************************************************)

PROCEDURE WriteRecord (f: File;  buffaddr: ADDRESS;
					count: CARDINAL): ErrorCode;

    (* Writes count bytes from memory location buffaddr.	*)

    VAR oldcount: CARDINAL;  status: ErrorCode;

    BEGIN
	status := OK;
	IF (f = NIL) OR (f^.guard <> GuardConst) THEN
	    status := FileNotOpen;
	END (*IF*);
	WHILE (status = OK) AND (count > 0) DO
	    oldcount := count;
	    status := SendToCurrentBuffer (f, buffaddr, count);
	    buffaddr := AddOffset (buffaddr, oldcount-count);
	END (*WHILE*);
	RETURN status;
    END WriteRecord;

(************************************************************************)
(*				READING					*)
(************************************************************************)

PROCEDURE FetchFromCurrentBuffer (f: File;  buffaddr: ADDRESS;
				VAR (*INOUT*) count: CARDINAL): ErrorCode;

    (* Reads up to count bytes from file f to memory location buffaddr.	*)
    (* On return, count has been decremented by the number of bytes	*)
    (* actually read.  Unlike ReadRecord (below), this procedure will	*)
    (* not read beyond the end of the current data buffer.		*)

    VAR amount: CARDINAL;  status: ErrorCode;

    BEGIN
	WITH f^ DO
	    IF AtEOF THEN
		RETURN OK;
	    ELSE
		(* Wait for any pending read. *)

		IF ReadInProgress THEN
		    status := WaitForReadComplete (f);
		    IF status <> OK THEN
			AtEOF := TRUE;
			RETURN status;
		    END (*IF*);
		END (*IF*);

		(* Work out how much data to transfer. *)

		amount := count;
		IF amount > BufferSize - current THEN
		    amount := BufferSize - current;
		END (*IF*);
		IF LONGCARD(amount) >= FileSize-ByteNumber THEN
		    amount := CARDINAL (FileSize - ByteNumber);
		    AtEOF := TRUE;
		END (*IF*);

		(* Perform the transfer. *)

		Copy (ADR(Bufptr^[current]), buffaddr, amount);
		INC (ByteNumber, LONGCARD(amount));  INC (current, amount);
		DEC (count, amount);

		(* If we have come to the end of the buffer, initiate	*)
		(* a new read operation.				*)

		IF current = BufferSize THEN
		    current := 0;
		    RETURN StartNextRead (f);
		ELSE
		    RETURN OK;
		END (*IF*);

	    END (*IF*);
	END (*WITH*);

    END FetchFromCurrentBuffer;

(************************************************************************)

PROCEDURE ReadByte (f: File): BYTE;

    (* Returns the next byte from the file.	*)

    VAR result: BYTE;  status: ErrorCode;  count: CARDINAL;

    BEGIN
	result := 0;
	IF (f <> NIL) AND (f^.guard = GuardConst) THEN
	    count := 1;
	    status := FetchFromCurrentBuffer (f, ADR(result), count);
	END (*IF*);
	RETURN result;
    END ReadByte;

(************************************************************************)

PROCEDURE ReadRecord (f: File;  buffaddr: ADDRESS;  desired: CARDINAL;
				VAR (*OUT*) actual: CARDINAL): ErrorCode;

    (* Reads up to "desired" bytes from file f to memory location	*)
    (* "buffaddr".  On return, "actual" gives the number of bytes read.	*)

    VAR status: ErrorCode;  count, oldcount: CARDINAL;

    BEGIN
	status := OK;  count := desired;
	IF (f = NIL) OR (f^.guard <> GuardConst) THEN
	    status := FileNotOpen;
	END (*IF*);
	WHILE (status = OK) AND (count > 0) AND NOT f^.AtEOF DO
	    oldcount := count;
	    status := FetchFromCurrentBuffer (f, buffaddr, count);
	    buffaddr := AddOffset (buffaddr, oldcount-count);
	END (*WHILE*);
	actual := desired-count;
	RETURN status;
    END ReadRecord;

(************************************************************************)
(*				RANDOM ACCESS				*)
(************************************************************************)

PROCEDURE SetPosition (f: File;  position: LONGCARD): ErrorCode;

    (* Ensures that the next read or write on this file will be at	*)
    (* byte number position in the file.  (The first byte in the file	*)
    (* is byte number 0.)  If a position greater than the file size	*)
    (* is specified, the length of the file will increase.		*)

    VAR NewBlock, dummy: BlockNumberType;
	status: ErrorCode;

    BEGIN
	IF f = NIL THEN RETURN FileNotOpen END(*IF*);
	WITH f^ DO
	    IF guard <> GuardConst THEN RETURN FileNotOpen END(*IF*);

	    (* Find the desired block number. *)

	    NewBlock := FindRelativeCluster (locator,
				CARDINAL(position DIV LONGCARD(BufferSize)));

	    IF NewBlock <> BufferHeader[BufferNumber].BlockNumber THEN

		(* Flush the current buffer, and read a new cluster. *)

		dummy := ReleaseCurrentBuffer (f);
		status := StartRead (f, NewBlock);
		IF status <> OK THEN RETURN status END (*IF*);

	    END (*IF*);

	    (* Set the position within the buffer. *)

	    current := CARDINAL (position MOD LONGCARD(BufferSize));
	    ByteNumber := position;
	    AtEOF := position >= FileSize;
	    IF position > FileSize THEN
		FileSize := position;
		LengthChanged := TRUE;
	    END (*IF*);

	END (*WITH*);

	RETURN OK;

    END SetPosition;

(************************************************************************)

PROCEDURE SavePosition (f: File): LONGCARD;

    (* Returns the current byte number in file f.	*)

    BEGIN
	RETURN f^.ByteNumber;
    END SavePosition;

(************************************************************************)

PROCEDURE FileSize (f: File): LONGCARD;

    (* Returns the length of the file in bytes.	*)

    BEGIN
	RETURN f^.FileSize;
    END FileSize;

(************************************************************************)

BEGIN
    IF testing THEN
	CreateMaintenancePage (Mpage);
	OpenWindow (debug, yellow, blue, 0, 23, 0, 79,
					simpleframe, doubledivider);
	Associate (debug, Mpage);
	SetCursor (debug, 1, 21);
	WriteString (debug, "Diagnostic output from Files module");
	ChangeScrollingRegion (debug, 3, 22);
    END (*IF*);
END Files.
