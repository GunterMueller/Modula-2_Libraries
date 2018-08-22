IMPLEMENTATION MODULE Directories;

	(****************************************************************)
	(*								*)
	(*			Disk directory lookup.			*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	22 March 1995				*)
	(*  Status:		Working.				*)
	(*								*)
	(*	Method of setting default directories is not very	*)
	(*	satisfactory, since it relies on assumptions about	*)
	(*	which drives are present.  Would be better for the	*)
	(*	device drivers to work out the default directories	*)
	(*	and add an extra parameter to DeviceName.		*)
	(*								*)
	(*	Now working with large partitions.			*)
	(*								*)
	(*	FindFreeCluster seems to be a bit slow.			*)
	(*	FindRelativeCluster not yet tested.			*)
	(*	Improvements to the cache system desirable.		*)
	(*	Need procedures for deleting and renaming files.	*)
	(*								*)
	(****************************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE, ADDRESS,
    (* proc *)	ADR;

FROM FileNames IMPORT
    (* type *)	FileName, NameList,
    (* proc *)	Parse, WriteFileName, DiscardNameList;

FROM Devices IMPORT
    (* type *)	RequestBlock,
    (* proc *)	SameDevice, NullDevice, VolumeSize, BlockRead, BlockWrite,
		GetDefaultDirectory;

FROM IOErrorCodes IMPORT
    (* proc *)	TranslateErrorCode;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM DMA IMPORT
    (* proc *)	AllocateDMABuffer;

FROM MaintenancePages IMPORT	(* for testing *)
    (* type *)	MaintenancePage,
    (* proc *)	CreateMaintenancePage, Associate;

FROM Windows IMPORT		(* for testing *)
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, ChangeScrollingRegion, SetCursor,
		WriteString, WriteChar, WriteLn, PressAnyKey;

FROM NumericIO IMPORT		(* for testing *)
    (* proc *)	WriteHexByte, WriteHexWord, WriteHexLongword,
		WriteAddress;

FROM LowLevel IMPORT
    (* proc *)	AddOffset, Copy, BlockFill, Far,
		LS, RS, IAND, IANDB, IOR;

FROM MiscPMOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)	BIOS;

FROM TimeOfDay IMPORT
    (* type *)	TimeRecord,
    (* proc *)	ReadClock;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

(************************************************************************)

CONST testing = TRUE;  Prompting = FALSE;

(************************************************************************)

CONST
    (* In this version, all devices are assumed to have the same block	*)
    (* size.								*)

    BlockSize = 512;

    (* In a directory, a deleted file is indicated by a special code	*)
    (* in the first character position of the file name field.		*)

    DeletedFileCode = CHR(0E5H);

    (* In the File Allocation Table, special codes are used to indicate	*)
    (* an unused cluster, and a cluster which is the end of a space	*)
    (* allocation chain.						*)

    FreeClusterCode = 0;
    EndOfChainCode = 0FFFFH;

(************************************************************************)
(*			    GLOBAL DATA TYPES				*)
(************************************************************************)

TYPE

    (* The following declaration gives the format of a directory entry	*)
    (* in the form it actually takes on the disk.  Note: for a deleted	*)
    (* file, the first byte is DeletedFileCode (=0E5H).			*)

    DirectoryEntry = RECORD
			Name: FileName;
			Attribute: BYTE;
			Reserved: ARRAY [0..9] OF BYTE;
			Time, Date: CARDINAL;
			StartingCluster: Cluster;
			FileSize: LONGCARD;
		     END (*RECORD*);

    (* The formats for date and time are:				*)
    (*	Time:	top 5 bits for hour, next 6 bits for minute, bottom	*)
    (*		5 bits for seconds (DIV 2, presumably).			*)
    (*	Date:	top 7 bits for year (0=1980), next 4 bits for month,	*)
    (*		bottom 5 bits for day of month.				*)

    (* We keep some of the recently accessed FAT and directory blocks	*)
    (* in a cache.  In the present version, we allow for a single	*)
    (* one-cluster cache per active device.  In a future version, this	*)
    (* should perhaps be expanded.  There would be a good case for	*)
    (* having separate caches for FAT and directory buffers.		*)

    CacheDetails =  RECORD
			modified: BOOLEAN;
			block: BlockNumberType;
			address: ADDRESS;
		    END (*RECORD*);

    (* For each currently active device, we keep track of details such	*)
    (* as the location of the directory.  The "valid" field is TRUE if	*)
    (* we believe that the other fields have been filled in correctly.	*)
    (* The "next" field allows us to string all these records together	*)
    (* as a linear list.  The other fields are:				*)
    (*	PackedFAT		indicates that each FAT entry is a	*)
    (*				packed 12-bit number.  If this field is	*)
    (*				FALSE, a FAT entry takes 16 bits	*)
    (*	device, unit		the device to which this record refers	*)
    (*	ClusterNumberOrigin	the block number which would correspond	*)
    (*				to a cluster number of zero		*)
    (*	NumberOfBlocks		the total number of blocks on this unit	*)
    (*	BlocksPerCluster	sectors in a cluster			*)
    (*	ClusterSize		bytes in a cluster			*)
    (*	LowestFreeCluster	cluster number below which it is known	*)
    (*				that there are no unallocated clusters	*)
    (*	MaxClusterNumber	maximum possible cluster number		*)
    (*	FATstart		first FAT block number			*)
    (*	DirectoryStart		first directory block number		*)
    (*	DirectoryEnd		last directory block number		*)
    (*	DirectoryCluster	cluster # of current default directory	*)
    (*	CachePtr		pointer to the cache record		*)

    MediumInfoPointer = POINTER TO MediumInfo;
    MediumInfo =    RECORD
			valid, PackedFAT: BOOLEAN;
			next: MediumInfoPointer;
			device: Device;
			unit: CARDINAL;
			ClusterNumberOrigin: BlockNumberType;
			NumberOfBlocks: BlockNumberType;
			BlocksPerCluster: CARDINAL;
			ClusterSize: CARDINAL;	(* bytes *)
			LowestFreeCluster, MaxClusterNumber: CARDINAL;
			FATstart, DirectoryStart,
				DirectoryEnd: BlockNumberType;
			DirectoryCluster: Cluster;
			CachePtr: POINTER TO CacheDetails;
		    END (*RECORD*);

    (* A Handle contains the information found by procedure Lookup	*)
    (* concerning the directory entry for a file.			*)

    Handle = POINTER TO
		    RECORD
			M: MediumInfoPointer;
			dirblock: BlockNumberType;
			diroffset: CARDINAL;
		    END (*RECORD*);

(************************************************************************)
(*			     GLOBAL VARIABLES				*)
(************************************************************************)

VAR
    Mpage: MaintenancePage;
    debug: Window;		(* used only for testing *)

    (* Head of the list of device information records.	*)

    MediumInfoHead: MediumInfoPointer;

    (* Default device and unit number.	*)

    DefaultDevice: Device;
    DefaultUnit: CARDINAL;

(************************************************************************)
(*				CACHE I/O				*)
(************************************************************************)
(*									*)
(*  Note: in the current version, there is a single one-cluster cache	*)
(*  per device; it's mostly used to cache FAT blocks, but sometimes	*)
(*  used when reading or writing a directory entry.  For		*)
(*  future versions, we should consider (a) whether we should cache	*)
(*  multiple clusters per device; (b) whether to have separate caches	*)
(*  for FATs and directories, or let them share the same cache system;	*)
(*  (c) whether to reserve cache blocks for a given device (as at	*)
(*  present) or to have a single pool.					*)
(*									*)
(************************************************************************)

PROCEDURE SyncCache (M: MediumInfoPointer): ErrorCode;

    (* Ensures that any changes to the cache are written out to the	*)
    (* device.								*)

    VAR status: ErrorCode;

    BEGIN
	status := OK;
	WITH M^ DO
	    WITH CachePtr^ DO
		IF modified THEN
		    status := BlockWrite (device, unit, block, address,
								ClusterSize);
		    modified := FALSE;
		END (*IF*);
	    END (*WITH*);
	END (*WITH*);
	RETURN status;
    END SyncCache;

(************************************************************************)

PROCEDURE DumpCache (CacheAddress: ADDRESS);

    (* For debugging: prints out first few bytes of cache.	*)

    VAR p: POINTER TO BYTE;
	j: CARDINAL;

    BEGIN
	p := CacheAddress;
	WriteLn (debug);
	FOR j := 0 TO 3FH DO
	    IF j MOD 16 = 0 THEN
		WriteLn (debug);
	    END (*IF*);
	    WriteChar (debug, ' ');  WriteHexByte (debug, p^);
	    p := AddOffset (p, 1);
	END (*FOR*);
	WriteLn (debug);
    END DumpCache;

(************************************************************************)

PROCEDURE LoadCache (M: MediumInfoPointer;  BlockNumber: BlockNumberType;
			VAR (*OUT*) CacheAddress: ADDRESS): ErrorCode;

    (* Ensures that a cluster starting with the specified block number	*)
    (* is in the in-memory cache, and makes CacheAddress point to the	*)
    (* cache data.							*)

    (* REMARK: BlockNumber must be on a cluster boundary; the rest of	*)
    (* this module ensures this property at present, but it should be	*)
    (* re-checked if the module is modified.				*)

    VAR status: ErrorCode;

    BEGIN
	status := OK;
	WITH M^ DO
	    WITH CachePtr^ DO
		IF address = NIL THEN
		    AllocateDMABuffer (address, ClusterSize);
		END (*IF*);
		IF block <> BlockNumber THEN
		    status := SyncCache (M);
		    IF status = OK THEN
			status := BlockRead (device, unit, BlockNumber,
						address, ClusterSize);
			block := BlockNumber;
		    END (*IF*);
		END (*IF*);
		CacheAddress := address;
	    END (*WITH*);
	END (*WITH*);

	RETURN status;
    END LoadCache;

(************************************************************************)

PROCEDURE CachedRead (M: MediumInfoPointer;  block: BlockNumberType;
			offset, bytecount: CARDINAL;
			VAR (*OUT*) result: ARRAY OF BYTE): ErrorCode;

    (* Reads bytecount bytes, starting at byte number offset from the	*)
    (* start of the given block on the device specified by M, into the	*)
    (* result array.  The function result is an error code.  A cluster	*)
    (* starting at the given block number is loaded into the cache if	*)
    (* it isn't already there.  It is permissible to cross block and	*)
    (* cluster boundaries, but parameter "block" must always be on a	*)
    (* cluster boundary.						*)

    VAR b1: CARDINAL;  place, CacheAddress: ADDRESS;
	status: ErrorCode;
	temp: POINTER TO CARDINAL;	(* for debugging *)

    BEGIN
	place := ADR (result);

	WITH M^ DO
	    WHILE offset >= ClusterSize DO
		block := block + LONGCARD(BlocksPerCluster);
		DEC (offset, ClusterSize);
	    END (*WHILE*);
	END (*WITH*);

	LOOP

	    (* Read a cluster into the cache. *)

	    status := LoadCache (M, block, CacheAddress);
	    IF status <> OK THEN
		EXIT (*LOOP*);
	    END (*IF*);

	    (* Calculate how many bytes to move from this cluster.	*)

	    IF offset+bytecount > M^.ClusterSize THEN
		b1 := M^.ClusterSize - offset;
	    ELSE
		b1 := bytecount;
	    END (*IF*);

	    Copy (AddOffset(CacheAddress, offset), place, b1);

	    (* Have we finished the job?	*)

	    DEC (bytecount, b1);
	    IF bytecount = 0 THEN
		EXIT (*LOOP*);
	    END (*IF*);

	    (* Still more data to transfer.	*)

	    place := AddOffset (place, b1);
	    INC (block, LONGCARD(M^.BlocksPerCluster));  offset := 0;

	END (*LOOP*);

	RETURN status;

    END CachedRead;

(************************************************************************)

PROCEDURE CachedWrite (M: MediumInfoPointer;  block: BlockNumberType;
				offset, bytecount: CARDINAL;
				VAR (*IN*) value: ARRAY OF BYTE): ErrorCode;

    (* Writes a value of length bytecount bytes to the device specified	*)
    (* by M, starting at byte number offset from the start of the given	*)
    (* block number.  The function result is an error code.  The cache	*)
    (* is reloaded if necessary.  It is permissible to cross block and	*)
    (* cluster boundaries, but parameter "block" should always be on a	*)
    (* cluster boundary.						*)

    VAR b1: CARDINAL;  place, CacheAddress: ADDRESS;
	status: ErrorCode;

    BEGIN
	place := ADR(value);
	WHILE offset >= M^.ClusterSize DO
	    INC (block, LONGCARD(M^.BlocksPerCluster));
	    DEC (offset, M^.ClusterSize);
	END (*WHILE*);
	LOOP
	    (* Read a block into the cache. *)

	    status := LoadCache (M, block, CacheAddress);
	    IF status <> OK THEN
		EXIT (*LOOP*);
	    END (*IF*);

	    (* Calculate how many bytes to move into the current block.	*)

	    IF offset+bytecount > M^.ClusterSize THEN
		b1 := M^.ClusterSize - offset;
	    ELSE
		b1 := bytecount;
	    END (*IF*);
	    Copy (place, AddOffset (CacheAddress, offset), b1);

	    (* Note that we don't write the updated cache block to the	*)
	    (* device at this stage.  Instead, we simply note that the	*)
	    (* cache has been modified.  Any attempt to re-load the	*)
	    (* cache with a different block will trigger the writing	*)
	    (* out of the current cached block.				*)

	    M^.CachePtr^.modified := TRUE;

	    (* Have we finished the job?	*)

	    DEC (bytecount, b1);
	    IF bytecount = 0 THEN
		EXIT (*LOOP*);
	    END (*IF*);

	    (* Still more data to transfer.	*)

	    place := AddOffset (place, b1);
	    INC (block, LONGCARD(M^.BlocksPerCluster));  offset := 0;

	END (*LOOP*);
	RETURN status;
    END CachedWrite;

(************************************************************************)
(*		TRANSLATING BLOCK NUMBER TO CLUSTER NUMBER		*)
(************************************************************************)

PROCEDURE BlockToClusterMap (M: MediumInfoPointer;  block: BlockNumberType)
							: Cluster;

    (* Translates a block number to a cluster number.	*)

    BEGIN
	WITH M^ DO
	    IF block <= ClusterNumberOrigin THEN
		RETURN 0;
	    ELSE
		RETURN Cluster((block-ClusterNumberOrigin)
					DIV LONGCARD(BlocksPerCluster));
	    END (*IF*);
	END (*WITH*);
    END BlockToClusterMap;

(************************************************************************)
(*		    LOOKING AFTER THE MEDIA INFORMATION			*)
(************************************************************************)

PROCEDURE CheckMediumInfo (M: MediumInfoPointer): ErrorCode;

    (* Tries to ensure that the values in M^ are valid.  Returns OK if	*)
    (* already valid or if we are able to load valid values from the	*)
    (* device boot block.  In the latter case, we also set the initial	*)
    (* default directory.  Returns an error code if we fail in the	*)
    (* attempt to read the boot block.					*)

    VAR status: ErrorCode;
	DirectoryName: ARRAY [0..63] OF CHAR;
	BootInfoPtr: POINTER TO RECORD
			JMPinstruction: ARRAY [0..2] OF BYTE;
			SystemID: ARRAY [0..7] OF CHAR;
			BytesPerSector: CARDINAL;
			SectorsPerCluster: BYTE;
			ReservedSectors: CARDINAL;
			CopiesOfFAT: BYTE;
			DirectoryEntries: CARDINAL;
			NumberOfSectors: CARDINAL; (* 0 for large partition *)
			FormatID: BYTE;
			SectorsPerFAT: CARDINAL;
			SectorsPerTrack: CARDINAL;
			NumberOfHeads: CARDINAL;
			SpecialReservedSectors: CARDINAL;
		     END (*RECORD*);

	(* This defines the layout of data in the boot record on the	*)
	(* device.  We use most but not all of this information.	*)

    BEGIN
	IF M^.valid THEN
	    IF testing THEN
		WriteLn (debug);
		WriteString (debug, "CheckMediumInfo: M^.valid is TRUE");
	    END (*IF*);
	    RETURN OK;
	END (*IF*);

	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "CheckMediumInfo: about to call AllocateDMABuffer");
	END (*IF*);
	AllocateDMABuffer (BootInfoPtr, BlockSize);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "CheckMediumInfo: return from AllocateDMABuffer");
	END (*IF*);
	WITH M^ DO
	    status := BlockRead (device, unit, 0,
					BootInfoPtr, BlockSize);
	    IF status <> OK THEN
		IF testing THEN
		    WriteLn (debug);
		    WriteString (debug, "Can't read boot record.");
		END (*IF*);
		DEALLOCATE (BootInfoPtr, BlockSize);
		RETURN status;
	    END (*IF*);
	    IF testing THEN
		WriteLn (debug);
		WriteString (debug, "Have successfully read boot record.");
	    END (*IF*);
	    WITH BootInfoPtr^ DO
		BlocksPerCluster := ORD(SectorsPerCluster);
		ClusterSize := BytesPerSector*BlocksPerCluster;
		FATstart := BlockNumberType(ReservedSectors);
		DirectoryStart := FATstart
			+ BlockNumberType(ORD(CopiesOfFAT)*SectorsPerFAT);
		DirectoryEnd := DirectoryStart
			+ BlockNumberType (DirectoryEntries DIV 16 - 1);
		ClusterNumberOrigin := DirectoryEnd
			- BlockNumberType (2*BlocksPerCluster) + 1;

		(* Note that the "NumberOfSectors" field is a CARDINAL,	*)
		(* which is insufficient for a large partition.  A	*)
		(* large partition is flagged by having NumberOfSectors	*)
		(* equal to 0, and in this case the true number of	*)
		(* sectors has been supplied to module Devices by the	*)
		(* device driver.					*)

		IF NumberOfSectors = 0 THEN
		    NumberOfBlocks := VolumeSize (device, unit);
		ELSE
		    NumberOfBlocks := BlockNumberType(NumberOfSectors);
		END (*IF*);

		MaxClusterNumber := BlockToClusterMap (M, NumberOfBlocks-1);
		PackedFAT := MaxClusterNumber < 0FF8H;
		valid := TRUE;
	    END (*WITH*);

	    DEALLOCATE (BootInfoPtr, BlockSize);

	    IF testing THEN
		WriteLn (debug);
		WriteString (debug, "ClusterNumberOrigin, BlocksPerCluster = ");
		WriteHexLongword (debug, ClusterNumberOrigin);
		WriteString (debug, "     ");
		WriteHexWord (debug, BlocksPerCluster);
		WriteString (debug, ".  FAT is ");
		IF NOT PackedFAT THEN WriteString (debug, "not "); END (*IF*);
		WriteString (debug, "packed.");
	    END (*IF*);

	    (* Set up the initial default directory.  Note: this causes	*)
	    (* a recursive call, since SetDefaultDirectory calls	*)
	    (* CheckMediumInfo, so it's important that we do this only	*)
	    (* after filling in all the MediumInfo fields.		*)

	    DirectoryCluster := 0;

	    GetDefaultDirectory (device, unit, DirectoryName);
	    IF testing THEN
		WriteLn (debug);
		WriteString (debug, "The directory string is ");
		WriteString (debug, DirectoryName);
		IF Prompting THEN PressAnyKey (debug) END(*IF*);
	    END (*IF*);

	    status := SetDefaultDirectory (DirectoryName);

	END (*WITH*);

	RETURN OK;

    END CheckMediumInfo;

(************************************************************************)

PROCEDURE DumpMediumInfoList;

    (* For testing: dumps the list whose head is MediumInfoHead.	*)

    VAR current: MediumInfoPointer;

    BEGIN
	current := MediumInfoHead;
	WriteLn (debug);
	WriteString (debug, "MediumInfo list: ");
	IF current = NIL THEN
	    WriteString (debug, "is empty");
	ELSE
	    WHILE current <> NIL DO
		WriteLn (debug);
		WITH current^ DO
		    IF valid THEN WriteString (debug, "T ")
		    ELSE WriteString (debug, "F ");
		    END (*IF*);
		    WriteHexWord (debug, unit);  WriteString (debug, "  ");
		    WriteHexLongword (debug, ClusterNumberOrigin);
	 	END (*WITH*);
		current := current^.next;
	    END (*WHILE*);
	END (*IF*);
    END DumpMediumInfoList;

(************************************************************************)

PROCEDURE FindMediumInfo (devicecode: Device;  unitnumber: CARDINAL)
							: MediumInfoPointer;

    (* Returns a pointer to the MediumInfo record for the given device	*)
    (* and unit.  (A new MediumInfo record is created if necessary.)	*)

    VAR current, previous: MediumInfoPointer;

    BEGIN
	current := MediumInfoHead;  previous := NIL;
	WHILE current <> NIL DO
	    IF SameDevice (current^.device, devicecode)
			AND (current^.unit = unitnumber) THEN
		RETURN current;
	    END (*IF*);
	    previous := current;  current := current^.next;
	END (*WHILE*);

	(* If we get this far, there is no existing MediumInfo record	*)
	(* for this device and unit, so we have to create one.		*)

	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "FindMediumInfo: no match found.");
	END (*IF*);
	NEW (current);
	WITH current^ DO
	    valid := FALSE;  next := NIL;
	    device := devicecode;  unit := unitnumber;
	    LowestFreeCluster := 2;
	    NEW (CachePtr);
	    WITH CachePtr^ DO
		modified := FALSE;
		block := 0;
		address := NIL;
	    END;
	END (*WITH*);
	IF previous = NIL THEN
	    MediumInfoHead := current;
	ELSE
	    previous^.next := current;
	END (*IF*);
	RETURN current;
    END FindMediumInfo;

(************************************************************************)
(*		LOOKING AFTER THE FILE ALLOCATION TABLE			*)
(************************************************************************)

PROCEDURE ClusterToBlockMap (M: MediumInfoPointer;
				clusternumber: Cluster): BlockNumberType;

    (* Given a cluster number, translates it to a block number.	*)

    VAR result: BlockNumberType;

    BEGIN
	WITH M^ DO
	    IF clusternumber > MaxClusterNumber THEN
		RETURN NoSuchBlock;
	    END (*IF*);
	    IF clusternumber = 0 THEN
		result := DirectoryStart;
	    ELSE
		result := ClusterNumberOrigin
			+ LONGCARD(clusternumber)*LONGCARD(BlocksPerCluster);
	    END (*IF*);
	END (*WITH*);
	RETURN result;
    END ClusterToBlockMap;

(************************************************************************)

PROCEDURE DecodeFAT (M: MediumInfoPointer;  entry: Cluster): Cluster;

    (* Returns entry number "entry" of the File Allocation Table (FAT).	*)
    (* For a packed FAT, each entry of the FAT is 12 bits long, so its	*)
    (* decoding is mildly messy; in some cases it is even possible for	*)
    (* an entry to lie partly at the end of one disk sector and partly	*)
    (* at the beginning of the next.					*)

    VAR FATblock: BlockNumberType;  result: Cluster;
	place: LONGCARD;  offset: CARDINAL;
	status: ErrorCode;

    BEGIN
	place := LONGCARD (entry);
	WITH M^ DO
	    IF PackedFAT THEN
		place := 3*place DIV 2;
	    ELSE
		place := 2*place;
	    END (*IF*);
	    FATblock := FATstart
		+ (place DIV LONGCARD(ClusterSize))*LONGCARD(BlocksPerCluster);
	    offset := CARDINAL(place MOD LONGCARD(ClusterSize));
	END (*WITH*);
	status := CachedRead (M, FATblock, offset, 2, result);
	IF status <> OK THEN
	    result := 0FFFFH;
	END (*IF*);
	IF M^.PackedFAT THEN
	    IF ODD(entry) THEN
		result := RS(result, 4);
	    END (*IF*);
	    result := IAND (result, 0FFFH);
	    IF result > 0FF7H THEN
		result := IOR (result, 0F000H);
	    END (*IF*);
	END (*IF*);
	RETURN result;
    END DecodeFAT;

(************************************************************************)

PROCEDURE WriteFAT (M: MediumInfoPointer;  entry, value: Cluster): ErrorCode;

    (* Writes the given value into the FAT.  As usual, the returned	*)
    (* result is an error code.						*)

    VAR FATblock: BlockNumberType;  place: LONGCARD;
	offset, result: CARDINAL;
	status: ErrorCode;

    BEGIN
	place := LONGCARD (entry);
	WITH M^ DO
	    IF PackedFAT THEN
		value := IAND (value, 0FFFH);
		place := 3*place DIV 2;
	    ELSE
		place := 2*place;
	    END (*IF*);

	    FATblock := FATstart
		+ LONGCARD(BlocksPerCluster)*(place DIV LONGCARD(ClusterSize));
	    offset := CARDINAL(place MOD LONGCARD(ClusterSize));
	END (*WITH*);

	IF M^.PackedFAT THEN

	    (* Read a 2-byte entry through the cache.	*)

	    status := CachedRead (M, FATblock, offset, 2, result);
	    IF status <> OK THEN
		RETURN status;
	    END (*IF*);

	    (* Now we must replace 12 bits of the result, leaving the	*)
	    (* other 4 bits intact.					*)

	    IF ODD(entry) THEN
		value := IOR (IAND (result, 0FH), LS(value, 4));
	    ELSE
		value := IOR (IAND (result, 0F000H), value);
	    END (*IF*);

	END (*IF*);

	(* Put the modified value back into the FAT.	*)

	RETURN CachedWrite (M, FATblock, offset, 2, value);

    END WriteFAT;

(************************************************************************)

PROCEDURE NextBlockNumber (fileid: Handle;
			   currentblock: BlockNumberType): BlockNumberType;

    (* Given the block number of the current cluster in a file, returns	*)
    (* the block number of the following cluster.			*)

    VAR M: MediumInfoPointer;
	cluster: Cluster;

    BEGIN
	M := fileid^.M;

	(* Special case: if we are down in the low block numbers, as	*)
	(* when reading the main directory, then we just step through	*)
	(* the clusters sequentially.					*)

	IF currentblock <= M^.DirectoryEnd THEN
	    currentblock := currentblock + BlockNumberType(M^.BlocksPerCluster);
	    IF currentblock > M^.DirectoryEnd THEN
		RETURN NoSuchBlock;
	    END (*IF*);
	    RETURN currentblock;
	END (*IF*);

	(* Normal case: we have to translate the block number into a	*)
	(* cluster number, and then look up the FAT to get the next	*)
	(* cluster number.						*)

	WITH M^ DO
	    cluster := BlockToClusterMap (M, currentblock);
	END (*WITH*);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Current cluster is ");
	    WriteHexWord (debug, cluster);
	END (*IF*);
	cluster := DecodeFAT (M, cluster);
	IF testing THEN
	    WriteString (debug, ", next cluster is ");
	    WriteHexWord (debug, cluster);
	END (*IF*);
	RETURN ClusterToBlockMap (M, cluster);
    END NextBlockNumber;

(************************************************************************)

PROCEDURE DeallocateCluster (M: MediumInfoPointer;
					cluster: Cluster): ErrorCode;

    (* Marks the given cluster as being no longer in use.	*)

    BEGIN
	IF cluster < M^.LowestFreeCluster THEN
	    M^.LowestFreeCluster := cluster;
	END (*IF*);
	RETURN WriteFAT (M, cluster, FreeClusterCode);
    END DeallocateCluster;

(************************************************************************)

PROCEDURE FindFreeCluster (M: MediumInfoPointer;
			VAR (*OUT*) ClusterNumber: Cluster): ErrorCode;

    (* Searches the FAT for a free cluster, returns its cluster number	*)
    (* and allocates the cluster by marking it as the end of a space	*)
    (* allocation chain.  The returned value is an error code.		*)

    VAR result: Cluster;

    BEGIN
	ClusterNumber := M^.LowestFreeCluster;
	LOOP
	    result := DecodeFAT (M, ClusterNumber);
	    IF result = FreeClusterCode THEN
		IF testing THEN
		    WriteLn (debug);
		    WriteString (debug, "FindFreeCluster: allocating cluster #");
		    WriteHexWord (debug, ClusterNumber);
		END (*IF*);
		M^.LowestFreeCluster := ClusterNumber + 1;
		RETURN WriteFAT (M, ClusterNumber, EndOfChainCode);
	    ELSIF ClusterNumber = M^.MaxClusterNumber THEN
		RETURN DeviceFull;
	    ELSE
		INC (ClusterNumber);
	    END (*IF*);
	END (*LOOP*);
    END FindFreeCluster;

(************************************************************************)

PROCEDURE AllocateCluster (M: MediumInfoPointer;  cluster: Cluster;
				VAR (*OUT*) result: Cluster): ErrorCode;

    (* Allocates a new free cluster, returns its cluster number as	*)
    (* result.  Parameter cluster shows the existing last cluster on	*)
    (* this space allocation chain.					*)

    VAR status: ErrorCode;

    BEGIN
	status := FindFreeCluster (M, result);
	IF status = OK THEN
	    status := WriteFAT (M, cluster, result);
	END (*IF*);
	RETURN status;
    END AllocateCluster;

(************************************************************************)

PROCEDURE AllocateBlock (fileid: Handle;
			currentblock: BlockNumberType): BlockNumberType;

    (* Allocates a new free cluster, and returns its block number.  The	*)
    (* variable currentblock shows the last block used by this file -	*)
    (* we need this to update the space allocation chain.		*)

    VAR M: MediumInfoPointer;  NewClusterNumber: Cluster;

    BEGIN
	WITH fileid^ DO
	    WITH M^ DO
		IF AllocateCluster (M, BlockToClusterMap(M, currentblock),
			NewClusterNumber) <> OK THEN RETURN NoSuchBlock;
		END (*IF*);
	    END (*WITH*);
	    RETURN ClusterToBlockMap (M, NewClusterNumber);
	END (*WITH*);
    END AllocateBlock;

(************************************************************************)

PROCEDURE AllocateEmptyCluster (M: MediumInfoPointer;
			current: BlockNumberType;
			VAR (*OUT*) newblock: BlockNumberType): ErrorCode;

    (* Similar to AllocateBlock, but ensures that the new cluster is	*)
    (* filled with zeros.						*)

    VAR p: POINTER TO ARRAY[0..0] OF BYTE;
	newcluster: Cluster;
	status: ErrorCode;

    BEGIN
	newblock := NoSuchBlock;
	WITH M^ DO
	    status := AllocateCluster (M, BlockToClusterMap (M, current),
							newcluster);
	    IF status <> OK THEN RETURN status END(*IF*);
	    newblock := ClusterToBlockMap (M, newcluster);
	    AllocateDMABuffer (p, ClusterSize);
	    BlockFill (Far(p), ClusterSize, 0);
	    status := BlockWrite (device, unit, newblock, p, ClusterSize);
	    DEALLOCATE (p, ClusterSize);
	END (*WITH*);
	RETURN status;
    END AllocateEmptyCluster;

(************************************************************************)

PROCEDURE FindRelativeCluster (fileid: Handle;  N: CARDINAL)
							: BlockNumberType;

    (* Returns the block number of the Nth cluster, where N = 0		*)
    (* corresponds to the starting cluster of the file.			*)

    CONST ClusterFieldOffset
		= SIZE(DirectoryEntry) - SIZE(LONGCARD) - SIZE(CARDINAL);

    VAR status: ErrorCode;
	oldcluster, cluster: Cluster;

    BEGIN

	WITH fileid^ DO

	    (* Find the starting cluster by looking up the directory.	*)
	    (* We could make this more efficient by storing the		*)
	    (* starting cluster in fileid^, but so far there has been	*)
	    (* no great motivation for making this operation efficient;	*)
	    (* this decision could be reviewed if we wanted to support	*)
	    (* faster random access.					*)

	    IF CachedRead (M, dirblock, diroffset + ClusterFieldOffset,
				2, cluster) <> OK THEN RETURN NoSuchBlock;
	    END (*IF*);

	    (* Now follow the space allocation chain - which is not as	*)
	    (* slow as it looks because of the high probability that	*)
	    (* entire chain will stay in the cache.			*)

	    WHILE N > 0 DO
		oldcluster := cluster;
		cluster := DecodeFAT (M, oldcluster);
		IF cluster = EndOfChainCode THEN
		    IF AllocateCluster (M, oldcluster, cluster) <> OK THEN
			RETURN NoSuchBlock;
		    END (*IF*);
		END (*IF*);
		DEC(N);
	    END (*WHILE*);

	    RETURN ClusterToBlockMap (M, cluster);

	END (*WITH*);

    END FindRelativeCluster;

(************************************************************************)
(*			SEARCHING A DIRECTORY				*)
(************************************************************************)

PROCEDURE NameMatch (name: FileName;  details: DirectoryEntry): BOOLEAN;

    (* Returns TRUE iff "name" matches the file name in "details".	*)

    BEGIN
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Comparing name with ");
	    WriteFileName (debug, details.Name);
	END (*IF*);

	(* Special case: if name.fname[0] = CHR(0) then we consider	*)
	(* any "deleted file name" to be a match.			*)

	IF name.fname[0] = CHR(0) THEN
	    RETURN (details.Name.fname[0] = CHR(0))
			OR (details.Name.fname[0] = DeletedFileCode);
	END (*IF*);

	(* Normal case: require a complete match.	*)

	RETURN (name.fname = details.Name.fname)
			AND (name.fext = details.Name.fext);
    END NameMatch;

(************************************************************************)

PROCEDURE SearchBuffer (VAR (*IN*) Array: ARRAY OF DirectoryEntry;
			entries: CARDINAL;
			name: FileName;
			VAR (*OUT*) details: DirectoryEntry;
			VAR (*OUT*) offset: CARDINAL): ErrorCode;

    (* Searches an array of "entries" directory entries for the		*)
    (* specified file name.  If the search succeeds, the value returned	*)
    (* is OK, the directory entry is returned in details, and offset	*)
    (* gives the offset within the array.  If the search fails, the	*)
    (* details record is left unmodified.				*)
    (* Special case 1: if name.fname[0] = CHR(0) then we are searching	*)
    (* for an unused directory entry.  In this case, a successful	*)
    (* search returns the location of an unused entry in offset.	*)
    (* Special case 2: a return with result NameNotFound and offset=0	*)
    (* means that we came across a directory entry whose filename	*)
    (* started with CHR(0) (and special case 1 did not apply, i.e. we	*)
    (* weren't wanting an unused entry).  This implies that there is no	*)
    (* point in searching the rest of the directory, since this case	*)
    (* marks the first directory slot which has never been used.	*)

    VAR entry: CARDINAL;
	status: ErrorCode;

    BEGIN
	(*# save, check(index=>off) *)
	entry := 0;  offset := 0;
	LOOP
	    IF NameMatch (name, Array[entry]) THEN
		details := Array[entry];  status := OK;
		EXIT (*LOOP*);
	    END (*IF*);
	    IF Array[entry].Name.fname[0] = CHR(0) THEN
		offset := 0;  status := NameNotFound;  EXIT (*LOOP*);
	    END (*IF*);
	    INC (entry);  INC (offset, SIZE(DirectoryEntry));
	    IF entry >= entries THEN
		status := NameNotFound;  EXIT (*LOOP*);
	    END (*IF*);
	END (*LOOP*);
	(*# restore *)
	RETURN status;
    END SearchBuffer;

(************************************************************************)

PROCEDURE SearchCluster (name: FileName;
			VAR (*INOUT*) fileid: Handle;
			VAR (*OUT*) details: DirectoryEntry): ErrorCode;

    (* Searches the disk cluster starting at block fileid^.dirblock for	*)
    (* the specified file name.  If the search succeeds, the value	*)
    (* returned is OK, the directory entry is returned in details, and	*)
    (* fileid^.diroffset gives the byte offset from the start of the	*)
    (* cluster.  If the search fails, the details record is unaltered.	*)
    (* Special case 1: if name.fname[0] = CHR(0) then we are searching	*)
    (* for an unused directory entry.  In this case, a successful	*)
    (* search returns the location of an unused entry in offset.	*)
    (* Special case 2: a return with result NameNotFound and offset=0	*)
    (* means that we came across a directory entry whose filename	*)
    (* started with CHR(0) (and special case 1 did not apply, i.e. we	*)
    (* weren't wanting an unused entry).  This implies that there is no	*)
    (* point in searching the rest of the directory, since this case	*)
    (* marks the first directory slot which has never been used.	*)

    TYPE DirectoryBlock = ARRAY [0..0] OF DirectoryEntry;

    VAR BufferPointer: POINTER TO DirectoryBlock;
	BufferSize: CARDINAL;
	status: ErrorCode;

    BEGIN
	WITH fileid^ DO
	    BufferSize := M^.ClusterSize;
	    AllocateDMABuffer (BufferPointer, BufferSize);
	    status := BlockRead (M^.device, M^.unit, dirblock,
				BufferPointer, BufferSize);
	END (*WITH*);
	IF status = OK THEN
	    status := SearchBuffer (BufferPointer^,
					BufferSize DIV SIZE(DirectoryEntry),
					name, details, fileid^.diroffset);
	END (*IF*);
	DEALLOCATE (BufferPointer, BufferSize);
	RETURN status;
    END SearchCluster;

(************************************************************************)

PROCEDURE SearchDirectory (name: FileName;
			VAR (*INOUT*) fileid: Handle;
			VAR (*OUT*) details: DirectoryEntry): ErrorCode;

    (* Searches a directory for file "name".  On entry, fileid^ shows	*)
    (* the starting block of the directory.  (The fileid^.diroffset	*)
    (* component is ignored.)  On successful return, details holds the	*)
    (* directory entry for the file being searched for, and fileid^	*)
    (* holds the block number and byte offset within the cluster of	*)
    (* that directory entry.  If the search fails, fileid^.dirblock is	*)
    (* restored to its original value, and the contents of details are	*)
    (* meaningless.  Special case: if name.fname[0] is CHR(0) then we	*)
    (* are searching for an unused directory entry.  In this case, a	*)
    (* successful search returns the location of an unused entry in	*)
    (* fileid^.  (If necessary, we expand the directory to create the	*)
    (* unused entry.)							*)

    VAR status: ErrorCode;  start, previousblock: BlockNumberType;
	SearchingRootDirectory: BOOLEAN;

    BEGIN
	WITH fileid^ DO
	    start := dirblock;
	    SearchingRootDirectory := dirblock <= M^.DirectoryEnd;
	END (*WITH*);

	LOOP
	    status := SearchCluster (name, fileid, details);
	    IF status <> NameNotFound THEN
		EXIT (*LOOP*);
	    END (*IF*);

	    (* No luck in the current cluster.  Is there any point in	*)
	    (* searching further?					*)

	    IF fileid^.diroffset = 0 THEN
		EXIT (*LOOP*);
	    END (*IF*);

	    (* Move to next cluster in this directory. *)

	    previousblock := fileid^.dirblock;
	    fileid^.dirblock := NextBlockNumber (fileid, previousblock);

	    IF fileid^.dirblock = NoSuchBlock THEN

		(* Have reached the end of the directory.  If searching	*)
		(* for an empty slot, and this is not the root		*)
		(* directory, extend the directory.			*)

		IF name.fname[0] = CHR(0) THEN
		    IF SearchingRootDirectory THEN
			status := DirectoryFull;
		    ELSE
			fileid^.diroffset := 0;
			status := AllocateEmptyCluster (fileid^.M,
					previousblock, fileid^.dirblock);
		    END (*IF*);
		END (*IF*);

		EXIT (*LOOP*);

	    END (*IF*);

	END (*LOOP*);

	IF status <> OK THEN
	    fileid^.dirblock := start;
	END (*IF*);
	RETURN status;

    END SearchDirectory;

(************************************************************************)
(*		    CREATING A NEW DIRECTORY ENTRY			*)
(************************************************************************)

PROCEDURE SetTimeAndDate (VAR (*OUT*) time, date: CARDINAL);

    (* Sets the two output parameters to the current time and date.	*)
    (* For formats, see the comments near the beginning of this module.	*)

    VAR CurrentTime: TimeRecord;

    (********************************************************************)

    PROCEDURE bin (val: SHORTCARD): CARDINAL;

	(* Converts a two-digit BCD value to binary.	*)

	BEGIN
	    RETURN CARDINAL(10*(val DIV 16) + (val MOD 16));
	END bin;

    (********************************************************************)

    BEGIN
	ReadClock (CurrentTime);
	WITH CurrentTime DO
	    IF year < 128 THEN year := 128 END(*IF*);
	    time := ORD(LS(bin(hours),11)) + ORD(LS(bin(minutes),5))
						+ (bin(seconds) DIV 2);
	    date := ORD(LS(bin(year)-80,9)) + ORD(LS(bin(month),5))
						+ bin(dayofmonth);
	END (*WITH*);
    END SetTimeAndDate;

(************************************************************************)

PROCEDURE CreateDirectoryEntry (name: FileName;
				VAR (*INOUT*) fileid: Handle;
				FirstCluster: Cluster): ErrorCode;

    (* Creates a new directory entry.  On entry, fileid^ identifies	*)
    (* the starting block of the directory (the diroffset component is	*)
    (* ignored).  On return, fileid^ gives the location of the newly	*)
    (* created entry.  A starting cluster has already been allocated	*)
    (* for the new file, and parameter FirstCluster gives its number.	*)

    VAR tempname: FileName;  status: ErrorCode;
	details: DirectoryEntry;  j: [0..9];

    BEGIN

	(* Search for an unused directory entry.  In this phase, the	*)
	(* details record is a dummy; we are interested only in the	*)
	(* status and updated fileid^.					*)

	tempname.fname[0] := CHR(0);
	status := SearchDirectory (tempname, fileid, details);
	IF status <> OK THEN
	    RETURN status;
	END (*IF*);

	WITH details DO

	    (* Fill in the details of the desired new entry.	*)

	    Name := name;  Attribute := 20H;
	    FOR j := 0 TO 9 DO
		Reserved[j] := 0;
	    END (*FOR*);
	    SetTimeAndDate (Time, Date);
	    StartingCluster := FirstCluster;  FileSize := 0;

	END (*WITH*);

	(* Write the new entry out to the device.	*)

	WITH fileid^ DO
	    RETURN CachedWrite (M, dirblock, diroffset,
					SIZE(DirectoryEntry), details);
	END (*WITH*);

    END CreateDirectoryEntry;

(************************************************************************)
(*			   CREATING A NEW FILE				*)
(************************************************************************)

PROCEDURE CreateNewFile (name: FileName;
			VAR (*INOUT*) fileid: Handle;
			VAR (*OUT*) BlockNumber: BlockNumberType): ErrorCode;

    (* Creates a new file with its starting block pre-allocated.  On	*)
    (* entry fileid^.M identifies the device, name is the file name,	*)
    (* and fileid^.dirblock is the starting block of the directory.	*)
    (* On exit, if the file has been successfully created, fileid^	*)
    (* points to the newly created directory entry and BlockNumber	*)
    (* gives the starting block of the new file.			*)

    VAR status, dummy: ErrorCode;
	M: MediumInfoPointer;
	DataCluster: Cluster;
	string: ARRAY [0..31] OF CHAR;

    BEGIN
	M := fileid^.M;  DataCluster := EndOfChainCode;

	(* Pre-allocate the first cluster of the new file.	*)

	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "About to call FindFreeCluster.");
	    IF Prompting THEN PressAnyKey (debug) END(*IF*);
	END (*IF*);
	status := FindFreeCluster (M, DataCluster);

	IF status = OK THEN
	    status := CreateDirectoryEntry (name, fileid, DataCluster);
	    IF status <> OK THEN
		dummy := DeallocateCluster (M, DataCluster);
	    END (*IF*);
	    IF testing THEN
		WriteLn (debug);
		WriteString (debug, "Created new directory entry, status = ");
		TranslateErrorCode (status, string);
		WriteString (debug, string);
		IF Prompting THEN PressAnyKey (debug) END(*IF*);
	    END (*IF*);
	END (*IF*);

	BlockNumber := ClusterToBlockMap (M, DataCluster);
	RETURN status;

    END CreateNewFile;

(************************************************************************)
(*			SEARCHING A DIRECTORY PATH			*)
(************************************************************************)

PROCEDURE SearchPath (	VAR (*INOUT*) path: NameList;
			VAR (*INOUT*) fileid: Handle;
			VAR (*OUT*) details: DirectoryEntry): ErrorCode;

    (* Searches for the file whose name (which might be a full path	*)
    (* name involving further subdirectories) is given in path.  On	*)
    (* entry, fileid^.dirblock shows which directory to search.  On	*)
    (* return with status = OK, details holds the directory entry for	*)
    (* the file being searched for, fileid^ gives the physical location	*)
    (* of its directory entry, and "path" has been fully discarded.  On	*)
    (* return with status <> OK, details and fileid^ reflect the result	*)
    (* of the partial search (i.e. details identifies the directory	*)
    (* found so far, and fileid^ points to the parent directory), and	*)
    (* "path" contains that part of the path which was not found.	*)

    VAR result: ErrorCode;
	current: NameList;

    BEGIN
	(* Set up a "details" record for the current directory, in	*)
	(* case the search fails at the first step.			*)

	WITH details DO
	    Attribute := 10H;
	    WITH fileid^ DO
		StartingCluster := BlockToClusterMap (M, dirblock);
	    END (*WITH*);
	END (*WITH*);

	IF path = NIL THEN RETURN NameNotFound END(*IF*);

	(* Each pass through the following loop steps one level down	*)
	(* in the directory tree.					*)

	LOOP
	    (* Search for "path" in the directory file.	    *)

	    result := SearchDirectory (path^.string, fileid, details);

	    (* If not found, return with path intact.	*)

	    IF result <> OK THEN EXIT (*LOOP*) END (*IF*);

	    (* If found, dispose of that part of the path already	*)
	    (* found, and move to the next name to find if any.		*)

	    current := path;
	    path := current^.child;
	    DISPOSE (current);
	    IF path = NIL THEN EXIT (*LOOP*) END (*IF*);

	    (* If there is still more of the path to search, what we	*)
	    (* have found so far should be a subdirectory.		*)

	    WITH details DO
		IF ORD(IANDB(Attribute, 10H)) = 0 THEN
		    RETURN NotADirectory;
		END (*IF*);
		WITH fileid^ DO
		    dirblock := ClusterToBlockMap (M, StartingCluster);
		END (*WITH*);
	    END (*WITH*);

	END (*LOOP*);

	(* The error code is a little more informative for the user if	*)
	(* we distinguish between two different kinds of "not found"	*)
	(* errors.							*)

	IF (result = NameNotFound) AND (path^.child <> NIL) THEN
	    result := DirectoryNotFound;
	END (*IF*);

	RETURN result;

    END SearchPath;

(************************************************************************)

PROCEDURE DirectoryLookup (newfile: BOOLEAN;  path: NameList;
		VAR (*INOUT*) fileid: Handle;
		VAR (*OUT*) BlockNumber: BlockNumberType;
		VAR (*OUT*) BytesInFile: LONGCARD)
							: ErrorCode;

    (* Returns the directory information for a file, whose name is in	*)
    (* path, on the device identified by fileid^.M, starting the search	*)
    (* at directory block fileid^.dirblock.  On successful return	*)
    (* fileid^ identifies the directory entry, and the remaining output	*)
    (* parameters give the file information.				*)
    (* Input parameter newfile is TRUE if the intention is to create a	*)
    (* new file.  The value returned is an error code (OK if no error).	*)

    VAR details: DirectoryEntry;
	M: MediumInfoPointer;  status: ErrorCode;

    BEGIN
	M := fileid^.M;

	status := SearchPath (path, fileid, details);

	(* What we do next depends on whether we are to create a new	*)
	(* file.  For a supposedly existing file, SearchPath should	*)
	(* have found the directory entry.  For a new file, SearchPath	*)
	(* should NOT have found an existing directory entry, and it	*)
	(* should return with the details record indicating the		*)
	(* directory rather than the file.				*)

	IF newfile THEN

	    BytesInFile := 0;
	    IF status = OK THEN
		status := DuplicateFileName;
	    ELSIF status = NameNotFound THEN
		IF ORD(IANDB(details.Attribute, 10H)) = 0 THEN
		    status := NotADirectory;
		ELSE
		    fileid^.dirblock := ClusterToBlockMap (
						M, details.StartingCluster);
		    status := CreateNewFile(path^.string, fileid, BlockNumber);
		END (*IF*);
	    END (*IF*);

	ELSIF status = OK THEN

	    (* We were looking for an existing file.  The information	*)
	    (* we want should by now be in the "details" record.	*)

	    WITH details DO
		BlockNumber := ClusterToBlockMap (M, StartingCluster);
		BytesInFile := FileSize;
	    END (*WITH*);

	ELSE
	    IF testing THEN
		WriteLn (debug);
		WriteString (debug, "DirectoryLookup: name not found.");
	    END (*IF*);
	END (*IF*);

	DiscardNameList (path);
	RETURN status;

    END DirectoryLookup;

(************************************************************************)
(*			SETTING THE DEFAULT DIRECTORY			*)
(************************************************************************)

PROCEDURE SetDefaultDirectory (VAR (*IN*) pathstring: ARRAY OF CHAR):ErrorCode;

    (* Sets the starting point for directory searches in subsequent	*)
    (* file operations.  Each device has a separate default directory.	*)
    (* If parameter path includes a device name, the default is set for	*)
    (* that device; otherwise, the default directory is set for the	*)
    (* current default device.						*)

    VAR device: Device;  unit: CARDINAL;
	pathlist: NameList;  fileid: Handle;
	MediumInfo: MediumInfoPointer;
	StartAtRoot: BOOLEAN;
	details: DirectoryEntry;
	status: ErrorCode;

    BEGIN
	(* Note that most of the following code duplicates what is in	*)
	(* procedure Lookup.  When I get time I should clean this up.	*)

	Parse (pathstring, device, unit, pathlist, StartAtRoot);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Device code is ");
	    WriteAddress (debug, ADDRESS(device));
	END (*IF*);

	(* Obtain a MediumInfo record for the device.  (This could	*)
	(* trigger a recursive call to SetDefaultDirectory, but that	*)
	(* should not be a problem for us.)				*)

	IF SameDevice (device, NullDevice()) THEN
	    device := DefaultDevice;  unit := DefaultUnit;
	END (*IF*);

	MediumInfo := FindMediumInfo (device, unit);
	status := CheckMediumInfo (MediumInfo);

	IF status <> OK THEN
	    RETURN status;
	END (*IF*);

	(* Create a directory locator for SearchPath to use.	*)

	NEW (fileid);
	WITH fileid^ DO
	    M := MediumInfo;
	    IF StartAtRoot THEN
		dirblock := M^.DirectoryStart;
	    ELSE
		dirblock := ClusterToBlockMap (M, M^.DirectoryCluster);
	    END (*IF*);
	    diroffset := 0;
	END (*WITH*);

	(* Find the directory entry.	*)

	status := SearchPath (pathlist, fileid, details);

	IF status = OK THEN

	    WITH details DO

		IF ORD(IANDB(Attribute, 10H)) = 0 THEN
		    status := NotADirectory;
		ELSE
		    MediumInfo^.DirectoryCluster := StartingCluster;
		END (*IF*);
	    END (*WITH*);

	END (*IF*);

	DiscardNameList (pathlist);
	RETURN status;

    END SetDefaultDirectory;

(************************************************************************)
(*	    DIRECTORY LOOKUP - THE EXTERNALLY CALLABLE VERSION		*)
(************************************************************************)

PROCEDURE Lookup (newfile: BOOLEAN;  name: ARRAY OF CHAR;
		VAR (*OUT*) device: Device;
		VAR (*OUT*) unit: CARDINAL;
		VAR (*OUT*) fileid: Handle;
		VAR (*OUT*) StartingBlock: BlockNumberType;
		VAR (*OUT*) BytesPerCluster: CARDINAL;
		VAR (*OUT*) BytesInFile: LONGCARD): ErrorCode;

    (* Parses the file name, returns the device code and unit number,	*)
    (* and looks up the device directory (which might involve some	*)
    (* subdirectory searches) to find the location of the directory	*)
    (* entry, the starting block number, cluster size in bytes, and	*)
    (* file size in bytes, for the file.  If newfile=TRUE, we create a	*)
    (* new directory entry for this file (or report an error if the	*)
    (* file already exists).  When creating a new file, we pre-allocate	*)
    (* the first block; this partially avoids the complication of	*)
    (* having to go back and modify the directory entry when the first	*)
    (* block of data is ready to be written (but we will still have to	*)
    (* modify the file size part of the entry when the file is closed).	*)

    VAR status: ErrorCode;
	path: NameList;
	MediumInfo: MediumInfoPointer;
	StartAtRoot: BOOLEAN;
	string: ARRAY [0..31] OF CHAR;

    BEGIN
	(* Decode the filename string.	*)

	Parse (name, device, unit, path, StartAtRoot);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Device code is ");
	    WriteAddress (debug, ADDRESS(device));
	END (*IF*);

	(* Obtain a MediumInfo record for the device.	*)

	IF SameDevice (device, NullDevice()) THEN
	    IF testing THEN
		WriteLn (debug);
		WriteString (debug, "Assuming the default device.");
	    END (*IF*);
	    device := DefaultDevice;  unit := DefaultUnit;
	END (*IF*);

	MediumInfo := FindMediumInfo (device, unit);
	status := CheckMediumInfo (MediumInfo);

	IF status <> OK THEN
	    IF testing THEN
		WriteLn (debug);
		WriteString (debug, "Can't check medium info, status is ");
		TranslateErrorCode (status, string);
		WriteString (debug, string);
		IF Prompting THEN PressAnyKey (debug) END(*IF*);
	    END (*IF*);
	    RETURN status;
	END (*IF*);

	BytesPerCluster := MediumInfo^.ClusterSize;

	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "CheckMediumInfo succeeded.");
	    WriteString (debug, "  Cluster size is ");
	    WriteHexWord (debug, BytesPerCluster);
	END (*IF*);

	(* Create a directory locator for this file.	*)

	NEW (fileid);
	WITH fileid^ DO
	    M := MediumInfo;
	    IF StartAtRoot THEN
		dirblock := M^.DirectoryStart;
	    ELSE
		dirblock := ClusterToBlockMap (M, M^.DirectoryCluster);
	    END (*IF*);
	    diroffset := 0;
	END (*WITH*);

	(* Find the directory entry.	*)

	status := DirectoryLookup (newfile, path, fileid,
						StartingBlock, BytesInFile);

	IF testing THEN
	    WriteLn (debug);
	    WriteHexWord (debug, BytesPerCluster);
	    WriteString (debug, " bytes per cluster, ");
	    WriteHexLongword (debug, BytesInFile);
	    WriteString (debug, " bytes in file.");
	    WriteLn (debug);
	    WriteString (debug, "The starting block is ");
	    WriteHexLongword (debug, StartingBlock);  WriteLn (debug);
	    WriteString (debug, "  and the status code is ");
	    TranslateErrorCode (status, string);
	    WriteString (debug, string);
	    IF Prompting THEN PressAnyKey (debug) END(*IF*);
	END (*IF*);

	RETURN status;

    END Lookup;

(************************************************************************)
(*		UPDATING THE FILE SIZE IN THE DIRECTORY			*)
(************************************************************************)

PROCEDURE UpdateFileSize (fileid: Handle;  NewSize: LONGCARD);

    (* Updates a directory entry to show a modified file size.	*)

    VAR entry: DirectoryEntry;
	status: ErrorCode;

    BEGIN

	WITH fileid^ DO
	    status := CachedRead (M, dirblock, diroffset,
					SIZE(DirectoryEntry), entry);
	    IF status = OK THEN
		entry.FileSize := NewSize;
		status := CachedWrite (M, dirblock, diroffset,
					SIZE(DirectoryEntry), entry);
	    END (*IF*);
	    status := SyncCache (M);
	END (*WITH*);

    END UpdateFileSize;

(************************************************************************)

PROCEDURE DiscardHandle (VAR (*INOUT*) fileid: Handle);

    (* To be called when a file is no longer going to be accessed.	*)

    BEGIN
	IF fileid <> NIL THEN
	    DISPOSE (fileid);
	END (*IF*);
    END DiscardHandle;

(************************************************************************)
(*				FINAL CLEANUP				*)
(************************************************************************)

PROCEDURE Cleanup;

    (* Releases the medium descriptors.  It would be nice to flush the	*)
    (* caches as well, but we can't do that without the risk of a	*)
    (* deadlock, because the device drivers might already have shut	*)
    (* themselves down by this stage.  This is not too disastrous: the	*)
    (* only cases where there would still be unflushed caches at this	*)
    (* stage would be where a file has not been closed, or when a	*)
    (* newly created file has length 0.					*)

    VAR current: MediumInfoPointer;

    BEGIN
	WHILE MediumInfoHead <> NIL DO
	    current := MediumInfoHead;
	    MediumInfoHead := current^.next;
	    DISPOSE (current^.CachePtr);
	    DISPOSE (current);
	END (*WHILE*);
    END Cleanup;

(************************************************************************)
(*			   MODULE INITIALISATION			*)
(************************************************************************)

PROCEDURE SetupDefaults ();

    (* Sets up the default device and unit	*)

    VAR dummy1: NameList;  dummy2: BOOLEAN;
	devname: ARRAY [0..2] OF CHAR;
	Registers: RegisterPacket;

    BEGIN

	(* Get the current default DOS device, and call this our	*)
	(* default device.						*)

	Registers.AH := 25;
	BIOS (33, Registers);
	devname[0] := CHR (ORD('A') + ORD(Registers.AL));
	devname[1] := ':';  devname[2] := CHR(0);

	Parse (devname, DefaultDevice, DefaultUnit, dummy1, dummy2);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Default device code is ");
	    WriteAddress (debug, ADDRESS(DefaultDevice));
	END (*IF*);

    END SetupDefaults;

(************************************************************************)

BEGIN
    IF testing THEN
	CreateMaintenancePage (Mpage);
	OpenWindow (debug, yellow, blue, 0, 23, 0, 79,
					simpleframe, doubledivider);
	Associate (debug, Mpage);
	SetCursor (debug, 1, 18);
	WriteString (debug, "Diagnostic output from Directories module");
	ChangeScrollingRegion (debug, 3, 22);
    END (*IF*);
    MediumInfoHead := NIL;
    SetTerminationProcedure (Cleanup);
    SetupDefaults ();
END Directories.
