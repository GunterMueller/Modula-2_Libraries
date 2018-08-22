IMPLEMENTATION MODULE HardDisk;

	(********************************************************)
	(*							*)
	(*		Device driver for hard disk		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	26 February 1995		*)
	(*  Status:		Working				*)
	(*	This module has been implicated in a number of	*)
	(*	mysterious bugs, but every time I've tried to	*)
	(*	track down problems they've turned out to be	*)
	(*	artefacts of VID or unfortunate interactions	*)
	(*	with SmartDrive.  Although it's hard to be	*)
	(*	completely sure that the module is bug-free,	*)
	(*	I'm reasonably certain that it is working as	*)
	(*	intended.					*)
	(*							*)
	(*	Note however that the safety of this module is	*)
	(*	suspect in an environment where the Windows	*)
	(*	SmartDrive program is running.			*)
	(*							*)
	(*	Note also that OS/2 will prevent the		*)
	(*	initialisation phase of this module from	*)
	(*	reading the partition table; this will cause	*)
	(*	the module to conclude that there are no	*)
	(*	logical drives, which of course means that	*)
	(*	the module does not do anything useful		*)
	(*	under OS/2.					*)
	(*							*)
	(********************************************************)

(************************************************************************)
(*									*)
(* The hardware interface is an array of 8 ports, starting at port 1F0H,*)
(* as follows:								*)
(*	0	read or write data					*)
(*	1	set precompensation cylinder, when written to		*)
(*		error register, when read				*)
(*	2	sector count						*)
(*	3	sector number						*)
(*	4	cylinder number (bottom 8 bits)				*)
(*	5	cylinder number (top 2 bits)				*)
(*	6	ECC/size/drive/head, stored as:				*)
(*			bits 3-0	head				*)
(*			bit 4		drive				*)
(*			bits 6, 5	sector size code: 00 for 256	*)
(*					bytes, 01 for 512, 10 for 1024,	*)
(*					11 for 128-byte sectors		*)
(*			bit 7		extend length to include ECC	*)
(*	7	command register, when written to			*)
(*		status register, when read				*)
(*									*)
(* The status register bits have the meaning:				*)
(*		bit 7	controller busy					*)
(*		bit 6	drive ready					*)
(*		bit 5	write fault					*)
(*		bit 4	seek complete					*)
(*		bit 3	data request					*)
(*		bit 2	data error corrected from ECC			*)
(*		bit 1	command in progress				*)
(*		bit 0	status error, see error register		*)
(*									*)
(* The bit definitions for the error register are shown in procedure	*)
(* CheckStatus.								*)
(*									*)
(* There is also a control byte at port 3F6H, for which we set bit 3	*)
(* to specify >8 heads; in addition setting bit 6 or bit 7 of this	*)
(* byte will disable retries.						*)
(*									*)
(* Interrupts are INT 76H, which is IRQ level 14, i.e. it is through	*)
(* the slave interrupt controller.					*)
(*									*)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE, ADDRESS;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode;

FROM LowLevel IMPORT
    (* proc *)	InByte, OutByte, InStringWord, OutStringWord,
		IANDB, IOR, LowByte, HighByte,
		MakePointer, AddOffset;

FROM MiscPMOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)	BIOS, EnterCriticalSection, LeaveCriticalSection;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Signal;

FROM Timer IMPORT
    (* proc *)	TimedWait;

FROM TaskControl IMPORT
    (* proc *)	CreateTask, WaitForInterrupt, CreateInterruptTask;

FROM Devices IMPORT
    (* type *)	BlockNumberType, Device, RequestBlock, RequestBlockPointer,
		OperationType,
    (* proc *)	InstallDeviceDriver, DeviceName, AcceptRequest;

FROM DMA IMPORT
    (* proc *)	CheckDMAAddress;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, WriteString, ChangeScrollingRegion;

FROM MaintenancePages IMPORT
    (* type *)	MaintenancePage,
    (* proc *)	CreateMaintenancePage, Associate;

(************************************************************************)

CONST
    testing = TRUE;		(* controls output to maintenance page *)
    DataPort = 01F0H;
    StatusPort = DataPort+7;
    DiskInterrupt = 76H;
    BlockSize = 512;		(* bytes per sector *)

TYPE
    (* The following declarations reflect the fact that a hard disk	*)
    (* may be partitioned into several logical drives, each of which	*)
    (* looks like a separate drive to the programmer.			*)

    PhysicalDriveNumber = [0..1];
    LogicalDriveNumber = [0..7];

    CommandArray = ARRAY [1..7] OF BYTE;

    (* Because we have to allow for a variety of disk types, there is a	*)
    (* fixed disk parameter table where each entry describes one	*)
    (* supported disk type.  Interrupt vector 41H (address 104H) is a	*)
    (* pointer to the table entry for drive 0, and vector 46H (address	*)
    (* 118H) points to the table entry for drive 1.  The table entry is	*)
    (* 16 bytes long:							*)
    (*	max # of cylinders	: CARDINAL;				*)
    (*	max # of heads		: BYTE;					*)
    (*	unused			: WORD;					*)
    (*	starting write precompensation cylinder: WORD;			*)
    (*	max ECC data burst length: BYTE;				*)
    (*	control byte		: BYTE;					*)
    (*					bit 7 disables retries -or-	*)
    (*					bit 6 disables retries		*)
    (*					bit 3 means more than 8 heads	*)
    (*	unused:			: ARRAY [1..3] OF BYTE;			*)
    (*	landing zone:		: WORD;					*)
    (*	sectors per track:	: BYTE;					*)
    (*	reserved		: BYTE					*)
    (* The relevant parts of this table are copied by the module	*)
    (* initialisation code into PhysicalDriveData.  Note that a lot of	*)
    (* this information is never used by this module.			*)

    DiskParameterTable = RECORD
			    maxcylinders: CARDINAL;
			    maxheads: BYTE;
			    dummy1: CARDINAL;		(* unused *)
			    precomp: CARDINAL;
			    maxECCburstlength: BYTE;
			    optionbyte: BYTE;
			    dummy2: ARRAY [1..3] OF BYTE; (* unused *)
			    landingzone: CARDINAL;
			    sectorspertrack: BYTE;
			    dummy3: BYTE;		(* unused *)
			 END (*RECORD*);

VAR
    log: Window;		(* for maintenance page output *)
    harddisk: Device;
    DataRequest: Semaphore;
    NumberOfPhysicalDrives: [0..MAX(PhysicalDriveNumber)+1];
    NumberOfLogicalDrives: [0..MAX(LogicalDriveNumber)+1];

    PhysicalDriveData: ARRAY PhysicalDriveNumber OF
			    RECORD
				SectorsPerTrack: CARDINAL;
				NumberOfHeads: CARDINAL;
				PrecompensationCylinder: CARDINAL;
				ExtraHeadOption: BYTE;
			    END (*RECORD*);

    LogicalUnit: ARRAY LogicalDriveNumber OF
			    RECORD
				physicaldrive: PhysicalDriveNumber;
				bootblock: BlockNumberType;
				numberofsectors: BlockNumberType;
			    END (*RECORD*);

(************************************************************************)
(*			HARDWARE STATUS CHECKS				*)
(************************************************************************)

PROCEDURE TestReady (VAR (*IN*) CommandBlock: CommandArray): ErrorCode;

    (* Checks that both the controller and the drive are ready to	*)
    (* accept a command.						*)

    CONST lots = 32767;

    VAR patience: CARDINAL;

    BEGIN

	(* First test the controller status byte, looping until the	*)
	(* busy bit is clear or until we run out of patience.		*)

	patience := lots;
	WHILE ORD(IANDB (InByte (StatusPort), 80H)) <> 0 DO
	    DEC (patience);
	    IF patience = 0 THEN
		RETURN ControllerNotListening;
	    END(*IF*);
	END (*WHILE*);

	(* Put out the ECC/size/drive/head code, to check whether the	*)
	(* drive is ready.						*)

	OutByte (DataPort+6, CommandBlock[6]);

	IF ORD(IANDB (InByte(StatusPort), 40H)) = 0 THEN
	    RETURN DriveNotReady;
	END (*IF*);

	RETURN OK;
    END TestReady;

(************************************************************************)

PROCEDURE CheckStatus (): ErrorCode;

    (* Returns the current status of the disk controller.	*)
    (* N.B. Reports OK if the controller is still busy.		*)

    VAR StatusByte: BYTE;

    BEGIN
	StatusByte := InByte (StatusPort);
	IF ORD(IANDB (StatusByte, 80H)) <> 0 THEN RETURN OK;
	ELSIF ORD(IANDB (StatusByte, 20H)) <> 0 THEN RETURN WriteFault;
	ELSIF ORD(IANDB (StatusByte, 40H)) = 0 THEN RETURN DriveNotReady;
	ELSIF ORD(IANDB (StatusByte, 10H)) = 0 THEN RETURN SeekFailure;

	(* If the low-order bit of the status register is set, we need	*)
	(* to look at the error register.				*)

	ELSIF ODD (StatusByte) THEN

	    StatusByte := InByte (DataPort+1);
	    IF ORD(IANDB(StatusByte,80H)) <> 0 THEN RETURN BadBlock
	    ELSIF ORD(IANDB(StatusByte,40H)) <> 0 THEN RETURN BadData
	    ELSIF ORD(IANDB(StatusByte,20H)) <> 0 THEN RETURN UndiagnosedFailure
	    ELSIF ORD(IANDB(StatusByte,10H)) <> 0 THEN RETURN SectorNotFound
	    ELSIF ORD(IANDB(StatusByte,08H)) <> 0 THEN RETURN UndiagnosedFailure
	    ELSIF ORD(IANDB(StatusByte,04H)) <> 0 THEN RETURN BadCommand
	    ELSIF ORD(IANDB(StatusByte,02H)) <> 0 THEN RETURN CalibrationFailure
	    ELSIF ORD(IANDB(StatusByte,01H)) <> 0 THEN RETURN BadData
	    ELSE RETURN OK;
	    END (*IF*);

	ELSE RETURN OK;
	END (*IF*);

    END CheckStatus;

(************************************************************************)
(*		  SENDING A COMMAND TO THE DISK CONTROLLER		*)
(************************************************************************)

PROCEDURE Command (VAR (*IN*) CommandBlock: CommandArray): ErrorCode;

    (* Outputs the command block.  *)

    VAR patience: CARDINAL;
	status: ErrorCode;
	j: [1..7];

    BEGIN
	patience := 1000;
	LOOP
	    status := TestReady (CommandBlock);
	    IF status = OK THEN EXIT (*LOOP*) END (*IF*);
	    IF status = ControllerNotListening THEN
		RETURN status;
	    END (*IF*);
	    DEC (patience);
	    IF patience = 0 THEN RETURN status; END (*IF*);
	END (*LOOP*);

	(* Send the command block to the device interface.	*)

	FOR j := 1 TO 7 DO
	    OutByte (DataPort+j, CommandBlock[j]);
	END (*FOR*);

	RETURN OK;

    END Command;

(************************************************************************)
(*			    THE INTERRUPT TASK				*)
(************************************************************************)

PROCEDURE DRQInterruptHandler;

    (* Performs a Signal(DataRequest) on each hardware interrupt.	*)

    BEGIN
	LOOP (*FOREVER*)
	    WaitForInterrupt;
	    Signal (DataRequest);
	END (*LOOP*);
    END DRQInterruptHandler;

(************************************************************************)
(*			CHECK FOR DISK READY				*)
(************************************************************************)

PROCEDURE WaitDRQ (VAR (*OUT*) TimedOut: BOOLEAN);

    (* Busy waits for a data request. *)

    VAR patience: CARDINAL;

    BEGIN
	patience := 100H;  TimedOut := FALSE;
	LOOP
	    IF ORD(IANDB (InByte(StatusPort), 8)) <> 0 THEN
		EXIT(*LOOP*);
	    END(*IF*);
	    DEC(patience);
	    IF patience = 0 THEN
		TimedOut := TRUE;
		EXIT (*LOOP*);
	    END (*IF*);
	END (*LOOP*);
    END WaitDRQ;

(************************************************************************)
(*		   THE ACTUAL READ AND WRITE OPERATIONS			*)
(************************************************************************)

PROCEDURE ReadOperation (VAR (*INOUT*) CommandBlock: CommandArray;
					BufferAddress: ADDRESS): ErrorCode;

    (* Data transfer from disk to DataBuffer, where CommandBlock	*)
    (* specifies the precise operation desired.				*)

    CONST largenumber = 1000;

    VAR status: ErrorCode;  TimedOut: BOOLEAN;
	sectors: SHORTCARD;

    BEGIN
	(* Check for 64K boundary error. *)

	IF NOT CheckDMAAddress (BufferAddress,
				BlockSize*VAL(CARDINAL,CommandBlock[2])) THEN
	    RETURN BadDMAAddress;
	END (*IF*);

	status := Command (CommandBlock);
	IF status <> OK THEN
	    RETURN status;
	END (*IF*);

	sectors := CommandBlock[2];
	REPEAT
	    TimedWait (DataRequest, largenumber, TimedOut);

	    IF TimedOut THEN
		RETURN TimeoutError;
	    END (*IF*);

	    (* Get the sector, via block input *)

	    InStringWord (DataPort, BufferAddress, 256 (*words*) );
	    BufferAddress := AddOffset (BufferAddress, BlockSize);

	    status := CheckStatus();
	    IF status <> OK THEN
		RETURN status;
	    END (*IF*);
	    DEC (sectors);

	UNTIL sectors = 0;
	RETURN OK;

    END ReadOperation;

(************************************************************************)

PROCEDURE WriteOperation (VAR (*INOUT*) CommandBlock: CommandArray;
					BufferAddress: ADDRESS): ErrorCode;

    (* Writes a block of data from BufferAddress^.  CommandBlock	*)
    (* specifies the disk block, amount of data to transfer, etc.	*)

    CONST largenumber = 1000;

    VAR status: ErrorCode;  TimedOut: BOOLEAN;

    BEGIN
	(* Check for 64K boundary error. *)

	IF NOT CheckDMAAddress (BufferAddress,
				BlockSize*VAL(CARDINAL,CommandBlock[2])) THEN
	    RETURN BadDMAAddress;
	END (*IF*);

	status := Command (CommandBlock);
	IF status <> OK THEN
	    RETURN status;
	END (*IF*);
	WaitDRQ (TimedOut);
	IF TimedOut THEN RETURN TimeoutError END (*IF*);

	REPEAT
	    (* Put the sector, via block output *)

	    OutStringWord (DataPort, BufferAddress, 256 (*words*) );
	    BufferAddress := AddOffset (BufferAddress, BlockSize);

	    TimedWait (DataRequest, largenumber, TimedOut);
	    IF TimedOut THEN RETURN TimeoutError END (*IF*);
	    IF CheckStatus() <> OK THEN RETURN status END (*IF*);

	    (* Remark: the termination test below is a check on the	*)
	    (* data request bit of the status register.  When the data	*)
	    (* request bit becomes 0, we know that there are no further	*)
	    (* sectors to transfer.					*)

	UNTIL ORD(IANDB(InByte (StatusPort), 8)) = 0;
	RETURN OK;

    END WriteOperation;

(************************************************************************)
(*			SETTING UP THE COMMAND BLOCK			*)
(************************************************************************)

PROCEDURE Decompose (BlockNumber: BlockNumberType;  drive: PhysicalDriveNumber;
			VAR (*OUT*) head, cylinder, sector: CARDINAL);

    (* Translates a block number into head/cylinder/sector coordinates.	*)
    (* The relationship is						*)
    (*	BlockNumber = SectorsPerTrack*(NumberOfHeads*cylinder+head)	*)
    (*						+ sector - 1		*)
    (* Note that sector number starts from 1, while the numbering	*)
    (* of blocks, heads, and cylinders all start from 0.		*)

    VAR H: CARDINAL;

    BEGIN
	WITH PhysicalDriveData[drive] DO
	    sector := CARDINAL(BlockNumber MOD LONGCARD(SectorsPerTrack)) + 1;
	    cylinder := CARDINAL(BlockNumber DIV LONGCARD(SectorsPerTrack));
	    H := NumberOfHeads;
	END (*WITH*);
	head := cylinder MOD H;
	cylinder := cylinder DIV H;
    END Decompose;

(************************************************************************)

PROCEDURE Encode (VAR (*OUT*) CommandBlock: CommandArray;  opcode: BYTE;
			drive: PhysicalDriveNumber;
			BlockNumber: BlockNumberType; count: CARDINAL);

    (* Given the drive number, block number, and byte count of a	*)
    (* disk operation, fills in the appropriate CommandBlock fields.	*)

    VAR sector, cylinder, head: CARDINAL;

    BEGIN

	(* Deal with the write precompensation cylinder, the "extra	*)
	(* head" option, and retry suppression.				*)

	WITH PhysicalDriveData[drive] DO
	    CommandBlock[1] := SHORTCARD(PrecompensationCylinder DIV 4);
	    OutByte (3F6H, ExtraHeadOption);
	    IF ORD(IANDB (ExtraHeadOption, 0C0H)) <> 0 THEN
		INC (opcode);			(* to disable retries *)
	    END (*IF*);
	END (*WITH*);

	(* CommandBlock[2] specifies the number of sectors.	*)

	CommandBlock[2] := SHORTCARD(count DIV BlockSize);

	(* Turn the block number into head/cylinder/sector form. *)

	Decompose (BlockNumber, drive, head, cylinder, sector);
	CommandBlock[3] := SHORTCARD(sector);
	CommandBlock[4] := LowByte (cylinder);
	CommandBlock[5] := HighByte (cylinder);		(* only 2 bits *)

	(* The 0A0H below is the code for ECC and 512-byte sectors.	*)

	CommandBlock[6] := BYTE(16*drive + head + 0A0H);
	CommandBlock[7] := opcode;

    END Encode;

(************************************************************************)
(*		    THE USER-CALLABLE READ OPERATIONS			*)
(************************************************************************)

PROCEDURE ReadPhysicalBlock (drive: PhysicalDriveNumber;
			BlockNumber: BlockNumberType;  count: CARDINAL;
			BufferAddress: ADDRESS): ErrorCode;

    (* Reads "count" bytes, starting at the beginning of disk block	*)
    (* "BlockNumber" on the specified drive, into a memory buffer.	*)

    VAR CommandBlock: CommandArray;  result: ErrorCode;

    BEGIN
	IF drive >= NumberOfPhysicalDrives THEN
	    result := NoSuchUnit;
	ELSE
	    Encode (CommandBlock, 20H, drive, BlockNumber, count);
	    result := ReadOperation (CommandBlock, BufferAddress);
	END (*IF*);
	RETURN result;
    END ReadPhysicalBlock;

(************************************************************************)

PROCEDURE ReadLogicalBlock (unit: LogicalDriveNumber;
			RelativeBlock: BlockNumberType;  count: CARDINAL;
			BufferAddress: ADDRESS): ErrorCode;

    (* Reads "count" bytes, starting at the beginning of disk block	*)
    (* "RelativeBlock" on the specified logical unit, into memory	*)
    (* starting at "BufferAddress".					*)

    VAR drive: PhysicalDriveNumber;  BlockNo: BlockNumberType;

    BEGIN
	WITH LogicalUnit[unit] DO
	    IF RelativeBlock >= numberofsectors THEN
		RETURN IllegalBlockNumber;
	    END (*IF*);
	    drive := physicaldrive;
	    BlockNo := bootblock + RelativeBlock;
	END (*WITH*);
	RETURN ReadPhysicalBlock (drive, BlockNo, count, BufferAddress);
    END ReadLogicalBlock;

(************************************************************************)
(*		    THE USER-CALLABLE WRITE OPERATIONS			*)
(************************************************************************)

PROCEDURE WritePhysicalBlock (drive: PhysicalDriveNumber;
				BlockNumber: BlockNumberType;  count: CARDINAL;
				BufferAddress: ADDRESS): ErrorCode;

    (* Writes "count" bytes, starting at the beginning of disk block	*)
    (* "BlockNumber" on the specified drive, from a memory buffer.	*)

    VAR CommandBlock: CommandArray;  result: ErrorCode;

    BEGIN
	IF drive >= NumberOfPhysicalDrives THEN
	    result := NoSuchUnit;
	ELSE
	    Encode (CommandBlock, 30H, drive, BlockNumber, count);
	    result := WriteOperation (CommandBlock, BufferAddress);
	END (*IF*);
	RETURN result;
    END WritePhysicalBlock;

(************************************************************************)

PROCEDURE WriteLogicalBlock (unit: LogicalDriveNumber;
			RelativeBlock: BlockNumberType;  count: CARDINAL;
			BufferAddress: ADDRESS): ErrorCode;

    (* Writes "count" bytes, starting at the beginning of disk block	*)
    (* "RelativeBlock" on the specified logical unit, from memory	*)
    (* starting at "BufferAddress".					*)

    VAR drive: PhysicalDriveNumber;  BlockNo: BlockNumberType;

    BEGIN
	WITH LogicalUnit[unit] DO
	    IF RelativeBlock >= numberofsectors THEN
		RETURN IllegalBlockNumber;
	    END (*IF*);
	    drive := physicaldrive;
	    BlockNo := bootblock + RelativeBlock;
	END (*WITH*);
	RETURN WritePhysicalBlock (drive, BlockNo, count, BufferAddress);
    END WriteLogicalBlock;

(************************************************************************)
(*		     INTERFACE TO "Devices" MODULE			*)
(************************************************************************)

PROCEDURE DiskOperation (VAR (*INOUT*) details: RequestBlock);

    (* Performs a disk transfer as specified in the request block.	*)

    BEGIN
	WITH details DO
	    IF operation = read THEN
		Status := ReadLogicalBlock (unit, BlockNumber,
						ByteCount, BufferAddress);
	    ELSIF operation = write THEN
		Status := WriteLogicalBlock (unit, BlockNumber,
						ByteCount, BufferAddress);
	    ELSIF operation = physicalread THEN
		Status := ReadPhysicalBlock (LogicalUnit[unit].physicaldrive,
					BlockNumber, BlockSize, BufferAddress);
	    ELSIF operation = physicalwrite THEN
		Status := WritePhysicalBlock (LogicalUnit[unit].physicaldrive,
					BlockNumber, BlockSize, BufferAddress);
	    ELSE
		Status := FeatureNotImplemented;
	    END (*IF*);
	END (*WITH*);
    END DiskOperation;

(************************************************************************)

PROCEDURE DiskRequestHandler;

    (* This procedure runs as an independent task.  Each time around	*)
    (* its main loop, it picks up one enqueued I/O request and executes	*)
    (* the request.  The requests are placed on the queue by calls to	*)
    (* module Devices.  On completion of an operation, we inform the	*)
    (* caller by performing a Signal on a user-specified semaphore.	*)

    CONST MaxNumberOfRetries = 3;

    VAR reply: ErrorCode;  retries: CARDINAL;
	RequestPointer: RequestBlockPointer;

    BEGIN
	LOOP
	    RequestPointer := AcceptRequest (harddisk);
	    WITH RequestPointer^ DO
		IF operation = shutdown THEN
		    Signal (DoneSemaphorePointer^);
		    EXIT (*LOOP*);
		END (*IF*);
	    END (*WITH*);
	    retries := 0;
	    REPEAT
		DiskOperation (RequestPointer^);
		reply := RequestPointer^.Status;
		INC (retries);
	    UNTIL (reply = OK) OR (retries>MaxNumberOfRetries);
	    Signal (RequestPointer^.DoneSemaphorePointer^);

	END (*LOOP*);

    END DiskRequestHandler;

(************************************************************************)
(*			     INITIALISATION				*)
(************************************************************************)

PROCEDURE SetUpDrive (d: PhysicalDriveNumber;  pplace: CARDINAL);

    (* Sets up PhysicalDriveData[d] by copying the appropriate		*)
    (* information from a table which already exists and whose address	*)
    (* is held at absolute location pplace in low memory.		*)

    (*# save, data(near_ptr=>off) *)

    TYPE TblPtr = POINTER TO DiskParameterTable;

    VAR TablePointer: TblPtr;
	PointerPointer: POINTER TO TblPtr;

    (*# restore *)

    BEGIN

	PointerPointer := MakePointer (0, pplace);
	TablePointer := PointerPointer^;
	IF TablePointer = FarNIL THEN
	    RETURN;
	END (*IF*);
	INC (NumberOfPhysicalDrives);

	WITH PhysicalDriveData[d] DO
	    SectorsPerTrack := CARDINAL(TablePointer^.sectorspertrack);
	    NumberOfHeads := CARDINAL(TablePointer^.maxheads);
	    PrecompensationCylinder := TablePointer^.precomp;
	    ExtraHeadOption := TablePointer^.optionbyte;
	END (*WITH*);

    END SetUpDrive;

(************************************************************************)

PROCEDURE DecodePartitionTable;

    (* On entry to this procedure, we have already initialised all data	*)
    (* pertaining to the physical drives, but do not yet have a logical	*)
    (* drive structure.  This procedure reads the partition table on	*)
    (* physical block 0 of each physical drive, sets up the logical	*)
    (* drive information, and installs us as a device driver known to	*)
    (* module Devices.							*)

    VAR bufptr: POINTER TO ARRAY [0..BlockSize-1] OF BYTE;
	drive: PhysicalDriveNumber;  unit: CARDINAL;
	bootblockptr, sizeptr: POINTER TO LONGCARD;
	subscript: [0..BlockSize-1];
	Registers: RegisterPacket;
	(*# save, data(near_ptr=>off) *)
	StringAddress: POINTER TO ARRAY [0..63] OF CHAR;
	(*# restore *)

    BEGIN
	FOR unit := 0 TO MAX(LogicalDriveNumber) DO
	    WITH LogicalUnit[unit] DO
		physicaldrive := 0;  bootblock := 0;
		numberofsectors := 0;
	    END (*WITH*);
	END (*FOR*);
	unit := 0;
	ALLOCATE (bufptr, BlockSize);
	FOR drive := 0 TO NumberOfPhysicalDrives-1 DO
	    IF ReadPhysicalBlock (drive, 0, BlockSize, bufptr) = OK
	      THEN
		FOR subscript := 01C0H TO 01F0H BY 10H DO
		    IF bufptr^[subscript] <> BYTE(0) THEN
			bootblockptr := AddOffset (bufptr, subscript+6);
			sizeptr := AddOffset (bufptr, subscript+10);
			WITH LogicalUnit[unit] DO
			    physicaldrive := drive;
			    bootblock := bootblockptr^;
			    numberofsectors := sizeptr^;
			END (*WITH*);
			INC (unit);
		    END (*IF*);
		END (*FOR*);
	    END (*IF*);
	END (*FOR*);
	DEALLOCATE (bufptr, BlockSize);

	(* Having collected all necessary information from the	*)
	(* partition tables, set up the drive names.		*)

	NumberOfLogicalDrives := unit;
	IF NumberOfLogicalDrives > 0 THEN
	    harddisk := InstallDeviceDriver (NumberOfLogicalDrives);
	    FOR unit := 0 TO NumberOfLogicalDrives-1 DO

		(* Read the current directory for the this logical drive.	*)

		WITH Registers DO
		    DL := SHORTCARD(unit) + 3;
		    AH := 71;
		    BIOS (33, Registers);
		    StringAddress := MakePointer (DS, SI);
		END (*WITH*);

		(* Pass the information to module Devices.	*)

		DeviceName (harddisk, unit, CHR(ORD("C")+unit),
			LogicalUnit[unit].numberofsectors, StringAddress^);

	    END (*FOR*);
	END (*IF*);

    END DecodePartitionTable;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

VAR Mpage: MaintenancePage;

BEGIN
    IF testing THEN
	CreateMaintenancePage (Mpage);
	OpenWindow (log, black, green, 0,23, 0,60, simpleframe, doubledivider);
	Associate (log, Mpage);
	WriteString (log,"         Diagnostic output from hard disk driver");
	ChangeScrollingRegion (log, 3, 22);
    END (*IF*);
    CreateSemaphore (DataRequest, 0);
    NumberOfPhysicalDrives := 0;
    SetUpDrive (0, 104H);
    SetUpDrive (1, 118H);
    IF NumberOfPhysicalDrives = 0 THEN
	NumberOfLogicalDrives := 0;
    ELSE
	CreateInterruptTask (DiskInterrupt, DRQInterruptHandler,
						"Hard disk int");
	DecodePartitionTable;
    END (*IF*);
    IF NumberOfLogicalDrives > 0 THEN
	CreateTask (DiskRequestHandler, 13, "Harddisk queue");
    END (*IF*);
END HardDisk.
