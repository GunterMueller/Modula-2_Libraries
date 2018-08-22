IMPLEMENTATION MODULE Devices;

	(********************************************************)
	(*							*)
	(*		Support for device drivers.		*)
	(*							*)
	(*  The aim of this module is to a measure of device	*)
	(*  independence in I/O operations.  It provides a	*)
	(*  uniform I/O interface to all device drivers which	*)
	(*  choose to make themselves known to module Devices.	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:		Working.			*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* proc *)	ADR;

FROM Queues IMPORT
    (* type *)	Queue,
    (* proc *)	CreateQueue, AddToQueue, TakeFromQueue;

FROM Semaphores IMPORT
    (* proc *)	CreateSemaphore, DestroySemaphore, Signal, Wait;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)	Physical;

FROM TerminationControl IMPORT
    (* proc *)	Crash, SetTerminationProcedure;

IMPORT Str;

(************************************************************************)

CONST CheckCode = 0C5E2H;

TYPE

    (* We allow external device names to be up to eight characters in	*)
    (* length, and default directory strings to be up to 64 characters	*)
    (* in length.  (This decision should perhaps be reviewed later.)	*)

    EightChar = ARRAY [0..7] OF CHAR;
    DirString = ARRAY [0..63] OF CHAR;

    (* The fields in a device descriptor are:				*)
    (*	  check		a constant, inserted to detect uninitialised	*)
    (*			pointers					*)
    (*	  UnitLimit	maximum unit number for this device		*)
    (*	  RequestQueue	the queue of pending I/O requests		*)
    (*	  capacity	the number of sectors, as supplied by procedure	*)
    (*			DeviceName, for each unit of this device	*)

    DeviceRec = RECORD
		    check: CARDINAL;
		    UnitLimit: CARDINAL;
		    RequestQueue: Queue;
		    capacity: ARRAY [0..0] OF BlockNumberType;
		END (*RECORD*);

    Device = POINTER TO DeviceRec;

    (* We maintain a binary structure to hold the equivalences between	*)
    (* external and internal device names.  Note that the information	*)
    (* kept here is associated with a (device, unit) pair; it cannot	*)
    (* simply be associated with a device, which is why we can't use	*)
    (* a DeviceRec record for this purpose.				*)

    TreePointer = POINTER TO NameRecord;
    NameRecord = RECORD
		    left, right: TreePointer;
		    string: EightChar;
		    devicecode: Device;
		    unitnumber: CARDINAL;
		    defaultdir: DirString;
		 END (*RECORD*);

(************************************************************************)

VAR
    (* DummyDevice is the device code returned for any unknown device	*)
    (* name.  Any attempted operation on DummyDevice will return a	*)
    (* "no such device" error.						*)

    DummyDevice: Device;

    (* Names points to head of the structure which holds device names.	*)

    Names: TreePointer;

(************************************************************************)
(*			     ERROR HANDLER				*)
(************************************************************************)

PROCEDURE Error (VAR (*INOUT*) details: RequestBlock;  ErrorNo: ErrorCode);

    (* Fills in the error code in the "details" record, and signals	*)
    (* that the operation is complete.					*)

    BEGIN
	WITH details DO
	    Status := ErrorNo;
	    Signal (DoneSemaphorePointer^);
	END (*WITH*);
    END Error;

(************************************************************************)
(*	  MAPPING BETWEEN EXTERNAL NAMES AND DEVICE INFORMATION		*)
(************************************************************************)

PROCEDURE CopyString (src: ARRAY OF CHAR;  VAR (*OUT*) dest: ARRAY OF CHAR;
					VAR (*INOUT*) place: CARDINAL);

    (* Copies src to dest, starting at dest[place].  On exit place	*)
    (* has been updated to point to the trailing Nul, or beyond the end	*)
    (* of dest if there was no room for the trailing Nul.		*)

    CONST Nul = CHR(0);

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	WHILE (j<=HIGH(src)) AND (place<=HIGH(dest)) AND (src[j] <> Nul) DO
	    dest[place] := src[j];  INC (j);  INC (place);
	END (*WHILE*);
	IF place <= HIGH(dest) THEN
	    dest[place] := Nul;
	END (*IF*);
    END CopyString;

(************************************************************************)

PROCEDURE CopyAndPad (name: ARRAY OF CHAR): EightChar;

    (* Converts a character string to a fixed-length string with	*)
    (* trailing space characters.					*)

    VAR result: EightChar;  j: [0..8];

    BEGIN
	result := "        ";  j := 0;
	WHILE (j<8) AND (j<=HIGH(name)) AND (name[j] <> CHR(0)) DO
	    result[j] := name[j];  INC (j);
	END (*WHILE*);
	RETURN result;
    END CopyAndPad;

(************************************************************************)

PROCEDURE Search (name: ARRAY OF CHAR;  VAR (*OUT*) device: Device;
			VAR (*OUT*) unit: CARDINAL;
			VAR (*OUT*) found: BOOLEAN);

    (* Searches the Names structure for a match with "name", and	*)
    (* returns the device and unit codes; or returns found = FALSE if	*)
    (* the name was not found.						*)

    VAR namestring: EightChar;  current: TreePointer;

    BEGIN
	namestring := CopyAndPad (name);  current := Names;  found := FALSE;
	LOOP
	    IF current = NIL THEN EXIT(*LOOP*) END (*IF*);
	    IF current^.string = namestring THEN
		found := TRUE;  EXIT(*LOOP*);
	    ELSIF Str.Compare (current^.string, namestring) < 0 THEN
		current := current^.left;
	    ELSE
		current := current^.right;
	    END (*IF*);
	END (*LOOP*);
	IF found THEN
	    device := current^.devicecode;
	    unit := current^.unitnumber;
	END (*IF*);
    END Search;

(************************************************************************)

PROCEDURE SearchTree (T: TreePointer;  dev: Device;  unitno: CARDINAL)
								: TreePointer;

    (* Searches tree T structure for a match with (dev, unitno), and	*)
    (* returns a pointer to the record found; or returns NIL if the	*)
    (* (dev, unitno) pair was not found.				*)

    VAR result: TreePointer;

    BEGIN
	IF T = NIL THEN RETURN NIL;
	ELSIF SameDevice(T^.devicecode, dev)
			AND (T^.unitnumber = unitno) THEN RETURN T;
	ELSE
	    result := SearchTree (T^.left, dev, unitno);
	    IF result <> NIL THEN RETURN result;
	    ELSE RETURN SearchTree (T^.right, dev, unitno);
	    END (*IF*);
	END (*IF*);
    END SearchTree;

(************************************************************************)
(*		  PROCEDURES CALLED BY THE USER TASKS			*)
(************************************************************************)

PROCEDURE SameDevice (d1, d2: Device): BOOLEAN;

    (* Tests the condition d1 = d2.	*)

    BEGIN
	RETURN Physical(d1) = Physical(d2);
    END SameDevice;

(************************************************************************)

PROCEDURE NullDevice (): Device;

    (* Returns the device code which this module uses internally to	*)
    (* mean "nonexistent device" or "unknown device".  I/O operations	*)
    (* on this device are of course impossible (if attempted, they will	*)
    (* result in the NoSuchDevice error code), but the device code can	*)
    (* be used by client modules as a marker to indicate that no	*)
    (* genuine device has yet been specified.				*)

    BEGIN
	RETURN DummyDevice;
    END NullDevice;

(************************************************************************)

PROCEDURE IdentifyDevice (name: ARRAY OF CHAR;  VAR (*OUT*) device: Device;
				VAR (*OUT*) unitnumber: CARDINAL);

    (* Given the character string form of a device name, returns the	*)
    (* device code and unit number.					*)

    VAR found: BOOLEAN;

    BEGIN
	Search (name, device, unitnumber, found);
	IF NOT found THEN
	    device := DummyDevice;
	    unitnumber := 0;
	END (*IF*);
    END IdentifyDevice;

(************************************************************************)

PROCEDURE GetDefaultDirectory (device: Device;  unit: CARDINAL;
				VAR (*OUT*) DirectoryString: ARRAY OF CHAR);

    (* Produces a string representing the default directory.  The	*)
    (* string includes the character form of the device name.		*)

    VAR T: TreePointer;  j: CARDINAL;

    BEGIN
	T := SearchTree (Names, device, unit);
	IF T = NIL THEN
	    DirectoryString[0] := CHR(0);
	ELSE
	    WITH T^ DO
		j := 0;
		CopyString (string, DirectoryString, j);
		WHILE DirectoryString[j-1] = ' ' DO
		    DEC (j);
		END (*WHILE*);
		CopyString (":\", DirectoryString, j);
		CopyString (defaultdir, DirectoryString, j);
	    END (*WITH*);
	END (*IF*);
    END GetDefaultDirectory;

(************************************************************************)

PROCEDURE VolumeSize (device: Device;  unitnumber: CARDINAL): BlockNumberType;

    (* Returns the number of sectors on (device, unitnumber).  Note	*)
    (* that the result is meaningful only for device drivers which	*)
    (* supply this information (see DeviceName below).			*)

    BEGIN
(*# save, check(index => off) *)
	RETURN device^.capacity[unitnumber];
(*# restore *)
    END VolumeSize;

(************************************************************************)

PROCEDURE IOrequest (VAR (*INOUT*) details: RequestBlock);

    (* Adds the requested I/O operation to the queue of pending		*)
    (* operations on details.device.  The operation might not be done	*)
    (* immediately since the device driver might still be working on an	*)
    (* earlier request.  On I/O completion, the device driver performs	*)
    (* a Signal(details.DoneSemaphorePointer^).				*)

    BEGIN
	WITH details DO
	    IF device^.check <> CheckCode THEN
		Error (details, NoSuchDevice);
	    ELSIF unit > device^.UnitLimit THEN
		Error (details, NoSuchUnit);
	    ELSE
		AddToQueue (device^.RequestQueue, ADR(details));
	    END (*IF*);
	END (*WITH*);
    END IOrequest;

(************************************************************************)

PROCEDURE SynchronousIO (op: OperationType;
			d: Device;  unitnumber: CARDINAL;
			Block: BlockNumberType;
			MemoryAddress: ADDRESS;  amount: CARDINAL): ErrorCode;

    (* Like IOrequest, but does not return until the operation is	*)
    (* complete.  This is for the convenience of callers who do not	*)
    (* want to deal with the complications of semaphores.		*)

    VAR RequestRecord: RequestBlock;
	DoneSemaphore: Semaphore;
	status: ErrorCode;

    BEGIN
	CreateSemaphore (DoneSemaphore, 0);
	WITH RequestRecord DO
	    operation := op;
	    device := d;
	    unit := unitnumber;
	    BlockNumber := Block;
	    BufferAddress := MemoryAddress;
	    ByteCount := amount;
	    DoneSemaphorePointer := ADR (DoneSemaphore);
	END (*WITH*);
	IOrequest (RequestRecord);
	Wait (DoneSemaphore);
	status := RequestRecord.Status;
	DestroySemaphore (DoneSemaphore);
	RETURN status;
    END SynchronousIO;

(************************************************************************)

PROCEDURE BlockRead (d: Device;  unitnumber: CARDINAL;
			BlockNumber: BlockNumberType;
			MemoryAddress: ADDRESS;  amount: CARDINAL): ErrorCode;

    (* Reads "amount" bytes into memory, starting at the beginning of	*)
    (* block number "BlockNumber" on the specified device.		*)

    BEGIN
	RETURN SynchronousIO (read, d, unitnumber, BlockNumber,
					MemoryAddress, amount);
    END BlockRead;

(************************************************************************)

PROCEDURE ReadPhysicalBlock (d: Device;  unitnumber: CARDINAL;
		BlockNo: BlockNumberType;  MemoryAddress: ADDRESS): ErrorCode;

    (* Reads one sector into memory, starting at the beginning of	*)
    (* physical block number "BlockNo" on the specified device.		*)

    BEGIN
	RETURN SynchronousIO (physicalread, d, unitnumber, BlockNo,
							MemoryAddress, 0);
    END ReadPhysicalBlock;

(************************************************************************)

PROCEDURE BlockWrite (d: Device;  unitnumber: CARDINAL;
			BlockNumber: BlockNumberType;
			MemoryAddress: ADDRESS;  amount: CARDINAL): ErrorCode;

    (* Writes "amount" bytes from memory to the device, starting at the	*)
    (* beginning of block number "BlockNumber" on the specified device.	*)

    BEGIN
	RETURN SynchronousIO (write, d, unitnumber, BlockNumber,
					MemoryAddress, amount);
    END BlockWrite;

(************************************************************************)
(*		PROCEDURES CALLED BY THE DEVICE DRIVERS			*)
(************************************************************************)

PROCEDURE InstallDeviceDriver (MaxUnitNumber: CARDINAL): Device;

    (* Called by the device driver as part of its initialization code,	*)
    (* to make the driver known to this module.  MaxUnitNumber is the	*)
    (* maximum unit number which this device driver will support (0 for	*)
    (* a single-unit device driver).  The other input parameter is	*)
    (* the external name of the device.  The value returned is the	*)
    (* means by which the device driver will henceforth identify itself	*)
    (* to this module.							*)

    VAR result: Device;  j: CARDINAL;

    BEGIN
	ALLOCATE (result, SIZE(DeviceRec)+MaxUnitNumber*SIZE(BlockNumberType));
	WITH result^ DO
	    check := CheckCode;
	    UnitLimit := MaxUnitNumber;
	    CreateQueue (RequestQueue);
	    FOR j := 0 TO MaxUnitNumber DO
(*# save, check(index => off) *)
		capacity[j] := 0;
(*# restore *)
	    END (*FOR*);
	END (*WITH*);
	RETURN result;
    END InstallDeviceDriver;

(************************************************************************)

PROCEDURE DeviceName (device: Device;  unit: CARDINAL;
			name: ARRAY OF CHAR;  size: BlockNumberType;
			DefaultDirString: ARRAY OF CHAR);

    (* Specifies an external name for a given device and unit number.	*)
    (* Duplicate names are permitted.  The size parameter gives the	*)
    (* number of blocks in the partition if this "device" is actually	*)
    (* a partition on a hard disk; but it is not necessarily meaningful	*)
    (* for other device types.  (This parameter is supplied only for	*)
    (* the benefit of the file system when it has to deal with the	*)
    (* special case of a large partition.)  Device drivers which are	*)
    (* not prepared to specify a meaningful size should supply a value	*)
    (* of 0 for the size.						*)
    (* DefaultDirString specifies the initial default directory.	*)

    VAR namestring: EightChar;  current, newptr: TreePointer;
	inserted: BOOLEAN;  j: CARDINAL;

    BEGIN
	(* For a faulty device driver, cause a crash - it's better to	*)
	(* get the crash at startup time than to let the program run	*)
	(* with a suspect driver.					*)

	IF (device = NIL) OR (device^.check <> CheckCode) THEN
	    Crash ("Faulty device driver");
	END(*IF*);

	(* Insert the size information into the device descriptor. *)

	WITH device^ DO
	    IF unit < UnitLimit THEN
(*# save, check(index => off) *)
		capacity[unit] := size;
(*# restore *)
	    END (*IF*);
	END (*WITH*);

	(* Create a NameRecord to hold the remaining information. *)

	namestring := CopyAndPad (name);
	NEW (newptr);
	WITH newptr^ DO
	    left := NIL;  right := NIL;  string := namestring;
	    devicecode := device;  unitnumber := unit;
	    j := 0;  CopyString (DefaultDirString, defaultdir, j);
	END (*WITH*);

	(* Insert the new record into the Names tree.	*)

	IF Names = NIL THEN
	    Names := newptr;
	ELSE
	    current := Names;  inserted := FALSE;
	    REPEAT
		IF Str.Compare (current^.string, namestring) < 0 THEN
		    IF current^.left = NIL THEN
			current^.left := newptr;  inserted := TRUE;
		    ELSE
			current := current^.left;
		    END (*IF*);
		ELSE
		    IF current^.right = NIL THEN
			current^.right := newptr;  inserted := TRUE;
		    ELSE
			current := current^.right;
		    END (*IF*);
		END (*IF*);
	    UNTIL inserted;
	END (*IF*);
    END DeviceName;

(************************************************************************)

PROCEDURE AcceptRequest (device: Device): RequestBlockPointer;

    (* Returns a pointer to the next request enqueued for this device.	*)
    (* If there is no next request, we wait until one appears.		*)

    BEGIN
	RETURN TakeFromQueue (device^.RequestQueue);
    END AcceptRequest;

(************************************************************************)
(*			TERMINATION HANDLER				*)
(************************************************************************)

PROCEDURE DeleteTree (VAR (*INOUT*) T: TreePointer);

    (* Destroys tree T.	*)

    BEGIN
	IF T <> NIL THEN
	    DeleteTree (T^.left);  DeleteTree (T^.right);
	    DISPOSE (T);
	END (*IF*);
    END DeleteTree;

(************************************************************************)

PROCEDURE CloseDown ();

    (* Cleans up at program termination.  Note that all device drivers	*)
    (* will have closed down by the time this procedure is called, as	*)
    (* they are higher-level modules.					*)
    (* Remark: have not yet made provision for destroying the device	*)
    (* descriptors.							*)

    BEGIN
	DeleteTree (Names);
    END CloseDown;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

BEGIN
    Names := NIL;
    SetTerminationProcedure (CloseDown);
    NEW (DummyDevice);
    WITH DummyDevice^ DO
	check := 0;
	UnitLimit := 0;
	capacity[0] := 0;
	CreateQueue (RequestQueue);
    END (*WITH*);
END Devices.
