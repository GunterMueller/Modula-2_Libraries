IMPLEMENTATION MODULE Floppy;

	(****************************************************************)
	(*								*)
	(*		Device driver for floppy disk.			*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	25 November 1994			*)
	(*  Status:		Working					*)
	(*								*)
	(*	Working with 360KB, 1.2MB, and 1.44MB disks, but not	*)
	(*	completely reliably, and more fiddling with delays	*)
	(*	while reading from controller will be needed.		*)
	(*								*)
	(*	The busy waits are inelegant and unreliable, and I	*)
	(*	should look for a better method.			*)
	(*								*)
	(*	There are intermittent failures of the controller,	*)
	(*	i.e. the controller is not always listening when it	*)
	(*	should be, and does not always send a reply when it	*)
	(*	should, which suggests that there might be some time	*)
	(*	delays which should be lengthened; but my tests have	*)
	(*	failed to track down the cause.  Luckily, the problem	*)
	(*	usually disappears on a retry.				*)
	(*								*)
	(*	Still need to do something about:			*)
	(*	    Implementation of verify, format			*)
	(*	    Handling case of 360K disk in 1.2M or 1.44M drive	*)
	(*	    Detection of disk change				*)
	(*								*)
	(****************************************************************)

(************************************************************************)
(*									*)
(* HARDWARE ASSUMPTIONS:						*)
(*									*)
(*  This module is set up to handle the 765 diskette controller, with	*)
(*  DMA transfers to/from main memory using channel 2 of the 8237A DMA	*)
(*  controller.  Up to four disk drives can be handled, but the present	*)
(*  version of the software assumes two drives.  (I dropped support for	*)
(*  the other two because I doubt that anyone will ever need it).	*)
(*									*)
(*  Ports 3F0H-3F7H are reserved for the diskette controller.  Only	*)
(*  four of these ports are actually used: 3F2H for some motor control	*)
(*  bits, 3F4H for the 765 status register, 3F5H for the 765 data	*)
(*  register, and 3F7H for setting the data rate.  Commands to the 765	*)
(*  controller are sent as byte strings to its data register, and	*)
(*  status information comes back through the same port.  Essentially	*)
(*  the only function of the 765 status register is to indicate whether	*)
(*  the data register is ready to send or receive information.		*)
(*									*)
(*  Interrupts from the diskette controller come through request line 6	*)
(*  of the master 8259 interrupt controller (which lives at ports	*)
(*  020H-03FH), which maps them to processor interrupt number 14.	*)
(*									*)
(*  The disks are formatted with 8, 9, 15, or 18 sectors per track, and	*)
(*  there are two heads per cylinder, i.e. both sides of the disk are	*)
(*  used.								*)
(*									*)
(*  All of that information is no doubt confusing to those unfamiliar	*)
(*  with the hardware.  From a software viewpoint, the following	*)
(*  sequence of operations is needed to perform a disk read or write:	*)
(*									*)
(*   1.	Tell the diskette controller to start the motor, if it is not	*)
(*	already running from an earlier operation.  A time delay is	*)
(*	then necessary to let the motor run up to its correct operating	*)
(*	speed.								*)
(*   2.	Load the DMA controller with the memory address to be used for	*)
(*	the data transfer.  (In most computers, the disk controller has	*)
(*	its own built-in DMA hardware.  In this computer, the DMA	*)
(*	controller is physically separate from the disk controller; but	*)
(*	the principle is the same, since we permanently dedicate	*)
(*	channel 2 of the DMA controller to floppy disk operations).	*)
(*   3.	Send the appropriate command bytes to the diskette controller	*)
(*	to seek (i.e. move the read-write head) to the desired track.	*)
(*   4.	Wait for the interrupt which announces that the seek operation	*)
(*	is done.							*)
(*   5.	Send the appropriate command bytes to the diskette controller	*)
(*	ports to start the read or write operation.  The data transfer	*)
(*	between the diskette controller and main memory occurs via the	*)
(*	DMA controller, and does not require any further software	*)
(*	intervention.							*)
(*   6.	When the operation is complete, the diskette controller sends	*)
(*	an interrupt request to the processor.  The interrupt routine	*)
(*	can then send a "stop motor" command to the diskette.		*)
(*									*)
(*  This description is oversimplified in that it ignores error		*)
(*  handling.  In fact a large part of the software in this module is	*)
(*  the code needed to deal with various types of error.		*)
(*									*)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE, ADDRESS,
    (* proc *)	ADR;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode,
    (* proc *)	TranslateErrorCode;

FROM Devices IMPORT
    (* type *)	Device, RequestBlock, RequestBlockPointer, OperationType,
    (* proc *)	InstallDeviceDriver, DeviceName, AcceptRequest, IOrequest;

FROM MaintenancePages IMPORT
    (* type *)	MaintenancePage,
    (* proc *)	CreateMaintenancePage, Associate;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteChar, WriteString, WriteLn,
		ChangeScrollingRegion, OpenSimpleWindow;

FROM NumericIO IMPORT
    (* proc *)	WriteCard, WriteHexByte, WriteHexWord, WriteAddress;

FROM LowLevel IMPORT
    (* proc *)	IANDB, IORB, INOTB, RS, OutByte, InByte;

FROM DMA IMPORT
    (* proc *)	LoadDMAparameters, CheckDMAAddress;

FROM MiscPMOS IMPORT
    (* proc *)	ShortDelay, ReadCMOS;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, DestroySemaphore, Wait, Signal;

FROM Timer IMPORT
    (* proc *)	Sleep, TimedWait;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateTask, CreateInterruptTask, WaitForInterrupt,
		CreateLock, DestroyLock, Obtain, Release;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

(************************************************************************)
(*			MISCELLANEOUS GLOBAL DATA			*)
(************************************************************************)

CONST testing = TRUE;		(* debugging control *)

VAR Mpage: MaintenancePage;
    log: Window;

(************************************************************************)
(*			LOTS OF USEFUL CONSTANTS			*)
(************************************************************************)

CONST

    (************************************)
    (* Formatting, etc., parameters.	*)
    (************************************)

    NumberOfHeads = 2;
    DataLengthCode = 0FFH;

    (*********************)
    (* Interrupt number. *)
    (*********************)

    DiskInterrupt = 14;

    (*****************)
    (* Port numbers. *)
    (*****************)

    BasePort = 03F0H;
    MotorControlPort = BasePort + 2;
    StatusPort = BasePort + 4;
    DataPort = BasePort + 5;
    DataRatePort = BasePort + 7;

    (********************************************************************)
    (* Flags in controller status register.  The high-order bit is set	*)
    (* if the controller is ready to communicate with the processor, 	*)
    (* and the next bit indicates the direction.  The low-order six	*)
    (* bits, which this software does not use, give more precise	*)
    (* information about why the controller is not ready.		*)
    (********************************************************************)

    StatusMask = 0C0H;
    ReadyToSend = BYTE(0C0H);
    ReadyToListen = BYTE(080H);

    (********************************************************************)
    (* Flags in internal controller status registers SR0, SR1, SR2, and	*)
    (* SR3.  (Note that these are distinct from the main status		*)
    (* register mentioned above).  We list here only those flags	*)
    (* checked by more than one procedure.  Some further flags are	*)
    (* defined locally in some procedures.				*)
    (********************************************************************)

    SeekEnd = 20H;	(* in SR0 *)

(************************************************************************)
(*			    DRIVE INFORMATION				*)
(************************************************************************)

VAR
    (* The variable "floppy" is the means by which this device driver	*)
    (* makes itself known to module Devices.				*)

    floppy: Device;

TYPE DriveNumber = SHORTCARD [0..1];

    (* In this version, we support only two drives.  See comments in	*)
    (* source about how to support four drives if required.		*)

TYPE DriveType = [0..5];

    (* The drive types which the software currently recognises are:	*)
    (*		0	no drive present				*)
    (*		1	double sided, 48 TPI, capacity 360KB		*)
    (*		2	high capacity, 96 TPI, capacity 1.2MB		*)
    (*		3	unknown type					*)
    (*		4	3.5", capacity 1.44MB				*)
    (*		5	unknown type					*)

CONST UnknownDriveType = MAX(DriveType);

    (* The "unknown" category does not mean that all is lost.  The	*)
    (* present version of the software has no provision for handling	*)
    (* unknown types; but in principle such provision could be added.	*)
    (* For example, one could work out how many tracks there were on	*)
    (* an unknown disk by checking which Seek operations work.		*)

TYPE DataRateCode = SHORTCARD [0..2];

    (* The data rate code is 0 for 500KBS, 1 for 300KBS, 2 for 250KBS.	*)
    (* Information in the DriveInfo array is partly an attribute of the	*)
    (* drive, and partly of the medium in the drive.  In this version	*)
    (* of the software, we allow for 360KB and 1.2MB drives, but have	*)
    (* not yet allowed for the case of a 360KB disk in a 1.2MB drive.	*)
    (* The hardware will support that case, but the software is not yet	*)
    (* set up to work out what size disk is present.			*)

VAR DriveInfo: ARRAY DriveNumber OF
		    RECORD
			drivetype: DriveType;
			SectorsPerTrack: CARDINAL;
			NumberOfCylinders: CARDINAL;
			SectorGap: BYTE;
			DataRate: DataRateCode;
			HeadSettlingTime: BYTE;	(* milliseconds *)
		    END (*RECORD*);

(************************************************************************)
(*		INFORMATION ABOUT THE DRIVE MOTORS			*)
(************************************************************************)
(*									*)
(*  The bits in the motor control port register are:			*)
(*	   bit	7	turn on drive motor 3				*)
(*		6	turn on drive motor 2				*)
(*		5	turn on drive motor 1				*)
(*		4	turn on drive motor 0				*)
(*		3	interrupt enable				*)
(*		2	reset (0=reset, 1=no reset)			*)
(*	      1-0	number of drive to select			*)
(*									*)
(*  Note that any number of motors may be running but only one drive	*)
(*  can be selected at any given time.  Because of this, and because	*)
(*  of controller limitations, we can only do I/O on one disk at a	*)
(*  time, but we do have the option of keeping the other motors running	*)
(*  in anticipation of future operations on the other drives.		*)
(*									*)
(************************************************************************)

VAR

    (* The following array is in fact an array of constants, giving the	*)
    (* motor control bits for the four drives.  We set up the values	*)
    (* 10H, 20H, 40H, 80H in the initialisation code.			*)

    MotorControlBit: ARRAY DriveNumber OF BYTE;

    (* SharedMotorData holds data which may be modified by more than	*)
    (* one task.  The lock field is for critical section protection.	*)
    (* The count field is used only during module initialisation, to	*)
    (* count how many motor control tasks are running.  MotorStatus	*)
    (* is the byte we will put out to the motor control port.		*)

    SharedMotorData:	RECORD
			    lock: Lock;
			    count: SHORTCARD;
			    MotorStatus: BYTE;
			END (*RECORD*);

    (* Interaction between the user task and the drive control task is	*)
    (* via several semaphores: MotorStartRequest is for requests to	*)
    (* start the motor, MotorUpToSpeed is for saying when the drive is	*)
    (* at operating speed, and MotorMayBeStopped is for telling the	*)
    (* motor control task that the drive is no longer needed.  There is	*)
    (* a separate motor control task for each motor.			*)

    MotorStartRequest, MotorUpToSpeed, MotorMayBeStopped:
					ARRAY DriveNumber OF Semaphore;

(************************************************************************)
(*		INFORMATION ABOUT THE CONTROLLER STATE			*)
(************************************************************************)

TYPE
    HeadNumber = SHORTCARD [0..1];

VAR
    ShutDownDesired: BOOLEAN;
    OperationDone: Semaphore;
    ResetNeeded: BOOLEAN;
    CalibrationNeeded: ARRAY DriveNumber OF BOOLEAN;
    CurrentCylinder: ARRAY DriveNumber OF CARDINAL;

    (* The controller status record is updated after every interrupt	*)
    (* from the disk controller.  We choose to make it global to the	*)
    (* module, at least in this version, because it is accessed by	*)
    (* several different procedures.					*)

    (* The amount of meaningful information stored here varies with the	*)
    (* operation performed.  The procedure which collects the		*)
    (* information interrogates the disk controller, and accepts as	*)
    (* many status bytes as the controller is prepared to deliver.	*)
    (* If the "valid" field in this record is FALSE, something has gone	*)
    (* wrong with collecting the status information itself.		*)

    (* Most commands produce seven bytes of reply: three bytes of	*)
    (* status information, mostly consisting of error flags, then three	*)
    (* bytes giving the cylinder, head, and sector which was reached at	*)
    (* the end of command execution, and finally a number giving sector	*)
    (* size (the code is log2(#bytes/128).  The Sense Interrupt Status	*)
    (* command produces only two bytes of reply (and these are given	*)
    (* immediately, without producing any interrupt); the first is the	*)
    (* contents of Status Register 0 of the controller, and the second	*)
    (* is the current cylinder number.  The Sense Drive Status command	*)
    (* produces only one byte of reply, as does any invalid command.	*)
    (* The Recalibrate, Specify, and Seek commands do not produce any	*)
    (* reply, but in those cases we can send a Sense Interrupt Status	*)
    (* command to force some status information to be sent.		*)

    (* Incidentally, it often turns out that some of the reply bytes	*)
    (* are worthless, since the controller has the bad habit of just	*)
    (* bouncing back the information about cylinder, etc., which it was	*)
    (* sent, rather than sending data actually read from the disk.	*)
    (* For example, a Seek operation can succeed even if there is no	*)
    (* disk in the drive, which means that it can be a little tricky	*)
    (* working out just why a command failed.  Even the "ready" flags	*)
    (* are useless; contrary to what the controller documentation says,	*)
    (* a drive can appear to be ready even when the door is open and	*)
    (* there is no disk present.					*)

    ControllerStatus:	RECORD
			    valid: BOOLEAN;
			    info: ARRAY [0..6] OF BYTE
			END (*RECORD*);

(************************************************************************)
(*			    DISK MOTOR CONTROL				*)
(************************************************************************)

PROCEDURE MotorControlTask;

    (* Several tasks execute this procedure, one for each drive.  The	*)
    (* client task is expected to perform the operations:		*)
    (*		Signal (MotorStartRequest[drive]);			*)
    (*		Wait (MotorUpToSpeed[drive]);				*)
    (*		do the desired disk operation;				*)
    (*		Signal (MotorMayBeStopped[drive]);			*)
    (* This task responds to the Signal operations by starting and	*)
    (* stopping the drive motor.  The reason for having a separate task	*)
    (* is that we leave the motor running between operations - thereby	*)
    (* avoiding a motor startup delay - as long as new start requests	*)
    (* are coming along frequently enough.  Thus, this task has a job	*)
    (* to do even after the disk operations for the client task have	*)
    (* been completed.							*)

    CONST
	UpToSpeedDelay = 250;  MotorStopDelay = 2000;
	TimeoutCheckInterval = 500;

    VAR drive: DriveNumber;
	TimedOut: BOOLEAN;
	motorlog: Window;

    BEGIN
	(* Preamble: since several tasks are executing this procedure,	*)
	(* we need to work out which drive the current invocation is	*)
	(* supposed to be looking after.				*)

	WITH SharedMotorData DO
	    Obtain (lock);
	    drive := count;  INC (count);
	    Release (lock);
	END (*WITH*);

	IF testing THEN
	    OpenWindow (motorlog, red, white, 4*CARDINAL(drive),
	    		4*CARDINAL(drive)+3, 61, 79, simpleframe, nodivider);
	    Associate (motorlog, Mpage);
	    WriteString (motorlog, "Drive number ");
	    WriteHexByte (motorlog, drive);
	    ChangeScrollingRegion (motorlog, 2, 2);
	END (*IF*);

	(* Each time around the main loop of this procedure, we start	*)
	(* and stop the motor once.  There is an inner loop which keeps	*)
	(* the motor running over several disk operations, as long as	*)
	(* motor start requests arrive sufficiently frequently.		*)

	LOOP	(* until ShutDownDesired *)

	    IF testing THEN
		WriteLn (motorlog);  WriteString (motorlog, "stopped");
	    END (*IF*);

	    (* Wait for a motor start request, periodically checking	*)
	    (* the ShutDownDesired flag.				*)

	    REPEAT
		IF ShutDownDesired THEN EXIT(*LOOP*) END(*IF*);
		TimedWait (MotorStartRequest[drive],
					TimeoutCheckInterval, TimedOut);
	    UNTIL NOT TimedOut;

	    (* A request has arrived.  Start the motor.	*)

	    WITH SharedMotorData DO
		Obtain (lock);
		MotorStatus := IORB(drive, IANDB (0FCH,
			IORB (MotorStatus, MotorControlBit[drive])));
		OutByte (MotorControlPort, MotorStatus);
		Release (lock);
	    END (*WITH*);
	    IF testing THEN
		WriteLn (motorlog);  WriteString (motorlog, "starting");
	    END (*IF*);

	    (* Give the motor time to reach its operating speed.	*)

	    Sleep (UpToSpeedDelay);

	    (* We remain in the following inner loop for as long as	*)
	    (* there is sufficient demand for keeping the motor going.	*)

	    REPEAT	(* until TimedOut *)

		(* Ensure that the drive is selected, and then inform	*)
		(* the client that it is ready to use.			*)

		WITH SharedMotorData DO
		    Obtain (lock);
		    IF IANDB (MotorStatus, 3) <> BYTE(drive) THEN
			MotorStatus := IORB(drive, IANDB (MotorStatus, 0FCH));
			OutByte (MotorControlPort, MotorStatus);
		    END (*IF*);
		    Release (lock);
		END (*WITH*);
		Signal (MotorUpToSpeed[drive]);

		IF testing THEN
		    WriteLn (motorlog);  WriteString (motorlog, "in use");
		END (*IF*);

		(* Wait until the client has finished using the drive.	*)

		Wait (MotorMayBeStopped[drive]);

		IF ShutDownDesired THEN
		    TimedOut := TRUE;
		ELSE

		    (* Here is where we must decide whether to stop the	*)
		    (* motor.  Wait - but not forever - for a new start	*)
		    (* request to arrive.				*)

		    IF testing THEN
			WriteLn(motorlog);WriteString(motorlog, "timing out");
		    END (*IF*);

		    TimedWait (MotorStartRequest[drive], MotorStopDelay,
								TimedOut);

		END (*IF*);

	    UNTIL TimedOut;

	    (* Stop the motor.  At this stage there might well be an	*)
	    (* operation in progress on some other drive, so we must be	*)
	    (* careful not to interfere with the "selected" field of	*)
	    (* the MotorStatus byte.					*)

	    WITH SharedMotorData DO
		Obtain (lock);
		MotorStatus := IANDB (MotorStatus,
				INOTB(MotorControlBit[drive]));
		OutByte (MotorControlPort, MotorStatus);
		Release (lock);
	    END (*WITH*);

	    IF testing THEN
		WriteLn (motorlog);  WriteString (motorlog, "stopped");
	    END (*IF*);

	END (* main LOOP *);

	IF testing THEN
	    CloseWindow (motorlog);
	END (*IF*);

	WITH SharedMotorData DO
	    Obtain (lock);
	    DEC (count);
	    Release (lock);
	END (*WITH*);

    END MotorControlTask;

(************************************************************************)

PROCEDURE CreateMotorControlTasks;

    (* Initialisation procedure: Sets up the motor control tasks, and	*)
    (* the global variables which they use.				*)

    CONST AllowInterrupts = 0CH;

    VAR drive: DriveNumber;

    BEGIN
	ShutDownDesired := FALSE;
	MotorControlBit[0] := 10H;
	MotorControlBit[1] := 20H;

	(* To support four drives, include the statements:
		MotorControlBit[2] := 40H;
		MotorControlBit[3] := 80H;
	*)

	WITH SharedMotorData DO
	    CreateLock (lock);
	    count := 0;
	    MotorStatus := AllowInterrupts;
	END (*WITH*);
	FOR drive := 0 TO MAX(DriveNumber) DO
	    CreateSemaphore (MotorStartRequest[drive], 0);
	    CreateSemaphore (MotorUpToSpeed[drive], 0);
	    CreateSemaphore (MotorMayBeStopped[drive], 0);
	    CreateTask (MotorControlTask, 13, "Floppy motor");
	END (*FOR*);
    END CreateMotorControlTasks;

(************************************************************************)

PROCEDURE ShutDownMotors;

    VAR drive, MotorsRunning: SHORTCARD;

    BEGIN
	ShutDownDesired := TRUE;
	REPEAT
	    Sleep (400);
	    WITH SharedMotorData DO
		Obtain (lock);
		MotorsRunning := count;
		Release (lock);
	    END (*WITH*);
	UNTIL MotorsRunning = 0;
	DestroyLock (SharedMotorData.lock);
	FOR drive := 0 TO MAX(DriveNumber) DO
	    DestroySemaphore (MotorStartRequest[drive]);
	    DestroySemaphore (MotorUpToSpeed[drive]);
	    DestroySemaphore (MotorMayBeStopped[drive]);
	END (*FOR*);
    END ShutDownMotors;

(************************************************************************)
(*			THE BASIC DISK OPERATIONS			*)
(************************************************************************)

PROCEDURE SendCommand (command: BYTE): BOOLEAN;

    (* The basic output operation to the disk controller command port.	*)
    (* It is complicated by the fact that the controller is not always	*)
    (* ready to receive a command, so we might have to busy wait for	*)
    (* the controller's being willing to listen to us.			*)
    (* The function result indicates success; it is FALSE if we were	*)
    (* unable to get the controller to listen.  In that case, this	*)
    (* procedure sets the global flag ResetNeeded.  This procedure also	*)
    (* returns FALSE, without sending anything, if ResetNeeded has	*)
    (* already been set by an earlier entry.				*)

    CONST MaxNumberOfRetries = 10;
	  pauselength = 300;

    VAR patience: [0..MaxNumberOfRetries];

    BEGIN

	(* Exit immediately if a reset is needed.  This can happen if a	*)
	(* command is more than one byte long; if something goes wrong	*)
	(* while sending such a command, we would only make matters	*)
	(* worse by sending the later bytes of that command.		*)

	IF ResetNeeded THEN
	    RETURN FALSE;
	END (*IF*);

	(* There needs to be a brief delay between reading and writing	*)
	(* the controller's data register and a read of its status	*)
	(* register.							*)

	ShortDelay (pauselength);

	(* We see whether the disk controller is idle by reading the	*)
	(* top two bits from its status port.  The idle state is shown	*)
	(* by binary 10 in those bits; the other bits are irrelevant to	*)
	(* us in this case.						*)

	patience := MaxNumberOfRetries;
	WHILE (patience > 0) AND
		(IANDB(InByte(StatusPort), StatusMask) <> ReadyToListen) DO
	    DEC (patience);
	END (*WHILE*);

	(* If the controller is still not idle, give up.	*)

	IF patience = 0 THEN
	    IF testing THEN
		WriteString (log, "SendCommand failure.  Status byte is ");
		WriteHexByte (log, InByte (StatusPort));  WriteLn (log);
	    END (*IF*);
	    ResetNeeded := TRUE;
	    RETURN FALSE;
	END (*IF*);

	(* All OK.  Send the desired command.	*)

	OutByte (DataPort, command);
	RETURN TRUE;

    END SendCommand;

(************************************************************************)

PROCEDURE WaitForCompletion;

    (* This procedure is called after every nontrivial command - that	*)
    (* is, every command which takes some time to complete, as distinct	*)
    (* from the commands which simply request status and which give an	*)
    (* immediate reply - to the disk controller.  We wait for the	*)
    (* interrupt which indicates completion of the operation, and then	*)
    (* if necessary issue a "sense" command to the controller, to make	*)
    (* it return some information about the status of the operation	*)
    (* just completed.  (In some cases, no sense command is needed,	*)
    (* because the controller spontaneously returns the information).	*)
    (* The returned information is left in the global variable called	*)
    (* ControllerStatus.						*)

    CONST SenseInterruptStatus = 8;  BusyBit = 10H;
	TimeoutPeriod = 500;	(* milliseconds *)
	pause1 = 20000;  pause2 = 20000;	(* arbitrary units *)

	(* PAUSELENGTH TESTS WITH 33 MHz PROCESSOR:		*)
	(*	8000 is big enough				*)
	(*	5000 produces intermittent failures		*)
	(*	1000 is too small				*)
	(* Earlier results on a slower processor:		*)
	(*	100 is certainly big enough			*)
	(*	60 failed on some tests				*)
	(*	50 works most of the time			*)
	(*	25 is too small					*)
	(* Have not yet tested the case pause1 <> pause2	*)
	(* The above figures are for the 1.2MB drive.  For my	*)
	(* drive B it seems that a longer pause is needed, so	*)
	(* I've arbitrarily increased the time.			*)

	(* ORIGINAL PAUSELENGTH TESTS WITH 286 PROCESSOR:	*)
	(*	1 was big enough				*)
	(*	0 was far too large!! - error in compiler?	*)

    VAR count: CARDINAL;  TimedOut: BOOLEAN;
	j: CARDINAL;	(* needed only for testing code *)

    BEGIN

	(* Wait for the interrupt.  If the interrupt does not happen	*)
	(* within a reasonable time, return with a "not ready"		*)
	(* indication in the controller status record (since the most	*)
	(* probable reason is that there is no disk in the drive).	*)
	(* If the interrupt occurs, issue a "sense" command if needed.	*)

	TimedWait (OperationDone, TimeoutPeriod, TimedOut);

	IF TimedOut THEN
	    IF testing THEN
		WriteString (log, "WaitForCompletion: timed out.");
		WriteLn (log);
		WriteString (log, "Interrupt masks:   master ");
		WriteHexByte (log, InByte(21H));
		WriteString (log, "   slave ");
		WriteHexByte (log, InByte (0A1H));
		WriteLn (log);
	    END (*IF*);
	    ResetNeeded := TRUE;
	    WITH ControllerStatus DO
		valid := TRUE;  info[0] := 48H;
		info[1] := 0;  info[2] := 0;
	    END (*WITH*);
	    RETURN;
	END (*IF*);

	(* Send a Sense command if needed.  Note that many operations	*)
	(* imply an automatic Sense, in which case it would be wrong to	*)
	(* send the explicit Sense command.  We make the distinction by	*)
	(* checking whether the controller is currently waiting for a	*)
	(* command.							*)

	IF IANDB (InByte(StatusPort),StatusMask) = ReadyToListen THEN
	    IF testing THEN
		WriteString (log, "WaitForCompletion: sending Sense command.");
		WriteLn (log);
	    END (*IF*);
	    IF NOT SendCommand(SenseInterruptStatus) THEN
		IF testing THEN
		    WriteString (log, "WaitForCompletion: Sense command not accepted.");
		    WriteLn (log);
		END (*IF*);
		ControllerStatus.valid := FALSE;
		RETURN;
	    END (*IF*);

	    (* A small delay is needed between writing a command and	*)
	    (* reading the status port.					*)

	    ShortDelay (pause1);

	END (*IF*);

	(* Each time around this loop, we pick up one byte of reply.	*)

	count := 0;
	LOOP

	    (* Read the controller status port to check whether the	*)
	    (* controller is willing to give us some information.	*)

	    IF IANDB (InByte(StatusPort),StatusMask) <> ReadyToSend THEN
		IF testing THEN
		    WriteString (log, "WaitForCompletion: controller not replying.");
		    WriteLn (log);
		    WriteCard (log, count);
		    WriteString (log, " bytes received so far.");
		    WriteLn (log);
		END (*IF*);
		ControllerStatus.valid := FALSE;
		RETURN;
	    END (*IF*);

	    (* OK, the controller is ready, read a byte.	*)

	    ControllerStatus.info[count] := InByte (DataPort);

	    (* A brief pause here, to let the controller get its breath.*)

	    ShortDelay (pause2);

	    (* Check the controller status port again, to see whether	*)
	    (* there is still more information to come.  If not, this	*)
	    (* operation is done and we can return.			*)

	    IF IANDB(InByte(StatusPort),BusyBit) = BYTE(0) THEN
		ControllerStatus.valid := TRUE;
		IF testing THEN
		    WriteString (log, "WaitForCompletion: data bytes are ");
		    FOR j := 0 TO count DO
			WriteChar (log, " ");
			WriteHexByte (log, ControllerStatus.info[j]);
		    END (*FOR*);
		    WriteLn (log);
		END (*IF*);
		RETURN;
	    END (*IF*);

	    (* Yes, more to come.  Go around the loop again, unless	*)
	    (* the controller is putting out more information than it	*)
	    (* should, in which case something is going wrong and the	*)
	    (* controller must be reset.				*)

	    IF count = 6 THEN
		ResetNeeded := TRUE;  ControllerStatus.valid := FALSE;
		RETURN;
	    END (*IF*);

	    INC (count);

	END (*LOOP*);

    END WaitForCompletion;

(************************************************************************)

PROCEDURE ResetController;

    (* This operation is performed when the software and controller	*)
    (* somehow got out of synchronism, so that we need to set the	*)
    (* controller back to a known state.				*)

    CONST TimingParameterSpecification = 3;

    VAR dummy: BOOLEAN;  drive: DriveNumber;

    BEGIN

	(* Pulse the reset bit low, and then let it go high again.	*)

	WITH SharedMotorData DO
	    Obtain (lock);
	    OutByte (MotorControlPort, IANDB(MotorStatus, 0FBH));
	    OutByte (MotorControlPort, MotorStatus);
	    Release (lock);
	END (*WITH*);
	FOR drive := 0 TO MAX(DriveNumber) DO
	    CalibrationNeeded[drive] := TRUE;
	END (*FOR*);
	ResetNeeded := FALSE;
	IF testing THEN
	    WriteString (log, "Reset performed, now waiting for completion.");
	    WriteLn (log);
	END (*IF*);

	(* Wait until the controller has done the operation, and then	*)
	(* check that the reset succeeded.				*)

	WaitForCompletion;
	IF NOT ControllerStatus.valid THEN
	    IF testing THEN
		WriteString (log, "Disk reset failed.");  WriteLn (log);
	    END (*IF*);
	    ResetNeeded := TRUE;

	ELSIF ControllerStatus.info[0] <> BYTE(0C0H) THEN
	    IF testing THEN
		WriteString (log, "Disk reset has failed.  SR0 = ");
		WriteHexByte (log, ControllerStatus.info[0]);  WriteLn (log);
	    END (*IF*);
	    ResetNeeded := TRUE;

	ELSE

	    (* Reset has succeeded.  Now send the controller a command	*)
	    (* which tells it the step rate, head unload time, and head	*)
	    (* load time.						*)

	    dummy := SendCommand (TimingParameterSpecification)
			AND SendCommand (0DFH) AND SendCommand (2);

	END (*IF*);

    END ResetController;

(************************************************************************)

PROCEDURE Recalibrate (drive: DriveNumber): ErrorCode;

    (* Puts the disk head back into a known state, by driving the head	*)
    (* to its outermost position.  This operation is needed whenever	*)
    (* the controller loses track of the head position, which can	*)
    (* happen now and then through cumulative positioning errors.	*)
    (* As usual, the returned value is an error code.			*)
    (* Remark: for an 80-track disk, recalibration can fail because the	*)
    (* hardware doesn't realise that a disk can have that many tracks,	*)
    (* and gives up before stepping the head across the whole 80	*)
    (* tracks.  Rather than check for this special case, which will not	*)
    (* arise very often, we allow the operation to fail.  On a retry,	*)
    (* the operation will succeed because by then the head has already	*)
    (* been moved most of the way to track 0.				*)

    CONST RecalibrateCode = 7;

    VAR success: BOOLEAN;

    BEGIN
	IF testing THEN
	    WriteString (log, "Recalibrating the drive.");  WriteLn (log);
	END (*IF*);

	success := SendCommand (RecalibrateCode) AND SendCommand (drive);
	IF ResetNeeded OR NOT success THEN
	    RETURN ControllerNotListening;
	END (*IF*);

	(* Check the status reply, to see whether the recalibration	*)
	(* has succeeded.  If not, a reset is needed.			*)

	WaitForCompletion;

	IF ControllerStatus.valid
		AND (ControllerStatus.info[0] = BYTE(SeekEnd + drive))
		AND (ControllerStatus.info[1] = BYTE(0))
	  THEN
	    CurrentCylinder[drive] := 0;  CalibrationNeeded[drive] := FALSE;
	    RETURN OK;
	ELSE
	    ResetNeeded := TRUE;
	    RETURN CalibrationFailure;
	END (*IF*);
    END Recalibrate;

(************************************************************************)

PROCEDURE Seek (drive: DriveNumber; cylinder: CARDINAL): ErrorCode;

    (* Drives the disk read/write head to the desired cylinder.  The	*)
    (* value returned is an error code (OK if no errors).		*)

    CONST SeekCode = 0FH;
	StepsPerCylinder = 1;

    VAR success: BOOLEAN;  result: ErrorCode;  position: BYTE;

    BEGIN

	(* Check for some special cases: illegal cylinder number,	*)
	(* recalibration needed, or position already OK.		*)

	IF cylinder >= DriveInfo[drive].NumberOfCylinders THEN
	    RETURN IllegalBlockNumber;
	END (*IF*);
	IF CalibrationNeeded[drive] THEN
	    result := Recalibrate (drive);
	    IF result <> OK THEN
		RETURN result;
	    END (*IF*);
	END (*IF*);
	IF cylinder = CurrentCylinder[drive] THEN
	    IF testing THEN
		WriteString (log, "Seek: already on desired cylinder.");
		WriteLn (log);
	    END (*IF*);
	    RETURN OK;
	END (*IF*);

	position := BYTE(cylinder*StepsPerCylinder);

	(* The command to the disk controller is a sequence of three	*)
	(* bytes, whose meaning is, I hope, obvious from the parameters	*)
	(* mentioned in the next statement.				*)
	(* NOTE: according to the documentation, the second byte of the	*)
	(* command should specify which head as well as which drive,	*)
	(* and the same for the SR0 value when we read back the		*)
	(* interrupt status.  From tests, it appears that we should not	*)
	(* specify the head, and that the "head" bit in SR0 is never	*)
	(* set after a seek.						*)

	success := SendCommand (SeekCode) AND SendCommand (drive)
			AND SendCommand (position);
	IF NOT success THEN
	    RETURN ControllerNotListening;
	END (*IF*);

	WaitForCompletion;
	IF NOT ControllerStatus.valid THEN
	    RETURN ControllerOutOfSync;
	ELSIF (ControllerStatus.info[0] <> BYTE(SeekEnd + drive))
			OR (ControllerStatus.info[1] <> position) THEN
	    IF testing THEN
		WriteString (log, "Seek error, needed reply ");
		WriteHexByte (log, SeekEnd+drive);
		WriteString(log, " ");
		WriteHexByte (log, position);
		WriteString (log, ", obtained reply ");
		WriteHexByte (log, ControllerStatus.info[0]);
		WriteString(log, " ");
		WriteHexByte (log, ControllerStatus.info[1]);
		WriteLn(log);
	    END (*IF*);
	    CalibrationNeeded[drive] := TRUE;
	    RETURN SeekFailure;
	ELSE
	    CurrentCylinder[drive] := cylinder;
	    RETURN OK;
	END (*IF*);

    END Seek;

(************************************************************************)

PROCEDURE DumpIDinformation (drive: DriveNumber;  head: HeadNumber);

    (* Reads current sector information from the disk, which might help	*)
    (* us determine more about the cause of an error.			*)

    (* Remark: my tests so far suggest that we don't get any error	*)
    (* information this way, but we do seem to get back information	*)
    (* about the current track and sector.				*)

    CONST ReadIDCommand = 4AH;

    VAR success: BOOLEAN;

    BEGIN

	success := SendCommand(ReadIDCommand)
			AND SendCommand(BYTE(4*head + drive));

	IF NOT success THEN
	    IF testing THEN
		WriteString (log, "Can't read ID information.");
		WriteLn (log);
	    END (*IF*);
	END (*IF*);

	WaitForCompletion;

	(* The information which we want is dumped to the screen by	*)
	(* procedure WaitForCompletion.					*)

    END DumpIDinformation;

(************************************************************************)

PROCEDURE DoTheOperation (operation: CARDINAL;  drive: DriveNumber;
				head: HeadNumber;
				cylinder, sector: CARDINAL): ErrorCode;

    (* Performs a disk operation (read=1, write=2) assuming that the	*)
    (* motor is running, the read/write head has been driven to the	*)
    (* correct cylinder, and the DMA controller is ready to go.		*)
    (* Notice that there is no specification of how much data to	*)
    (* transfer.  That detail is handled by the DMA controller, which	*)
    (* has already been set up before this procedure was called.	*)

    CONST ReadCommand = 0E6H;  WriteCommand = 0C5H;

    VAR opcode: BYTE;  success: BOOLEAN;
	SectorsTransferred: CARDINAL;

    BEGIN
	OutByte (DataRatePort, DriveInfo[drive].DataRate);
	CASE operation OF
	    1:	opcode := ReadCommand;
	  |
	    2:	opcode := WriteCommand;
	END (*CASE*);

	(* The operation is performed by sending the disk controller a	*)
	(* command string starting with "opcode".  The significance of	*)
	(* the remaining bytes will, I believe, be obvious from the	*)
	(* statement below.						*)

	success := SendCommand(opcode) AND SendCommand(4*head + drive)
			AND SendCommand(BYTE(cylinder)) AND SendCommand(head)
			AND SendCommand(BYTE(sector))
			AND SendCommand(2)	(* sector size code *)
			AND SendCommand(BYTE(DriveInfo[drive].SectorsPerTrack))
			AND SendCommand(DriveInfo[drive].SectorGap)
			AND SendCommand(DataLengthCode);

	IF NOT success THEN
	    RETURN ControllerNotListening;
	END (*IF*);

	WaitForCompletion;
	WITH ControllerStatus DO

	    IF NOT valid THEN
		RETURN ControllerOutOfSync;
	    END(*IF*);

	    IF ORD(IANDB(info[1], 2)) <> 0 THEN
		RETURN WriteProtected;
	    END (*IF*);

	    IF (ORD(IANDB(info[1], 5)) <> 0)
			OR (ORD(IANDB(info[2], 12H)) <> 0) THEN
		CalibrationNeeded[drive] := TRUE;
		RETURN SectorNotFound;
	    END (*IF*);

	    IF (ORD(IANDB(info[1], 20H)) <> 0)
			OR (ORD(IANDB(info[2], 21H)) <> 0) THEN
		RETURN BadData;
	    END (*IF*);

	    IF (info[1] <> BYTE(0)) OR (info[2] <> BYTE(0)) THEN
		RETURN UndiagnosedFailure;
	    END (*IF*);

	    (* In the following test, we mask out the "head" bit of	*)
	    (* status register 0, because it does confusing things at	*)
	    (* the end of a multi-sector read.				*)

	    IF IANDB(info[0], 0FBH) <> BYTE(drive) THEN
		RETURN DriveNotReady;
	    END (*IF*);

	    (* The next little calculation works out how much data was	*)
	    (* actually transferred.  Entries 3,4,5 of the "info" field	*)
	    (* give the ending cylinder, head, and sector respectively.	*)
	    (* Remark: we never use this information except when testing*)

	    SectorsTransferred := DriveInfo[drive].SectorsPerTrack*
				((ORD(info[3])-cylinder)*NumberOfHeads
						+CARDINAL(info[4])-ORD(head))
			+ ORD(info[5]) - sector;
	    IF testing THEN
		WriteCard (log, SectorsTransferred);
		WriteString (log, " sectors were transferred.");
		WriteLn (log);
	    END (*IF*);

	    RETURN OK;

	END (*WITH*);

    END DoTheOperation;

(************************************************************************)
(*			THE DISK INTERRUPT TASK				*)
(************************************************************************)

PROCEDURE DiskInterruptTask;

    (* Responds to the disk interrupts.  The interrupt could occur for	*)
    (* a variety of reasons, depending on what operation was in		*)
    (* progress.  Rather than sort that out here, we simply perform a	*)
    (* semaphore Signal to wake up whichever task was waiting for the	*)
    (* interrupt.							*)

    VAR status: BYTE;

    BEGIN
	LOOP (*FOREVER*)
	    WaitForInterrupt;
	    status := InByte(StatusPort);

	    (* We occasionally get an interrupt with 0 in the status	*)
	    (* port, and I can find nothing in the data sheets to	*)
	    (* explain this - it is perhaps a power-on interrupt.  In	*)
	    (* any case, it is certainly not a "command done" signal,	*)
	    (* therefore we can afford to ignore it.			*)

	    IF status <> BYTE(0) THEN
		Signal (OperationDone);
	    END (*IF*);
	END (*LOOP*);
    END DiskInterruptTask;

(************************************************************************)
(*			DATA TRANSFER PROCEDURES			*)
(************************************************************************)

PROCEDURE DiskOperation (VAR (*INOUT*) details: RequestBlock);

    (* Performs a disk transfer as specified in the request block.	*)

    CONST DMAchannel = 2;  DefaultSectorSize = 512;

    VAR command: CARDINAL;  drive: DriveNumber;  BlockNumber: CARDINAL;
	head: HeadNumber;  cylinder, sector: CARDINAL;
	result: ErrorCode;

    BEGIN
	WITH details DO
	    CASE operation OF
		verify:		command := 0;
		|
		read:		command := 1;
		|
		write:		command := 2;
		|
		physicalread:	ByteCount := DefaultSectorSize;
				command := 1;
		|
		physicalwrite:	ByteCount := DefaultSectorSize;
				command := 2;
		|
		ELSE
			Status := FeatureNotImplemented;
			RETURN;
	    END (*CASE*);
	    drive := SHORTCARD(unit);
	END (*WITH*);

	Signal (MotorStartRequest [drive]);

	(* Convert the block number to the head/cylinder/sector form	*)
	(* which the hardware expects.  The relationship is		*)
	(*  BlockNumber = SectorsPerTrack*(NumberOfHeads*cylinder+head)	*)
	(*				+ sector - 1			*)
	(* Note that sector number starts from 1, while the numbering	*)
	(* of blocks, heads, and cylinders all start from 0.		*)

	BlockNumber := CARDINAL (details.BlockNumber);
	WITH DriveInfo[drive] DO
	    sector := BlockNumber MOD SectorsPerTrack + 1;
	    cylinder := BlockNumber DIV SectorsPerTrack;
	END (*WITH*);
	head := SHORTCARD(cylinder MOD CARDINAL(NumberOfHeads));
	cylinder := cylinder DIV NumberOfHeads;

	Wait (MotorUpToSpeed [drive]);
	result := Seek (drive, cylinder);
	IF testing THEN
	    WriteString (log, "Return from procedure Seek");
	    WriteLn (log);
	END (*IF*);
	IF result = OK THEN
	    IF testing THEN
		WriteString (log, "Successful completion of seek operation");
		WriteLn (log);
	    END (*IF*);

	    (* The seek has apparently been successful.  Set up the	*)
	    (* DMA controller.						*)

	    IF testing THEN
		WriteString (log, "Setting up DMA, buffer address is ");
		WriteAddress (log, details.BufferAddress);
		WriteLn (log);
	    END (*IF*);

	    WITH details DO
		IF NOT CheckDMAAddress (BufferAddress, ByteCount) THEN
		    Signal (MotorMayBeStopped [drive] );
		    Status := BadDMAAddress;
		    RETURN;
		END (*IF*);
		LoadDMAparameters (DMAchannel, command, BufferAddress,
							ByteCount);
	    END (*WITH*);

	    (* For a write operation, wait for the head settling time.	*)
	    (* For a read we do not bother, since there is a chance	*)
	    (* that the head will have settled anyway by the time the	*)
	    (* desired sector comes around, and the rare problems will	*)
	    (* be solved when we re-try the operation.			*)

	    IF command = 2 THEN
		Sleep (CARDINAL(DriveInfo[drive].HeadSettlingTime));
	    END (*IF*);

	    (* All OK so far, perform the actual transfer.	*)

	    result := DoTheOperation (command, drive, head, cylinder, sector);
	    IF result <> OK THEN
		DumpIDinformation (drive, head);
	    END (*IF*);
	ELSE
	    IF testing THEN
		WriteString (log, "Seek failed");
		WriteLn (log);
	    END (*IF*);
	END (*IF*);

	Signal (MotorMayBeStopped [drive] );
	IF ResetNeeded THEN
	    ResetController;
	END (*IF*);
	details.Status := result;

    END DiskOperation;

(************************************************************************)
(*			INTERFACE TO THE FILE SYSTEM			*)
(************************************************************************)

PROCEDURE DiskRequestHandler;

    (* This procedure runs as an independent task.  Each time around	*)
    (* its main loop, it picks up one enqueued I/O request and executes	*)
    (* the request.  The requests are placed on the queue by calls to	*)
    (* module Devices.  On completion of an operation, we inform the	*)
    (* caller by performing a Signal on a user-specified semaphore.	*)

    CONST MaxNumberOfRetries = 3;  StopCode = 0FFFFH;

    VAR reply: ErrorCode;  retries: CARDINAL;
	RequestPointer: RequestBlockPointer;
	string: ARRAY [0..31] OF CHAR;

    BEGIN
	LOOP
	    RequestPointer := AcceptRequest (floppy);
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
		IF testing AND (reply <> OK) THEN
			WriteString (log, "DISK ERROR: error code ");
			TranslateErrorCode (reply, string);
			WriteString (log, string);  WriteLn (log);
		END (*IF*);
		INC (retries);
	    UNTIL (reply = OK) OR (retries>MaxNumberOfRetries);
	    Signal (RequestPointer^.DoneSemaphorePointer^);
	END (*LOOP*);
    END DiskRequestHandler;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

PROCEDURE InitialiseDriveParameters (drive: DriveNumber;  type: DriveType);

    (* Sets up the information we need about the given drive.		*)
    (* Notes:								*)
    (*    1.	The Sector Gap values shown are for read/write/verify.	*)
    (*		When formatting a disk, use 054H for a 1.2MB disk, and	*)
    (*		050H for a 360KB disk.					*)
    (*    2.	I have not yet allowed for the possibility of a 360K	*)
    (*		disk in a 1.2M drive.  In that case, use 023H for the	*)
    (*		sector gap, and a data rate code of 1.			*)

    BEGIN
	WITH DriveInfo[drive] DO
	    drivetype := type;
	    CASE type OF
	 	    0:		(* no drive present *)
			SectorsPerTrack := 9;
			NumberOfCylinders := 0;
			SectorGap := 02AH;  DataRate := 2;
			HeadSettlingTime := 20;
		|
		    1:		(* 360KB drive *)
			SectorsPerTrack := 9;
			NumberOfCylinders := 40;
			SectorGap := 02AH;  DataRate := 2;
			HeadSettlingTime := 20;
		|
		    2:		(* 1.2MB drive *)
			SectorsPerTrack := 15;
			NumberOfCylinders := 80;
			SectorGap := 01BH;  DataRate := 0;
			HeadSettlingTime := 15;
		|
		    4:		(* 1.44MB drive - still have to fix details *)
				(* Note: for a disk formatted at low	*)
				(* density, the parameters for case 1	*)
				(* seem to work.			*)
			SectorsPerTrack := 18;
			NumberOfCylinders := 80;
			SectorGap := 01BH;  DataRate := 0;
			HeadSettlingTime := 15;
		|
		    ELSE	(* unknown type *)
			SectorsPerTrack := 9;
			NumberOfCylinders := 0;
			SectorGap := 02AH;  DataRate := 2;
			HeadSettlingTime := 20;
	    END (*CASE*);
	END (*WITH*);
    END InitialiseDriveParameters;

(************************************************************************)

PROCEDURE SetupDriveInformation;

    (* Sets up the information we need about each drive.  In this	*)
    (* version, we get information from CMOS about the type of the	*)
    (* first two drives, but we can't say much about the other two.	*)

    VAR DriveTypeCode: BYTE;

    BEGIN
	DriveTypeCode := ReadCMOS (10H);

	IF testing THEN
	    WriteString (log, "DriveTypeCode from CMOS is ");
	    WriteHexByte (log, DriveTypeCode);
	    WriteString (log, " hexadecimal.");
	    WriteLn (log);
	END (*IF*);

	InitialiseDriveParameters (0, RS(CARDINAL(DriveTypeCode), 4));
	InitialiseDriveParameters (1, DriveType(IANDB(DriveTypeCode, 0FH)));

	(* The following two calls should be included only if four	*)
	(* drives are to be supported.					*)
(*
	InitialiseDriveParameters (2, UnknownDriveType);
	InitialiseDriveParameters (3, UnknownDriveType);
*)

    END SetupDriveInformation;

(************************************************************************)
(*				SHUTDOWN				*)
(************************************************************************)

PROCEDURE ShutDown;

    (* Brings the tasks in this module to an orderly halt.  This is to	*)
    (* guard against the possibly catastrophic consequences of program	*)
    (* termination while a disk operation is still in progress.		*)

    VAR Sync: Semaphore;  RequestRecord: RequestBlock;
	DontPanic: Window;

    BEGIN
	OpenSimpleWindow (DontPanic, 11, 13, 33, 45);
	WriteString (DontPanic, "DON'T PANIC");
	IF testing THEN
	    WriteString (log, "Starting the shutdown operation.");
	    WriteLn (log);
	END (*IF*);

	(* Send a shutdown request to the task which deals with the	*)
	(* queue of floppy disk operation requests.			*)

	CreateSemaphore (Sync, 0);
	WITH RequestRecord DO
	    operation := shutdown;
	    device := floppy;
	    unit := 0;
	    DoneSemaphorePointer := ADR (Sync);
	END (*WITH*);
	IOrequest (RequestRecord);
	Wait (Sync);
	DestroySemaphore (Sync);

	IF testing THEN
	    WriteString (log, "About to close down the motor control tasks.");
	    WriteLn (log);
	END (*IF*);

	(* Close down the motor control tasks, and destroy the		*)
	(* semaphores belonging to them.				*)

	ShutDownMotors;

	IF testing THEN
	    WriteString (log, "Motor control tasks are now stopped.");
	    WriteLn (log);
	END (*IF*);

	(* Dispose of miscellaneous global variables.	*)

	DestroySemaphore (OperationDone);
	CloseWindow (DontPanic);

    END ShutDown;

(************************************************************************)
(*			MODULE INITIALISATION				*)
(************************************************************************)

BEGIN
    IF testing THEN
	CreateMaintenancePage (Mpage);
	OpenWindow (log, black, green, 0,23, 0,60, simpleframe, doubledivider);
	Associate (log, Mpage);
	WriteString (log,"         Diagnostic output from floppy disk driver");
	ChangeScrollingRegion (log, 3, 22);
    END (*IF*);

    SetupDriveInformation;

    (* Create the motor control tasks, and the information they need.	*)

    CreateMotorControlTasks;

    CreateSemaphore (OperationDone, 0);
    CreateInterruptTask (DiskInterrupt, DiskInterruptTask, "Floppy int");
    IF testing THEN
	WriteString (log, "Have now installed the interrupt task.");
	WriteLn (log);
	WriteString (log, "Interrupt masks:   master ");
	WriteHexByte (log, InByte(21H));
	WriteString (log, "   slave ");
	WriteHexByte (log, InByte (0A1H));
	WriteLn (log);
    END (*IF*);
    ResetController;
    floppy := InstallDeviceDriver (1);
    DeviceName (floppy, 0, "A", 0, "");
    DeviceName (floppy, 1, "B", 0, "");
    CreateTask (DiskRequestHandler, 14, "Floppy request");
    SetTerminationProcedure (ShutDown);

END Floppy.
