IMPLEMENTATION MODULE Logger;

	(********************************************************)
	(*							*)
	(*		Logging data to disk file		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	17 August 1994			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

(* Depending on what device you are using for logging, you may need	*)
(* to import only one of the following two modules.			*)

IMPORT Floppy;
(*IMPORT HardDisk;*)

FROM Files IMPORT
    (* type *)	File,
    (* proc *)	OpenFile, CloseFile, WriteByte;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode,
    (* proc *)	TranslateErrorCode;

FROM Keyboard IMPORT
    (* proc *)	InKey, PutBack;

FROM MaintenancePages IMPORT
    (* type *)	MaintenancePage,
    (* proc *)	CreateMaintenancePage, Associate;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteLn, EditString,
		SetCursor, PressAnyKey, WriteChar;

FROM NumericIO IMPORT
    (* proc *)	ReadBufferedCardinal, WriteRJCard;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM LossyQueues IMPORT
    (* type *)	LossyQueue,
    (* proc *)	CreateQueue, PutQueue, GetQueue;

FROM TaskControl IMPORT
    (* proc *)	CreateTask;

FROM Timer IMPORT
    (* proc *)	Sleep;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

FROM Mouse IMPORT
    (* proc *)	MouseAvailable;

FROM UserInterface IMPORT
    (* type *)	UIWindow, Capability, CapabilitySet,
    (* proc *)	AllowMouseControl;

(************************************************************************)

CONST testing = TRUE;
VAR Mpage: MaintenancePage;
    heading, debug: Window;

CONST
    ElementCapacity = 4;

TYPE

    (* For a lossy queue element, there are two special cases:	*)
    (*		count = -1	means open the log file		*)
    (*		count = 0	means close the log file	*)

    LossyQueueElement = RECORD
			    count: INTEGER;
			    datum: ARRAY [0..ElementCapacity-1] OF INTEGER;
			END (*RECORD*);

VAR

    (* The file to which data are being written.	*)

    log: File;

    (* The name of the log file.	*)

    filename: ARRAY [0..31] OF CHAR;

    (* The queue holding the data to be sent to the output file.	*)

    Lossy: LossyQueue;

    (* A screen window which is displayed while logging is in progress.	*)

    LogWindow: Window;

    (* Semaphore to control access to the log window.	*)

    LogWindowAccess: Semaphore;

    (* Second log window for testing purposes.  *)

    Log2Window: Window;

    (* Semaphore to synchronise file opening.	*)

    FileOpenHandled: Semaphore;

    (* Logging=TRUE while data logging is in progress.  AbortRequested	*)
    (* can be set to TRUE to request that logging be terminated.	*)

    Logging, AbortRequested: BOOLEAN;

    (* A count of the number of data values yet to be collected.	*)

    LogCount: CARDINAL;

    (* Count of data which could not be sent to disk because the output	*)
    (* queue overflowed.						*)

    DataLost: CARDINAL;

    (* Count of output errors.  We abort logging if it gets too large.	*)

    ErrorCount: CARDINAL;

    (* Flags involved in shutting down the module.	*)

    ShutDownDesired: BOOLEAN;
    ShutDownCompleted: Semaphore;

(************************************************************************)
(*			    ERROR MESSAGE HANDLER			*)
(************************************************************************)

PROCEDURE ReportError (code: ErrorCode);

    (* Puts out a screen message about the I/O error, stops logging if	*)
    (* error count gets too large.					*)

    CONST ErrorLimit = 20;

    VAR ErrorWindow: Window;  dummy: UIWindow;
	messbuffer: ARRAY [0..39] OF CHAR;

    BEGIN
	OpenWindow (ErrorWindow, white, blue, 14, 17, 22, 58,
					doubleframe, nodivider);
	IF MouseAvailable() THEN
	    dummy := AllowMouseControl (ErrorWindow, "Disk error message",
				CapabilitySet {wshow, wmove, whide});
	END (*IF*);
	WriteString (ErrorWindow, "Disk error ");
	TranslateErrorCode (code, messbuffer);
	WriteString (ErrorWindow, messbuffer);
	INC (ErrorCount);
	IF ErrorCount > ErrorLimit THEN
	    WriteLn (ErrorWindow);
	    WriteString (ErrorWindow, "Too many errors, aborting logging.");
	    ErrorCount := 0;  AbortRequested := TRUE;
	END (*IF*);
	PressAnyKey (ErrorWindow);  CloseWindow (ErrorWindow);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Return from ReportError");
	END (*IF*);
    END ReportError;

(************************************************************************)
(*				FILE OUTPUT				*)
(************************************************************************)

PROCEDURE PutChar (ch: CHAR);

    (* Sends one character to the log file.	*)

    VAR errorcode: ErrorCode;

    BEGIN
	IF testing THEN
	    IF ch = CHR(13) THEN
		WriteLn (Log2Window);
	    ELSIF ch <> CHR(10) THEN
		WriteChar (Log2Window, ch);
	    END (*IF*);
	END (*IF*);
	errorcode := WriteByte (log, ch);
	IF errorcode <> OK THEN
	    ReportError (errorcode);
	END (*IF*);
    END PutChar;

(************************************************************************)

PROCEDURE PutInteger (value: INTEGER);

    (* Converts value to ASCII, sends it to the log file.	*)

    BEGIN
	IF value < 0 THEN
	    PutChar ("-");  value := -value;
	END (*IF*);
	IF value > 9 THEN
	    PutInteger (value DIV 10);
	    value := value MOD 10;
	END (*IF*);
	PutChar (CHR(ORD("0")+ORD(value)));
    END PutInteger;

(************************************************************************)
(*		     SENDING QUEUED DATA TO THE LOG FILE		*)
(************************************************************************)

PROCEDURE LogTask;

    (* Takes data from the lossy queue, converts it to ASCII and sends	*)
    (* it to the log file.  Data received while the log file is closed	*)
    (* (this is possible, if logging is aborted) are discarded.		*)

    CONST CR = CHR(13);  LF = CHR(10);

    VAR j: CARDINAL;  status: ErrorCode;
	buffer: LossyQueueElement;
	LogFileOpen: BOOLEAN;
	w: Window;  dummy: UIWindow;

    BEGIN
	IF testing THEN
	    OpenWindow (Log2Window,white,red,15,24,0,47,simpleframe,nodivider);
	    Associate (Log2Window, Mpage);
	    WriteString (Log2Window, "Log2Window opened.");
	    WriteLn (Log2Window);
	    OpenWindow (w,white,black, 11,14,0,79,simpleframe,nodivider);
	    Associate (w, Mpage);
	    IF MouseAvailable() THEN
		dummy := AllowMouseControl (Log2Window, "Log task testing #2",
				CapabilitySet {wshow, wmove, whide});
		dummy := AllowMouseControl (w, "Log task progress",
				CapabilitySet {wshow, wmove, whide});
	    END (*IF*);
	    WriteString (w, "LogTask started.");  WriteLn (w);
	END (*IF*);
	LogFileOpen := FALSE;
	LOOP	(* until file closed and ShutDownDesired *)
	    GetQueue (Lossy, buffer);
	    IF testing THEN
		WriteLn(w);
		WriteString(w, "Return from GetQueue.");
	    END (*IF*);
	    WITH buffer DO
		IF (count = -1) THEN
		    IF testing THEN
			WriteString(w, ".. count is -1");
		    END (*IF*);
		    ErrorCount := 0;
		    IF NOT LogFileOpen THEN
			IF testing THEN
			    WriteLn (w);
			    WriteString(w, "About to open log file");
			END (*IF*);
			status := OpenFile (log, filename, TRUE);
			IF testing THEN
			    WriteLn (w);
			    WriteString(w, "Have opened log file");
			END (*IF*);
			IF status = OK THEN
			    LogFileOpen := TRUE;
			ELSE
			    ReportError (status);  AbortRequested := TRUE;
			END (*IF*);
		    END (*IF*);
		    Signal (FileOpenHandled);
		ELSIF count = 0 THEN
		    IF testing THEN
			WriteString(w, ".. count is zero");
		    END (*IF*);
		    IF LogFileOpen THEN
			IF testing THEN
			    WriteLn(w);
			    WriteString(w, "Closing log file.");
			END (*IF*);
			CloseFile (log);  LogFileOpen := FALSE;
		    END (*IF*);
		    IF ShutDownDesired THEN
			IF testing THEN
			    WriteLn(w);
			    WriteString(w, "Stopping LogTask.");
			END (*IF*);
			EXIT (*LOOP*);
		    END (*IF*);
		ELSIF LogFileOpen THEN
		    FOR j := 0 TO CARDINAL(count)-1 DO
			PutInteger (datum[j]);
			PutChar (" ");
		    END (*FOR*);
		    PutChar (CR);  PutChar (LF);
		ELSE
		    AbortRequested := TRUE;
		END (*IF*);
	    END (*WITH*);
	END (*LOOP*);
	IF testing THEN
	    WriteLn(w);
	    WriteString(w, "Left LogTask main loop.");
	END (*IF*);
	IF testing THEN
	    WriteLn(w);
	    WriteString(w, "LogTask is terminating.");
	    CloseWindow (w);
	END (*IF*);
	Signal (ShutDownCompleted);
    END LogTask;

(************************************************************************)

PROCEDURE PutSpecialBuffer (code: INTEGER);

    (* Queues a command to open or close the log file.  This procedure	*)
    (* is called only by user tasks and initialisation code.		*)

    VAR buffer: LossyQueueElement;

    BEGIN
	buffer.count := code;
	LOOP
(*
	    IF testing THEN
		WriteLn (debug);
		WriteString (debug, "Putting special buffer element.");
	    END (*IF*);
*)
	    IF PutQueue (Lossy, buffer) THEN
		EXIT (*LOOP*);
	    END (*IF*);
(*
	    IF testing THEN
		WriteString (debug, " - Put was unsuccessful.");
	    END (*IF*);
*)
	    Sleep (1);
	END (*LOOP*);
(*
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Leaving procedure PutSpecialBuffer.");
	END (*IF*);
*)
    END PutSpecialBuffer;

(************************************************************************)
(*			THE USER-CALLABLE PROCEDURES			*)
(************************************************************************)

PROCEDURE StartLogging(): BOOLEAN;

    (* Opens the log file.  Returns FALSE if open failed.		*)

    CONST Return = CHR(13);

    VAR ch: CHAR;  dummy: UIWindow;

    BEGIN
	IF Logging THEN
	    Wait (LogWindowAccess);
	    SetCursor (LogWindow, 1, 1);
	    WriteString (LogWindow, "Logging is already in progress.");
	    Signal (LogWindowAccess);
	ELSE
	    AbortRequested := FALSE;
	    Wait (LogWindowAccess);
	    OpenWindow (LogWindow,white,red,0,5,0,47,simpleframe,nodivider);
	    IF MouseAvailable() THEN
		dummy := AllowMouseControl (LogWindow, "Logger status",
				CapabilitySet {wshow, wmove, whide});
	    END (*IF*);
	    WriteString (LogWindow, "Enter the name of the log file:");
	    WriteLn (LogWindow);
	    EditString (LogWindow, filename, HIGH(filename));
	    ch := InKey();
	    IF ch <> Return THEN PutBack(ch) END(*IF*);
	    WriteLn (LogWindow);
	    Signal (LogWindowAccess);
	    PutSpecialBuffer (-1);
	    Wait (FileOpenHandled);
	    IF AbortRequested THEN
		LogCount := 0
	    ELSE
		Wait (LogWindowAccess);
		WriteString (LogWindow, "Maximum number of readings to log?");
		LogCount := ReadBufferedCardinal (LogWindow, 6);
		Signal (LogWindowAccess);
		ch := InKey();
		IF ch <> Return THEN PutBack(ch) END(*IF*);
	    END (*IF*);
	    Logging := TRUE;
	    IF LogCount = 0 THEN
		StopLogging;
	    ELSE
		DataLost := 0;
		Wait (LogWindowAccess);
		SetCursor (LogWindow, 4, 1);
		WriteString (LogWindow, "Data lost:      0");
		Signal (LogWindowAccess);
	    END (*IF*);
	END (*IF*);
	RETURN Logging;
    END StartLogging;

(************************************************************************)

PROCEDURE Log (VAR (*IN*) data: ARRAY OF INTEGER);

    (* Sends the data to the log file (assumed to be already open).	*)
    (* This is done via a lossy queue.					*)

    CONST CR = CHR(13);  LF = CHR(10);

    VAR j: CARDINAL;  success: BOOLEAN;
	buffer: LossyQueueElement;

    BEGIN
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Entering procedure Log");
	END (*IF*);
	IF AbortRequested THEN
	    StopLogging;
	END (*IF*);
	IF NOT Logging THEN RETURN END (*IF*);
	WITH buffer DO
	    count := HIGH(data)+1;
	    IF count > ElementCapacity THEN
		count := ElementCapacity;
	    END (*IF*);
	    FOR j := 0 TO CARDINAL(count-1) DO
		datum[j] := data[j];
	    END (*FOR*);
	END (*WITH*);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Log: calling PutQueue");
	END (*IF*);
	success := PutQueue (Lossy, buffer);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Log: return from PutQueue");
	END (*IF*);
	IF NOT success THEN
	    INC (DataLost);
	    Wait (LogWindowAccess);
	    SetCursor (LogWindow, 4, 12);
	    WriteRJCard (LogWindow, DataLost, 6);
	    Signal (LogWindowAccess);
	    RETURN;
	END (*IF*);
	DEC (LogCount);
	Wait (LogWindowAccess);
	SetCursor (LogWindow, 3, 35);  WriteRJCard (LogWindow, LogCount, 6);
	Signal (LogWindowAccess);
	IF LogCount = 0 THEN
	    StopLogging;
	END (*IF*);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Leaving procedure Log");
	END (*IF*);
    END Log;

(************************************************************************)

PROCEDURE StopLogging;

    (* Closes the log file, after writing out any pending data.		*)

    BEGIN
	IF Logging THEN
	    PutSpecialBuffer (0);
	    Wait (LogWindowAccess);
	    CloseWindow (LogWindow);
	    Signal (LogWindowAccess);
	    Logging := FALSE;  AbortRequested := FALSE;
	END (*IF*);
    END StopLogging;

(************************************************************************)
(*			     PROGRAM CLOSEDOWN				*)
(************************************************************************)

PROCEDURE CloseDown;

    BEGIN
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Starting closedown of logger.");
	END (*IF*);
	StopLogging;
	ShutDownDesired := TRUE;
	PutSpecialBuffer(0);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Waiting for logger shutdown to complete.");
	END (*IF*);
	Wait (ShutDownCompleted);
	IF testing THEN
	    WriteLn (debug);
	    WriteString (debug, "Logger has shut down.");
	END (*IF*);
    END CloseDown;

(************************************************************************)
(*			   MODULE INITIALISATION			*)
(************************************************************************)

VAR dummy: UIWindow;

BEGIN
    IF testing THEN
	CreateMaintenancePage (Mpage);
	OpenWindow (heading, black, white, 0,2, 0,79, simpleframe, nodivider);
	OpenWindow (debug, black, white, 18,23, 0,79, simpleframe, nodivider);
	Associate (heading, Mpage);
	Associate (debug, Mpage);
	IF MouseAvailable() THEN
	    dummy := AllowMouseControl (debug, "Logger debugging",
				CapabilitySet {wshow, wmove, whide});
	END (*IF*);
	SetCursor (heading, 1, 22);
	WriteString (heading,"Diagnostic output from Logger module");
	WriteString (debug, "Logger: initialisation code executing");
    END (*IF*);
    Logging := FALSE;  ShutDownDesired := FALSE;  AbortRequested := FALSE;
    SetTerminationProcedure (CloseDown);
    CreateSemaphore (FileOpenHandled, 0);
    CreateSemaphore (ShutDownCompleted, 0);
    CreateSemaphore (LogWindowAccess, 1);
    filename := "A:LOG.DAT";
    CreateQueue (Lossy, 100, SIZE(LossyQueueElement));
    CreateTask (LogTask, 2, "Log task");
END Logger.
