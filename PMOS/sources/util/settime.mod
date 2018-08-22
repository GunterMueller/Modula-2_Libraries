MODULE SetTime;

	(****************************************************************)
	(*								*)
	(*		  Setting the real-time clock			*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	18 March 1995				*)
	(*  Status:		Working					*)
	(*	Note: no provision at present for setting the century;	*)
	(*	this version of the program always sets the century	*)
	(*	to 19.							*)
	(*								*)
	(****************************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE;

FROM TimeOfDay IMPORT
    (* type *)	TimeRecord,
    (* proc *)	ReadClock, SetClock, SetBinaryMode;

FROM MiscPMOS IMPORT
    (* proc *)	WriteCMOS;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteChar,
		SetCursor, EraseLine, Blink, ColourSwap;

FROM NumericIO IMPORT
    (* proc *)	WriteHexByte;

FROM Lib IMPORT
    (* proc *)	Dos;

FROM Keyboard IMPORT
    (* proc *)	InKey;

FROM Timer IMPORT
    (* proc *)	Sleep;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* proc *)	CreateTask;

FROM LowLevel IMPORT
    (* proc *)	IANDB, IORB;

FROM MiscPMOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)	EnterCriticalSection, LeaveCriticalSection;

(************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST
    Nul = CHR(0);  Esc = CHR(27);
    Digits = CharSet {'0'..'9'};

TYPE
    subscript = [0..8];
    subset = SET OF subscript;

VAR
    (* LocalTime is the local copy of the time record, as we edit it.	*)
    (* It is periodically updated from the "true" time supplied by	*)
    (* module CMOS.  LTprotect gives critical section protection.	*)

    LocalTime: TimeRecord;
    LTprotect: Semaphore;

    (* Many of the procedures in this module work as if LocalTime were	*)
    (* an array of 9 bytes.  Variable 'protected' gives the array	*)
    (* subscript of the entry which must not be automatically updated	*)
    (* since it is being edited, and 'blinking' is the array subscript	*)
    (* of the entry which should display as a blinking field.		*)

    protected, blinking: subscript;

    (* Window editwindow is shared among the display and editing	*)
    (* procedures.  Semaphore ScreenAccess protects access to it.	*)

    editwindow: Window;
    ScreenAccess: Semaphore;

(************************************************************************)
(*			    SCREEN OPERATIONS				*)
(************************************************************************)

PROCEDURE LocateCursor (j: subscript;  VAR (*OUT*) row, column: CARDINAL);

    (* Returns the screen coordinates of the place where LocalTime[j]	*)
    (* is displayed.							*)

    BEGIN
	CASE j OF
	    0:	row := 2;  column := 12;	(* ticks (not shown) *)
	  |
	    1:	row := 4;  column := 9;		(* seconds *)
	  |
	    2:	row := 4;  column := 6;		(* minutes *)
	  |
	    3:	row := 4;  column := 3;		(* hours *)
	  |
	    4:	row := 2;  column := 1;		(* day of week *)
	  |
	    5:	row := 3;  column := 3;		(* day of month *)
	  |
	    6:	row := 3;  column := 6;		(* month *)
	  |
	    7:	row := 3;  column := 9;		(* year *)
	END (*CASE*);
    END LocateCursor;

(************************************************************************)

PROCEDURE InitialDisplay;

    BEGIN
	SetCursor (editwindow, 2, 3);  WriteChar (editwindow, '=');
	SetCursor (editwindow, 3, 5);  WriteString (editwindow, "/  /");
	SetCursor (editwindow, 4, 5);  WriteString (editwindow, ":  :");
    END InitialDisplay;

(************************************************************************)
(*	     PROCEDURES TO WORK ON A TIME RECORD AS AN ARRAY		*)
(************************************************************************)

PROCEDURE Get (VAR (*IN*) array: ARRAY OF BYTE;  j: subscript): BYTE;

    (* Returns the j'th entry of array.	*)

    BEGIN
	RETURN array[j];
    END Get;

(************************************************************************)

PROCEDURE Put (value: BYTE;  VAR (*OUT*) array: ARRAY OF BYTE;  j: subscript);

    (* Stores value in array[j].	*)

    BEGIN
	array[j] := value;
    END Put;

(************************************************************************)
(*		    TASK TO DISPLAY THE DATE AND TIME			*)
(************************************************************************)

PROCEDURE WriteDay (w: Window; dayinweek: BYTE);

    (* Writes the name of the day *)

    BEGIN
	CASE ORD(dayinweek) OF
	    1:	WriteString (w, "Mon");
	  |
	    2:	WriteString (w, "Tues");
	  |
	    3:	WriteString (w, "Wednes");
	  |
	    4:	WriteString (w, "Thurs");
	  |
	    5:	WriteString (w, "Fri");
	  |
	    6:	WriteString (w, "Satur");
	  |
	    7:	WriteString (w, "Sun");
	  |
	    ELSE
		WriteHexByte (w, dayinweek);
	END (*CASE*);
	WriteString (w, "day");
    END WriteDay;

(************************************************************************)

PROCEDURE ShowTime;

    (* Continuously displays the time and date on the screen.	*)

    VAR w: Window;  row, column: CARDINAL;
	j: subscript;  value: BYTE;
	copyoftime: TimeRecord;

    BEGIN
	LOOP
	    Sleep (500);
	    Wait (LTprotect);
	    ReadClock (copyoftime);
	    FOR j := 1 TO 7 DO
		IF j <> protected THEN
		    value := Get (copyoftime, j);
		    Put (value, LocalTime, j);
		    Wait (ScreenAccess);
		    LocateCursor (j, row, column);
		    SetCursor (editwindow, row, column);
		    WriteHexByte (editwindow, value);
		    IF j = blinking THEN
			Blink (editwindow, row, column, 2);
		    END (*IF*);
		    IF j = 4 THEN
			SetCursor (editwindow, row, column+3);
			WriteDay (editwindow, value);
			EraseLine (editwindow, 1);
		    END (*IF*);
		    Signal (ScreenAccess);
		END (*IF*);
	    END (*FOR*);
	    Signal (LTprotect);
	END (*LOOP*);
    END ShowTime;

(************************************************************************)
(*			 EDITING THE DATE AND TIME			*)
(************************************************************************)

PROCEDURE CursorMovement (VAR (*INOUT*) j: subscript);

    (* On entry, a Nul has already been read from the keyboard, so we	*)
    (* presume that that was the start of an arrow-key code.  Adjusts	*)
    (* the field number j appropriately.  Any keyboard code which makes	*)
    (* no sense as a cursor movement is treated as a no-operation.	*)
    (* To understand the cursor movements, note that the fields on the	*)
    (* screen display are laid out in the form:				*)
    (*		4		Looks strange, I know, but that's	*)
    (*		5  6  7		a compromise between what CMOS returns	*)
    (*		3  2  1		and the preferred human-readable form	*)

    VAR ch: CHAR;

    BEGIN
	ch := InKey();
	IF ch = "H" THEN				(* cursor up *)
	    IF j IN subset {5,6,7} THEN j := 4
	    ELSIF j<>4 THEN j := 8-j
	    END (*IF*);
	ELSIF ch = "P" THEN				(* cursor down *)
	    IF j = 4 THEN INC(j)
	    ELSIF j IN subset {5,6,7} THEN j := 8-j
	    END (*IF*);
	ELSIF ch = "M" THEN				(* cursor right *)
	    IF j IN subset {5,6} THEN INC(j)
	    ELSIF j IN subset {2,3} THEN DEC(j)
	    END (*IF*);
	ELSIF ch = "K" THEN				(* cursor left *)
	    IF j IN subset {1,2} THEN INC(j)
	    ELSIF j IN subset {6,7} THEN DEC(j)
	    END (*IF*);
	END (*IF*);
    END CursorMovement;

(************************************************************************)

PROCEDURE WriteHighlighted (j: subscript;  value: BYTE);

    (* Writes value on the screen, in reverse video, in the location	*)
    (* reserved for field j.						*)

    VAR row, column: CARDINAL;

    BEGIN
	LocateCursor (j, row, column);
	Wait (ScreenAccess);
	SetCursor (editwindow, row, column);
	WriteHexByte (editwindow, value);
	ColourSwap (editwindow, row, column, 2);
	Signal (ScreenAccess);
    END WriteHighlighted;

(************************************************************************)
(*			   EDITING PROCEDURES				*)
(************************************************************************)

PROCEDURE EditField (j: subscript;  ch: CHAR);

    (* Edits field j, where the user has already typed character ch.	*)

    VAR	value: BYTE;

    BEGIN
	protected := j;

	(* Use ch to update the first digit of the current value. *)

	value := IORB (IANDB (Get (LocalTime, j), 0FH),
				BYTE (16*(ORD(ch)-ORD('0'))));
	WriteHighlighted (j, value);

	(* Now pick up and use the second digit. *)

	ch := InKey();
	IF ch IN Digits THEN
	    value := IORB (IANDB (value, 0F0H), BYTE (ORD(ch)-ORD('0')));
	    WriteHighlighted (j, value);
	END (*IF*);

	(* Update the master record of the time.  *)

	Wait (LTprotect);
	Put (value, LocalTime, j);
	Put (0, LocalTime, 0);
	SetClock (LocalTime);
	Signal (LTprotect);

	protected := 0;
    END EditField;

(************************************************************************)

PROCEDURE Edit;

    (* The master editing routine.  Allows the keyboard user to adjust	*)
    (* the displayed date and time fields.				*)

    VAR j: subscript;
	row, col: CARDINAL;
	ch: CHAR;

    BEGIN
	j := 4;
	LOOP
	    blinking := j;
	    ch := InKey ();
	    blinking := 0;
	    IF ch = Esc THEN EXIT(*LOOP*)
	    ELSIF ch IN Digits THEN EditField(j, ch)
	    ELSIF ch = Nul THEN CursorMovement (j)
	    END(*IF*);
 	END (*LOOP*);
    END Edit;

(************************************************************************)
(*			SETTING THE MS-DOS DATE/TIME			*)
(************************************************************************)

PROCEDURE binary (BCDvalue: BYTE): CARDINAL;

    (* Converts a BCD value to binary.	*)

    VAR val: CARDINAL;

    BEGIN
	val := CARDINAL (BCDvalue);
	RETURN 10*(val DIV 16) + val MOD 16;
    END binary;

(************************************************************************)

PROCEDURE SetDosTime;

    (* Sets the date and time kept by MS-DOS to agree with the values	*)
    (* held by CMOS.  Note the important critical section problem: the	*)
    (* operations of setting the date and setting the time are separate	*)
    (* DOS functions, but of course we must do the setting in one	*)
    (* indivisible operation to avoid a problem which can occur at	*)
    (* midnight.							*)

    VAR copyoftime: TimeRecord;
	args: RegisterPacket;
	savedPSW: CARDINAL;

    BEGIN
	ReadClock (copyoftime);
	savedPSW := EnterCriticalSection();
	WITH copyoftime DO
	    WITH args DO
		DH := BYTE(binary (month));
		DL := BYTE(binary (dayofmonth));
		CX := 100*binary(century) + binary(year);
		AH := 43;	(* Set Date *)
		AL := 0;
	    END (*WITH*);
	    Dos (args);
	    WITH args DO
		CH := BYTE(binary (hours));
		CL := BYTE(binary (minutes));
		DH := BYTE(binary (seconds));
		DL := BYTE((25*binary(ticks) + 2) DIV 4);
		AH := 45;	(* Set Time *)
	    END (*WITH*);
	    Dos (args);
	END (*WITH*);
	LeaveCriticalSection (savedPSW);
    END SetDosTime;

(************************************************************************)
(*			     MAIN PROCEDURE				*)
(************************************************************************)

PROCEDURE SetTheClock;

    VAR message: Window;

    BEGIN
	OpenWindow (message, blue, white, 5, 17, 14, 65, simpleframe,nodivider);
	SetCursor (message, 1, 9);
	WriteString (message, "Setting the CMOS real-time clock");
	SetCursor (message, 2, 4);
	WriteString (message,
			"This version also sets the MS-DOS date/time");
	SetCursor (message, 10, 13);
	WriteString (message, "Use the Esc key to exit");

	(* Temporary arrangement - the century field is constant.	*)

	WriteCMOS (032H, 019H);
	Put (019H, LocalTime, 8);

	(* Now let the user do any modifications.	*)
	
	Edit;

	CloseWindow (message);
    END SetTheClock;

(************************************************************************)
(*			    INITIALIZATION				*)
(************************************************************************)

BEGIN
    CreateSemaphore (ScreenAccess, 1);
    CreateSemaphore (LTprotect, 1);
    OpenWindow (editwindow, red, white, 9,14, 33,46, doubleframe, nodivider);
    SetBinaryMode (FALSE);
    protected := 0;  blinking := 0;
    InitialDisplay;
    CreateTask (ShowTime, 2, "Show time");
    SetTheClock;
    SetDosTime;
END SetTime.
