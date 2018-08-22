IMPLEMENTATION MODULE SerialMouse;

	(********************************************************)
	(*							*)
	(*		  Serial mouse driver			*)
	(*							*)
	(*	This module provides support for a mouse on	*)
	(*	COM1 or COM2.  Note that this is not the	*)
	(*	only mouse driver in PMOS.  The PP preprocessor	*)
	(*	chooses which driver to use based on the	*)
	(*	settings in file PP.CFG				*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	16 March 1995			*)
	(*  Status:		OK				*)
	(*							*)
	(*	Note: this file contains preprocessor		*)
	(*	directives; use PP to update it.		*)
	(*							*)
	(********************************************************)

FROM Types IMPORT
    (* type *)	FarBytePointer;

FROM Mouse0 IMPORT
    (* type *)	Buttons, Events;

FROM LowLevel IMPORT
    (* proc *)	IANDB, IORB, RSB, LSB, ROLB, OutByte,
		MakePointer, FarAddOffset, FarSubtractOffset;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateTask, CreateLock, Obtain, Release;

FROM SerialIO IMPORT
    (* type *)	Parity,
    (* proc *)	InitSerialChannel, MouseReset, ReadSerial;

FROM TextVideo IMPORT
    (* proc *)	VideoKind;

(************************************************************************)
(*			DUMMY EVENT HANDLER				*)
(************************************************************************)

    (*<TopSpeed3*)
    (*# save, call(c_conv => off, same_ds => off, near_call => off) *)
    (*# call(reg_param => (ax,bx,cx,dx,st0,st6,st5,st4,st3)) *)
    (*>*)

PROCEDURE DummyEventHandler (A: EventSet;  B: ButtonSet;  X, Y : CARDINAL);

    (* Does nothing - this is here purely because the compiler doesn't	*)
    (* recognise NULLPROC as being type-compatible with an event	*)
    (* handler compiled when the call pragma c_conv is set.		*)

    BEGIN
    END DummyEventHandler;

    (*<TopSpeed3*) (*# restore *) (*>*)

(************************************************************************)
(*			     GLOBAL DATA				*)
(************************************************************************)

TYPE
    MouseType = (NoMouse, MS, Logitech, PC);

CONST
    (*<MouseChannel=COM1*) MouseChannel = 1; (*>*)
    (*<MouseChannel=COM2 MouseChannel = 2; >*)

    (*<MouseKind=Logitech MouseKind = Logitech; >*)
    (*<MouseKind=MS MouseKind = MS; >*)
    (*<MouseKind=PC MouseKind = PC; >*)
    (*<(MouseKind<>Logitech)&(MouseKind<>MS)&(MouseKind<>PC)*)
    MouseKind = NoMouse;
    (*>*)

TYPE
    Parser = PROCEDURE (VAR ButtonSet, VAR SHORTINT, VAR SHORTINT);
    SignedByte = RECORD
		    CASE :BOOLEAN OF
			FALSE:	b: BYTE;
		      |	TRUE:	i: SHORTINT;
		    END (*CASE*);
		 END (*RECORD*);

CONST
    pageincr = 4096;
    rowincr = 160;

VAR
    (* Segment of the memory-mapped display, and whether it is monochrome. *)

    ScreenSeg: CARDINAL;
    BlackAndWhite: BOOLEAN;

    (* Flag to indicate that a mouse is present. *)

    MousePresent: BOOLEAN;

    (* Number of buttons on the mouse. *)

    NumberOfButtons: CARDINAL;

    (* The procedure to be called to decode the mouse data.  We give	*)
    (* this a value during module initialisation.			*)

    ParseMouseInput: Parser;

    (* A decoding array to help in translating mouse data to button	*)
    (* values.  We set up the values during module initialisation.	*)

    ButtonMap: ARRAY SHORTCARD[0..7] OF ButtonSet;

    (* Flag to say that we expect four bytes (rather than the normal	*)
    (* three) of mouse data.						*)

    ExpectingFourBytes: BOOLEAN;

    (* The current state of the mouse, updated by the interpreter task.	*)

    MouseState: RECORD
		    access: Lock;
		    ButtonsDown: ButtonSet;
		    X, Y: CARDINAL;
		END (*RECORD*);

    (* The rectangle defining the boundaries of mouse movement. *)

    Limits: RECORD
		access: Lock;
		left, right, top, bottom: CARDINAL;
	    END (*RECORD*);

    (* Mappings for translating button states to events.  These	*)
    (* values are filled in during module initialisation.	*)

    Map1, Map2: ARRAY Buttons OF Events;

    (* The caller-specified event handler. *)

    UserHandler: RECORD
		     access: Lock;
		     trigger: EventSet;
		     HandlerProc: EventHandler;
		 END (*RECORD*);

    (* Details about the screen cursor. *)

    Cursor: RECORD
		access: Lock;
		mousepage, row, col: CARDINAL;
		ScreenPos: FarBytePointer;
		ValueUnder: BYTE;
	    END (*RECORD*);

    (* The screen cursor is visible iff Visibility > 0. *)

    Visibility: INTEGER;

    (* A tuning parameter which controls the point at which cursor	*)
    (* movement is sped up.						*)

    Threshold: INTEGER;
    	
(************************************************************************)
(*		MANIPULATING THE MOUSE CURSOR ON THE SCREEN		*)
(************************************************************************)

PROCEDURE TurnCursorOff;

    (* Turns the screen cursor off, assuming it was on. *)

    BEGIN
	WITH Cursor DO
	    Obtain (access);
	    IF BYTE(ScreenPos^) = ROLB(ValueUnder,4) THEN
		ScreenPos^ := ValueUnder;
	    END (*IF*);
	    Release (access);
	END (*WITH*);
    END TurnCursorOff;

(************************************************************************)

PROCEDURE TurnCursorOn;

    (* Turns the screen cursor on, assuming it was off. *)

    BEGIN
	WITH Cursor DO
	    Obtain (access);
	    ValueUnder := ScreenPos^;
	    ScreenPos^ := ROLB(ValueUnder,4);
	    Release (access);
	END (*WITH*);
    END TurnCursorOn;

(************************************************************************)

PROCEDURE MoveCursor (r, c: INTEGER);

    (* Moves the cursor to screen row r and column c. *)

    VAR change: INTEGER;

    BEGIN
	IF Visibility > 0 THEN TurnCursorOff END(*IF*);
	WITH Cursor DO
	    Obtain (access);
	    change := rowincr*(r-INTEGER(row)) + 2*(c-INTEGER(col));
	    IF change >= 0 THEN
		ScreenPos := FarAddOffset (ScreenPos, change);
	    ELSE
		ScreenPos := FarSubtractOffset (ScreenPos, -change);
	    END (*IF*);
	    row := r;  col := c;
	    Release (access);
	END (*WITH*);
	IF Visibility > 0 THEN TurnCursorOn END(*IF*);
    END MoveCursor;

(************************************************************************)
(*		INPUT PARSERS FOR SPECIFIC MOUSE TYPES			*)
(************************************************************************)

PROCEDURE GetCode(): BYTE;

    (* Reads one byte from the mouse port. *)

    VAR value: BYTE;

    BEGIN
	ReadSerial (MouseChannel, value);
	RETURN value;
    END GetCode;

(************************************************************************)

PROCEDURE ParseLogitechInput (VAR (*OUT*) buttons: ButtonSet;
				VAR (*OUT*) Xmove, Ymove: SHORTINT);

    (* Reads a single "unit" of information from the mouse port, and	*)
    (* translates this into a button state and two motion values.	*)
    (* For the Logitech mouse I don't know the exact rules, but they	*)
    (* are roughly as shown in the following code.			*)

    (* THIS PROCEDURE IS WORKING, BUT IT'S NOT CLEAR WHETHER IT'S DONE.	*)
    (* I can't work out whether the coding I'm assuming is "true"	*)
    (* Logitech, or just the MS mode of a Logitech mouse.		*)

    VAR code: BYTE;  v3: SignedByte;

    BEGIN
	code := GetCode();

	(* It's possible that at this stage we have just picked up the	*)
	(* fourth byte of a four-byte sequence.				*)

	IF code = BYTE(20H) THEN
	    WITH MouseState DO
		Obtain (access);
		buttons := ButtonsDown + ButtonSet{MiddleButton};
		Release (access);
	    END (*WITH*);
	    ExpectingFourBytes := TRUE;
	    Xmove := 0;  Ymove := 0;
	    RETURN;
	END (*IF*);

	(* The first 7-bit number picked up should have its high-order	*)
	(* bit set, but we have to allow for resynchronisation after an	*)
	(* error in the input.						*)

	WHILE ORD(IANDB (code, 040H)) = 0 DO
	    code := GetCode();
	END (*WHILE*);

	(* The code just picked up has bit 6 set, and bits 5 and 4 as	*)
	(* indicators for the left and right buttons.  Bits 3-0 are	*)
	(* the high-order bits of the motion values.			*)

	buttons := ButtonMap[RSB(IANDB(code,30H),4)];

	(* The next two input codes are 6-bit numbers, to which we must	*)
	(* prepend some of the bits from the first byte.		*)

	v3.b := IORB(GetCode(), LSB(code,6));
	Xmove := v3.i;
	v3.b := IORB(GetCode(), LSB(IANDB(code, 0CH),4));
	Ymove := v3.i;

	(* The fourth byte, if present, is 20H for middle button down,	*)
	(* and 00 for a middle button release.				*)

	IF ExpectingFourBytes THEN
	    IF GetCode() = BYTE(20H) THEN
		INCL (buttons, MiddleButton);
	    ELSE
		ExpectingFourBytes := FALSE;
	    END (*IF*);
	END (*IF*);

    END ParseLogitechInput;

(************************************************************************)

PROCEDURE ParseMSInput (VAR (*OUT*) buttons: ButtonSet;
				VAR (*OUT*) Xmove, Ymove: SHORTINT);

    (* Reads a single "unit" of information from the mouse port, and	*)
    (* translates this into a button state and two motion values.	*)
    (* For a Microsoft mouse a "unit" is three 7-bit codes, where the	*)
    (* first code has its high-order bit set.				*)

    VAR code: BYTE;  v3: SignedByte;

    BEGIN

	(* The first 7-bit number picked up should have its high-order	*)
	(* bit set, but we have to allow for resynchronisation after an	*)
	(* error in the input.						*)

	REPEAT
	    code := GetCode();
	UNTIL ORD(IANDB (code, 040H)) <> 0;

	(* The code just picked up has bit 6 set, and bits 5 and 4 as	*)
	(* indicators for the left and right buttons.  (The middle	*)
	(* button doesn't seem to be detected, so I guess the Microsoft	*)
	(* encoding doesn't allow for three-button mice.)  Bits 3-0 are	*)
	(* the high-order bits of the motion values.			*)

	buttons := ButtonMap[RSB(IANDB(code,30H),4)];

	(* The next two input codes are 6-bit numbers, to which we must	*)
	(* prepend some of the bits from the first byte.		*)

	v3.b := IORB(GetCode(), LSB(code,6));
	Xmove := v3.i;
	v3.b := IORB(GetCode(), LSB(IANDB(code, 0CH),4));
	Ymove := v3.i;

    END ParseMSInput;

(************************************************************************)

PROCEDURE ParsePCInput (VAR (*OUT*) buttons: ButtonSet;
				VAR (*OUT*) Xmove, Ymove: SHORTINT);

    (* Reads a single "unit" of information from the mouse port, and	*)
    (* translates this into a button state and two motion values.	*)
    (* For the PC mouse a "unit" is five bytes long, with the first	*)
    (* byte having the binary form 10000xxx.				*)

    VAR value: SignedByte;

    BEGIN
	REPEAT
	    value.b := GetCode();
	UNTIL IANDB (value.b, 0F8H) = BYTE(80H);

	buttons := ButtonMap[IANDB(value.b,7)];

	value.b := GetCode();
	Xmove := value.i;
	value.b := GetCode();
	Ymove := -value.i;

	(* In this version we ignore the last two bytes.  (I don't know	*)
	(* what they indicate.)						*)

	value.b := GetCode();
	value.b := GetCode();

    END ParsePCInput;

(************************************************************************)
(*			THE INTERPRETER TASK				*)
(************************************************************************)

PROCEDURE ButtonEvents (oldbuttons, newbuttons: ButtonSet): EventSet;

    (* Calculates the events corresponding to a change in button state	*)
    (* from "oldbuttons" to "newbuttons".				*)

    VAR B: Buttons;  result: EventSet;

    BEGIN
	result := EventSet {};
	FOR B := LeftButton TO MiddleButton DO
	    IF B IN newbuttons-oldbuttons THEN
		INCL (result, Map1[B]);
	    ELSIF B IN oldbuttons-newbuttons THEN
		INCL (result, Map2[B]);
	    END (*IF*);
	END (*FOR*);
	RETURN result;
    END ButtonEvents;

(************************************************************************)

PROCEDURE MouseInterpreter;

    (* Runs as a separate task, picking up signals from the mouse and	*)
    (* updating the state appropriately.				*)

    VAR buttons, oldbuttons: ButtonSet;  events, enabled: EventSet;
	Xmove, Ymove, temp: INTEGER;
	shortXmove, shortYmove: SHORTINT;
	oldX, newX, oldY, newY: INTEGER;
	Handler: EventHandler;

    BEGIN
	LOOP
	    (* Obtain the next mouse input string. *)

	    ParseMouseInput (buttons, shortXmove, shortYmove);
	    Xmove := VAL(INTEGER, shortXmove);
	    Ymove := VAL(INTEGER, shortYmove);

	    (* It's convenient for the user if we amplify the magnitude	*)
	    (* of fast mouse movements.					*)

	    IF ABS(Xmove) > Threshold THEN
		Xmove := 3*Xmove;
	    END (*IF*);
	    IF ABS(Ymove) > Threshold THEN
		Ymove := 2*Ymove;
	    END (*IF*);

	    (* Update the mouse state. *)

	    WITH MouseState DO
		Obtain (access);
		oldbuttons := ButtonsDown;
		ButtonsDown := buttons;
		oldX := X;  oldY := Y;
		WITH Limits DO
		    Obtain (access);

		    temp := oldX + Xmove;
		    IF temp < VAL(INTEGER,left) THEN newX := left
		    ELSIF temp > VAL(INTEGER,right) THEN newX := right
		    ELSE newX := temp
		    END (*IF*);

		    temp := oldY + Ymove;
		    IF temp < VAL(INTEGER,top) THEN newY := top
		    ELSIF temp > VAL(INTEGER,bottom) THEN newY := bottom
		    ELSE newY := temp
		    END (*IF*);

		    Release (access);
		END (*WITH*);
		X := newX;  Y := newY;
		Release (access);
	    END (*WITH*);

	    (* Turn the (oldbuttons,buttons) combination into an	*)
	    (* "EventSet" value, and move the physical cursor if	*)
	    (* necessary.						*)

	    events := ButtonEvents (oldbuttons, buttons);
	    IF (newX DIV 8 <> oldX DIV 8) OR (newY DIV 8 <> oldY DIV 8) THEN
		INCL (events, Motion);
		MoveCursor (newY DIV 8, newX DIV 8);
	    END (*IF*);

	    (* Call the event handler if appropriate. *)

	    WITH UserHandler DO
		Obtain (access);
		Handler := HandlerProc;
		enabled := trigger;
		Release (access);
	    END (*WITH*);
	    IF (enabled*events <> EventSet{})
				AND (Handler <> DummyEventHandler) THEN
		Handler (events, buttons, newX, newY);
	    END (*IF*);

	END (*LOOP*);

    END MouseInterpreter;

(************************************************************************)
(*		    THE EXTERNALLY CALLABLE PROCEDURES			*)
(************************************************************************)

PROCEDURE Reset (VAR (*OUT*) MousePresent: BOOLEAN;
			VAR (*OUT*) NumberOfButtons: CARDINAL);

    (* Initializes mouse, returning MousePresent as FALSE if no mouse	*)
    (* available and as TRUE if it is, and NumberOfButtons as the	*)
    (* number of buttons for the mouse if installed.			*)

    BEGIN
	NumberOfButtons := 3;
	IF MouseKind = Logitech THEN
	    InitSerialChannel (MouseChannel, 1200, 7, NoParity, 1);
	ELSIF MouseKind = MS THEN
	    InitSerialChannel (MouseChannel, 1200, 7, Always1, 1);
	    NumberOfButtons := 2;

	    (* The following reset operation should have no effect on a	*)
	    (* Microsoft mouse, but should force a Logitech mouse to	*)
	    (* enter Microsoft compatibility mode.			*)

	    (* MouseReset (MouseChannel); *)

	    (* THIS CODE IS NOT WORKING, IT LOOKS AS IF THE INFORMATION	*)
	    (* I'VE RECEIVED IS WRONG.					*)

	ELSIF MouseKind = PC THEN
	    InitSerialChannel (MouseChannel, 1200, 8, NoParity, 1);
	END (*IF*);
	MousePresent := TRUE;
	Visibility := 0;
	SetCursorPos (320, 100);
    END Reset;

(************************************************************************)

PROCEDURE SetCursorPos (X, Y : CARDINAL);

    (* Initialises the mouse position. *)

    BEGIN
	Obtain (MouseState.access);
	MouseState.X := X;  MouseState.Y := Y;
	Release (MouseState.access);
	IF Visibility > 0 THEN TurnCursorOff END(*IF*);
	WITH Cursor DO
	    Obtain (access);
	    row := Y DIV 8;  col := X DIV 8;
	    ScreenPos := MakePointer (ScreenSeg,
				pageincr*mousepage + rowincr*row + 2*col + 1);
	    Release (access);
	END (*WITH*);
	IF Visibility > 0 THEN TurnCursorOn END(*IF*);
    END SetCursorPos;

(************************************************************************)

PROCEDURE ShowCursor;

    (* Makes the mouse cursor visible on the screen. *)

    BEGIN
	IF Visibility = 0 THEN TurnCursorOn END(*IF*);
	INC (Visibility);
    END ShowCursor;

(************************************************************************)

PROCEDURE HideCursor;

    (* Makes the mouse cursor invisible. *)

    BEGIN
	DEC (Visibility);
	IF Visibility = 0 THEN TurnCursorOff END(*IF*);
    END HideCursor;

(************************************************************************)

PROCEDURE SetPage (page: CARDINAL);

    (* Sets the hardware screen page where the mouse is visible. *)

    VAR change: INTEGER;

    BEGIN
	IF Visibility > 0 THEN TurnCursorOff END(*IF*);
	WITH Cursor DO
	    Obtain (access);
	    IF page > mousepage THEN
		ScreenPos := FarAddOffset (ScreenPos, pageincr*(page-mousepage));
	    ELSE
		ScreenPos := FarSubtractOffset(ScreenPos,
					pageincr*(mousepage-page));
	    END (*IF*);
	    mousepage := page;
	    Release (access);
	END (*WITH*);
	IF Visibility > 0 THEN TurnCursorOn END(*IF*);
    END SetPage;

(************************************************************************)

PROCEDURE SetHorizontalLimits (MinX, MaxX : CARDINAL);

    (* Sets the horizontal cursor limits. *)

    BEGIN
	WITH Limits DO
	    Obtain (access);
	    left := MinX;  right := MaxX;
	    Release (access);
	END (*WITH*);
    END SetHorizontalLimits;

(************************************************************************)

PROCEDURE SetVerticalLimits (MinY, MaxY : CARDINAL);

    (* Sets the vertical cursor limits. *)

    BEGIN
	WITH Limits DO
	    Obtain (access);
	    top := MinY;  bottom := MaxY;
	    Release (access);
	END (*WITH*);
    END SetVerticalLimits;

(************************************************************************)

PROCEDURE GetPosBut (VAR (*OUT*) buttons: ButtonSet;
				VAR (*OUT*) Xposition, Yposition: CARDINAL);

    (* Returns the current mouse position and the state of the buttons.	*)
    (* Note: the units here are not the same as for procedure		*)
    (* GetTextMousePosition.  In both this procedure and in the event	*)
    (* handlers the position is presented in units of 1/8th of a	*)
    (* character width or height.					*)

    BEGIN
	WITH MouseState DO
	    Obtain (access);
	    buttons := ButtonsDown;
	    Xposition := X;  Yposition := Y;
	    Release (access);
	END (*WITH*);
    END GetPosBut;

(************************************************************************)

PROCEDURE SetEventHandler (DetectedEvents: EventSet;
					Handler: EventHandler);

    (* Nominates the procedure to be called whenever an event in the	*)
    (* set DetectedEvents occurs.					*)

    BEGIN
	WITH UserHandler DO
	    Obtain (access);
	    HandlerProc := Handler;
	    trigger := DetectedEvents;
	    Release (access);
	END (*WITH*);
    END SetEventHandler;

(************************************************************************)
(*			MODULE INITIALISATION				*)
(************************************************************************)

PROCEDURE SetUpMappings;

    (* Initializes some of the global variables. *)

    BEGIN

	IF MouseKind = Logitech THEN
	    ParseMouseInput := ParseLogitechInput;
	    Threshold := 20;
	    ButtonMap[0] := ButtonSet {};
	    ButtonMap[1] := ButtonSet {RightButton};
	    ButtonMap[2] := ButtonSet {LeftButton};
	    ButtonMap[3] := ButtonSet {LeftButton, RightButton};
	ELSIF MouseKind = MS THEN
	    ParseMouseInput := ParseMSInput;
	    NumberOfButtons := 2;
	    Threshold := 10;
	    ButtonMap[0] := ButtonSet {};
	    ButtonMap[1] := ButtonSet {RightButton};
	    ButtonMap[2] := ButtonSet {LeftButton};
	    ButtonMap[3] := ButtonSet {LeftButton, RightButton};
	ELSIF MouseKind = PC THEN
	    ParseMouseInput := ParsePCInput;
	    Threshold := 2;
	    ButtonMap[0] := ButtonSet {LeftButton,MiddleButton,RightButton};
	    ButtonMap[1] := ButtonSet {LeftButton, MiddleButton};
	    ButtonMap[2] := ButtonSet {LeftButton, RightButton};
	    ButtonMap[3] := ButtonSet {LeftButton};
	    ButtonMap[4] := ButtonSet {MiddleButton, RightButton};
	    ButtonMap[5] := ButtonSet {MiddleButton};
	    ButtonMap[6] := ButtonSet {RightButton};
	    ButtonMap[7] := ButtonSet {};
	END (*IF*);

	Map1[LeftButton] := LeftDown;
	Map1[RightButton] := RightDown;
	Map1[MiddleButton] := MiddleDown;

	Map2[LeftButton] := LeftUp;
	Map2[RightButton] := RightUp;
	Map2[MiddleButton] := MiddleUp;

    END SetUpMappings;

(************************************************************************)

PROCEDURE InitialiseMouseDriver(): BOOLEAN;

    (* Does all initialisation needed for this module.  We make this a	*)
    (* procedure rather than an initialisation section because module	*)
    (* Mouse has to decide which mouse driver to use.  The function	*)
    (* result indicates success; if it is FALSE, none of the following	*)
    (* procedures will work.  Note: this is not an end-user procedure,	*)
    (* it's intended to be called only by module Mouse.			*)

    BEGIN
	VideoKind (ScreenSeg, BlackAndWhite);
	ExpectingFourBytes := FALSE;
	WITH Limits DO
	    CreateLock (access);
	    left := 0;  right := 639;
	    top := 0;  bottom := 199;
	END (*WITH*);
	WITH MouseState DO
	    CreateLock (access);
	    ButtonsDown := ButtonSet{};
	    X := 320;  Y := 100;
	END (*WITH*);
	Visibility := 0;
	WITH Cursor DO
	    CreateLock (access);
	    mousepage := 0;  row := 12;  col := 40;
	    ScreenPos := MakePointer (ScreenSeg, rowincr*row + 2*col + 1);
	    ValueUnder := ScreenPos^;
	END (*WITH*);
	SetUpMappings;
	WITH UserHandler DO
	    CreateLock (access);
	    HandlerProc := DummyEventHandler;
	    trigger := EventSet{};
	END (*WITH*);
	CreateTask (MouseInterpreter, 9, "Mouse Interprete");
	RETURN TRUE;
    END InitialiseMouseDriver;

(************************************************************************)

END SerialMouse.
