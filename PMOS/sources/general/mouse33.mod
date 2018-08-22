IMPLEMENTATION MODULE Mouse33;

	(****************************************************************)
	(*								*)
	(*		   Mouse driver using INT 33H			*)
	(*		to call a resident mouse driver			*)
	(*								*)
	(*	Author:		P.D. Terry, Rhodes University,		*)
	(*			 Sun  01-10-1993			*)
	(*			Based on an earlier version by		*)
	(*				Roger Carvalho			*)
	(*	Modified by:	Peter Moylan				*)
	(*			(initially to run with TopSpeed v3,	*)
	(*			 subsequently modified to handle both	*)
	(*			 TopSpeed v1.17 and TopSpeed 3.10 with	*)
	(*			 the aid of the PP preprocessor.)	*)
	(*	Last edited:	22 February 1995			*)
	(*	Status:		Working, not fully tested.  See note	*)
	(*			below about problems with Microsoft	*)
	(*			mouse driver.				*)
	(*								*)
	(*		Everything that I've tested works, but this	*)
	(*		module has been tested only superficially	*)
	(*		by me (PJM) so far.  The code should		*)
	(*		however be fairly trustworthy, because I have	*)
	(*		tested the parts which differ from the		*)
	(*		implementation by Pat Terry.			*)
	(*								*)
	(*	NOTE: This file contains preprocessor directives, and	*)
	(*	needs to be customised by running the PP preprocessor.	*)
	(*								*)
	(****************************************************************)
	(*								*)
	(*		PROBLEMS WITH MICROSOFT MOUSE DRIVER		*)
	(*								*)
	(*	Although this module works with the Logitech mouse	*)
	(*	driver and with the OS/2 mouse drivers, it fails with	*)
	(*	at least one version of the Microsoft mouse driver	*)
	(*	for DOS.  The problem is that the mouse driver tries	*)
	(*      to take over the timer interrupt vector, and this is	*)
	(*      incompatible with the PMOS module Timer.  As a result	*)
	(*      you should NOT use this module in conjunction with	*)
	(*      a Microsoft mouse driver; use module SerialMouse	*)
	(*      instead.						*)
	(*								*)
	(*	Possible fix: specify ChainTimerInt := TRUE in the	*)
	(*	PP.CFG file.  I haven't tested this extensively		*)
	(*	enough to be sure it's a good solution, but it works	*)
	(*	in at least some cases.					*)
	(*								*)
	(*	This problem apparently does not arise when running	*)
	(*	from inside Microsoft Windows.				*)
	(*								*)
	(****************************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE, ADDRESS,
    (* proc *)	ADR;

FROM Mouse0 IMPORT
    (* type *)	Buttons, ButtonSet, EventSet, EventHandler;

FROM MiscPMOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)	BIOS;

FROM LowLevel IMPORT
    (* proc *)	SEGMENT, OFFSET, MakePointer;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

VAR DriverInstalled: BOOLEAN;

(************************************************************************)

PROCEDURE Reset (VAR (*OUT*) MousePresent: BOOLEAN;
                        VAR (*OUT*) NumberOfButtons: CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 0;  BIOS (33H, R);
	MousePresent := R.AX # 0;
	IF MousePresent THEN
	    NumberOfButtons := R.BX
	ELSE
	    NumberOfButtons := 0;
	END (*IF*);
    END Reset;

(************************************************************************)

PROCEDURE ShowCursor;

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 1;  BIOS (33H, R);
    END ShowCursor;

(************************************************************************)

PROCEDURE HideCursor;

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 2;  BIOS (33H, R);
    END HideCursor;

(************************************************************************)

PROCEDURE GetPosBut (VAR Buttons: ButtonSet;  VAR X, Y: CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 3;  BIOS (33H, R);
	Buttons := ButtonSet(R.BX);  X := R.CX;  Y := R.DX;
    END GetPosBut;

(************************************************************************)

PROCEDURE SetCursorPos (X, Y : CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 4;  R.CX := X;  R.DX := Y;  BIOS (33H, R);
    END SetCursorPos;

(************************************************************************)

PROCEDURE GetButPress (Button : Buttons; VAR Status : ButtonSet;
                         VAR Count : CARDINAL; VAR X, Y : CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 5;  R.BX := ORD(Button);
	BIOS (33H, R);
	Status := ButtonSet(R.AX);  Count := R.BX;
	X := R.CX;  Y := R.DX;
    END GetButPress;

(************************************************************************)

PROCEDURE GetButRelease (Button : Buttons; VAR Status : ButtonSet;
                           VAR Count : CARDINAL; VAR X ,Y : CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 6;  R.BX := ORD(Button);  BIOS (33H, R);
	Status := ButtonSet(R.AX);  Count := R.BX;
	X := R.CX;  Y := R.DX;
    END GetButRelease;

(************************************************************************)

PROCEDURE SetHorizontalLimits (MinX, MaxX : CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 7;  R.CX := MinX;  R.DX := MaxX;  BIOS (33H, R);
    END SetHorizontalLimits;

(************************************************************************)

PROCEDURE SetVerticalLimits (MinY, MaxY : CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 8;  R.CX := MinY;  R.DX := MaxY;  BIOS (33H, R);
    END SetVerticalLimits;

(************************************************************************)

PROCEDURE SetGraphicsCursor (Cursor: GraphicCursor);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 9;  R.BX := Cursor.HotX;  R.CX := Cursor.HotY;
	R.DX := ORD(OFFSET (ADR(Cursor.ScreenMask)));
	R.ES := ORD(SEGMENT (ADR(Cursor.ScreenMask)));
	BIOS (33H, R);
    END SetGraphicsCursor;

(************************************************************************)

PROCEDURE SetTextCursor (Hardware: BOOLEAN;  Start, Stop: CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 10;  R.BX := ORD(Hardware);  R.CX := Start;  R.DX := Stop;
	BIOS (33H, R);
    END SetTextCursor;

(************************************************************************)

PROCEDURE ReadMotionCounters (VAR X, Y : CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 11;  BIOS (33H, R);  X := R.CX;  Y := R.DX;
    END ReadMotionCounters;

(************************************************************************)

(*<~TopSpeed3

(* The following record holds a machine language procedure.  (The	*)
(* initialisation section of this module fills in the code.)  The	*)
(* purpose of this procedure is to push AX, BX, CX, DX onto the stack	*)
(* and then call the client's event handler - i.e. this is an interface	*)
(* between a caller which passes parameters in registers and a called	*)
(* procedure which expects the parameters to be on the stack.		*)

VAR PrivateHandler: RECORD
			PushDS, PushCS, PopDSO,
			PushAX, PushBX, PushCX, PushDX : BYTE;
			FarCall : BYTE;
			handlerProc : EventHandler;
			PopDS, FarRet : BYTE;
		    END (*RECORD*);
>*)

(************************************************************************)

PROCEDURE SetEventHandler (Mask : EventSet;  Handler : EventHandler);

    VAR R: RegisterPacket;
	subr: RECORD
		CASE :BOOLEAN OF
		  | FALSE:
			(*<TopSpeed3*) proc: EventHandler; (*>*)
			(*<~TopSpeed3 address: ADDRESS; >*)
		  | TRUE:
			offset, segment: CARDINAL;
		END (*CASE*);
	      END (*RECORD*);

    BEGIN
	(*<TopSpeed3*) subr.proc := Handler; (*>*)
	(*<~TopSpeed3
	PrivateHandler.handlerProc := Handler;
	subr.address := ADR(PrivateHandler);
	>*)
	R.AX := 12; R.CX := CARDINAL(Mask);
	R.DX := subr.offset; R.ES := subr.segment;
	BIOS (33H, R);
    END SetEventHandler;

(************************************************************************)

PROCEDURE LightPenOn;

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 13;  BIOS (33H, R);
    END LightPenOn;

(************************************************************************)

PROCEDURE LightPenOff;

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 14;  BIOS (33H, R);
    END LightPenOff;

(************************************************************************)

PROCEDURE SetMickeysPerPixel (HorMPP, VerMPP : CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 15;  R.CX := HorMPP;  R.DX := VerMPP;
	BIOS (33H, R);
    END SetMickeysPerPixel;

(************************************************************************)

PROCEDURE ConditionalOff (Left, Top, Right, Bottom : CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 16;  R.CX := Left;  R.DX := Top;
	R.SI := Right;  R.DI := Bottom;
	BIOS (33H, R);
    END ConditionalOff;

(************************************************************************)

PROCEDURE SetSpeedThreshold (Threshold : CARDINAL);

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 19;  R.DX := Threshold;  BIOS (33H, R);
    END SetSpeedThreshold;

(************************************************************************)

PROCEDURE SetPage (page: CARDINAL);

    (* Sets the hardware screen page where the mouse is visible. *)

    VAR R: RegisterPacket;

    BEGIN
	R.AX := 29;  R.BX := page;  BIOS (33H, R);
    END SetPage;

(************************************************************************)

PROCEDURE Cleanup;

    (* Final cleanup on program termination. *)

    VAR HasMouse: BOOLEAN;  Count: CARDINAL;

    BEGIN
	Reset (HasMouse, Count);
    END Cleanup;

(************************************************************************)
(*			MODULE INITIALISATION				*)
(************************************************************************)

PROCEDURE InitialiseMouseDriver(): BOOLEAN;

    (* Does all initialisation needed for this module.  We make this a	*)
    (* procedure rather than an initialisation section because module	*)
    (* Mouse has to decide which mouse driver to use.  The function	*)
    (* result indicates success; if it is FALSE, none of the following	*)
    (* procedures will work.  Note: this is not an end-user procedure,	*)
    (* it's intended to be called only by module Mouse.			*)

    CONST IRET = CHR(207);

    (*<TopSpeed3*)
    (*# save, data(near_ptr=>off) *)
    (*>*)
    VAR p: POINTER TO RECORD
			  CASE :BOOLEAN OF
			      TRUE:	Routine: POINTER TO CHAR;
			    | FALSE:	segment, offset : CARDINAL;
			  END (*CASE*);
		      END (*RECORD*);
    (*<TopSpeed3*)
    (*# restore *)
    (*>*)

    BEGIN
	(*<~UseMouse RETURN FALSE; >*)
	(*<UseMouse*)
	p := MakePointer (0, 204);
	DriverInstalled := (p^.segment # 0) AND (p^.offset # 0)
					AND (p^.Routine^ # IRET);
	IF DriverInstalled THEN
	    (*<~TopSpeed3
	    WITH PrivateHandler DO
		PushCS  := VAL(BYTE,0EH);
		PopDSO  := VAL(BYTE,1FH);
		PushAX  := VAL(BYTE,50H);
		PushBX  := VAL(BYTE,53H);
		PushCX  := VAL(BYTE,51H);
		PushDX  := VAL(BYTE,52H);
		FarCall := VAL(BYTE,9AH);
		FarRet  := VAL(BYTE,0CBH);
		PushDS  := VAL(BYTE,1EH);
		PopDS   := VAL(BYTE,1FH);
	    END (*WITH*);
	    >*)
	    SetTerminationProcedure (Cleanup);
	END (*IF*);
	RETURN DriverInstalled;
	(*>*)
    END InitialiseMouseDriver;

(************************************************************************)

END Mouse33.
