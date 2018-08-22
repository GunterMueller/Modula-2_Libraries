IMPLEMENTATION MODULE TextVideo;

	(********************************************************)
	(*							*)
	(*		Low-level screen functions		*)
	(*							*)
	(*	The function of this module is to look after	*)
	(*	the low-level screen initialisation needed	*)
	(*	by the text-mode modules GlassTTY and Windows.	*)
	(*	This reduces the likelihood of conflicts	*)
	(*	arising in programs which import both of those	*)
	(*	modules.					*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	30 January 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM LowLevel IMPORT
    (* proc *)	SEGMENT, Virtual, OutByte, HighByte, LowByte;

FROM MiscPMOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)	BIOS, EnterCriticalSection, LeaveCriticalSection;

IMPORT Screen;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

(************************************************************************)

CONST
    VideoInt = 16;		(* Interrupt number for BIOS calls	*)
    PageSize = 4096;		(* Size of a hardware page buffer	*)
    BytesPerChar = 2;

VAR
    (* A flag to say whether we have a monochrome adaptor.	*)

    Monochrome: BOOLEAN;

    (* ScreenSeg is a segment selector for the hardware video buffer,	*)
    (* and CRTCport is the port number to use when addressing the CRT	*)
    (* controller chip.  They are variables because their values depend	*)
    (* on whether a colour or monochrome interface is installed.	*)

    ScreenSeg: CARDINAL;
    CRTCport: CARDINAL;

(************************************************************************)
(*			    CURSOR CONTROL				*)
(************************************************************************)

PROCEDURE SetCursorShape (startline, endline: CARDINAL);

    (* Sets the start and end scan lines for the cursor.  This has to	*)
    (* be done with a BIOS call, rather than by programming the CRTC	*)
    (* registers directly, to ensure correct treatment over the variety	*)
    (* of different video interfaces which could be present.		*)

    VAR BIOSframe: RegisterPacket;

    BEGIN
	WITH BIOSframe DO
	    AH := 1;  CH := SHORTCARD(startline);
	    CL := SHORTCARD(endline);
	END (*WITH*);
	BIOS (VideoInt, BIOSframe);
    END SetCursorShape;

(************************************************************************)

PROCEDURE PositionCursor (visible: BOOLEAN;  position: CARDINAL;
					blockcursor: BOOLEAN);

    (* Displays a blinking screen cursor at the specified position.	*)
    (* Note that this procedure actually has to halve the specified	*)
    (* argument to take account of the fact that attribute bytes take	*)
    (* space in the character buffer but are not counted by the cursor	*)
    (* setting hardware.						*)

    VAR start, end: CARDINAL;

    BEGIN
	position := position DIV BytesPerChar;
	OutByte (CRTCport, 14);	(* the "cursor position higher" register *)
	OutByte (CRTCport+1, HighByte(position));
	OutByte (CRTCport, 15);	(* the "cursor position lower" register *)
	OutByte (CRTCport+1, LowByte(position));
	IF Monochrome THEN end := 12 ELSE end := 7 END (*IF*);
	IF blockcursor THEN start := 1 ELSE start := end-1 END(*IF*);
	IF visible THEN SetCursorShape (start, end)
	ELSE SetCursorShape (16, 0);
	END (*IF*);
    END PositionCursor;

(************************************************************************)
(*		      OPERATIONS ON THE VIDEO MODE			*)
(************************************************************************)

PROCEDURE SetVideoMode (newmode: CARDINAL);

    (* Sets the video mode, as defined in the BIOS. At present only	*)
    (* modes 3 (25*80 colour) and 7 (25*80 monochrome) are supported.	*)

    VAR Registers: RegisterPacket;  dummy: BOOLEAN;
	
    BEGIN
	dummy := Screen.SetVideoMode (newmode, FALSE);
	Screen.GetAddresses (ScreenSeg, CRTCport);
    END SetVideoMode;

(************************************************************************)

PROCEDURE SetTextPage (page: SHORTCARD);

    (* Changes the active display page.	*)

    VAR position: CARDINAL;

    BEGIN
	position := PageSize*VAL(CARDINAL,page) DIV BytesPerChar;
	OutByte (CRTCport, 12);	(* the "start address higher" register *)
	OutByte (CRTCport+1, HighByte(position));
	OutByte (CRTCport, 13);	(* the "start address lower" register *)
	OutByte (CRTCport+1, LowByte(position));
    END SetTextPage;

(************************************************************************)

PROCEDURE RestoreCursor;

    (* Makes the screen cursor visible at the top of the screen.	*)

    BEGIN
	PositionCursor (TRUE, 0, FALSE);
    END RestoreCursor;

(************************************************************************)

PROCEDURE RestoreOriginalMode;

    (* Sets the video mode back to what it was before this program ran.	*)

    BEGIN
	Screen.RestoreOriginalMode;
	RestoreCursor;
    END RestoreOriginalMode;

(************************************************************************)

PROCEDURE ShutDown;

    (* Makes sure that the screen cursor is turned on at program	*)
    (* termination.  Note that we're setting a termination procedure	*)
    (* inside a termination procedure, to ensure that the cursor is	*)
    (* turned on in pass 2 of the termination processing.		*)

    BEGIN
	SetTerminationProcedure (RestoreCursor);
    END ShutDown;

(************************************************************************)
(*			EQUIPMENT KIND DETECTION			*)
(************************************************************************)

PROCEDURE VideoKind (VAR (*OUT*) ScreenSegment: CARDINAL;
				VAR (*OUT*) BlackAndWhite: BOOLEAN);

    (* Returns the segment of the screen memory, and a flag saying	*)
    (* whether we are using a monochrome adaptor.			*)

    BEGIN
	ScreenSegment := ScreenSeg;  BlackAndWhite := Monochrome;
    END VideoKind;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

VAR Registers: RegisterPacket;
    Adaptor: Screen.VideoAdaptorType;

BEGIN
    Adaptor := Screen.VideoKind();
    Monochrome := (Adaptor=Screen.MDA) OR (Adaptor=Screen.Hercules);
    SetTerminationProcedure (ShutDown);

    (* Set an appropriate text mode: 25*80 colour if possible, or	*)
    (* 25*80 monochrome for a monochrome adaptor.			*)

    IF Monochrome THEN
	SetVideoMode (7);
    ELSE
	SetVideoMode (3);
    END (*IF*);

END TextVideo.
