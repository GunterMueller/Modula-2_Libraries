IMPLEMENTATION MODULE MiniWindows;

	(********************************************************)
	(*							*)
	(*	A simple implementation of screen windows.	*)
	(*							*)
	(*	This version uses the MiniKernel demonstration	*)
	(*	kernel.						*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	16 August 1992			*)
	(*  Status:		NEW VERSION                     *)
	(*                      This is a version specifically  *)
	(*                      tailored for the SemaTest       *)
	(*                      demo, under TopSpeed.		*)
	(*							*)
	(********************************************************)

(************************************************************************)
(*									*)
(* There are two versions of the basic "write character" operation.	*)
(* Procedure WriteChar, which is recommended for general use, does not	*)
(* recognise the concept of a "control character".  Every character	*)
(* code is considered to represent a character to be displayed on the	*)
(* screen.  Procedure Write is similar but treats CHR(0) up to CHR(31)	*)
(* as control codes.  This leads to obscure programs, so you are	*)
(* advised to avoid Write unless you want your programs to look like	*)
(* BASIC programs.  Its main intended uses are to echo keyboard input,	*)
(* and to print a file which contains embedded control characters.	*)
(*									*)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)	ADDRESS,
    (* proc *)	ADR;

FROM MiscPMOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)  Copy, CopyUp, MakePointer, IXOR, InKey, PutBack,
		BIOS;
(*
FROM DummySemaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;
*)
FROM Storage IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure, ReportTerminationError;
(*
FROM LowLevelIO IMPORT
    (* proc *)	OutByte;

FROM Logic IMPORT
    (* proc *)	HighByte, LowByte, IXOR;

FROM BIOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)	BIOS;

FROM MemoryModel IMPORT
    (* proc *)	MakePointer, Virtual, SEGMENT;
*)

FROM MiniKernel IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

(*
FROM BlockMove IMPORT
    (* proc *)	Copy, CopyUp;
*)
	(* Remark: We use Copy in preference to CopyUp wherever		*)
	(* possible, because it is marginally more efficient.  These	*)
	(* two procedures are equivalent in their final result, except	*)
	(* for the case (which occurs in scrolling) where the source	*)
	(* and destination strings overlap in memory.			*)
(*
FROM Keyboard IMPORT
    (* proc *)	InKey, PutBack;
*)
(************************************************************************)
(*	If you want black-and-white operation even though your		*)
(*	display supports colour (e.g. for the case where the colours	*)
(*	are not very readable), set ForcedMonochrome to TRUE.		*)
(*	Otherwise, this module selects colour operation if		*)
(*	one of the colour modes was active at program startup.		*)
(************************************************************************)

CONST ForcedMonochrome = FALSE;

(************************************************************************)

CONST
    VideoInt = 16;			(* interrupt number for BIOS	*)
    BytesPerChar = 2;			(* # bytes/char in video buffer	*)
    CharsPerRow = MaxColumnNumber + 1;	(* characters per row		*)
    BytesPerRow = BytesPerChar*CharsPerRow;
    buffersize = BytesPerRow * (MaxRowNumber+1);
					(* size in bytes of video buffer*)
    DefaultTabs =
"        T       T       T       T       T       T       T       T       T";

(************************************************************************)

TYPE
    buffersubscript = [0..buffersize - 1];
    Rectangle = RECORD
		    top, bottom: RowRange;
		    left, right: ColumnRange;
		END (*RECORD*);

    Window = POINTER TO WindowData;

	(****************************************************************)
	(*								*)
	(* WindowData records are linked (by next and previous) as a	*)
	(* doubly linked list, to implement a stack of windows.		*)
	(* Variable TopWindow points to the top of this stack.		*)
	(*								*)
	(* The row and column values stored in this record are actual	*)
	(* screen row and column, i.e. they are not window-relative.	*)
	(*								*)
	(* The ScreenPosition field is a subscript into the hardware	*)
	(* video buffer.  It can be computed easily from the "row" and	*)
	(* "column" fields, but it is more convenient to keep this	*)
	(* technically redundant variable.				*)
	(*								*)
	(* The "obscured" field indicates whether this window is wholly	*)
	(* or partially obscured by another window on the screen.  By	*)
	(* keeping track of this, we can avoid some unnecessary screen	*)
	(* refreshing.							*)
	(*								*)
	(* The "blockcursor" field specifies what kind of cursor to	*)
	(* display: a block cursor if TRUE, an underline cursor if	*)
	(* FALSE.  Most of the time this is irrelevant, as we display	*)
	(* a cursor only during input.					*)
	(*								*)
	(* The "buffer" array holds a copy of what is supposed to be	*)
	(* transferred to the video buffer.				*)
	(* 								*)
	(****************************************************************)

    WindowData = RECORD
		    next, previous: Window;
		    frame: FrameType;  divider: DividerType;
		    tabstops: ARRAY ColumnRange OF CHAR;
		    ScrollRegion, DefaultScrollRegion: Rectangle;
		    FirstRow, LastRow, row: RowRange;
		    FirstColumn, LastColumn: ColumnRange;
		    column: [0..MAX(ColumnRange)+1];
		    ScreenPosition: buffersubscript;
		    foreground, background: Colour;
		    CurrentAttributes: CHAR;
		    obscured, blockcursor: BOOLEAN;
		    buffer: ARRAY buffersubscript OF CHAR;
		 END (*RECORD*);

(************************************************************************)
(*		   NOTE ON CRITICAL SECTION PROTECTION			*)
(* The potential deadlock problems in this module are surprisingly	*)
(* subtle, arising from the fact that a procedure incorporating one	*)
(* form of critical section protection may call other procedures which	*)
(* themselves contain critical section protection.  To avoid these	*)
(* problems, we use an ordered resource policy.  Each critical section	*)
(* protection semaphore is given a "level", which for clarity is shown	*)
(* as the last character of its name.  A piece of code is said to be	*)
(* executing at level N if it is inside a critical section protected	*)
(* by a semaphore whose level is N (and not inside a critical section	*)
(* protected by any semaphore of any higher level).  The rule which	*)
(* avoids deadlock is: to lock a semaphore at level N, we must be	*)
(* executing at a level < N.						*)
(************************************************************************)

VAR

    (* BlackAndWhite is true if we have a monochrome display.		*)

    BlackAndWhite: BOOLEAN;

    (* BlankRow is set up by the initialisation code as a row of space	*)
    (* characters.  Note however that the attribute codes need to be	*)
    (* filled in before each use.					*)

    BlankRow: ARRAY [0..BytesPerRow-1] OF CHAR;

    (* Access to BlankRow is a critical section, so we protect it with	*)
    (* a semaphore.							*)

    BlankRowAccess1: Semaphore;

    (* StackAccess2 is used to protect access to the shared data	*)
    (* structure which defines the stack of windows.			*)

    StackAccess2: Semaphore;

    (* TopWindow is the current top of the stack of windows.		*)

    TopWindow: Window;

    (* ScreenAccess3 is used to protect access to memory in segment	*)
    (* ScreenSeg, i.e. the memory belonging to the physical screen.	*)

    ScreenAccess3: Semaphore;

    (* ScreenSeg is a segment selector for the hardware video buffer,	*)
    (* and VideoPage is the page number.  (In fact, I don't use		*)
    (* VideoPage in this version, but it's here to document the way to	*)
    (* find its value, in case I need it in a future version.)		*)
    (* CRTCport is the port number to use when addressing the CRT	*)
    (* controller chip.  It is a variable, because the address depends	*)
    (* on whether a colour or monochrome interface is installed.	*)

    ScreenSeg: CARDINAL;
    VideoPage: SHORTCARD;
    CRTCport: CARDINAL;

    (* PhysicalCursor keeps track of the blinking screen cursor.  The	*)
    (* CursorWindow field shows which window, if any, currently "owns"	*)
    (* the physical cursor, and the access4 semaphore ensures that	*)
    (* there can be at most one such window.  Note: semaphore access4	*)
    (* is used to protect both variable CursorWindow and the hardware	*)
    (* operations of turning the cursor on and off.			*)

    PhysicalCursor: RECORD
			access4: Semaphore;
			CursorWindow: Window;
		    END (*RECORD*);

(************************************************************************)
(*		    MISCELLANEOUS SCREEN OPERATIONS			*)
(************************************************************************)

PROCEDURE SetCursorShape (startline, endline: CARDINAL);

    (* Sets the start and end scan lines for the cursor.  This has to	*)
    (* be done with a BIOS call, rather than by programming the CRTC	*)
    (* registers directly, to ensure correct treatment over the variety	*)
    (* of different video interfaces which could be present.		*)
    (* Caller must be executing at level 4.				*)
(*
    VAR BIOSframe: RegisterPacket;
*)
    BEGIN
(*
	WITH BIOSframe DO
	    AH := 1;  CH := startline;  CL := endline;
	END (*WITH*);
	BIOS (VideoInt, BIOSframe);

*)
    END SetCursorShape;

(************************************************************************)

PROCEDURE CursorOff;

    (* Turns the cursor off by putting 01 in bits 5 and 4 of the cursor	*)
    (* start register of the 6845 CRT controller.  Remark: according to	*)
    (* the documentation, it should be bits 6 and 5; I do not as yet	*)
    (* have an explanation of this discrepancy.				*)
    (* Caller must be executing at level 4.				*)

    BEGIN
	SetCursorShape (16,0);
    END CursorOff;

(************************************************************************)

PROCEDURE CursorOn (position: buffersubscript;  blockcursor: BOOLEAN);

    (* Displays a blinking screen cursor at the specified position.	*)
    (* Note that this procedure actually has to halve the specified	*)
    (* argument to take account of the fact that attribute bytes take	*)
    (* space in the character buffer but are not counted by the cursor	*)
    (* setting hardware.						*)
    (* Caller must be executing at level 4.				*)

    VAR start, end: CARDINAL;

    BEGIN
(*
	position := position DIV BytesPerChar;
	OutByte (CRTCport, 14);	(* the "cursor position higher" register *)
	OutByte (CRTCport+1, HighByte(position));
	OutByte (CRTCport, 15);	(* the "cursor position lower" register *)
	OutByte (CRTCport+1, LowByte(position));
	IF BlackAndWhite THEN end := 12 ELSE end := 7 END (*IF*);
	IF blockcursor THEN start := 1 ELSE start := end-1 END(*IF*);
	SetCursorShape (start, end);
*)
    END CursorOn;

(************************************************************************)
(*		MANIPULATION OF THE STACK OF WINDOWS			*)
(************************************************************************)

PROCEDURE UnLink (w: Window);

    (* Removes w^ from the stack, but otherwise leaves it unchanged.	*)
    (* Caller must have locked StackAccess2.				*)

    BEGIN
	IF w^.previous <> NIL THEN w^.previous^.next := w^.next END (*IF*);
	IF w^.next <> NIL THEN w^.next^.previous := w^.previous END (*IF*);
	IF TopWindow = w THEN TopWindow := w^.next END (*IF*);
	w^.previous := NIL;  w^.next := NIL;
    END UnLink;

(************************************************************************)
(*			   SCREEN REFRESHING				*)
(************************************************************************)

PROCEDURE PartialRefresh (w: Window;  startrow, endrow: RowRange;
					startcol, endcol: ColumnRange);

    (* Re-draws the image of window w on the screen, in the area	*)
    (* bounded by the given absolute screen coordinates.  The ranges	*)
    (* specified are inclusive limits.					*)
    (* Caller must be executing at level <3.				*)

    VAR i: RowRange;
	offset, count: buffersubscript;

    BEGIN
	WITH w^ DO

	    (* Work out the overlap between the region and the window.	*)

	    IF FirstRow > startrow THEN startrow := FirstRow END (*IF*);
	    IF LastRow < endrow THEN endrow := LastRow END (*IF*);
	    IF FirstColumn > startcol THEN startcol := FirstColumn END (*IF*);
	    IF LastColumn < endcol THEN endcol := LastColumn END (*IF*);

	    (* Refresh that region, if it is nonempty.	*)

	    IF (startrow <= endrow) AND (startcol <= endcol) THEN
		offset := BytesPerRow*startrow + BytesPerChar*startcol;
		count := BytesPerChar*(endcol - startcol + 1);
		Wait (ScreenAccess3);
		FOR i := startrow TO endrow DO
		    Copy (ADR(buffer[offset]),
			MakePointer(ScreenSeg, offset), count);
		    INC (offset, BytesPerRow);
		END (*FOR*);
		Signal (ScreenAccess3);
	    END (*IF*);
	END (*WITH*);
    END PartialRefresh;

(************************************************************************)

PROCEDURE Refresh (w: Window);

    (* Re-draws the image of window w on the screen.	*)
    (* Caller must be executing at level <3.		*)

    VAR i: RowRange;
	offset, count: buffersubscript;

    BEGIN
	WITH w^ DO
	    offset := BytesPerRow*FirstRow + BytesPerChar*FirstColumn;
	    count := BytesPerChar*(LastColumn - FirstColumn + 1);
	    Wait (ScreenAccess3);
	    FOR i := FirstRow TO LastRow DO
		Copy (ADR(buffer[offset]),
			MakePointer(ScreenSeg, offset), count);
		INC (offset, BytesPerRow);
	    END (*FOR*);
	    Signal (ScreenAccess3);
	    obscured := FALSE;
	END (*WITH*);
    END Refresh;

(************************************************************************)

PROCEDURE ComputeCollisions (w: Window);

    (* Updates the "obscured" field of all windows which are below this	*)
    (* one on the stack, and sets w^.obscured to FALSE.  Turns on the	*)
    (* blinking screen cursor if it belongs to this window, and turns	*)
    (* it off if this window obscures it.				*)
    (* Caller must have locked StackAccess2.				*)
    (* Caller must be executing at level <4.				*)

    VAR left, right: ColumnRange;
	top, bottom: RowRange;
	w2: Window;
	clear: BOOLEAN;

    BEGIN
	(* Take note of the screen location of this window. *)

	WITH w^ DO
	    obscured := FALSE;
	    left := FirstColumn;  right := LastColumn;
	    top := FirstRow;  bottom := LastRow;
	    w2 := next;
	END (*WITH*);

	(* Decide whether to turn the physical cursor on or off. *)

	WITH PhysicalCursor DO
	    Wait (access4);
	    IF CursorWindow = w THEN
		CursorOn (w^.ScreenPosition, w^.blockcursor);
	    ELSIF CursorWindow <> NIL THEN
		WITH CursorWindow^ DO
		    clear := (row < top) OR (row > bottom)
				OR (column < left) OR (column > right);
	        END (*WITH*);
		IF NOT clear THEN
		    CursorOff;
		END (*IF*);
	    END (*IF*);
	    Signal (access4);
	END (*WITH*);

	(* Update the "obscured" information for all windows under	*)
	(* the current window.  (For those which are already obscured	*)
	(* by some other window, no further check is needed.)		*)

	WHILE w2 <> NIL DO
	    WITH w2^ DO
		IF NOT obscured THEN
		    clear := (LastColumn < left) OR (FirstColumn > right)
				OR (LastRow < top) OR (FirstRow > bottom);
		    obscured := NOT clear;
		END (*IF*);
	    END (*WITH*);
	    w2 := w2^.next;
	END (*WHILE*);
    END ComputeCollisions;

(************************************************************************)

PROCEDURE PutOnTopI (w: Window);

    (* Makes w the top of stack, and refreshes its image on the screen.	*)
    (* This procedure does the same job as PutOnTop (see below), but	*)
    (* different entry assumptions.					*)
    (* The caller must have locked StackAccess2.			*)
    (* The caller must be executing at level 2.				*)

    BEGIN
	UnLink (w);
	IF TopWindow <> NIL THEN TopWindow^.previous := w END (*IF*);
	w^.next := TopWindow;  TopWindow := w;
	Refresh (w);  ComputeCollisions (w);
    END PutOnTopI;

(************************************************************************)

PROCEDURE PutOnTop (w: Window);

    (* Makes w the top of stack, and refreshes its image on the screen.	*)
    (* This is the externally callable version.				*)
    (* The caller must be executing at a level <2.			*)

    BEGIN
	Wait (StackAccess2);
	IF TopWindow <> w THEN
	    PutOnTopI (w);
	ELSE
	    WITH PhysicalCursor DO
		Wait (access4);
		IF CursorWindow = w THEN
		    CursorOn (w^.ScreenPosition, w^.blockcursor);
		END (*IF*);
		Signal (access4);
	    END (*WITH*);
	END (*IF*);
	Signal (StackAccess2);
    END PutOnTop;

(************************************************************************)

PROCEDURE Repaint(startrow, endrow: RowRange; startcol, endcol: ColumnRange);

    (* Repaints the specified (inclusive) rectangular region on the	*)
    (* screen, and sets the physical screen cursor as necessary.	*)
    (* The caller must be executing at level 0.				*)

    CONST NormalVideo = CHR(07H);

    VAR i: RowRange;
	offset, count: buffersubscript;
	p: Window;

    BEGIN
	(* First, clear the region.	*)

	Wait (BlankRowAccess1);
	FOR offset := 1 TO BytesPerRow-1 BY BytesPerChar DO
	    BlankRow[offset] := NormalVideo;
	END (*FOR*);
	offset := BytesPerRow*startrow + BytesPerChar*startcol;
	count := BytesPerChar*(endcol - startcol + 1);
	Wait (ScreenAccess3);
	FOR i := startrow TO endrow DO
	    Copy (ADR(BlankRow), MakePointer(ScreenSeg, offset), count);
	    INC (offset, BytesPerRow);
	END (*FOR*);
	Signal (ScreenAccess3);
	Signal (BlankRowAccess1);

	(* Now refresh all open windows (or, more precisely, the parts	*)
	(* of them which lie in the affected region).			*)

	Wait (StackAccess2);
	IF TopWindow <> NIL THEN
	    p := TopWindow;
	    WHILE p^.next <> NIL DO  p := p^.next  END (*WHILE*);
	    REPEAT
		PartialRefresh (p, startrow, endrow, startcol, endcol);
		ComputeCollisions (p);  p := p^.previous;
	    UNTIL p = NIL;
	END (*IF*);
	Signal (StackAccess2);
    END Repaint;

(************************************************************************)

PROCEDURE RefreshDisplay;

    (* Rewrites every open window.  Should not normally be needed, but	*)
    (* available for use in cases the display is corrupted by, for	*)
    (* example, software which bypasses this module and writes directly	*)
    (* to the screen.							*)
    (* The caller must be executing at level 0.				*)

    BEGIN
	Wait (PhysicalCursor.access4);
	CursorOff;
	Signal (PhysicalCursor.access4);
	Repaint (0, MAX(RowRange), 0, MAX(ColumnRange));
    END RefreshDisplay;

(************************************************************************)
(*			SETTING THE SCROLLING REGION			*)
(************************************************************************)

PROCEDURE InScrollingRegion (w: Window): BOOLEAN;

    (* Returns TRUE iff the current cursor position of window w is	*)
    (* inside its scrolling region.					*)

    BEGIN
	WITH w^ DO
	    WITH ScrollRegion DO
		RETURN (row >= top) AND (row <= bottom)
			AND (column >= left) AND (column <= right);
	    END (*WITH*);
	END (*WITH*);
    END InScrollingRegion;

(************************************************************************)

PROCEDURE ChangeScrollingRegion (w: Window;  firstline, lastline: RowRange);

    (* Changes the scrolling region of window w to the new line		*)
    (* boundaries given, and sets its cursor to the start of the new	*)
    (* scrolling region.  The line numbers are window-relative.		*)

    VAR horizontal, vertical, leftT, rightT: CHAR;
	j: ColumnRange;  place: buffersubscript;

    BEGIN
	(* Although the user specifies window-relative line numbers,	*)
	(* we use screen-relative numbers internally.  Adjust the	*)
	(* parameters to take this into account.			*)

	INC (firstline, w^.FirstRow);
	INC (lastline, w^.FirstRow);

	(* Work out what characters to use for the frame and divider.	*)

	horizontal := 'Ä';  vertical := '³';
	leftT := 'Ã';  rightT := '´';
	WITH w^ DO
	    IF divider = doubledivider THEN
		horizontal := 'Í';
	    END (*IF*);
	    IF frame = doubleframe THEN
		vertical := 'º';
		IF divider = doubledivider THEN
		    leftT := 'Ì';  rightT := '¹';
		ELSE
		    leftT := 'Ç';  rightT := '¶';
		END (*IF*);
	    ELSIF divider = doubledivider THEN
		leftT := 'Æ';  rightT := 'µ';
	    END (*IF*);

	    (* Clean up the frame. *)

	    ScrollRegion := DefaultScrollRegion;
	    IF frame <> noframe THEN

		(* Remove the left and right T belonging to the	*)
		(* old divider bars, if necessary.		*)

		IF ScrollRegion.top - 1 > FirstRow THEN
		    place := BytesPerRow*(ScrollRegion.top - 1)
					+ BytesPerChar*FirstColumn;
		    buffer[place] := vertical;
		    buffer[place + BytesPerChar*(LastColumn - FirstColumn)]
					:= vertical;
		END (*IF*);

		IF ScrollRegion.bottom + 1 < LastRow THEN
		    place := BytesPerRow*(ScrollRegion.bottom+1)
					+ BytesPerChar*FirstColumn;
		    buffer[place] := vertical;
		    buffer[place + BytesPerChar*(LastColumn - FirstColumn)]
					:= vertical;
		END (*IF*);
	    END (*IF*);

	    (* Put in the new divider bars.	*)

	    IF divider <> nodivider THEN

		(* Put in the top horizontal bar.	*)

		IF firstline > FirstRow + 1 THEN
		    place := BytesPerRow*(firstline-1)
					+ BytesPerChar*FirstColumn;
		    IF frame <> noframe THEN
			buffer[place] := leftT;  INC (place, BytesPerChar);
		    END (*IF*);
		    FOR j := ScrollRegion.left TO ScrollRegion.right DO
			buffer[place] := horizontal;  INC(place, BytesPerChar);
		    END (*FOR*);
		    IF frame <> noframe THEN
			buffer[place] := rightT;
		    END (*IF*);
		END (*IF*);

		(* Put in the bottom horizontal bar.	*)

		IF lastline < LastRow - 1 THEN
		    place := BytesPerRow*(lastline+1)
					+ BytesPerChar*FirstColumn;
		    IF frame <> noframe THEN
			buffer[place] := leftT;  INC (place, BytesPerChar);
		    END (*IF*);
		    FOR j := ScrollRegion.left TO ScrollRegion.right DO
			buffer[place] := horizontal; INC(place, BytesPerChar);
		    END (*FOR*);
		    IF frame <> noframe THEN
			buffer[place] := rightT;
		    END (*IF*);
		END (*IF*);

	    END (*IF*);

	    (* Finally, update the scrolling region parameters.	*)

	    WITH ScrollRegion DO
		top := firstline;  bottom := lastline;
	    END (*WITH*);
	    DefaultScrollRegion := ScrollRegion;
	    SetCursor (w, firstline - FirstRow,
				ScrollRegion.left - FirstColumn);
	    Wait (StackAccess2);
	    IF NOT obscured THEN
		Refresh (w);
	    END (*IF*);
	    Signal (StackAccess2);
	END (*WITH*);
    END ChangeScrollingRegion;

(************************************************************************)

PROCEDURE NewScrollingRegion (w: Window;  firstline, lastline: RowRange;
				firstcolumn, lastcolumn: ColumnRange);

    (* Changes the scrolling region of w to be the specified rectangle,	*)
    (* but unlike ChangeScrollingRegion this procedure does not redraw	*)
    (* the dividers.  Furthermore the old scrolling region set by	*)
    (* ChangeScrollingRegion is remembered and may be restored by a	*)
    (* call to ResetScrollingRegion.					*)

    BEGIN
	WITH w^ DO
	    WITH ScrollRegion DO
		top := FirstRow+firstline;  bottom := FirstRow+lastline;
		left := FirstColumn+firstcolumn;
		right := FirstColumn+lastcolumn;
	    END (*WITH*);
	END (*WITH*);
    END NewScrollingRegion;

(************************************************************************)

PROCEDURE ResetScrollingRegion (w: Window);

    (* Changes the scrolling region of w back to what it was the last	*)
    (* time ChangeScrollingRegion was called.  If ChangeScrollingRegion	*)
    (* was never called, the scrolling region goes back to being the	*)
    (* entire window minus the frame (if any).				*)

    BEGIN
	w^.ScrollRegion := w^.DefaultScrollRegion;
    END ResetScrollingRegion;

(************************************************************************)
(*			    OPENING A WINDOW				*)
(************************************************************************)

PROCEDURE FillInFrame (w: Window);

    (* Puts the box around the window into the window buffer.	*)

    VAR i: RowRange;  j: ColumnRange;
	corner: ARRAY [1..4] OF CHAR;
	horizontal, vertical: CHAR;
	place, offset: buffersubscript;

    BEGIN
	IF w^.frame = simpleframe THEN
	    corner[1] := 'Ú';  corner[2] := '¿';
	    corner[3] := 'À';  corner[4] := 'Ù';
	    horizontal := 'Ä';  vertical := '³';
	ELSE
	    corner[1] := 'É';  corner[2] := '»';
	    corner[3] := 'È';  corner[4] := '¼';
	    horizontal := 'Í';  vertical := 'º';
	END (*IF*);

	WITH w^ DO

	    offset := BytesPerChar*(LastColumn - FirstColumn);
	    place := BytesPerRow*FirstRow + BytesPerChar*FirstColumn;
	    buffer[place] := corner[1];
	    buffer[place+offset] := corner[2];
	    INC (place, BytesPerRow);

	    FOR i := FirstRow + 1 TO LastRow - 1 DO
		buffer[place] := vertical;
		buffer[place+offset] := vertical;
		INC (place, BytesPerRow);
	    END (*FOR*);

	    buffer[place] := corner[3];
	    buffer[place+offset] := corner[4];

	    offset := BytesPerRow*(LastRow - FirstRow);
	    FOR j := FirstColumn + 1 TO LastColumn - 1 DO
		INC (place, BytesPerChar);
		buffer[place-offset] := horizontal;
		buffer[place] := horizontal;
	    END (*FOR*);

	END (*WITH*);
    END FillInFrame;

(************************************************************************)

PROCEDURE MakeMonochrome (VAR (*INOUT*) foreground, background: Colour);

    (* Changes the two given colours to a suitable B/W combination.	*)

    BEGIN
	IF (foreground = black) OR (foreground = darkgrey) THEN
	    background := white
	ELSE
	    IF foreground > white THEN
		foreground := intensewhite
	    ELSE
		foreground := white;
	    END (*IF*);
	    background := black;
	END (*IF*);
    END MakeMonochrome;

(************************************************************************)

PROCEDURE OpenWindow (VAR (*OUT*) w: Window;
			ForegroundColour, BackgroundColour: Colour;
			firstline, lastline: RowRange;
			firstcol, lastcol: ColumnRange;
			FrameDesired: FrameType;
			DividerDesired: DividerType);

    (* Creates a new window, and makes it the current window, filled	*)
    (* initially with space characters.					*)
    (* The caller must be executing at level 0.				*)

    VAR i: RowRange;  j: ColumnRange;  k: buffersubscript;

    BEGIN

	(* Create the new window, and fill in all its fields.	*)

	NEW (w);
	WITH w^ DO
	    previous := NIL;  next := NIL;  blockcursor := FALSE;
	    foreground := ForegroundColour;  background := BackgroundColour;
	    IF BlackAndWhite OR ForcedMonochrome THEN
		MakeMonochrome (foreground, background);
	    END (*IF*);
	    CurrentAttributes := CHR(16*ORD(background) + ORD(foreground));
	    frame := FrameDesired;  divider := DividerDesired;
	    FirstRow := firstline;  LastRow := lastline;
	    FirstColumn := firstcol;  LastColumn := lastcol;
	    tabstops := DefaultTabs;
	    IF frame <> noframe THEN
		FOR j := MAX(ColumnRange) TO 1 BY -1 DO
		    tabstops[j] := tabstops[j-1];
		END (*FOR*);
		tabstops[0] := " ";
	    END (*IF*);

	    (* Set the window contents to all space characters.	*)

	    Wait (BlankRowAccess1);
	    FOR k := 1 TO BytesPerRow-1 BY BytesPerChar DO
		BlankRow[k] := CurrentAttributes;
	    END (*FOR*);
	    FOR i := 0 TO MaxRowNumber DO
		Copy (ADR(BlankRow), ADR(buffer[i*BytesPerRow]),
						BytesPerRow);
	    END (*FOR*);
	    Signal (BlankRowAccess1);

	    (* Set up a default scrolling region.	*)

	    WITH ScrollRegion DO
		top := FirstRow;  bottom := LastRow;
		left := FirstColumn;  right := LastColumn;
	    END (*WITH*);

	    (* Make the frame.	*)

	    IF frame <> noframe THEN
		FillInFrame(w);
		WITH ScrollRegion DO
		    INC (top);  INC (left);  DEC (bottom);  DEC (right);
		END (*WITH*);
	    END (*IF*);

	    DefaultScrollRegion := ScrollRegion;
	    row := ScrollRegion.top;  column := ScrollRegion.left;
	    ScreenPosition := BytesPerRow*row + BytesPerChar*column;

	END (*WITH*);

	(* Put the new window on top of the stack of active windows.	*)

	PutOnTop (w);

    END OpenWindow;

(************************************************************************)

PROCEDURE OpenSimpleWindow (VAR (*OUT*) w: Window;
			firstline, lastline: RowRange;
			firstcol, lastcol: ColumnRange);

    (* Identical to OpenWindow, except that you don't get any choice	*)
    (* about the colours or frame.  The window is white-on-black with	*)
    (* a simple frame and no dividers for the scrolling region.  This	*)
    (* version of OpenWindow is useful for those with monochrome	*)
    (* displays who don't want to be bothered with importing the types	*)
    (* Colour, FrameType, and DividerType.				*)

    BEGIN
	OpenWindow (w, white, black, firstline, lastline,
			firstcol, lastcol, simpleframe, nodivider);
    END OpenSimpleWindow;

(************************************************************************)
(*		   CHANGING THE POSITION OF A WINDOW			*)
(************************************************************************)

PROCEDURE ShiftWindow (w: Window;  rowchange, columnchange: INTEGER);

    (* Moves w on the screen.  The second and third arguments may be	*)
    (* negative.  The amount of move may be reduced to prevent a move	*)
    (* off the edge of the screen.					*)
    (* The caller must be executing at level 0.				*)

    VAR byteshift: INTEGER;

    BEGIN
(*
	PutOnTop (w);
	WITH w^ DO

	    (* First check that the shift is a sensible one.	*)

	    IF FirstRow+rowchange < 0 THEN rowchange := -FirstRow
	    ELSIF LastRow+rowchange > MaxRowNumber THEN
		rowchange := MaxRowNumber - LastRow
	    END (*IF*);

	    IF FirstColumn+columnchange < 0 THEN columnchange := -FirstColumn
	    ELSIF LastColumn+columnchange > MaxColumnNumber THEN
		columnchange := MaxColumnNumber - LastColumn
	    END (*IF*);

	    byteshift := BytesPerRow*rowchange + BytesPerChar*columnchange;

	    (* Shift the buffer contents.	*)

	    IF byteshift < 0 THEN
		Copy (ADR(buffer[-byteshift]), ADR(buffer[0]),
						buffersize+byteshift)
	    ELSE
		CopyUp (ADR(buffer[0]), ADR(buffer[byteshift]),
						buffersize-byteshift)
	    END (*IF*);

	    (* Adjust the affected window parameters.	*)

	    WITH ScrollRegion DO
		INC (top, rowchange);  INC (bottom, rowchange);
		INC (left, columnchange);  INC (right, columnchange);
	    END (*WITH*);
	    WITH DefaultScrollRegion DO
		INC (top, rowchange);  INC (bottom, rowchange);
		INC (left, columnchange);  INC (right, columnchange);
	    END (*WITH*);
	    INC (FirstRow, rowchange);  INC (LastRow, rowchange);
	    INC (row, rowchange);
	    INC (FirstColumn, columnchange);  INC (LastColumn, columnchange);
	    INC (column, columnchange);
	    INC (ScreenPosition, byteshift);

	    (* Refresh the two affected areas of the screen.	*)

	    Repaint (FirstRow-rowchange, LastRow-rowchange,
			FirstColumn-columnchange, LastColumn-columnchange);
	    Repaint (FirstRow, LastRow, FirstColumn, LastColumn);

	END (*WITH*);
*)
    END ShiftWindow;

(************************************************************************)
(*			    CLOSING A WINDOW				*)
(************************************************************************)

PROCEDURE CloseWindow (VAR (*INOUT*) w: Window);

    (* Reclaims the buffer space used for this window, and puts the	*)
    (* next window on top of the stack.					*)
    (* The caller must be executing at level 0.				*)

    VAR p: Window;

    BEGIN
	Wait (StackAccess2);
	UnLink (w);
	Signal (StackAccess2);

	(* Repaint the part of the screen which this window occupied.	*)

	WITH w^ DO
	    Repaint (FirstRow, LastRow, FirstColumn, LastColumn);
	END (*WITH*);

	DISPOSE (w);

    END CloseWindow;

(************************************************************************)
(*	     	OPERATIONS ON CHARACTER ATTRIBUTES			*)
(************************************************************************)

PROCEDURE ColourSwap (w: Window; r: RowRange; c: ColumnRange;
							nchar: CARDINAL);

    (* Switches the foreground and background colours for nchar		*)
    (* characters, starting at location (r,c).  The row and column	*)
    (* numbers are window-relative, not absolute screen coordinates.	*)
    (* This is our colour equivalent of the "reverse video" operation.	*)
    (* NOTE: This procedure will not wrap around to a new row.		*)
    (* The caller must be executing at level <3.			*)

    VAR k, start: buffersubscript;  oldattribute: CARDINAL;

    BEGIN
	WITH w^ DO
	    start := BytesPerRow*(r+FirstRow)
			+ BytesPerChar*(c+FirstColumn) + 1;
	    FOR k := start TO start+BytesPerChar*(nchar-1) BY BytesPerChar DO
		oldattribute := ORD(buffer[k]);
		buffer[k] := CHR (16*(oldattribute MOD 16)
					+ (oldattribute DIV 16));
	    END (*FOR*);
	    Wait (StackAccess2);
	    IF obscured THEN PutOnTopI(w)
	    ELSE
		Wait (ScreenAccess3);
		Copy (ADR(buffer[start]), MakePointer(ScreenSeg, start),
				BytesPerChar*nchar);
		Signal (ScreenAccess3);
	    END (*IF*);
	    Signal (StackAccess2);
	END (*WITH*);
    END ColourSwap;

(************************************************************************)

PROCEDURE Blink (w: Window; r: RowRange; c: ColumnRange; nchar: CARDINAL);

    (* Toggles the blinking status - that is, turns blinking on if it	*)
    (* was off, and vice versa - for nchar characters, starting at	*)
    (* relative location (r,c) in window w.				*)
    (* NOTE: This procedure will not wrap around to a new row.		*)
    (* The caller must be executing at level <3.			*)

    VAR k, start: buffersubscript;

    BEGIN
	WITH w^ DO
	    start := BytesPerRow*(r+FirstRow)
			+ BytesPerChar*(c+FirstColumn) + 1;
	    FOR k := start TO start+BytesPerChar*(nchar-1) BY BytesPerChar DO
		buffer[k] := CHR (IXOR(ORD(buffer[k]), 80H));
	    END (*FOR*);
	    Wait (StackAccess2);
	    IF obscured THEN PutOnTopI(w)
	    ELSE
		Wait (ScreenAccess3);
		Copy (ADR(buffer[start]), MakePointer(ScreenSeg, start),
				BytesPerChar*nchar);
		Signal (ScreenAccess3);
	    END (*IF*);
	    Signal (StackAccess2);
	END (*WITH*);
    END Blink;

(************************************************************************)
(*			    CURSOR OPERATIONS				*)
(************************************************************************)

PROCEDURE SetCursor (w: Window; r: RowRange; c: ColumnRange);

    (* Sets the cursor for window w to relative row r, column c.	*)

    BEGIN
	WITH w^ DO
	    row := r + FirstRow;  column := c + FirstColumn;
	    ScreenPosition := BytesPerRow*row + BytesPerChar*column;
	END (*WITH*);
    END SetCursor;

(************************************************************************)

PROCEDURE SaveCursor (w: Window; VAR (*OUT*) r, c: CARDINAL);

    (* Returns the current cursor position of window w.		*)

    BEGIN
	WITH w^ DO
	    r := row - FirstRow;  c := column - FirstColumn;
	END (*WITH*);
    END SaveCursor;

(************************************************************************)

PROCEDURE CursorLeft (w: Window);

    (* Moves the window cursor one position left.  If it falls off the	*)
    (* left edge of the window, move to the right edge in the same row.	*)

    BEGIN
	WITH w^ DO
	    IF column = FirstColumn THEN
		column := LastColumn;
		ScreenPosition := BytesPerRow*row + BytesPerChar*column;
	    ELSE
		DEC (column);  DEC (ScreenPosition, BytesPerChar);
	    END (*IF*);
	END (*WITH*);
    END CursorLeft;

(************************************************************************)

PROCEDURE CursorRight (w: Window);

    (* Moves the window cursor one position right.  If it falls off the	*)
    (* right edge of the window, move to the left edge in the same row.	*)

    BEGIN
	WITH w^ DO
	    IF column = LastColumn THEN
		column := FirstColumn;
		ScreenPosition := BytesPerRow*row + BytesPerChar*column;
	    ELSE
		INC (column);  INC (ScreenPosition, BytesPerChar);
	    END (*IF*);
	END (*WITH*);
    END CursorRight;

(************************************************************************)

PROCEDURE CursorUp (w: Window);

    (* Moves the window cursor one position up.  If it falls off the	*)
    (* top edge of the window, it moves to the bottom edge in the same	*)
    (* column.								*)

    BEGIN
	WITH w^ DO
	    IF row = FirstRow THEN
		row := LastRow;
		ScreenPosition := BytesPerRow*row + BytesPerChar*column;
	    ELSE
		DEC (row);  DEC (ScreenPosition, BytesPerRow);
	    END (*IF*);
	END (*WITH*);
    END CursorUp;

(************************************************************************)

PROCEDURE CursorDown (w: Window);

    (* Moves the window cursor one position down.  If it falls off the	*)
    (* bottom edge of the window, it moves to the top edge in the same	*)
    (* column.								*)

    BEGIN
	WITH w^ DO
	    IF row = LastRow THEN
		row := FirstRow;
		ScreenPosition := BytesPerRow*row + BytesPerChar*column;
	    ELSE
		INC (row);  INC (ScreenPosition, BytesPerRow);
	    END (*IF*);
	END (*WITH*);
    END CursorDown;

(************************************************************************)

PROCEDURE ScrollUp (w: Window);

    (* Scrolls window w up by one line, both on the screen and in its	*)
    (* buffer.  The last row is filled with spaces.			*)
    (* The caller must be executing at level <3.			*)

    VAR k: buffersubscript;

    BEGIN
	WITH w^ DO
	    Wait (BlankRowAccess1);
	    FOR k := 1 TO BytesPerRow-1 BY BytesPerChar DO
		BlankRow[k] := CurrentAttributes;
	    END (*FOR*);
	    WITH ScrollRegion DO
		k := BytesPerRow * top;
		Copy (ADR(buffer[k+BytesPerRow]), ADR(buffer[k]),
			BytesPerRow*(bottom-top));
		Copy (ADR(BlankRow),
			ADR(buffer[BytesPerRow*bottom+BytesPerChar*left]),
				BytesPerChar*(right-left+1));
	    END (*WITH*);
	    Signal (BlankRowAccess1);
	    Wait (StackAccess2);
	    IF obscured THEN PutOnTopI(w) ELSE Refresh (w);
	    END (*IF*);
	    Signal (StackAccess2);
	END (*WITH*);
    END ScrollUp;

(************************************************************************)

PROCEDURE ScrollDown (w: Window);

    (* Scrolls window w down by one line, both on the screen and in its	*)
    (* buffer.  The first row is filled with spaces.			*)
    (* The caller must be executing at level <3.			*)

    VAR k: buffersubscript;

    BEGIN
	WITH w^ DO
	    Wait (BlankRowAccess1);
	    FOR k := 1 TO BytesPerRow-1 BY BytesPerChar DO
		BlankRow[k] := CurrentAttributes;
	    END (*FOR*);
	    WITH ScrollRegion DO
		k := BytesPerRow * top;
		CopyUp (ADR(buffer[k]), ADR(buffer[k+BytesPerRow]),
			BytesPerRow*(bottom-top));
		Copy (ADR(BlankRow), ADR(buffer[k+BytesPerChar*left]),
			BytesPerChar*(right-left+1));
	    END (*WITH*);
	    Signal (BlankRowAccess1);
	    Wait (StackAccess2);
	    IF obscured THEN PutOnTopI(w) ELSE Refresh (w);
	    END (*IF*);
	    Signal (StackAccess2);
	END (*WITH*);
    END ScrollDown;

(************************************************************************)
(*			    MAIN OUTPUT ROUTINES			*)
(************************************************************************)

PROCEDURE WriteChar (w: Window; ch: CHAR);

    (* Writes one character to window w, and updates the cursor for	*)
    (* this window.  As a side-effect, this window becomes the		*)
    (* currently active window if it was obscured.  Wraps around to the	*)
    (* next line if we are about to run off the end of the current	*)
    (* line.  This procedure does not recognise the concept of a	*)
    (* control character.  Every possible value of ch produces		*)
    (* something readable on the screen.				*)
    (* The caller must be executing at level <3.			*)

    VAR screenloc: POINTER TO CHAR;

    BEGIN
	WITH w^ DO
	    (* Wrap to a new line if we about to leave the scrolling	*)
	    (* region or if we are outside the legal writing region.	*)

	    IF (column = ScrollRegion.right + 1)
			OR (column > DefaultScrollRegion.right) THEN
		DEC (column);  WriteLn (w);
	    END (*IF*);

	    buffer[ScreenPosition] := ch;
	    buffer[ScreenPosition + 1] := CurrentAttributes;

	    Wait (StackAccess2);
	    IF obscured THEN PutOnTopI(w) END (*IF*);
	    Wait (ScreenAccess3);
	    screenloc := MakePointer (ScreenSeg, ScreenPosition);
	    screenloc^ := ch;
	    screenloc := MakePointer (ScreenSeg, ScreenPosition+1);
	    screenloc^ := CurrentAttributes;
	    Signal (ScreenAccess3);
	    Signal (StackAccess2);

	    (* Note that the following statement may cause column to	*)
	    (* go beyond the edge of the window; but this will be	*)
	    (* picked up on the next call to WriteChar.  We prefer not	*)
	    (* to do a WriteLn just yet, because that could cause an	*)
	    (* unintended scroll operation when writing to the bottom	*)
	    (* right of the window.					*)

	    INC (column);  INC (ScreenPosition, BytesPerChar);
	END (*WITH*);
    END WriteChar;

(************************************************************************)

PROCEDURE WriteLn (w: Window);

    (* Moves the cursor of window w to the start of the next row.  If	*)
    (* we are already at the last row, the window scrolls up.		*)

    BEGIN
	WITH w^ DO
	    IF InScrollingRegion (w) THEN
		column := ScrollRegion.left;
		IF row = ScrollRegion.bottom THEN ScrollUp (w)
		ELSE INC (row);
		END (*IF*);
	    ELSE
		column := DefaultScrollRegion.left;
		IF row >= LastRow THEN row := LastRow
		ELSE INC (row);
		END (*IF*);
	    END (*IF*);
	    ScreenPosition := BytesPerRow*row + BytesPerChar*column;
	END (*WITH*);
    END WriteLn;

(************************************************************************)

PROCEDURE WriteString (w: Window; text: ARRAY OF CHAR);

    (* Writes a sequence of characters, terminated either by NUL or by	*)
    (* the end of the array.						*)

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	LOOP
	    IF ORD (text[j]) = 0 THEN EXIT (*LOOP*)  END (*IF*);
	    WriteChar (w, text[j]);  INC (j);
	    IF j > HIGH (text) THEN EXIT (*LOOP*)  END (*IF*);
	END (*LOOP*);
    END WriteString;

(************************************************************************)

PROCEDURE Write (w: Window; ch: CHAR);

    (* A version of procedure WriteChar which looks after some of the	*)
    (* control characters.						*)

    BEGIN
	IF ch >= " " THEN WriteChar (w, ch)
	ELSIF ORD(ch) = 8 THEN		(* backspace *)
	    CursorLeft(w)
	ELSIF ORD(ch) = 9 THEN		(* tab *)
	    WITH w^ DO
		REPEAT
		    WriteChar (w, " ");
		UNTIL (column=MAX(ColumnRange)) OR (tabstops[column]="T");
	    END (*WITH*);
	ELSIF ORD(ch) = 10 THEN		(* line feed - ignore *)
	ELSIF ORD(ch) = 13 THEN		(* carriage return *)
	    WriteLn(w)
	ELSE				(* other control character *)
	    WriteChar (w, "^");  WriteChar (w, CHR(ORD(ch)+64))
	END (*IF*);
    END Write;

(************************************************************************)
(*				INPUT					*)
(************************************************************************)

PROCEDURE ReadBack (w: Window;  r: RowRange;  c: ColumnRange): CHAR;

    (* Returns the character which currently occupies relative location	*)
    (* (r,c) on the display of window w.				*)

    BEGIN
	WITH w^ DO
	    RETURN buffer[BytesPerRow*(r+FirstRow)
				+ BytesPerChar*(c+FirstColumn)];
	END (*WITH*);
    END ReadBack;

(************************************************************************)

PROCEDURE ReadCharWithoutEcho (w: Window;  VAR (*OUT*) ch: CHAR);

    (* Read one character.  Window w is forced to be the top window,	*)
    (* in order that the cursor be visible.  The critical section	*)
    (* protection provided by PhysicalCursor.access ensures that only	*)
    (* one window at a time is involved in keyboard input.  Procedure	*)
    (* PutOnTop will turn on the physical cursor when it discovers that	*)
    (* w = CursorWindow.  The cursor might subsequently disappear if a	*)
    (* task switch leads to another window obscuring it, but at least	*)
    (* we ensure that the physical cursor cannot turn up in some other	*)
    (* window until the current task has left its critical section.	*)
    (* The caller must be executing at a level <2.			*)

    BEGIN
	WITH PhysicalCursor DO
	    Wait (access4);  CursorWindow := w;  Signal (access4);
	    PutOnTop(w);  ch := InKey();

	    (* By now, another window might be on top as the result of	*)
	    (* a task switch.  We put the current window back on top to	*)
	    (* give some feedback to the user, and turn off the		*)
	    (* blinking cursor.						*)

	    Wait(access4); CursorWindow := NIL; CursorOff; Signal(access4);
	    PutOnTop(w);

	END (*WITH*);
    END ReadCharWithoutEcho;

(************************************************************************)

PROCEDURE ReadChar (w: Window;  VAR (*OUT*) ch: CHAR);

    (* Like ReadCharWithoutEcho, but the input character is echoed.	*)

    BEGIN
	ReadCharWithoutEcho (w, ch);  Write (w, ch);
    END ReadChar;

(************************************************************************)

PROCEDURE LookaheadChar (w: Window): CHAR;

    (* Reads a character without consuming it.  That is, the character	*)
    (* remains available to be read by ReadChar.  This allows the	*)
    (* caller to check whether the character is really wanted.		*)

    VAR ch: CHAR;

    BEGIN
	ReadCharWithoutEcho (w, ch);  PutBack (ch);
	RETURN ch;
    END LookaheadChar;

(************************************************************************)

PROCEDURE PressAnyKey (w: Window);

    (* Types a "Press any key to continue" message.	*)

    VAR dummy: CHAR;

    BEGIN
	WriteLn (w);
	WriteString (w, "Press any key to continue.");
	ReadChar (w, dummy);
	EraseLine (w, 0);  CursorUp(w);
    END PressAnyKey;

(************************************************************************)

PROCEDURE ReadString (w: Window;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Reads a character string, terminated by carriage return.		*)

    VAR j: CARDINAL;  ch: CHAR;

    BEGIN
	FOR j := 0 TO HIGH(result) DO
	    result[j] := " ";
	END (*FOR*);
	j := 0;
	LOOP
	    ReadChar (w, ch);
	    IF ORD(ch) = 13 THEN
		result[j] := CHR(0);  EXIT(*LOOP*)
	    ELSIF ORD(ch) = 8 THEN	(* backspace *)
		IF j > 0 THEN
		    CursorLeft(w);  WriteChar(w, " ");  CursorLeft(w);
		    DEC (j);
		END (*IF*);
	    ELSE
		result[j] := ch;
		IF j = HIGH(result) THEN EXIT(*LOOP*) END(*IF*);
		INC (j);
	    END(*IF*);
	END (*LOOP*);
    END ReadString;

(************************************************************************)

PROCEDURE EditString (w: Window;  VAR (*INOUT*) result: ARRAY OF CHAR;
						fieldsize: CARDINAL);

    (* Reads a character string, where a default result is supplied by	*)
    (* the caller.  The final result is the state of the string at the	*)
    (* time where the keyboard user types a carriage return or uses a	*)
    (* cursor movement key to move out of the displayed field.  If the	*)
    (* user types Esc at any time, then we return with result[0] = Esc.	*)
    (* A carriage return or Esc as terminator is not returned to the	*)
    (* caller, but if the terminator is a cursor control key then it	*)
    (* it remains available, via Keyboard.Inkey, to the caller.		*)
    (* At most fieldsize characters of the string can be edited, and	*)
    (* perhaps fewer if the result array is smaller or if there is	*)
    (* insufficient space in the window.				*)

    CONST Esc = CHR(01BH);  Space = " ";

    VAR place, k: CARDINAL;  ch: CHAR;  limit: ColumnRange;
	SavedAttributes: CHAR;
	startrow, startcolumn: CARDINAL;
	InsertMode, SavedCursorType: BOOLEAN;

    (********************************************************************)

    PROCEDURE RewriteString ();

	BEGIN
	    SetCursor (w, startrow, startcolumn);
	    WriteString (w, result);
	    SetCursor (w, startrow, startcolumn+place);
	END RewriteString;

    (********************************************************************)

    PROCEDURE GoToEnd;

	(* Puts the cursor just after the last non-blank character.	*)

	BEGIN
	    place := limit+1;
	    WHILE (place > 0) AND (result[place-1] = Space) DO
		DEC (place);
	    END (*WHILE*);
	    SetCursor (w, startrow, startcolumn+place);
	END GoToEnd;

    (********************************************************************)

    PROCEDURE HandleControlChar(): BOOLEAN;

	(* Called after detecting the CHR(0) which means that a control	*)
	(* character has been typed.  Performs the appropriate actions,	*)
	(* returns TRUE iff editing is finished.			*)

	BEGIN
	    ReadCharWithoutEcho (w, ch);
	    IF ch = "K" THEN				(* cursor left *)
		IF place = 0 THEN
		    PutBack(ch);  PutBack(CHR(0));
		    RETURN TRUE;
		END (*IF*);
		CursorLeft(w);  DEC (place);
	    ELSIF ch = "M" THEN				(* cursor right *)
		IF place > limit THEN
		    PutBack(ch);  PutBack(CHR(0));
		    RETURN TRUE;
		END (*IF*);
		CursorRight(w);  INC (place);
	    ELSIF (ch = "H") OR (ch = "P") THEN		(* cursor up/down *)
		PutBack(ch);  PutBack(CHR(0));
		RETURN TRUE;
	    ELSIF ch = "G" THEN				(* home *)
		place := 0;
		SetCursor (w, startrow, startcolumn);
	    ELSIF ch = "O" THEN				(* end *)
		GoToEnd;
	    ELSIF ch = "R" THEN				(* insert *)
		w^.blockcursor := InsertMode;
		InsertMode := NOT InsertMode;
	    ELSIF ch = "S" THEN				(* delete right *)
		IF place <= limit THEN
		    FOR k := place TO limit-1 DO
			result[k] := result[k+1];
		    END (*FOR*);
		    result[limit] := Space;
		    RewriteString ();
		END (*IF*);
	    END (*IF*);
	    RETURN FALSE;
	END HandleControlChar;

    (********************************************************************)

    BEGIN	(* Body of EditString *)

	SaveCursor (w, startrow, startcolumn);
	SavedCursorType := w^.blockcursor;
	InsertMode := TRUE;  w^.blockcursor := FALSE;

	(* Compute a limit which stops us from running off the window.	*)

	WITH w^ DO
	    IF InScrollingRegion(w) THEN
		limit := ScrollRegion.right;
	    ELSE
		limit := DefaultScrollRegion.right;
	    END (*IF*);
	    DEC (limit, FirstColumn + startcolumn);
	    SavedAttributes := CurrentAttributes;
	END (*WITH*);
	IF HIGH(result) < limit THEN
	    limit := HIGH(result);
	END (*IF*);
	IF fieldsize <= limit THEN
	    limit := fieldsize - 1;
	END (*IF*);

	(* Preprocessing: for a Nul-terminated string, remove the Nul	*)
	(* and pad out the string with spaces at the right.  Otherwise	*)
	(* we get problems if, for example, the Nul is deleted.		*)

	place := 0;
	LOOP
	    IF result[place] = CHR(0) THEN
		FOR k := place TO limit DO
		    result[k] := Space;
		END (*FOR*);
		EXIT (*LOOP*);
	    END (*IF*);
	    IF place = limit THEN EXIT(*LOOP*) END(*IF*);
	    INC (place);
	END (*LOOP*);
	FOR k := limit+1 TO HIGH(result) DO
	    result[k] := CHR(0);
	END (*FOR*);

	(* Write the string, using reverse video.	*)

	WriteString (w, result);
	ColourSwap (w, startrow, startcolumn, limit+1);
	WITH w^ DO
	    CurrentAttributes := CHR(16*ORD(foreground) + ORD(background));
	END (*WITH*);
	place := 0;
	SetCursor (w, startrow, startcolumn);
	PutOnTop(w);

	(* Now the main editing loop.	*)

	LOOP
	    ReadCharWithoutEcho (w, ch);
	    IF ORD(ch) = 0 THEN				(* control char *)
		IF HandleControlChar() THEN
		    EXIT (*LOOP*);
		END (*IF*);
	    ELSIF ch = Esc THEN				(* Esc *)
		result[0] := Esc;  EXIT(*LOOP*);
	    ELSIF ORD(ch) = 13 THEN			(* carriage return *)
		EXIT(*LOOP*)
	    ELSIF ORD(ch) = 8 THEN			(* delete left *)
		IF place > 0 THEN
		    DEC (place);
		    FOR k := place TO limit-1 DO
			result[k] := result[k+1];
		    END (*FOR*);
		    result[limit] := Space;
		    RewriteString ();
		END (*IF*);
	    ELSIF place <= limit THEN			(* any other char *)
		IF InsertMode THEN
		    FOR k := limit TO place+1 BY -1 DO
			result[k] := result[k-1];
		    END (*FOR*);
		    RewriteString ();
		END (*IF*);
		result[place] := ch;  WriteChar (w, ch);
		INC (place);
	    END(*IF*);
	END (*LOOP*);
	ColourSwap (w, startrow, startcolumn, limit+1);
	w^.blockcursor := SavedCursorType;
	w^.CurrentAttributes := SavedAttributes;
    END EditString;

(************************************************************************)
(*		    MISCELLANEOUS CONTROL OPERATIONS			*)
(************************************************************************)

PROCEDURE EraseLine (w: Window;  option: CARDINAL);

    (* Replaces some or all of the current line, except for the border,	*)
    (* with space characters.  The window cursor is moved to the	*)
    (* location of the first erased character.  The options are:	*)
    (*		0	the whole of the line, except for the border	*)
    (*		1	from the current cursor position onwards	*)
    (*		2	from the start to just before the cursor	*)
    (* If we are inside a scrolling region, then only that part of the	*)
    (* line inside the scrolling region is affected.			*)

    VAR first, last: ColumnRange;
	k, firstk, lastk: buffersubscript;

    BEGIN
	WITH w^ DO
	    IF InScrollingRegion(w) THEN
		first := ScrollRegion.left;  last := ScrollRegion.right;
	    ELSE
		first := DefaultScrollRegion.left;
		last := DefaultScrollRegion.right;
	    END (*IF*);
	    IF option = 1 THEN first := column
	    ELSIF option = 2 THEN last := column - 1
	    END (*IF*);
	    firstk := BytesPerRow*row + BytesPerChar*first;
	    lastk := BytesPerRow*row + BytesPerChar*last;
	    FOR k := firstk TO lastk BY BytesPerChar DO
		buffer[k] := " ";  buffer[k+1] := CurrentAttributes;
	    END (*FOR*);
	    Wait (ScreenAccess3);
	    IF NOT obscured THEN
		Copy (ADR(buffer[firstk]), MakePointer(ScreenSeg,firstk),
					lastk - firstk + 2);
	    END (*IF*);
	    Signal (ScreenAccess3);
	    column := first;  ScreenPosition := firstk;
	END (*WITH*);
    END EraseLine;

(************************************************************************)
(*			     TERMINATION				*)
(************************************************************************)

PROCEDURE CleanUp;

    (* Called at program termination.  If termination was caused by an	*)
    (* error, displays the error and waits for the user to press any	*)
    (* key.  Then closes all open windows, and turns on the blinking	*)
    (* screen cursor.							*)

    VAR w: Window;  dummy: CHAR;

    BEGIN
	(* For abnormal termination, put an error diagnostic on the	*)
	(* screen.  We lock ScreenAccess3 while doing this in order to	*)
	(* prevent corruption of our error message by another task,	*)
	(* and in particular to prevent screen scrolling.		*)
(*
	Wait (ScreenAccess3);
	IF ReportTerminationError() THEN
	    dummy := InKey();
	END (*IF*);
	Signal (ScreenAccess3);
*)
	(* Close all open windows.	*)

	LOOP
	    Wait (StackAccess2);
	    w := TopWindow;
	    Signal (StackAccess2);
	    IF w = NIL THEN EXIT(*LOOP*) END(*IF*);
	    CloseWindow (w);
	END (*LOOP*);

	(* Restore the usual screen cursor shape.	*)

	Wait (PhysicalCursor.access4);
	CursorOn (0, FALSE);
	Signal (PhysicalCursor.access4);

    END CleanUp;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

VAR j: buffersubscript;
    Registers: RegisterPacket;

BEGIN
    FOR j := 0 TO HIGH(BlankRow) DO
	BlankRow[j] := " ";
    END (*FOR*);
    CreateSemaphore (BlankRowAccess1, 1);

    TopWindow := NIL;
    CreateSemaphore (StackAccess2, 1);

    (* The BIOS video call 0FH returns the current video state.  From	*)
    (* this we can work out whether to use colour or mono.		*)
(*
    ScreenSeg := SEGMENT(Virtual(0B0000H));    (* assume mono initially *)
*)
    ScreenSeg := 0B000H;
    BlackAndWhite := TRUE;

    WITH Registers DO
	AH := 0FH;  BIOS (VideoInt, Registers);
	VideoPage := BH;
	IF AL <> 7 THEN		(* must be colour *)

	    (* ScreenSeg := SEGMENT(Virtual(0B8000H)); *)
	    ScreenSeg := 0B800H;
	    BlackAndWhite := FALSE;

	    (* Set the screen to 25*80 colour.	*)

	    AH := 0;  AL := 3;  BIOS (VideoInt, Registers);

	END (*IF*);
    END (*WITH*);

    IF BlackAndWhite THEN
	CRTCport := 03B4H;
    ELSE
	CRTCport := 03D4H;
    END (*IF*);

    CreateSemaphore (ScreenAccess3, 1);

    (* Initialise the screen cursor information. *)

    CursorOff;
    WITH PhysicalCursor DO
	CreateSemaphore (access4, 1);
	CursorWindow := NIL;
    END (*WITH*);

    SetTerminationProcedure (CleanUp);

    (* Blank the screen, to erase otherwise annoying background stuff	*)
    (* left by other programs.						*)

    Repaint (0,MaxRowNumber,0,MaxColumnNumber);

END MiniWindows.
