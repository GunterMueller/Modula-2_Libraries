IMPLEMENTATION MODULE Tiles;

	(********************************************************)
	(*							*)
	(*	    Support module for screen graphics		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:		Working				*)
	(*	The logic for forcing a restart when checking	*)
	(*	 for things to merge could be made less		*)
	(*	 conservative - is this worth the extra effort?	*)
	(*	Still have to check for missing features.	*)
	(*	Text implemented inefficiently.			*)
	(*							*)
	(*	Now adding a scrolling mechanism.  Procedure	*)
	(*	ScrollContents is basically working but is	*)
	(*	still missing the mechanism to fix up		*)
	(*	scrolled points.				*)
	(*							*)
	(********************************************************)

FROM ScreenGeometry IMPORT
    (* proc *)	Inside;

FROM Graphics IMPORT
    (* proc *)	Fill, PlotDot, PlotLine, PlotRectangle, ClippedLine,
		ClippedString, ClippedUpString, ACopy;

FROM Screen IMPORT
    (* proc *)	RestoreOriginalMode;

FROM Queues IMPORT
    (* type *)	Queue,
    (* proc *)	CreateQueue, AddToQueue, TakeFromQueue, Empty, DestroyQueue;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateLock, Obtain, Release;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

FROM Keyboard IMPORT	(* for testing *)
    (* proc *)	InKey;

(************************************************************************)

CONST testing = FALSE;		(* If TRUE, we display TileSlots *)

CONST BigNumber = MAX(CARDINAL) - 1;

TYPE
    TextPointer = POINTER TO ARRAY [0..BigNumber] OF CHAR;

    (* A PointList is a set of Points.	*)

    PointList = POINTER TO PointListRecord;
    PointListRecord =
		    RECORD
			next: PointList;
			datum: Point;
			colour: ColourType;
		    END (*RECORD*);

    (* A LineList is a set of Lines defined by their endpoints.	*)

    LineList = POINTER TO LineListRecord;
    LineListRecord =
		    RECORD
			next: LineList;
			end1, end2: Point;
			colour: ColourType;
		    END (*RECORD*);

    (* A StringList is a set of text strings.	*)

    StringList = POINTER TO StringListRecord;
    StringListRecord =
		    RECORD
			next: StringList;
			location: Point;
			colour: ColourType;
			length: CARDINAL;
			textptr: TextPointer;
		    END (*RECORD*);

    (* A TileSlot is defined later - see the TileSlotInfo declaration.	*)

    TileSlot = POINTER TO TileSlotInfo;

    (* A Tile is an object holding graphic data to be displayed on the	*)
    (* screen.  Each Tile matches exactly one TileSlot, but multiple	*)
    (* Tiles can occupy the same TileSlot.  The field "under" shows the	*)
    (* next Tile which occupies the same TileSlot, "next" shows the	*)
    (* next Tile in the same TileSet, and "set" is the TileSet itself.	*)

    Tile = POINTER TO TileRecord;
    TileRecord =    RECORD
			under: Tile;
			next: Tile;
			set: TileSet;
			slot: TileSlot;
		 	points: PointList;
		    END (*RECORD*);

    (* A TileSet - which is the only data type of interest to clients	*)
    (* of this module - is a set of Tiles, plus a set of lines and text	*)
    (* strings to be plotted.  The background field gives the common	*)
    (* background colour for all the tiles in the set.  The retain	*)
    (* field specifies whether to save newly plotted data for future	*)
    (* refreshing.							*)

    TileSet = POINTER TO TileSetRecord;
    TileSetRecord = RECORD
			head, tail: Tile;
			background: ColourType;
			retain: BOOLEAN;
			lines: LineList;
			strings, upstrings: StringList;
		    END (*RECORD*);

    (* A TileStack is a stack of tiles - each belonging to a distinct	*)
    (* TileSet - occupying the same position on the screen.		*)

    TileStack = Tile;

    (* A TileSlot represents a rectangular region on the screen, which	*)
    (* can contain zero or more Tiles.  This module maintains a master	*)
    (* list of TileSlots, showing how the screen is currently tiled.	*)
    (* Initially there is just one TileSlot covering the whole screen.	*)
    (* Each time CreateTileSet is called there is a likelihood that	*)
    (* existing TileSlots will have to be broken up, so the master list	*)
    (* of TileSlots changes with time.  When a TileSlot is broken up,	*)
    (* the corresponding Tiles must of course also be broken up.	*)
    (* The nextslot field in a TileSlot record points to the next	*)
    (* TileSlot in the master list, and the stacktop field points to a	*)
    (* a stack of tiles which occupy this TileSlot.  The mark field is	*)
    (* normally FALSE; we set it to indicate a candidate for merging	*)
    (* with adjacent slots.						*)
    (* So far I haven't given enough thought as to the most desirable	*)
    (* ordering of the TileSlot list.  It should possibly be ordered	*)
    (* to make searching faster.					*)

    TileSlotInfo =  RECORD
			shape: Rectangle;
			nextslot: TileSlot;
			stacktop: TileStack;
			mark: BOOLEAN;
		    END (*RECORD*);

(************************************************************************)

VAR
    (* SlotListHead is the head of the master list of TileSlots.	*)

    SlotListHead: TileSlot;

    (* MainLock protects all entry points to this module.  Because of	*)
    (* the heavily interlocked nature of the data structures maintained	*)
    (* by this module, and in particular because operations on one	*)
    (* TileSet can affect the internal structure of other TileSets,	*)
    (* there is little point in trying for finer granularity in the	*)
    (* critical section protection.					*)

    MainLock: Lock;

(************************************************************************)

(*
PROCEDURE DebugPause (message: ARRAY OF CHAR);

    (* For testing - can remove from production version.	*)

    VAR dummy: CHAR;

    BEGIN
	GlassTTY.SetCursor (24, 0);
	GlassTTY.WriteString (message);
	dummy := InKey();
    END DebugPause;
*)

(************************************************************************)
(*	  OPERATIONS ON TYPES PointList, LineList, AND StringList	*)
(************************************************************************)

PROCEDURE DiscardPointList (VAR (*INOUT*) PL: PointList);

    (* Destroys a PointList.	*)

    VAR following: PointList;

    BEGIN
	WHILE PL <> NIL DO
	    following := PL^.next;
	    DISPOSE (PL);
	    PL := following;
	END (*WHILE*);
    END DiscardPointList;

(************************************************************************)

PROCEDURE DiscardLineList (VAR (*INOUT*) LL: LineList);

    (* Destroys a LineList.	*)

    VAR following: LineList;

    BEGIN
	WHILE LL <> NIL DO
	    following := LL^.next;
	    DISPOSE (LL);
	    LL := following;
	END (*WHILE*);
    END DiscardLineList;

(************************************************************************)

PROCEDURE PlotLineListClipped (LL: LineList;  R: Rectangle);

    (* Plots all lines in LL, clipping them on the display such that	*)
    (* only the part inside R is shown.					*)

    BEGIN
	WHILE LL <> NIL DO
	    WITH LL^ DO
		WITH R DO
		    ClippedLine (end1.x, end1.y, end2.x, end2.y, colour,
						left, right, bottom, top);
		END (*WITH*);
	    END (*WITH*);
	    LL := LL^.next;
	END (*WHILE*);
    END PlotLineListClipped;

(************************************************************************)

PROCEDURE DiscardStringList (VAR (*INOUT*) SL: StringList);

    (* Destroys a StringList.	*)

    VAR following: StringList;

    BEGIN
	WHILE SL <> NIL DO
	    following := SL^.next;
	    DEALLOCATE (SL^.textptr, SL^.length);
	    DISPOSE (SL);
	    SL := following;
	END (*WHILE*);
    END DiscardStringList;

(************************************************************************)

PROCEDURE PlotStringListClipped (SL: StringList;  R: Rectangle);

    (* Plots all strings in SL, clipping them on the display such that	*)
    (* only the part inside R is shown.					*)

    BEGIN
	WHILE SL <> NIL DO
	    WITH SL^ DO
		WITH R DO
		    ClippedString (textptr^, location.x, location.y,
						length, colour,
						left, right, bottom, top);
		END (*WITH*);
	    END (*WITH*);
	    SL := SL^.next;
	END (*WHILE*);
    END PlotStringListClipped;

(************************************************************************)

PROCEDURE PlotUpStringListClipped (USL: StringList;  R: Rectangle);

    (* Plots all strings in USL, clipping them on the display such	*)
    (* that only the part inside R is shown.				*)

    BEGIN
	WHILE USL <> NIL DO
	    WITH USL^ DO
		WITH R DO
		    ClippedUpString (textptr^, location.x, location.y,
						length, colour,
						left, right, bottom, top);
		END (*WITH*);
	    END (*WITH*);
	    USL := USL^.next;
	END (*WHILE*);
    END PlotUpStringListClipped;

(************************************************************************)
(*			OPERATIONS ON TYPE Tile				*)
(************************************************************************)

PROCEDURE Unlink (tile: Tile);

    (* Removes the tile from its stack, without otherwise changing it.	*)

    VAR slot: TileSlot;  current, above: Tile;

    BEGIN
	slot := tile^.slot;
	current := slot^.stacktop;  above := NIL;
	WHILE current <> tile DO
	    above := current;  current := current^.under;
	END (*WHILE*);
	IF above = NIL THEN
	    slot^.stacktop := current^.under;
	ELSE
	    above^.under := current^.under;
	END (*IF*);
	current^.under := NIL;
    END Unlink;

(************************************************************************)

PROCEDURE Display (slot: TileSlot);

    (* Displays the contents of the tile on top of the stack for the	*)
    (* given slot.  If the slot is empty, clears the screen area	*)
    (* described by slot^.shape.					*)

    (* Remark: should perhaps include a parameter to say whether the	*)
    (* entire TileSet is being redisplayed - in which case it would be	*)
    (* more efficient to skip displaying lines and strings in this	*)
    (* procedure, and let the caller display the unclipped versions.	*)

    VAR PL: PointList;  LL: LineList;  toptile: Tile;
	background: ColourType;  R: Rectangle;

    BEGIN
	WITH slot^ DO
	    R := shape;
	    toptile := stacktop;
	    IF toptile = NIL THEN background := 0;
	    ELSE background := toptile^.set^.background;
	    END (*IF*);
	END (*WITH*);

	(* Fill in the background *)

	WITH R DO
	    Fill (left, bottom, right, top, background);
	END (*WITH*);

	(* Is there a tile present?	*)

	IF toptile = NIL THEN
	    RETURN;
	END (*IF*);

	(* Yes, display its contents.	*)

	PL := toptile^.points;
	WHILE PL <> NIL DO
	    WITH PL^ DO
		WITH datum DO
		    PlotDot (x, y, colour);
		END (*WITH*);
	    END (*WITH*);
	    PL := PL^.next;
	END (*WHILE*);

	WITH toptile^.set^ DO
	    PlotLineListClipped (lines, R);
	    PlotStringListClipped (strings, R);
	    PlotUpStringListClipped (upstrings, R);
	END (*WITH*);

    END Display;

(************************************************************************)

PROCEDURE CreateTile (VAR (*OUT*) T: Tile;  Slot: TileSlot);

    (* Creates a new tile T on top of the stack for the given slot.	*)
    (* The tile is not attached to any set.				*)

    BEGIN
	NEW (T);
	WITH T^ DO
	    under := Slot^.stacktop;  next := NIL;  set := NIL;
	    slot := Slot;  points := NIL;
	END (*WITH*);
	Slot^.stacktop := T;
    END CreateTile;

(************************************************************************)

PROCEDURE DiscardTile (tile: Tile);

    (* Destroys a tile.  This includes removing the tile from its stack	*)
    (* and updating the screen display if necessary.			*)

    VAR wasontop: BOOLEAN;

    BEGIN
	WITH tile^ DO
	    wasontop := tile = slot^.stacktop;
	    Unlink (tile);
	    IF wasontop THEN Display (slot) END(*IF*);
	    DiscardPointList (points);
	END (*WITH*);
	DISPOSE (tile);
    END DiscardTile;

(************************************************************************)

PROCEDURE ClearTileData (tile: Tile);

    (* Removes all points from the tile, and displays the blanked	*)
    (* region if the tile is on top of its stack.			*)

    BEGIN
	WITH tile^ DO
	    DiscardPointList (points);
	    IF tile = slot^.stacktop THEN
		Display (slot);
	    END (*IF*);
	END (*WITH*);
    END ClearTileData;

(************************************************************************)

PROCEDURE TileSetMemory (T: TileSet;  memory: BOOLEAN);

    (* Specifying a FALSE value for the memory parameter means that	*)
    (* subsequent data sent to this TileSet will be written to the	*)
    (* screen but not remembered.  This saves time and memory, the only	*)
    (* penalty being that data covered by an overlapping TileSet will	*)
    (* be lost.  Specifying TRUE restores the default condition, where	*)
    (* all data are retained for refreshing the screen when necessary.	*)

    BEGIN
	T^.retain := memory;
    END TileSetMemory;

(************************************************************************)

PROCEDURE PutTileOnTop (tile: Tile);

    (* Puts a tile on the top of its stack and displays it.  (Does	*)
    (* nothing if the tile is already on top of its stack.)		*)

    VAR slot: TileSlot;

    BEGIN
	slot := tile^.slot;
	IF slot^.stacktop <> tile THEN
	    Unlink (tile);
	    tile^.under := slot^.stacktop;
	    slot^.stacktop := tile;
	    Display (slot);
	END (*IF*);
    END PutTileOnTop;

(************************************************************************)

PROCEDURE FindTile (TS: TileSet;  p: Point): Tile;

    (* Returns the tile in TS whose TileSlot contains p.  Also puts	*)
    (* this tile on the top of its stack and displays it.  NOTE: we	*)
    (* assume that the caller has checked that p lies in the region	*)
    (* covered by TS.							*)

    VAR current: Tile;  x, y: CARDINAL;

    BEGIN
	x := p.x;  y := p.y;
	current := TS^.head;
	LOOP
	    IF Inside (x, y, current^.slot^.shape) THEN
		PutTileOnTop (current);
		RETURN current;
	    END (*IF*);
	    current := current^.next;
	END (*LOOP*);
    END FindTile;

(************************************************************************)

PROCEDURE AddPointToTile (p: Point;  colour: ColourType;  T: Tile);

    (* Appends p to the list of points in T.	*)

    VAR PL: PointList;

    BEGIN
	NEW (PL);
	WITH PL^ DO
	    next := T^.points;  datum := p;
	END (*WITH*);
	PL^.colour := colour;
	T^.points := PL;
    END AddPointToTile;

(************************************************************************)

PROCEDURE SplitTile (VAR (*INOUT*) T: Tile;  VAR (*OUT*) T2: Tile;
			S2: TileSlot;  bound: CARDINAL;  Xsplit: BOOLEAN);

    (* Creates a new tile T2, to fit into slot S2 and in the same	*)
    (* TileSet as T; and moves some of the data from T to T2.  The data	*)
    (* moved are those with horizontal coordinate >= bound in the case	*)
    (* Xsplit = TRUE, or those with vertical coordinate >= bound in the	*)
    (* case Xsplit = FALSE.  T2 is left on the top of the S2 stack.	*)
    (* Remark: the order of the points in their PointList is altered	*)
    (* as they are shifted, but this shouldn't matter since the list	*)
    (* is not being used as an ordered set.				*)

    VAR pcurrent, pprevious, pfollowing: PointList;
	test: CARDINAL;

    BEGIN
	CreateTile (T2, S2);  T2^.set := T^.set;
	T2^.next := T^.next;  T^.next := T2;

	(* Work through the points in T^.points, shifting them to	*)
	(* T2^.points as necessary.					*)

	pcurrent := T^.points;  pprevious := NIL;
	WHILE pcurrent <> NIL DO
	    pfollowing := pcurrent^.next;
	    WITH pcurrent^.datum DO
		IF Xsplit THEN test := x ELSE test := y END(*IF*);
	    END (*WITH*);
	    IF test >= bound THEN

		(* Remove the point from T^.points.	*)

		IF pprevious = NIL THEN T^.points := pfollowing
		ELSE pprevious^.next := pfollowing;
		END (*IF*);

		(* Put the point into T2^.points.	*)

		pcurrent^.next := T2^.points;  T2^.points := pcurrent;

	    ELSE
		pprevious := pcurrent;
	    END (*IF*);
	    pcurrent := pfollowing;

	END (*WHILE*);

    END SplitTile;

(************************************************************************)

PROCEDURE MergeTiles (VAR (*INOUT*) T1, T2: Tile);

    (* The opposite operation to SplitTile: all data from T2 are moved	*)
    (* into T1, and T2 is destroyed.					*)

    VAR previous, current: Tile;  plast: PointList;

    BEGIN
	(* Find the predecessor of T2 in its TileSet. *)

	previous := NIL;  current := T2^.set^.head;
	WHILE current <> T2 DO
	    previous := current;  current := current^.next;
	END (*WHILE*);

	(* Remove T2 from the set. *)

	IF previous = NIL THEN T2^.set^.head := T2^.next
	ELSE previous^.next := T2^.next
	END (*IF*);
	IF T2 = T2^.set^.tail THEN
	    T2^.set^.tail := previous;
	END (*IF*);

	(* Move T2's PointList into T1. *)

	plast := T1^.points;
	IF plast = NIL THEN
	    T1^.points := T2^.points;
	ELSE
	    WHILE plast^.next <> NIL DO
		plast := plast^.next;
	    END (*WHILE*);
	    plast^.next := T2^.points;
	END (*IF*);

	(* All done, discard T2. *)

	DISPOSE (T2);

    END MergeTiles;

(************************************************************************)

PROCEDURE MatchingStack (stack1, stack2: TileStack): BOOLEAN;

    (* The input parameters each point to the top of a stack of tiles.	*)
    (* We return TRUE if the two stacks are equal in the following	*)
    (* sense: for each tile in stack1, there is a corresponding tile in	*)
    (* stack2 (and vice versa) belonging to the same TileSet.		*)

    VAR copy: TileStack;

    BEGIN
	(* Simplification: if the tiles on the two stacks really do	*)
	(* belong to the same TileSet, then they have gone through the	*)
	(* same history, and therefore should be stacked in the same	*)
	(* order.							*)

	LOOP
	    IF stack1 = NIL THEN
		RETURN (stack2 = NIL);
	    END (*IF*);
	    IF stack2 = NIL THEN
		RETURN FALSE;
	    END (*IF*);
	    IF stack1^.set <> stack2^.set THEN
		RETURN FALSE;
	    END (*IF*);
	    stack1 := stack1^.under;
	    stack2 := stack2^.under;
	END (*LOOP*);

    END MatchingStack;

(************************************************************************)

PROCEDURE MergeStacks (VAR (*OUT*) stack1: TileStack;  stack2: TileStack);

    (* The input parameters each point to the top of a stack of tiles.	*)
    (* On exit all data from tiles in stack2 have been moved into the	*)
    (* corresponding tiles in stack1, and the tiles in stack2 have been	*)
    (* destroyed.							*)

    VAR T1, T2: Tile;

    BEGIN
	T1 := stack1;
	WHILE stack2 <> NIL DO
	    T2 := stack2;  stack2 := stack2^.under;
	    MergeTiles (T1, T2);
	    T1 := T1^.under;
	END (*WHILE*);
    END MergeStacks;

(************************************************************************)
(*			OPERATIONS ON TYPE TileSlot			*)
(************************************************************************)

PROCEDURE DisplayAllSlots (S: TileSlot;  colour: ColourType);

    (* For testing: draws the boundaries of all TileSlots, pauses a	*)
    (* while, and then erases the drawing.  Because this procedure is	*)
    (* used only during module testing, we're not particularly fussy	*)
    (* about leaving the screen picture in a completely clean state.	*)
    (* The parameters specify the slot we're currently working on - we	*)
    (* display S in colour "colour".					*)

    VAR dummy: CHAR;

    PROCEDURE DrawSlotOutlines (colour: ColourType);

	VAR current: TileSlot;

	BEGIN
	    current := SlotListHead;
	    WHILE current <> NIL DO
		PlotRectangle (current^.shape, colour);
		current := current^.nextslot;
	    END (*WHILE*);
	END DrawSlotOutlines;

    (********************************************************************)

    BEGIN
	DrawSlotOutlines (1);
	IF S <> NIL THEN
	    PlotRectangle (S^.shape, colour);
	END (*IF*);
	Release (MainLock);
	dummy := InKey();
	Obtain (MainLock);
	DrawSlotOutlines (0);
    END DisplayAllSlots;

(************************************************************************)

PROCEDURE FindSlot (x, y: CARDINAL): TileSlot;

    (* Returns a slot containing the point (x,y).	*)

    VAR p: TileSlot;

    BEGIN
	p := SlotListHead;
	LOOP
	    IF Inside (x, y, p^.shape) THEN
		RETURN p;
	    END (*IF*);
	    p := p^.nextslot;
	END (*LOOP*);
    END FindSlot;

(************************************************************************)

PROCEDURE SplitSlot (VAR (*INOUT*) S: TileSlot;  bound: CARDINAL;
							Xsplit: BOOLEAN);

    (* Breaks S into two adjacent tile slots - side by side in the case	*)
    (* Xsplit = TRUE, or one on top of the other when Xsplit = FALSE.	*)
    (* On return S is the leftmost or bottommost, as appropriate, and	*)
    (* S^.nextslot is the other.					*)

    VAR S2: TileSlot;  T, T2, bottom: Tile;

    BEGIN
	NEW (S2);
	S2^ := S^;
	S^.nextslot := S2;
	IF Xsplit THEN
	    S^.shape.right := bound - 1;
	    S2^.shape.left := bound;
	ELSE
	    S^.shape.top := bound - 1;
	    S2^.shape.bottom := bound;
	END (*IF*);
	S2^.stacktop := NIL;

	(* This completes the splitting of the TileSlot itself.  Now we	*)
	(* must also split every Tile in the stack for the original	*)
	(* TileSlot, and construct a stack for the S2 TileSlot.  Note	*)
	(* that, because a newly created tile goes on top of the stack	*)
	(* for its slot, we need to shuffle stack elements to avoid a	*)
	(* situation where the stack constructed for S2 would be upside	*)
	(* down relative to the stack for S.				*)

	T := S^.stacktop;
	IF T <> NIL THEN
	    SplitTile (T, bottom, S2, bound, Xsplit);
	    LOOP
		T := T^.under;
		IF T = NIL THEN EXIT(*LOOP*) END(*IF*);
		SplitTile (T, T2, S2, bound, Xsplit);

		(* Move the newly created tile T2 and move it from the	*)
		(* top to the bottom of S2's stack.  This ensures that	*)
		(* the new stack is built in the same order as the	*)
		(* original stack.					*)

		S2^.stacktop := T2^.under;  T2^.under := NIL;
		bottom^.under := T2;  bottom := T2;
	    END (*LOOP*);
	END (*IF*);

	IF testing THEN
	    DisplayAllSlots (S, 4);
	END (*IF*);

    END SplitSlot;

(************************************************************************)
(*			COMBINING ADJACENT SLOTS			*)
(************************************************************************)

PROCEDURE Join (VAR (*INOUT*) S1, S2: TileSlot;  preS2: TileSlot;
						newshape: Rectangle);

    (* On entry, S1 and S2 have already been found to be suitable for	*)
    (* combining, preS2 is the predecessor of S2 in the master list of	*)
    (* TileSlots, and newshape is the shape of the union of S1 and S2.	*)
    (* On exit, S1 is the union, the old S2 has been destroyed, and	*)
    (* S2 is the successor of the old S2 in the master list.		*)

    VAR following: TileSlot;

    BEGIN
	(* Remove S2 from the master list of TileSlots. *)

	following := S2^.nextslot;
	IF preS2 = NIL THEN SlotListHead := following
	ELSE preS2^.nextslot := following
	END (*IF*);

	(* Combine corresponding tiles in S1 and S2,	*)
	(* leaving the result in S1.			*)

	S1^.shape := newshape;
	MergeStacks (S1^.stacktop, S2^.stacktop);
	DISPOSE (S2);
	S2 := following;

	IF testing THEN
	    DisplayAllSlots (S1, 3);
	END (*IF*);

    END Join;

(************************************************************************)

PROCEDURE UpDownMatch (R1, R2: Rectangle;
				VAR (*OUT*) union: Rectangle): BOOLEAN;

    (* If R1 and R2 are vertically adjacent rectangles, returns TRUE	*)
    (* and sets "union" to be the combined rectangle.  Otherwise	*)
    (* returns FALSE, and the "union" result is meaningless.		*)

    BEGIN
	union := R1;
	IF (R1.left = R2.left) AND (R1.right = R2.right) THEN
	    (* Possible above/below adjacency *)
	    IF R2.bottom = R1.top + 1 THEN
		union.top := R2.top;
		RETURN TRUE;
	    ELSIF R1.bottom = R2.top + 1 THEN
		union.bottom := R2.bottom;
		RETURN TRUE;
	    ELSE
		RETURN FALSE;
	    END (*IF*);
	ELSE
	    RETURN FALSE;
	END (*IF*);
    END UpDownMatch;

(************************************************************************)

PROCEDURE SideMatch (VAR (*INOUT*) S1, S2, preS2: TileSlot;
			VAR (*OUT*) union: Rectangle;
			VAR (*OUT*) HaveSplit: BOOLEAN): BOOLEAN;

    (* Like UpDownMatch, but checks for left/right adjacency.  However	*)
    (* we're more generous in this case about the meaning of "adjacent"	*)
    (* since we're prepared to split S1 and/or S2 to make the heights	*)
    (* match.  (This is why the first two parameters have to specify	*)
    (* TileSlots rather than simply their shapes.)  If a split occurs	*)
    (* the pieces split off have their "mark" fields set - since those	*)
    (* pieces will have to be re-evaluated for possible further		*)
    (* combinations - and we return with "HaveSplit" set to TRUE.	*)
    (* Exception: if we can guarantee that the slots needing to be	*)
    (* rechecked come after S1 in the master list of slots, then we	*)
    (* don't set HaveSplit.						*)
    (* Note: parameter preS2 is the predecessor of S2 in the master	*)
    (* list of tile slots; it's updated appropriately if we have to	*)
    (* modify that list.						*)

    VAR R1, R2: Rectangle;  adjacent: BOOLEAN;

    BEGIN
	HaveSplit := FALSE;
	R1 := S1^.shape;  R2 := S2^.shape;
	union := R1;
	IF (R1.bottom > R2.top) OR (R1.top < R2.bottom) THEN
	    adjacent := FALSE;
	ELSIF R2.left = R1.right + 1 THEN
	    union.right := R2.right;
	    adjacent := TRUE;
	ELSIF R1.left = R2.right + 1 THEN
	    union.left := R2.left;
	    adjacent := TRUE;
	ELSE
	    adjacent := FALSE;
	END (*IF*);

	IF adjacent THEN

	    (* The two rectangles are adjacent, but we haven't yet	*)
	    (* checked their heights.  Perform one or two splits, as	*)
	    (* necessary, to get the heights to line up.		*)

	    IF R1.top > R2.top THEN
		S1^.mark := TRUE;
		SplitSlot (S1, R2.top+1, FALSE);
		union.top := R2.top;
		IF preS2 = S1 THEN
		    preS2 := S1^.nextslot;
		END (*IF*);
	    ELSIF R1.top < R2.top THEN
		S2^.mark := TRUE;
		SplitSlot (S2, R1.top+1, FALSE);
		HaveSplit := TRUE;
	    END (*IF*);

	    IF R1.bottom < R2.bottom THEN
		S1^.mark := TRUE;
		SplitSlot (S1, R2.bottom, FALSE);
		HaveSplit := TRUE;
		IF preS2 = S1 THEN
		    preS2 := S1^.nextslot;
		END (*IF*);
		S1 := S1^.nextslot;
		union.bottom := R2.bottom;
	    ELSIF R1.bottom > R2.bottom THEN
		S2^.mark := TRUE;
		SplitSlot (S2, R1.bottom, FALSE);
		preS2 := S2;  S2 := S2^.nextslot;
		HaveSplit := TRUE;
	    END (*IF*);

	END (*IF*);

	RETURN adjacent;

    END SideMatch;

(************************************************************************)

PROCEDURE MergeWithNeighbours (VAR (*INOUT*) S: TileSlot): BOOLEAN;

    (* Combines S with its neighbours if this turns out to be possible.	*)
    (* The condition for combining slots is that the tiles in the two	*)
    (* slots belong to the same TileSets, and that the union of their	*)
    (* shapes is again a rectangular region.  A function result of	*)
    (* TRUE indicates that all recombinations (if any) involving S	*)
    (* have already been taken care of by this procedure, and further	*)
    (* that no slots preceding S in the master list of TileSlots have	*)
    (* been modified.  A result of FALSE indicates that this procedure	*)
    (* may have created the conditions for further recombinations, i.e.	*)
    (* that the caller needs to rescan the master list of TileSlots.	*)

    VAR NoChange, HaveSplit, NoRescanNeeded: BOOLEAN;
	previous, S2: TileSlot;  union: Rectangle;

    BEGIN
	NoRescanNeeded := TRUE;
	REPEAT
	    NoChange := TRUE;

	    (* Search for a candidate S2 to be merged with S.	*)

	    previous := NIL;  S2 := SlotListHead;

	    WHILE S2 <> NIL DO

		(* Check for adjacency.  In the above/below case we	*)
		(* simply merge.  In the left/right case we merge when	*)
		(* an exact match is found, but we also allow for the	*)
		(* possibility of a left or right neighbour which is	*)
		(* taller or shorter than S.  In the latter case, we	*)
		(* perform a split followed by a join.			*)

		IF (S2 <> S) AND MatchingStack (S^.stacktop, S2^.stacktop) AND
			(UpDownMatch (S^.shape, S2^.shape, union) OR
			    SideMatch (S, S2, previous, union, HaveSplit)) THEN
		    Join (S, S2, previous, union);
		    NoChange := FALSE;
		    IF HaveSplit THEN
			NoRescanNeeded := FALSE;  S2 := NIL;
		    END (*IF*);
		ELSE
		    previous := S2;
		    S2 := S2^.nextslot;
		END (*IF*);

	    END (*WHILE S2 <> NIL*);

	UNTIL NoChange;

	S^.mark := FALSE;
	RETURN NoRescanNeeded;

    END MergeWithNeighbours;

(************************************************************************)

PROCEDURE RecombineSlots;

    (* Goes through the master list of slots, checking all slots which	*)
    (* have their "mark" field set, and combining adjacent slots where	*)
    (* possible.  Note: in the process of doing this we sometimes have	*)
    (* to mark previously unmarked slots, so we have to keep looping	*)
    (* until we are sure we have cleared all marks.			*)

    VAR slot: TileSlot;  scanned: BOOLEAN;

    BEGIN
	REPEAT
	    scanned := TRUE;
	    slot := SlotListHead;
	    WHILE slot <> NIL DO
		IF slot^.mark THEN
		    scanned := scanned AND MergeWithNeighbours (slot);
		END (*IF*);
		slot := slot^.nextslot;
	    END (*WHILE*);
	UNTIL scanned;
    END RecombineSlots;

(************************************************************************)
(*			OPERATIONS ON TYPE TileSet			*)
(************************************************************************)

PROCEDURE CreateEmptyTileSet (VAR (*OUT*) TS: TileSet;  colour: ColourType);

    (* Creates a TileSet containing no tiles.	*)

    BEGIN
	NEW (TS);
	WITH TS^ DO
	    head := NIL;  tail := NIL;  lines := NIL;  strings := NIL;
	    upstrings := NIL;  background := colour;  retain := TRUE;
	END (*WITH*);
    END CreateEmptyTileSet;

(************************************************************************)

PROCEDURE AddToTileSet (VAR (*INOUT*) TS: TileSet;  S: TileSlot);

    (* Creates a new tile in slot S, adds it to set TS, and displays it.*)

    VAR p: Tile;

    BEGIN
	CreateTile (p, S);  p^.set := TS;
	Display (S);
	WITH TS^ DO
	    IF tail = NIL THEN head := p
	    ELSE tail^.next := p
	    END (*IF*);
	    tail := p;
	END (*WITH*);
    END AddToTileSet;

(************************************************************************)

PROCEDURE PutTileSetOnTop (TS: TileSet);

    (* Ensures that TS is fully displayed on the screen.	*)

    VAR tile: Tile;

    BEGIN
	tile := TS^.head;
	WHILE tile <> NIL DO
	    PutTileOnTop (tile);
	    tile := tile^.next;
	END (*WHILE*);
    END PutTileSetOnTop;

(************************************************************************)

PROCEDURE Redraw (TS: TileSet);

    (* Refreshes the visible parts of TS on the screen.  This procedure	*)
    (* is for use in the case where the TileSet contents have been	*)
    (* changed and what is already on the screen is obsolete.  (For the	*)
    (* case where we are simply adding to the existing contents there	*)
    (* are faster methods than calling this procedure.)			*)

    VAR tile: Tile;

    BEGIN
	tile := TS^.head;
	WHILE tile <> NIL DO
	    WITH tile^ DO
		IF tile = slot^.stacktop THEN
		    Display (slot);
		END (*IF*);
	    END (*WITH*);
	    tile := tile^.next;
	END (*WHILE*);
    END Redraw;

(************************************************************************)
(*		THE MAIN EXTERNALLY CALLABLE PROCEDURES			*)
(************************************************************************)

PROCEDURE CreateTileSet (border: Rectangle; background: ColourType): TileSet;

    (* Creates a TileSet which covers the given rectangular region.	*)
    (* The second parameter specifies the background colour.		*)
    (* This will usually require breaking up tiles of previously	*)
    (* created TileSets, but since the caller does not have access to	*)
    (* the internal structure of a TileSet this restructuring is	*)
    (* transparent to the caller.					*)

    VAR result: TileSet;  S: TileSlot;
	StillToHandle: Queue;
	RLptr: POINTER TO Rectangle;

    BEGIN
	Obtain (MainLock);

	(* Start by creating an empty tile set, and a list of		*)
	(* rectangles still to be dealt with.				*)

	CreateEmptyTileSet (result, background);
	CreateQueue (StillToHandle);
	NEW (RLptr);  RLptr^ := border;
	AddToQueue (StillToHandle, RLptr);

	(* The following loop breaks the rectangle into tiles by	*)
	(* gradually breaking up the rectangle.  The loop body has two	*)
	(* phases: (a) find a tile slot which matches or is contained	*)
	(* in the desired rectangle; (b) break off any parts of the	*)
	(* rectangle which do not fit into that slot, to be dealt with	*)
	(* in subsequent passes through the loop.			*)

	REPEAT
	    RLptr := TakeFromQueue (StillToHandle);
	    border := RLptr^;  DISPOSE (RLptr);

	    (* Find a tile slot containing the bottom left corner.	*)

	    S := FindSlot (border.left, border.bottom);

	    (* Break up S, if necessary, so that no part of S lies	*)
	    (* outside the rectangular region.  The order of these	*)
	    (* operations is significant: we work on the bottom and top	*)
	    (* edges before looking at the left and right edges, to	*)
	    (* ensure that horizontal cuts are done before vertical	*)
	    (* cuts.							*)

	    IF S^.shape.bottom < border.bottom THEN
		SplitSlot (S, border.bottom, FALSE);
		S := S^.nextslot;
	    END (*IF*);
	    IF S^.shape.top > border.top THEN
		SplitSlot (S, border.top+1, FALSE);
	    END (*IF*);

	    IF S^.shape.left < border.left THEN
		SplitSlot (S, border.left, TRUE);
		S := S^.nextslot;
	    END (*IF*);
	    IF S^.shape.right > border.right THEN
		SplitSlot (S, border.right+1, TRUE);
	    END (*IF*);

	    (* We have now split TileSlots to the point where the lower	*)
	    (* left corner of S is aligned with the lower left corner	*)
	    (* of border, and the whole of S either matches or is	*)
	    (* contained within the desired rectangle; so we have a	*)
	    (* slot we can use in the final result.  All that remains	*)
	    (* is to separate out those parts of the rectangle which	*)
	    (* don't fit inside S, and which will be dealt with in	*)
	    (* subsequent passes through the loop.			*)

	    IF border.right > S^.shape.right THEN
		NEW (RLptr);
		RLptr^ := border;  RLptr^.left := S^.shape.right + 1;
		border.right := S^.shape.right;
		AddToQueue (StillToHandle, RLptr);
	    END (*IF*);
	    IF border.top > S^.shape.top THEN
		NEW (RLptr);
		RLptr^ := border;  RLptr^.bottom := S^.shape.top + 1;
		border.top := S^.shape.top;
		AddToQueue (StillToHandle, RLptr);
	    END (*IF*);

	    AddToTileSet (result, S);

	UNTIL Empty (StillToHandle);

	DestroyQueue (StillToHandle);
	Release (MainLock);
	RETURN result;

    END CreateTileSet;

(************************************************************************)

PROCEDURE DiscardTileSet (VAR (*INOUT*) TS: TileSet);

    (* Destroys TileSet TS.	*)

    VAR current, following: Tile;

    BEGIN
	Obtain (MainLock);
	DiscardLineList (TS^.lines);
	DiscardStringList (TS^.strings);
	DiscardStringList (TS^.upstrings);
	current := TS^.head;
	WHILE current <> NIL DO
	    current^.slot^.mark := TRUE;
	    following := current^.next;
	    DiscardTile (current);
	    current := following;
	END (*WHILE*);
	DISPOSE (TS);
	RecombineSlots;
	Release (MainLock);
    END DiscardTileSet;

(************************************************************************)

PROCEDURE ClearTileSet (T: TileSet);

    (* Removes all points, lines, and text from T, and re-displays	*)
    (* the visible parts of T.						*)

    VAR current: Tile;

    BEGIN
	Obtain (MainLock);
	DiscardLineList (T^.lines);
	DiscardStringList (T^.strings);
	DiscardStringList (T^.upstrings);
	current := T^.head;
	WHILE current <> NIL DO
	    ClearTileData (current);
	    current := current^.next;
	END (*WHILE*);
	Release (MainLock);
    END ClearTileSet;

(************************************************************************)

PROCEDURE AddPoint (T: TileSet;  p: Point;  colour: ColourType);

    (* Adds a new point to TileSet T, and displays it on the screen.	*)

    VAR tile: Tile;

    BEGIN
	Obtain (MainLock);
	tile := FindTile (T, p);
	IF T^.retain THEN
	    AddPointToTile (p, colour, tile);
	END (*IF*);
	PlotDot (p.x, p.y, colour);
	Release (MainLock);
    END AddPoint;

(************************************************************************)

PROCEDURE AddLine (T: TileSet;  start, finish: Point;  colour: ColourType);

    (* Adds a new line to TileSet T, and displays it on the screen.	*)

    VAR LL: LineList;

    BEGIN
	Obtain (MainLock);
	PutTileSetOnTop (T);
	IF T^.retain THEN
	    NEW (LL);
	    WITH LL^ DO
		next := T^.lines;  end1 := start;  end2 := finish;
	    END (*WITH*);
	    LL^.colour := colour;
	    T^.lines := LL;
	END (*IF*);
	PlotLine (start.x, start.y, finish.x, finish.y, colour);
	Release (MainLock);
    END AddLine;

(************************************************************************)

PROCEDURE AddRectangle (T: TileSet;  R: Rectangle;  colour: ColourType);

    (* Draws a rectangular shape.  A shorthand for four AddLine calls.	*)

    VAR start, end: Point;

    BEGIN
	WITH R DO
	    WITH start DO
		x := left;  y := bottom;
	    END (*WITH*);
	    WITH end DO
		x := right;  y := bottom;
	    END (*WITH*);
	END (*WITH*);
	AddLine (T, start, end, colour);
	end.x := R.left;  end.y := R.top;
	AddLine (T, start, end, colour);
	start.x := R.right;  start.y := R.top;
	AddLine (T, start, end, colour);
	end.x := R.right;  end.y := R.bottom;
	AddLine (T, start, end, colour);
    END AddRectangle;

(************************************************************************)

PROCEDURE AddString (T: TileSet;  place: Point;
			VAR (*IN*) text: ARRAY OF CHAR;
			count: CARDINAL;  colour: ColourType;  R: Rectangle);

    (* Adds a string of count characters to tileset T, and displays it.	*)
    (* Points outside rectangle R are not displayed.			*)

    VAR SL: StringList;  j: CARDINAL;

    BEGIN
	Obtain (MainLock);
	PutTileSetOnTop (T);
	IF T^.retain THEN
	    NEW (SL);
	    WITH SL^ DO
		next := T^.strings;  location := place;
		length := count;
		ALLOCATE (textptr, count);
		FOR j := 0 TO count-1 DO
		    textptr^[j] := text[j];
		END (*FOR*);
	    END (*WITH*);
	    SL^.colour := colour;
	    T^.strings := SL;
	END (*IF*);
	WITH R DO
	    ClippedString (SL^.textptr^, place.x, place.y, count, colour,
						left, right, bottom, top);
	END (*WITH*);
	Release (MainLock);
    END AddString;

(************************************************************************)

PROCEDURE AddRotatedString (T: TileSet;  place: Point;
			VAR (*IN*) text: ARRAY OF CHAR;
			count: CARDINAL;  colour: ColourType;  R: Rectangle);

    (* Like AddString, but writes in the +Y direction.	*)

    VAR USL: StringList;  j: CARDINAL;

    BEGIN
	Obtain (MainLock);
	PutTileSetOnTop (T);
	IF T^.retain THEN
	    NEW (USL);
	    WITH USL^ DO
		next := T^.upstrings;  location := place;
		length := count;
		ALLOCATE (textptr, count);
		FOR j := 0 TO count-1 DO
		    textptr^[j] := text[j];
		END (*FOR*);
	    END (*WITH*);
	    USL^.colour := colour;
	    T^.upstrings := USL;
	END (*IF*);
	WITH R DO
	    ClippedUpString (USL^.textptr^, place.x, place.y, count, colour,
						left, right, bottom, top);
	END (*WITH*);
	Release (MainLock);

    END AddRotatedString;

(************************************************************************)

PROCEDURE ShiftTextUp (VAR (*INOUT*) List: StringList;
				amount: CARDINAL;  limit: INTEGER);

    (* Moves all character strings up by "amount" rows, discarding what	*)
    (* falls above the limit.						*)

    (* Layout fault: this procedure should be moved higher up in the	*)
    (* module after I have completed implemented scrolling.		*)

    VAR previous, current, following: StringList;
	felloff: BOOLEAN;

    BEGIN
	previous := NIL;  current := List;
	WHILE current <> NIL DO
	    following := current^.next;
	    WITH current^.location DO
		INC (y, amount);
		felloff := y > limit
	    END (*WITH*);
	    IF felloff THEN
		IF previous = NIL THEN List := following
		ELSE previous^.next := following;
		END (*IF*);
		DEALLOCATE (current^.textptr, current^.length);
		DISPOSE (current);
	    ELSE
		previous := current;
	    END (*IF*);
	    current := following;
	END (*WHILE*);
    END ShiftTextUp;

(************************************************************************)

PROCEDURE ShiftLinesUp (List: LineList;  amount: CARDINAL;  limit: INTEGER);

    (* Moves all lines up by "amount" rows, discarding what falls above	*)
    (* the limit.							*)

    (* Layout fault: this procedure should be moved higher up in the	*)
    (* module after I have completed implemented scrolling.		*)

    VAR previous, current, following: LineList;
	felloff: BOOLEAN;

    BEGIN
	previous := NIL;  current := List;
	WHILE current <> NIL DO
	    following := current^.next;
	    WITH current^.end1 DO
		INC (y, amount);
		felloff := y > limit;
	    END (*WITH*);
	    WITH current^.end2 DO
		INC (y, amount);
		felloff := felloff AND (y > limit);
	    END (*WITH*);
	    IF felloff THEN
		IF previous = NIL THEN List := following
		ELSE previous^.next := following;
		END (*IF*);
		DISPOSE (current);
	    ELSE
		previous := current;
	    END (*IF*);
	    current := following;
	END (*WHILE*);
    END ShiftLinesUp;

(************************************************************************)

PROCEDURE ScrollContents (TS: TileSet;  amount: INTEGER;  R: Rectangle);

    (* Moves all data within R up by "amount" rows, discarding what	*)
    (* falls outside the rectangle.					*)

    BEGIN
	Obtain (MainLock);
	PutTileSetOnTop (TS);

	WITH R DO

	    (* Physically move the data on the screen. *)

	    ACopy (left, top-amount, right-left+1,
				top-amount-bottom+1, 0, amount);

	    (* Clear the vacated section at the bottom. *)

	    Fill (left, bottom, right, bottom+amount-1,
						TS^.background);

	END (*WITH*);

	(* Modify our records of where everything is. *)
	(* The shifting of points is not yet properly implemented. *)
(*
	ShiftPointsUp (TS, amount, R.top);
*)
	ShiftLinesUp (TS^.lines, amount, R.top);
	ShiftTextUp (TS^.strings, amount, R.top);
	ShiftTextUp (TS^.upstrings, amount, R.top);
	Release (MainLock);

    END ScrollContents;

(************************************************************************)
(*			CLEANUP ON TERMINATION				*)
(************************************************************************)

PROCEDURE Cleanup;

    (* Tidies up all the leftover data on program termination, and	*)
    (* restores the original display mode.				*)

    VAR current: TileSlot;

    BEGIN
	Obtain (MainLock);
	WHILE SlotListHead <> NIL DO
	    IF SlotListHead^.stacktop = NIL THEN
		current := SlotListHead;
		SlotListHead := SlotListHead^.nextslot;
		DISPOSE (current);
	    ELSE
		DiscardTileSet (SlotListHead^.stacktop^.set);
	    END (*IF*);
	END (*WHILE*);
	RestoreOriginalMode;
	Release (MainLock);
    END Cleanup;

(************************************************************************)
(*			MODULE INITIALISATION				*)
(************************************************************************)

PROCEDURE SetUpInitialTileSlot;

    (* Creates the master list of TileSlots.  Since we don't yet know	*)
    (* what video mode will be used, we assume a very large screen.	*)
    (* The fact that the initial tile is certainly too large is not a	*)
    (* problem, since it will be split as new TileSets are created,	*)
    (* and the overhead of holding TileSlots for the unusable parts is	*)
    (* minor.								*)

    CONST Large = MAX(INTEGER)-1;

    BEGIN
	Obtain (MainLock);
	NEW (SlotListHead);
	WITH SlotListHead^ DO
	    WITH shape DO
		left := 0;  bottom := 0;
		top := Large;  right := Large;
	    END (*WITH*);
	    nextslot := NIL;  stacktop := NIL;  mark := FALSE;
	END (*WITH*);
	Release (MainLock);
    END SetUpInitialTileSlot;

(************************************************************************)

BEGIN
    CreateLock (MainLock);
    SetUpInitialTileSlot;
    SetTerminationProcedure (Cleanup);
END Tiles.
