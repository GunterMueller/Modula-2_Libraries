IMPLEMENTATION MODULE RowEditor;

	(********************************************************)
	(*							*)
	(*		Screen data capture			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:						*)
	(*	Basic features working.  Known faults are:	*)
	(*	  1.	(fixed)					*)
	(*	  2.	The criterion for deciding in which	*)
	(*		field to start editing could be better.	*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* proc *)	ADR;

FROM Trace IMPORT
    (* proc *)	InTrace, OutTrace, Pause;

FROM Windows IMPORT
    (* proc *)	SetCursor, SaveCursor,
		(* and for debugging: *)
    (* proc *)	OpenSimpleWindow, CloseWindow, WriteLn, WriteString;

FROM NumericIO IMPORT				(* for debugging *)
    (* proc *)	WriteAddress, WriteCard;

FROM Keyboard IMPORT
    (* proc *)	InKey, PutBack;

FROM FieldEditor IMPORT
    (* proc *)	WriteField, EditField, SameType, DefineFieldType;

FROM Menus IMPORT
    (* proc *)	DisplayMenu, SelectFromMenu;

FROM ListEditor IMPORT
    (* type *)	List, ListFormat,
    (* proc *)	WriteList, EditList;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)	AddOffset;

(************************************************************************)

CONST testing = FALSE;

CONST Esc = CHR(27);

TYPE
    ListPtr = POINTER TO List;

    (* The fields in a Field record are:				*)
    (*		pointer:	address of the variable in this field	*)
    (*		column:		screen position				*)
    (*		size:		# of char positions to use on screen	*)
    (*		type:		type of the variable			*)
    (*		left, right:	pointers to adjacent fields		*)
    (* Special case: if size=0 then it is up to the component editor to	*)
    (* handle the cursor movement keys and to give the user feedback	*)
    (* on where the cursor is.  This special case arises when the	*)
    (* field has internal structure of its own.				*)

    FieldPointer = POINTER TO FieldRecord;
    FieldRecord =   RECORD
			pointer: ADDRESS;
			column: CARDINAL;
			size: CARDINAL;
			type: FieldType;
			left, right: FieldPointer;
		    END (*RECORD*);

    StructureRow = FieldPointer;

    (* For a menu, the pointer field in the Field record points at a	*)
    (* "MenuHead" record rather than directly at the user variable.	*)
    (* The MenuHead record contains the address of the user variable,	*)
    (* the specification of which Menu to use, and the size of the	*)
    (* space to allocate on the screen.					*)

    MenuHead =  RECORD
		    address: POINTER TO CARDINAL;
		    menu: Menu;
		    lines, width: CARDINAL;
		END (*RECORD*);
    MenuPointer = POINTER TO MenuHead;

    (* For a linear list, the pointer field in the Field record points	*)
    (* at a "ListHead" record rather than directly at the user		*)
    (* variable.  The ListHead record contains the address of the user	*)
    (* variable (which is itself a pointer to the head of the list),	*)
    (* and the format to use when writing or editing the list.		*)

    ListHead =  RECORD
		    address: ListPtr;
		    format: ListFormat;
		END (*RECORD*);
    ListHeadPtr = POINTER TO ListHead;

VAR MenuType, ListType: FieldType;

(************************************************************************)
(*			      SCREEN OUTPUT				*)
(************************************************************************)

PROCEDURE WriteMenuField (w: Window;  headptr: MenuPointer;  dummy: CARDINAL);

    BEGIN
	WITH headptr^ DO
	    DisplayMenu (w, menu, lines, width, address^);
	END (*WITH*);
    END WriteMenuField;

(************************************************************************)

PROCEDURE WriteListField (w: Window;  headptr: ListHeadPtr;  dummy: CARDINAL);

    BEGIN
	WriteList (w, headptr^.address^, headptr^.format);
    END WriteListField;

(************************************************************************)

PROCEDURE WriteRow (w: Window;  R: StructureRow;  line: CARDINAL);

    (* Writes R on row "line" of window w.	*)

    BEGIN
	WHILE R <> NIL DO
	    WITH R^ DO
		SetCursor (w, line, column);
		WriteField (w, pointer, type, size);
	    END (*WITH*);
	    R := R^.right;
	END (*WHILE*);
    END WriteRow;

(************************************************************************)

PROCEDURE StartColumn (R: StructureRow): CARDINAL;

    (* Returns the screen column of the first field in R.	*)

    BEGIN
	RETURN R^.column;
    END StartColumn;

(************************************************************************)
(*			THE BUILT-IN FIELD EDITORS			*)
(************************************************************************)

PROCEDURE EditMenuField (w: Window;  VAR (*INOUT*) headptr: MenuPointer;
							dummy: CARDINAL);

    VAR result, row, col: CARDINAL;

    BEGIN
	SaveCursor (w, row, col);
	WITH headptr^ DO
	    result := SelectFromMenu (menu);
	    IF result = 0 THEN
		SetCursor (w, row, col);
		DisplayMenu (w, menu, lines, width, address^);
	    ELSE
		address^ := result;
	    END (*IF*);
	END (*WITH*);
    END EditMenuField;

(************************************************************************)

PROCEDURE EditListField (w: Window;  VAR (*INOUT*) headptr: ListHeadPtr;
							dummy: CARDINAL);

    BEGIN
	EditList (w, headptr^.address^, headptr^.format);
    END EditListField;

(************************************************************************)
(*		   INTRODUCING A NEW FIELD TO THE SYSTEM		*)
(************************************************************************)

PROCEDURE NewRow (VariableAddress: ADDRESS;  ftype: FieldType;
			screencolumn, width: CARDINAL): StructureRow;

    (* Creates a new row containing a single field.	*)

    VAR result: StructureRow;

    BEGIN
	NEW (result);
	WITH result^ DO
	    pointer := VariableAddress;
	    column := screencolumn;
	    size := width;  type := ftype;
	    left := NIL;  right := NIL;
	END (*WITH*);
	RETURN result;
    END NewRow;

(************************************************************************)

PROCEDURE NewMenu (VAR (*IN*) variable: CARDINAL;  M: Menu;
			screencolumn, rows, columns: CARDINAL): StructureRow;

    (* Creates a new row containing a menu field.  The screencolumn	*)
    (* field specifies the leftmost column within the screen window,	*)
    (* the rows and columns fields give the size on the screen.		*)

    VAR MP: MenuPointer;

    BEGIN
	NEW (MP);
	WITH MP^ DO
	    address := ADR (variable);  menu := M;
	    lines := rows;  width := columns;
	END (*WITH*);
	RETURN NewRow (MP, MenuType, screencolumn, 0);
    END NewMenu;

(************************************************************************)

PROCEDURE NewList (VAR (*IN*) variable: List;  f: ListFormat;
				screencolumn: CARDINAL): StructureRow;

    (* Creates a new row containing a list field.	*)

    VAR listhead: ListHeadPtr;

    BEGIN
	NEW (listhead);
	WITH listhead^ DO
	    address := ADR (variable);  format := f;
	END (*WITH*);
	RETURN NewRow (listhead, ListType, screencolumn, 0);
    END NewList;

(************************************************************************)
(*			    TEST PROCEDURES				*)
(************************************************************************)

PROCEDURE DumpField (w: Window;  F: FieldPointer);

    (* For debugging: writes a representation of F^ to the screen.	*)

    BEGIN
	WriteLn (w);
	WriteAddress (w, F);  WriteString (w, "> ");
	WITH F^ DO
	    WriteAddress (w, pointer);  WriteString (w, ", ");
	    WriteCard (w, column);  WriteString (w, ", ");
	    WriteCard (w, size);  WriteString (w, ", ");
	    WriteAddress (w, left);  WriteString (w, ", ");
	    WriteAddress (w, right);
	END (*WITH*);
    END DumpField;

(************************************************************************)

PROCEDURE DumpRow (w: Window;  R: StructureRow);

    (* For debugging: writes a representation of R to the screen.	*)

    BEGIN
	IF R = NIL THEN
	    WriteLn (w);  WriteString (w, "  <empty row>");
	END (*IF*);
	WHILE R <> NIL DO
	    DumpField (w, R);  R := R^.right;
	END (*WHILE*);
    END DumpRow;

(************************************************************************)

PROCEDURE DebugDump (caller: ARRAY OF CHAR;  R: StructureRow);

    (* For debugging: identifies the caller and dumps R.	*)

    VAR w: Window;

    BEGIN
	OpenSimpleWindow (w, 0, 10, 0, 79);
	WriteString (w, "DebugDump called from ");
	WriteString (w, caller);
	DumpRow (w, R);
	Pause;
	CloseWindow (w);
    END DebugDump;

(************************************************************************)
(*		  CONSTRUCTING COMPLEX STRUCTURE TYPES			*)
(************************************************************************)

PROCEDURE CombineRows (VAR (*INOUT*) A: StructureRow;  B: StructureRow);

    (* Merges the row to which B is pointing with the row to which A is	*)
    (* pointing, leaving A pointing to the result.  (The structure of	*)
    (* the B row is lost in the process of doing this).			*)

    VAR previous, current: FieldPointer;

    BEGIN
	InTrace ("CombineRows");
	previous := NIL;  current := A;
	WHILE B <> NIL DO

	    (* Find a place to insert the first element on the B list. *)

	    LOOP
		IF current = NIL THEN EXIT (*LOOP*) END (*IF*);
		IF B^.column < current^.column THEN EXIT (*LOOP*) END (*IF*);
		previous := current;  current := current^.right;
	    END (*LOOP*);

	    (* Take the whole of the B list, insert it after previous^,	*)
	    (* then take what remains of the original destination list	*)
	    (* and call it the B list.  Swapping lists like this is a	*)
	    (* little unconventional, but it works, and in many cases	*)
	    (* it speeds up the merge.					*)

	    IF previous = NIL THEN
		A := B;
	    ELSE
		previous^.right := B;
	    END (*IF*);
	    B^.left := previous;
	    previous := B;  B := current;  current := previous^.right;

	END (*WHILE*);
	OutTrace ("CombineRows");
    END CombineRows;

(************************************************************************)

PROCEDURE CopyOfRow (R: StructureRow): StructureRow;

    (* Makes a duplicate copy of R.  The variables to be edited are not	*)
    (* duplicated; we simply set up a duplicate set of pointers.	*)

    VAR result: StructureRow;  newfield: FieldPointer;
	oldheadptr, headptr: ListHeadPtr;
	oldmenuheadptr, menuheadptr: MenuPointer;

    BEGIN
	InTrace ("CopyOfRow");
	result := NIL;
	WHILE R <> NIL DO
	    NEW (newfield);
	    WITH newfield^ DO
		IF SameType (R^.type, MenuType) THEN
		    oldmenuheadptr := R^.pointer;
		    NEW (menuheadptr);  menuheadptr^ := oldmenuheadptr^;
		    pointer := menuheadptr;
		ELSIF SameType (R^.type, ListType) THEN
		    oldheadptr := R^.pointer;
		    NEW (headptr);  headptr^ := oldheadptr^;
		    pointer := headptr;
		ELSE
		    pointer := R^.pointer;
		END (*IF*);
		column := R^.column;
		size := R^.size;  type := R^.type;
		left := NIL;  right := NIL;
	    END (*WITH*);
	    CombineRows (result, newfield);
	    R := R^.right;
	END (*WHILE*);
	OutTrace ("CopyOfRow");
	IF testing THEN
	    DebugDump ("CopyOfRow", result);
	END (*IF*);
	RETURN result;
    END CopyOfRow;

(************************************************************************)

PROCEDURE DeleteRow (R: StructureRow);

    (* Deallocates the storage which was used in setting up row R.	*)
    (* Note that this has nothing to do with the space used by		*)
    (* variables to which R gives access; we delete only the overhead	*)
    (* space which was originally allocated by this module.		*)

    VAR temp: FieldPointer;

    BEGIN
	InTrace ("DeleteRow");
	WHILE R <> NIL DO
	    temp := R^.right;
	    IF SameType (R^.type, MenuType) THEN
		DEALLOCATE (R^.pointer, SIZE(MenuHead));
	    ELSIF SameType (R^.type, ListType) THEN
		DEALLOCATE (R^.pointer, SIZE(ListHead));
	    END;
	    DISPOSE (R);
	    R := temp;
	END (*WHILE*);
	OutTrace ("DeleteRow");
    END DeleteRow;

(************************************************************************)

PROCEDURE AdjustRow (R: StructureRow;  addroffset, columnoffset: CARDINAL);

    (* Adjusts the pointer and column fields of all elements of R	*)
    (* by adding the specified offsets to those fields.			*)

    VAR headptr: ListHeadPtr;  menuheadptr: MenuPointer;

    BEGIN
	InTrace ("AdjustRow");
	WHILE R <> NIL DO
	    WITH R^ DO
		IF SameType (type, MenuType) THEN
		    menuheadptr := pointer;
		    WITH menuheadptr^ DO
			address := AddOffset (address, addroffset);
		    END (*WITH*);
		ELSIF SameType (type, ListType) THEN
		    headptr := pointer;
		    WITH headptr^ DO
			address := AddOffset (address, addroffset);
		    END (*WITH*);
		ELSE
		    pointer := AddOffset (pointer, addroffset);
		END (*IF*);
		INC (column, columnoffset);
	    END (*WITH*);
	    R := R^.right;
	END (*WHILE*);
	OutTrace ("AdjustRow");
    END AdjustRow;

(************************************************************************)
(*			   EDITING A STRUCTURE				*)
(************************************************************************)

PROCEDURE SetStartingPoint (VAR (*INOUT*) R: FieldPointer;
						screencolumn: CARDINAL);

    (* On entry, R points to the first field in a row.  On exit, R	*)
    (* points to the field in that row whose "column" field most	*)
    (* closely matches the second argument to this procedure.		*)

    VAR next: FieldPointer;

    BEGIN
	IF R = NIL THEN RETURN END (*IF*);
	LOOP
	    next := R^.right;
	    IF next = NIL THEN EXIT(*LOOP*) END (*IF*);
	    IF 2*screencolumn < R^.column + R^.size + next^.column THEN
		EXIT (*LOOP*);
	    END (*IF*);
	    R := next;
	END (*LOOP*);
    END SetStartingPoint;

(************************************************************************)

PROCEDURE EditRow (w: Window;  R: StructureRow;  screenrow: CARDINAL);

    (* Displays structure R in window w, and allows the keyboard user	*)
    (* to edit the components of R.  It is assumed that w is already	*)
    (* open and that R has already been fully defined.			*)
    (* On return, the screen cursor is at the start of the field	*)
    (* corresponding to the selected record of the structure.  The key	*)
    (* which caused us to return remains available to the caller.	*)

    CONST Return = CHR(13);

    VAR dummy, screencolumn: CARDINAL;  nextchar: CHAR;

    BEGIN
	IF R = NIL THEN
	    RETURN;
	END (*IF*);
	SaveCursor (w, dummy, screencolumn);
	WriteRow (w, R, screenrow);
	SetStartingPoint (R, screencolumn);

	LOOP
	    (* Call the field editor for the current component.	*)

	    WITH R^ DO
		SetCursor (w, screenrow, column);
		EditField (w, pointer, type, size);
		nextchar := InKey();
	    END (*WITH*);

	    (* The Return key has a special function: we interpret it	*)
	    (* as "cursor right" if there is another field to the	*)
	    (* right, and otherwise we return it to the caller.		*)

	    IF nextchar = Return THEN
		IF R^.right <> NIL THEN R := R^.right
		ELSE EXIT (*LOOP*);
		END (*IF*);
	    ELSIF nextchar <> CHR(0) THEN
		EXIT (*LOOP*);
	    ELSE

		(* Now check for "cursor left" and "cursor right".	*)
		(* Any other key will be given back to the caller.	*)

		nextchar := InKey();
		IF nextchar = "K" THEN	(* cursor left *)
		    IF R^.left <> NIL THEN
			R := R^.left;
		    END (*IF*);
		ELSIF nextchar = "M" THEN	(* cursor right *)
		    IF R^.right <> NIL THEN
			R := R^.right;
		    END (*IF*);
		ELSE
		    PutBack (nextchar);  nextchar := CHR(0);
		    EXIT (*LOOP*)
		END(*IF*);

	    END (*IF*);

	END (*LOOP*);

	PutBack (nextchar);

    END EditRow;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    MenuType := DefineFieldType (WriteMenuField, EditMenuField);
    ListType := DefineFieldType (WriteListField, EditListField);
END RowEditor.
