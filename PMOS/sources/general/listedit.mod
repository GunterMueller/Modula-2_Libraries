IMPLEMENTATION MODULE ListEditor;

	(********************************************************)
	(*							*)
	(*		"Generic" list editor			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:		Seems OK			*)
	(*							*)
	(********************************************************)

FROM FieldEditor IMPORT
    (* proc *)	WriteField, EditField;

FROM Windows IMPORT
    (* type *)	ColumnRange,
    (* proc *)	SaveCursor, SetCursor, WriteString;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM Keyboard IMPORT
    (* proc *)	InKey, PutBack;

(************************************************************************)

CONST CursorLeftCode = "K";  CursorRightCode = "M";

TYPE

    String = ARRAY ColumnRange OF CHAR;

    (* The following definition shows the general structure of the sort	*)
    (* of lists which we can handle.					*)

    InternalList = POINTER TO InternalListElement;
    InternalListElement = RECORD
				next: InternalList;
				component: ADDRESS;
			  END (*RECORD*);

    (* Details needed for displaying a list on the screen.	*)

    ListFormat = POINTER TO ListFormatRecord;
    ListFormatRecord =	RECORD
			   header, separator, trailer: String;
			   ctype: FieldType;
			END (*RECORD*);

(************************************************************************)
(*			DEFINING A NEW LIST FORMAT			*)
(************************************************************************)

PROCEDURE CopyString (source: ARRAY OF CHAR;  VAR (*OUT*) destination: String);

    (* Copies the source string to the destination string.  Care is	*)
    (* needed here in getting the terminator right, because of the	*)
    (* strange way the FTL compiler deals with string constants.	*)

    VAR j, last: ColumnRange;

    BEGIN
	last := MAX(ColumnRange);
	IF last > HIGH(source) THEN
	    last := HIGH(source);
	    destination[last+1] := CHR(0);
	END (*IF*);
	FOR j := 0 TO last DO
	    destination[j] := source[j];
	END (*FOR*);
    END CopyString;

(************************************************************************)

PROCEDURE DefineListFormat (s1, s2, s3: ARRAY OF CHAR;
				ComponentType: FieldType): ListFormat;

    (* Sets up the output format for a class of lists.	*)

    VAR result: ListFormat;

    BEGIN
	NEW (result);
	WITH result^ DO
	    CopyString (s1, header);
	    CopyString (s2, separator);
	    CopyString (s3, trailer);
	    ctype := ComponentType;
	END (*WITH*);
	RETURN result;
    END DefineListFormat;

(************************************************************************)

PROCEDURE DiscardFormat (format: ListFormat);

    (* A notification from the user that this format will not be used	*)
    (* again (unless it is redefined by another call to procedure	*)
    (* DefineListFormat).  Use of this procedure is optional, but is	*)
    (* recommended for the sake of "clean" memory management.		*)

    BEGIN
	DISPOSE (format);
    END DiscardFormat;

(************************************************************************)
(*			     SCREEN OUTPUT				*)
(************************************************************************)

PROCEDURE WriteTail (w: Window;  L: InternalList;  format: ListFormat);

    (* Writes L on the screen, without its header string.  The screen	*)
    (* cursor is left at the character position following the trailer.	*)

    BEGIN
	IF L <> NIL THEN
	    LOOP
		WriteField (w, L^.component, format^.ctype, 0);
		L := L^.next;
		IF L = NIL THEN EXIT(*LOOP*) END(*IF*);
		WriteString (w, format^.separator);
	    END (*LOOP*);
	END (*IF*);
	WriteString (w, format^.trailer);
    END WriteTail;

(************************************************************************)

PROCEDURE WriteList (w: Window;  L: List;  format: ListFormat);

    (* Writes L on the screen, including its delimiters.  This		*)
    (* procedure is not actually used in this module, but is provided	*)
    (* as something that a client module may find useful.		*)

    BEGIN
	WriteString (w, format^.header);
	WriteTail (w, L, format);
    END WriteList;

(************************************************************************)
(*			     KEYBOARD INPUT				*)
(************************************************************************)

PROCEDURE FunctionKey (code: CHAR): BOOLEAN;

    (* Checks for CHR(0) followed by the given code from the keyboard.	*)
    (* If found, returns TRUE.  If not found, returns FALSE and no	*)
    (* keyboard input is consumed.					*)

    VAR ch: CHAR;

    BEGIN
	ch := InKey();
	IF ch <> CHR(0) THEN
	    PutBack (ch);  RETURN FALSE;
	END (*IF*);
	ch := InKey();
	IF ch <> code THEN
	    PutBack (ch);  PutBack (CHR(0));  RETURN FALSE;
	END (*IF*);
	RETURN TRUE;
    END FunctionKey;

(************************************************************************)
(*				  EDITING				*)
(************************************************************************)
(*									*)
(*  Each of the editing procedures in this module has the properties:	*)
(*									*)
(*   1.	We leave the procedure on seeing a keyboard character which	*)
(*	does not belong to us.  The terminating keystroke is returned	*)
(*	to the keyboard driver so that it can still be read by the	*)
(*	caller.								*)
(*   2.	The window cursor is left just beyond the "trailer" string	*)
(*	which terminates the written form of the list.			*)
(*   3.	The list being edited may be empty on entry and/or exit.	*)
(*									*)
(*  The differences among the procedures are in terms of the starting	*)
(*  point on the screen - that is, whether we start by writing a list	*)
(*  separator, a component, or a header string.				*)
(*									*)
(************************************************************************)

PROCEDURE EditFromSeparator (w: Window;  VAR (*INOUT*) L: InternalList;
					format: ListFormat);  FORWARD;

(************************************************************************)

PROCEDURE EditFromComponent (w: Window;  VAR (*INOUT*) L: InternalList;
						format: ListFormat);

    (* This procedure is the one which does most of the real work.	*)
    (* On entry, the screen cursor is at the place where the first	*)
    (* component must be written, i.e. the header or separator, as	*)
    (* appropriate, has already been written.  The screen display is	*)
    (* correct on exit in all cases except where an empty list is	*)
    (* returned and the caller had placed a separator in front of the	*)
    (* displayed list.  In that case, it is the caller's responsibility	*)
    (* to get rid of the separator.					*)

    VAR temp: InternalList;
	startrow, newrow, startcolumn, newcolumn: CARDINAL;

    BEGIN

	(* If we are at the end of a list, extend it to allow a new	*)
	(* component to be added.  (If the user does not take us up on	*)
	(* that offer, the new element will be deleted - see below.)	*)

	IF L = NIL THEN
	    NEW (L);
	    WITH L^ DO
		next := NIL;  component := NIL;
	    END (*WITH*);
	END (*IF*);

	SaveCursor (w, startrow, startcolumn);
	WriteTail (w, L, format);

	LOOP
	    SetCursor (w, startrow, startcolumn);

	    (* Edit the current list component.  Assumption: the	*)
	    (* component editor will leave the window cursor at the	*)
	    (* character position beyond the end of that component.	*)

	    EditField (w, L^.component, format^.ctype, 0);

	    (* If the user deleted this component - either as an	*)
	    (* explicit deletion or by ignoring the invitation to add a	*)
	    (* new component - discard the corresponding list element.	*)

	    IF L^.component = NIL THEN
		temp := L;  L := L^.next;  DISPOSE (temp);
		SetCursor (w, startrow, startcolumn);
		WriteTail (w, L, format);

		(* If the list is now empty, even after we have given	*)
		(* the user the chance to extend it, we can conclude	*)
		(* that the user has finished editing this list.	*)

		IF L = NIL THEN
		    EXIT (*LOOP*);
		END (*IF*);

		(* If the user wants to move right at this stage,	*)
		(* consume one "cursor right" keypress, because we are	*)
		(* already at the element which used to be to the right	*)
		(* of the now-deleted one.				*)

		IF FunctionKey (CursorRightCode) THEN
	 	    (* do nothing with the keypress	*)
		END (*IF*);

	    ELSE	(* component <> NIL *)

		(* Just in case the component editor has messed up	*)
		(* some of the screen display, write a clean copy.	*)

		SaveCursor (w, newrow, newcolumn);
		SetCursor (w, startrow, startcolumn);
		WriteTail (w, L, format);

		(* At this stage, a "cursor right" means that we should	*)
		(* move on to the next group, and any other key should	*)
		(* be given to the caller to deal with.			*)

		IF NOT FunctionKey (CursorRightCode) THEN
		    EXIT (*LOOP*);
		END (*IF*);

		(* We have indeed received a "cursor right" code.	*)

		SetCursor (w, newrow, newcolumn);
		EditFromSeparator (w, L^.next, format);
		IF NOT FunctionKey (CursorLeftCode) THEN
		    EXIT (*LOOP*)
		END (*IF*);

	    END (*IF*);

	END (*LOOP*);

    END EditFromComponent;

(************************************************************************)

PROCEDURE EditFromSeparator (w: Window;  VAR (*INOUT*) L: InternalList;
						format: ListFormat);

    (* On entry, the screen cursor is at the place where the separator	*)
    (* must be written, or the trailer in the case L = NIL.  The case	*)
    (* L = NIL is possible on entry and/or exit.  On exit, the screen	*)
    (* display has been correctly updated.				*)

    VAR startrow, startcolumn: CARDINAL;

    BEGIN
	SaveCursor (w, startrow, startcolumn);

	(* Write the separator, and do the actual editing.	*)

	WriteString (w, format^.separator);
	EditFromComponent (w, L, format);

	(* If we are left with an empty list, overwrite the redundant	*)
	(* separator.							*)

	IF L = NIL THEN
	    SetCursor (w, startrow, startcolumn);
	    WriteString (w, format^.trailer);
	END (*IF*);

    END EditFromSeparator;

(************************************************************************)

PROCEDURE EditList (w: Window;  VAR (*INOUT*) L: List;  format: ListFormat);

    (* Edits a list at the current cursor position in window w.  We	*)
    (* leave this procedure on seeing a keyboard character which does	*)
    (* not belong to us.  The terminating keystroke is returned to the	*)
    (* keyboard driver so that it can still be read by the caller.	*)
    (* The window cursor is left just beyond the "trailer" string which	*)
    (* terminates the written form of the list.				*)

    VAR Alias: InternalList;

    BEGIN
	Alias := L;
	WriteString (w, format^.header);
	EditFromComponent (w, Alias, format);
	L := Alias;
    END EditList;

(************************************************************************)

END ListEditor.
