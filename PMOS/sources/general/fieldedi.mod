IMPLEMENTATION MODULE FieldEditor;

	(********************************************************)
	(*							*)
	(*		Screen editing utilities		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:		Seems to be OK			*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

FROM NumericIO IMPORT
    (* proc *)	WriteHexByte, EditHexByte, WriteRJCard, EditCardinal;

FROM RealIO IMPORT
    (* proc *)	WriteReal, EditReal;

(************************************************************************)

TYPE
    BytePtr = POINTER TO BYTE;
    CardPtr = POINTER TO CARDINAL;
    RealPtr = POINTER TO REAL;

    FieldType =  POINTER TO RECORD
				FieldWriter: WriteProc;
				FieldEditor: EditProc;
			    END (*RECORD*);

(************************************************************************)
(*		SCREEN OUTPUT FOR THE PREDEFINED TYPES			*)
(************************************************************************)

PROCEDURE WriteCardinalField (w: Window;  address: CardPtr;  width: CARDINAL);

    BEGIN
	WriteRJCard (w, address^, width);
    END WriteCardinalField;

(************************************************************************)

PROCEDURE WriteByteField (w: Window;  address: BytePtr;  width: CARDINAL);

    BEGIN
	WriteHexByte (w, address^);
    END WriteByteField;

(************************************************************************)

PROCEDURE WriteRealField (w: Window;  address: RealPtr;  width: CARDINAL);

    BEGIN
	WriteReal (w, address^, width);
    END WriteRealField;

(************************************************************************)
(*		    EDITORS FOR THE PREDEFINED TYPES			*)
(************************************************************************)

PROCEDURE EditByteField (w: Window;  VAR (*INOUT*) address: BytePtr;
						width: CARDINAL);

    BEGIN
	EditHexByte (w, address^);
    END EditByteField;

(************************************************************************)

PROCEDURE EditCardinalField (w: Window;  VAR (*INOUT*) address: CardPtr;
						width: CARDINAL);

    BEGIN
	EditCardinal (w, address^, width);
    END EditCardinalField;

(************************************************************************)

PROCEDURE EditRealField (w: Window;  VAR (*INOUT*) address: RealPtr;
							width: CARDINAL);

    BEGIN
	EditReal (w, address^, width);
    END EditRealField;

(************************************************************************)
(*			   COMPARING TYPES				*)
(************************************************************************)

PROCEDURE SameType (t1, t2: FieldType): BOOLEAN;

    (* Returns TRUE iff t1 = t2.	*)

    BEGIN
	RETURN t1 = t2;
    END SameType;

(************************************************************************)
(*			DEFINING A NEW TYPE				*)
(************************************************************************)

PROCEDURE DefineFieldType (Writer: WriteProc;  Editor: EditProc): FieldType;

    (* Introduces a new field type into the system.  Writer is a	*)
    (* user-supplied procedure to write a variable of the new type.	*)
    (* Editor is the user-supplied procedure for editing a variable of	*)
    (* that type.							*)

    VAR result: FieldType;

    BEGIN
	NEW (result);
	WITH result^ DO
	    FieldWriter := Writer;
	    FieldEditor := Editor;
	END (*WITH*);
	RETURN result;
    END DefineFieldType;

(************************************************************************)

PROCEDURE DiscardFieldType (type: FieldType);

    (* A notification from the user that this type will not be used	*)
    (* again (unless it is redefined by another call to procedure	*)
    (* DefineFieldType).  Use of this procedure is optional, but is	*)
    (* recommended for the sake of "clean" memory management.		*)

    BEGIN
	DISPOSE (type);
    END DiscardFieldType;

(************************************************************************)
(*			    SCREEN OUTPUT				*)
(************************************************************************)

PROCEDURE WriteField (w: Window;  address: ADDRESS;  type: FieldType;
							width: CARDINAL);

    (* Writes address^ on the screen at the current cursor position in	*)
    (* window w.  The width parameter specifies how many character	*)
    (* positions to use.  Use width=0 for variable-width fields for	*)
    (* which the write procedure for that type must work out the width.	*)

    BEGIN
	WITH type^ DO
	    FieldWriter (w, address, width);
	END (*WITH*);
    END WriteField;

(************************************************************************)
(*			     THE EDITOR					*)
(************************************************************************)

PROCEDURE EditField (w: Window;  VAR (*INOUT*) address: ADDRESS;
				type: FieldType;  width: CARDINAL);

    (* Edits the variable at the given address, and of the given type,	*)
    (* at the current cursor position in window w.  The width parameter	*)
    (* specifies how many character positions are to be used on the	*)
    (* screen.  Set width=0 for variable-width fields where the editor	*)
    (* must determine the width.  We leave this procedure on seeing a	*)
    (* keyboard character which does not belong to us.  The cursor is	*)
    (* left just beyond the last character of the field as it is	*)
    (* displayed.  The terminating keystroke is returned to the		*)
    (* keyboard driver so that it can still be read by the caller.	*)

    BEGIN
	WITH type^ DO
	    FieldEditor (w, address, width);
	END (*WITH*);
    END EditField;

(************************************************************************)
(*			SETTING UP THE PREDEFINED TYPES			*)
(************************************************************************)

PROCEDURE FinalCleanup;

    BEGIN
	DiscardFieldType (Byte);
	DiscardFieldType (Cardinal);  DiscardFieldType (Real);
    END FinalCleanup;

(************************************************************************)

BEGIN
    Byte := DefineFieldType (WriteByteField, EditByteField);
    Cardinal := DefineFieldType (WriteCardinalField, EditCardinalField);
    Real := DefineFieldType (WriteRealField, EditRealField);
    SetTerminationProcedure (FinalCleanup);
END FieldEditor.
