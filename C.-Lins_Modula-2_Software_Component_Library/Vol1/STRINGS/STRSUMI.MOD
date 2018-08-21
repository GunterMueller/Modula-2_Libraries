IMPLEMENTATION MODULE StrSUMI; 
(*==========================================================
    Version  : 1.00  21 May 1989  C. Lins
    Compiler : TopSpeed Modula-2 Compiler
    Code Size: R-  bytes
    Component: Monolithic Structure String (Opaque version)
	Sequential Unbounded Managed Iterator

	The major change necessary was the freeing items of
	a string. This effected the following routines:
	* SetSize
		All items must be freed when the string is cleared or
		when the string size is increased and the value is not
		preserved.
		When the string length is decreased, any items beyond
		the end of the new string length must be freed.
	* Replace
		Must free the items being overwritten.
	* SetItem
		Must free the single item being overwritten.
	
	Secondarily, the assignment operation derived from the
	string's data type id must be used instead of simple
	assignment. Simple assignment (:=) is still used internally
	by Insert and Delete to move items within the string.
	
	For the JPI version we must keep track of the physically
	allocated size of the array for when we deallocate the array.
	
    REVISION HISTORY
    v1.00  21 May 1989  C. Lins:
	Initial re-implementation from StringCSUMI module.

    c 1989 Charles A. Lins.
==========================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) NullHandler, ExitOnError, Raise;

FROM Items IMPORT
    (*--Cons*) NullItem,
    (*--Type*) Item, AssignProc, ChangeProc, CompareProc,
			   AccessProc, LoopAccessProc, LoopChangeProc,
			   DisposeProc;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM StringEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

FROM TypeManager IMPORT
	(*--Cons*) NullType,
    (*--Type*) TypeID,
	(*--Proc*) AssignOf, CompareOf, DisposeOf;

    (*-----------------------*)


TYPE  Substring  = POINTER TO ARRAY Position OF Item;

TYPE  UnboundedString = RECORD
		dataID : TypeID;	  (*-- data type of the string *)
		size   : CARDINAL;	  (*-- actual allocated size *)
	length : CARDINAL;    (*-- Current String Length *)
	items  : Substring;   (*-- ARRAY[1..length] of Items *)
      END (*-- UnboundedString *);

TYPE  String = POINTER TO UnboundedString;


    (*-----------------------*)


VAR   stringError : Exceptions;
VAR   handler     : ARRAY Exceptions OF HandlerProc;

    (*-----------------------*)

PROCEDURE StringError ()	: Exceptions  (*-- out   *);
BEGIN
  RETURN stringError;
END StringError;
(*----------------------------*)

PROCEDURE GetHandler  (    theError   : Exceptions  (*-- in    *))
	: HandlerProc (*-- out   *);
BEGIN
  RETURN handler[theError];
END GetHandler;
(*----------------------------*)

PROCEDURE SetHandler  (    theError   : Exceptions  (*-- in    *);
	theHandler : HandlerProc (*-- in    *));
BEGIN
  handler[theError] := theHandler;
END SetHandler;
(*----------------------------*)

PROCEDURE RaiseErrIn  (    theRoutine : Operations    (*-- in    *);
	theError   : Exceptions    (*-- in    *));
BEGIN
  stringError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handler[theError]);
END RaiseErrIn;
(*----------------------------*)


    (*-----------------------*)

PROCEDURE LengthSubstr  (VAR theSubstring : ARRAY OF Item (*-- in    *))
	: CARDINAL      (*-- out   *);
BEGIN
  IF (HIGH(theSubstring) = 0) & (theSubstring[0] = NullItem) THEN
    RETURN 0;
  END (*--if*);
  RETURN HIGH(theSubstring) + 1;
END LengthSubstr;
(*----------------------------*)

PROCEDURE FromToOK      (    fromIndex    : Position   (*-- in    *);
	toIndex      : Position   (*-- in    *);
	stringLength : CARDINAL   (*-- in    *))
	: BOOLEAN    (*-- out   *);
BEGIN
  RETURN (toIndex <= stringLength) & (fromIndex <= toIndex);
END FromToOK;
(*----------------------------*)


TYPE  SetOptions = (preservevalue, trashvalue);
VAR   setSizeOK  : BOOLEAN;

PROCEDURE SetSize   (VAR theString   : UnboundedString (*-- inout *);
	totheSize   : CARDINAL	(*-- in    *);
	valueOption : SetOptions      (*-- in    *));

CONST itemSize = SIZE(Item);
VAR   newItems : Substring;
      index    : CARDINAL; (*-- loop index over items *)
	  free	   : DisposeProc;
	  assign   : AssignProc;

BEGIN
  setSizeOK := TRUE;
  WITH theString DO
    IF (totheSize = 0) THEN
	  free := DisposeOf(dataID);
	  FOR index := MIN(Position) TO length DO
	  	free(items^[index]);
	  END (*--for*);
      Deallocate(items, itemSize * size);
	  size := 0;
    ELSIF (items = NIL) THEN
      Allocate(items, itemSize * totheSize);
      IF (items = NIL) THEN
	setSizeOK := FALSE;
	RETURN;
      END (*--if*);
	  size := totheSize;

    ELSIF (totheSize > size) THEN
      Allocate(newItems, itemSize * totheSize);
      IF (newItems = NIL) THEN
	setSizeOK := FALSE;
	RETURN;
      END (*--if*);

      IF (valueOption = preservevalue) THEN
	  	assign := AssignOf(dataID);
	FOR index := MIN(Position) TO length DO
	newItems^[index] := assign(items^[index]);
	END (*--for*);
	  ELSE
	  	free := DisposeOf(dataID);
	  	FOR index := MIN(Position) TO length DO
	  	  free(items^[index]);
	  	END (*--for*);
      END (*--if*);

      Deallocate(items, itemSize * size);
	  size  := totheSize;
      items := newItems;
	ELSIF (valueOption = trashvalue) THEN
	  free := DisposeOf(dataID);
	  FOR index := totheSize+1 TO length DO
	  	free(items^[index]);
	  END (*--for*);
    END (*--if*);
    length := totheSize;
  END (*--with*);
END SetSize;
(*----------------------------*)


    (*-----------------------*)

PROCEDURE Create    (    theDataType: TypeID     (*-- in    *)) 
									: String     (*-- out   *);

VAR   newString : String;

BEGIN
  stringError := noerr;
  Allocate(newString, SIZE(UnboundedString));
  IF (newString # NIL) THEN
    WITH newString^ DO
	  dataID := theDataType;
	  size   := 0;
      length := 0;
      items  := NIL;
    END (*--with*);
    RETURN newString;
  END (*--if*);
  RaiseErrIn(create, overflow);
  RETURN NullString;
END Create;
(*----------------------------*)


PROCEDURE Destroy   (VAR theString  : String     (*-- inout *));
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    SetSize(theString^, 0, trashvalue);
    Deallocate(theString);
  ELSE
    RaiseErrIn(destroy, undefined);
  END (*--if*);
END Destroy;
(*----------------------------*)


PROCEDURE Clear     (VAR theString  : String     (*-- inout *));
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    SetSize(theString^, 0, trashvalue);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*----------------------------*)


PROCEDURE Assign    (    theString  : String     (*-- in    *);
	VAR toString   : String     (*-- inout *));

VAR   index : CARDINAL; (*-- loop index over items *)
	  assignment : AssignProc;

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    IF (theString # toString) THEN
      IF (toString # NIL) THEN
	SetSize(toString^, theString^.length, trashvalue);
	IF ~setSizeOK THEN
	RaiseErrIn(assign, overflow);
	RETURN;
	END (*--if*);
      ELSE
	toString := Create(theString^.dataID);
	IF (stringError # noerr) THEN
	RETURN;
	END (*--if*);
      END (*--if*);

      WITH theString^ DO
	  	assignment := AssignOf(dataID);
	FOR index := MIN(Position) TO length DO
	toString^.items^[index] := assignment(items^[index]);
	END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*----------------------------*)


PROCEDURE Prepend   (    theString  : String     (*-- in    *);
	VAR toString   : String     (*-- inout *));
BEGIN
  Insert(theString, toString, 1);
END Prepend;
(*----------------------------*)

PROCEDURE Append    (    theString  : String     (*-- in    *);
	VAR toString   : String     (*-- inout *));
BEGIN
  Insert(theString, toString, LengthOf(toString) + 1);
END Append;
(*----------------------------*)


PROCEDURE Insert    (    theString  : String     (*-- in    *);
	VAR toString   : String     (*-- inout *);
	theIndex   : Position   (*-- in    *));

VAR   oldLength : CARDINAL;
      newLength : CARDINAL;
      index     : CARDINAL; (*-- loop index over items *)
	  assignment: AssignProc;

BEGIN
  stringError := noerr;
  IF (toString # NIL) & (theString # NIL) THEN

    WITH toString^ DO
      oldLength := length;
      newLength := theString^.length + length;

      IF (theIndex > oldLength + 1) THEN
	RaiseErrIn(insert, positionerr);
      ELSE
	SetSize(toString^, newLength, preservevalue);
	IF setSizeOK THEN

	FOR index := oldLength TO theIndex BY -1 DO
	items^[index + theString^.length] := items^[index];
	END (*--for*);

	  	  assignment := AssignOf(dataID);
	FOR index := MIN(Position) TO theString^.length DO
	items^[theIndex + index - 1] := assignment(theString^.items^[index]);
	END (*--for*);
	ELSE
	RaiseErrIn(insert, overflow);
	END (*--if*);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(insert, undefined);
  END (*--if*);
END Insert;
(*----------------------------*)


PROCEDURE Delete    (VAR theString  : String     (*-- inout *);
	fromIndex  : Position   (*-- in    *);
	toIndex    : Position   (*-- in    *));

VAR   index  : CARDINAL; (*-- loop index over items *)
      offset : CARDINAL; (*-- distance to move items *)
	  free   : DisposeProc;

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF FromToOK(fromIndex, toIndex, length) THEN

	  	free := DisposeOf(dataID);
		FOR index := fromIndex TO toIndex DO
		  free(items^[index]);
		END (*--for*);

	offset := toIndex - fromIndex + 1;
	FOR index := toIndex + 1 TO length DO
	items^[index - offset] := items^[index];
	END (*--for*);
	SetSize(theString^, length - offset, preservevalue);
      ELSE
	RaiseErrIn(delete, positionerr);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(delete, undefined);
  END (*--if*);
END Delete;
(*----------------------------*)


PROCEDURE Replace   (VAR theString  : String     (*-- inout *);
	theIndex   : Position   (*-- in    *);
	withString : String     (*-- in    *));

VAR   endIndex   : CARDINAL;
      index      : CARDINAL; (*-- loop index over items *)
	  oldLength  : CARDINAL;
	  assignment : AssignProc;
	  free       : DisposeProc;

BEGIN
  stringError := noerr;
  IF (theString # NIL) & (withString # NIL) THEN
    WITH theString^ DO
      IF (theIndex <= length) THEN

		oldLength := length;
	endIndex  := theIndex + withString^.length - 1;
	IF (endIndex > length) THEN
	SetSize(theString^, endIndex, preservevalue);
	IF ~setSizeOK THEN
	RaiseErrIn(replace, overflow);
	RETURN;
	END (*--if*);
	END (*--if*);

		(*
		  -- Free the items about to be overwritten. We do this after
		  -- increasing the string's size, since if that fails we won't
		  -- damage the original string. Note that there's always at
		  -- least one item being freed as Replace cannot be used as
		  -- an append operation.
		*)
	  	free := DisposeOf(dataID);
		FOR index := theIndex TO oldLength DO
		  free(items^[index]);
		END (*--for*);
		
	  	assignment := AssignOf(dataID);
	FOR index := MIN(Position) TO withString^.length DO
	items^[theIndex + index - 1] := assignment(withString^.items^[index]);
	END (*--for*);
      ELSE
	RaiseErrIn(replace, positionerr);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(replace, undefined);
  END (*--if*);
END Replace;
(*----------------------------*)


PROCEDURE SetItem   (VAR theString  : String     (*-- inout *);
	theIndex   : Position   (*-- in    *);
	theItem    : Item       (*-- in    *));

VAR	  free : DisposeProc;

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF (theIndex <= length) THEN
	  	free := DisposeOf(dataID);
		free(items^[theIndex]);
	items^[theIndex] := theItem;
      ELSE
	RaiseErrIn(setitem, positionerr);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(setitem, undefined);
  END (*--if*);
END SetItem;
(*----------------------------*)


PROCEDURE Construct (VAR theString   : String	(*-- inout *);
						 theDataType : TypeID		 (*-- in    *);
	theSubstring: ARRAY OF Item (*-- in    *));

VAR   index     : CARDINAL; (*-- loop index over items *)
      lenSubstr : CARDINAL; (*-- # of items in substring *)
      newString : String;   (*-- new string, if necessary *)

BEGIN
  stringError := noerr;
  lenSubstr := LengthSubstr(theSubstring);

  IF (theString # NIL) THEN
    IF (lenSubstr = 0) THEN
      Clear(theString);
      RETURN;
    END (*--if*);
  ELSIF (lenSubstr = 0) THEN
    RaiseErrIn(construct, positionerr);
  ELSE
    newString := Create(theDataType);
    IF (stringError # noerr) THEN
      RETURN;
    END (*--if*);
    theString := newString;
  END (*--if*);

  SetSize(theString^, lenSubstr, trashvalue);
  IF setSizeOK THEN
    WITH theString^ DO
      index := MIN(Position);
      WHILE (index <= lenSubstr) &
	(theSubstring[index - 1] # NullItem) DO
	items^[index] := theSubstring[index - 1];
	INC(index);
      END (*--while*);
    END (*--with*);
	SetSize(theString^, index-1, preservevalue);
  ELSE
    RaiseErrIn(construct, overflow);
  END (*--if*);
END Construct;
(*----------------------------*)


PROCEDURE IsDefined  (    theString : String   (*-- in    *))
	: BOOLEAN  (*-- out   *);
BEGIN
  RETURN (theString # NIL);
END IsDefined;
(*----------------------------*)

PROCEDURE IsEmpty    (    theString : String   (*-- in    *))
	: BOOLEAN  (*-- out   *);
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    RETURN theString^.length = 0;
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*----------------------------*)

PROCEDURE LengthOf   (    theString : String   (*-- in    *))
	: CARDINAL (*-- out   *);
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    RETURN theString^.length;
  END (*--if*);
  RaiseErrIn(lengthof, undefined);
  RETURN 0;
END LengthOf;
(*----------------------------*)

PROCEDURE TypeOf     (    theString : String   (*-- in    *))
	: TypeID   (*-- out   *);
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    RETURN theString^.dataID;
  END (*--if*);
  RaiseErrIn(typeof, undefined);
  RETURN NullType;
END TypeOf;
(*----------------------------*)

PROCEDURE Compare    (    left      : String   (*-- in    *);
	right     : String   (*-- in    *))
	: Relation (*-- out   *);

VAR   index     : CARDINAL; (*-- Index into items arrays *)
      minLength : CARDINAL; (*-- Smaller of the two string lengths *)
      relOrder  : Relation; (*-- Most recent comparison result *)
	  comparison: CompareProc;

BEGIN
  stringError := noerr;
  relOrder    := incomparable;
  IF (left # NIL) & (right # NIL) THEN

	IF (left^.dataID = right^.dataID) THEN
      WITH left^ DO
	    comparison := CompareOf(dataID);
	IF (length = right^.length) THEN
	relOrder  := equal;
	minLength := length;
	ELSIF (length < right^.length) THEN
	relOrder  := less;
	minLength := length;
	ELSE
	relOrder  := greater;
	minLength := right^.length;
	END (*--if*);
      END (*--with*);

      index := MIN(Position);
      LOOP
	IF (index > minLength) THEN
	EXIT (*--loop*);
	END (*--if*);

	IF (comparison(left^.items^[index], right^.items^[index]) = less) THEN
	relOrder := less;
	EXIT (*--loop*);
	ELSIF (comparison(left^.items^[index], right^.items^[index]) = greater) THEN
	relOrder := greater;
	EXIT (*--loop*);
	END (*--if*);

	INC(index);

      END (*--loop*);
    ELSE
  	  RaiseErrIn(compare, typeerror);
    END (*--if*);
  ELSE
    RaiseErrIn(compare, undefined);
  END (*--if*);

  RETURN relOrder;
END Compare;
(*----------------------------*)


PROCEDURE IsEqual    (    left      : String   (*-- in    *);
	right     : String   (*-- in    *))
	: BOOLEAN  (*-- out   *);
BEGIN
  RETURN Compare(left, right) = equal;
END IsEqual;
(*----------------------------*)

PROCEDURE ItemOf     (    theString : String   (*-- in    *);
	theIndex  : Position (*-- in    *))
	: Item     (*-- out   *);
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF (theIndex <= length) THEN
	RETURN items^[theIndex];
      END (*--if*);
    END (*--with*);
    RaiseErrIn(itemof, positionerr);
  ELSE
    RaiseErrIn(itemof, undefined);
  END (*--if*);
  RETURN NullItem;
END ItemOf;
(*----------------------------*)

PROCEDURE SliceOf   (    theString  : String	(*-- in    *);
	fromIndex  : Position      (*-- in    *);
	toIndex    : Position      (*-- in    *);
	VAR theSlice   : ARRAY OF Item (*-- out   *));

VAR   index     : CARDINAL; (*-- loop index over items *)
      sliceSize : CARDINAL; (*-- # items between from & to indexes *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF FromToOK(fromIndex, toIndex, length) THEN
	sliceSize := toIndex - fromIndex;
	IF (sliceSize <= HIGH(theSlice)) THEN
	FOR index := fromIndex TO toIndex DO
	theSlice[index - fromIndex] := items^[index];
	END (*--for*);
	IF (sliceSize < HIGH(theSlice)) THEN
	theSlice[sliceSize + 1] := NullItem;
	END (*--if*);
	ELSE
	RaiseErrIn(sliceof, overflow);
	END (*--if*);
      ELSE
	RaiseErrIn(sliceof, positionerr);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(sliceof, undefined);
  END (*--if*);
END SliceOf;
(*----------------------------*)


PROCEDURE SubstringOf(    theString  : String	(*-- in    *);
	VAR toSubstring: ARRAY OF Item (*-- out   *));

VAR   index      : CARDINAL; (*-- loop index over items *)
      copyLength : CARDINAL; (*-- # items to copy into substring *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF (length > HIGH(toSubstring) + 1) THEN
	RaiseErrIn(substringof, overflow);
	copyLength := HIGH(toSubstring) + 1;
      ELSE
	copyLength := length;
      END (*--if*);
      FOR index := MIN(Position) TO copyLength DO
	toSubstring[index - 1] := items^[index];
      END (*--for*);
      IF copyLength < HIGH(toSubstring) THEN
	toSubstring[copyLength + 1] := NullItem;
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(substringof, undefined);
  END (*--if*);
END SubstringOf;
(*----------------------------*)


PROCEDURE LoopOver   (    theString : String	(*-- in    *);
	theProcess: LoopAccessProc (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      FOR index := MIN(Position) TO length DO
	IF ~theProcess(items^[index]) THEN
	RETURN;
	END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(loopover, undefined);
  END (*--if*);
END LoopOver;
(*----------------------------*)

PROCEDURE LoopChange (    theString : String	(*-- in    *);
	theProcess: LoopChangeProc (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      FOR index := MIN(Position) TO length DO
	IF ~theProcess(items^[index]) THEN
	RETURN;
	END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(loopchange, undefined);
  END (*--if*);
END LoopChange;
(*----------------------------*)


PROCEDURE Traverse   (    theString : String	(*-- in    *);
	theProcess: AccessProc     (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      FOR index := MIN(Position) TO length DO
	theProcess(items^[index]);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(traverse, undefined);
  END (*--if*);
END Traverse;
(*----------------------------*)


PROCEDURE TravChange (    theString : String	(*-- in    *);
	theProcess: ChangeProc     (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      FOR index := MIN(Position) TO length DO
	theProcess(items^[index]);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(travchange, undefined);
  END (*--if*);
END TravChange;
(*----------------------------*)


BEGIN
  FOR stringError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(stringError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  stringError := noerr;
END StringSUMI.
