IMPLEMENTATION MODULE StrSBMN;
(*==============================================================
    Version  : 1.00  29 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structure String (Opaque version)
               Sequential Bounded Managed Non-Iterator

    REVISION HISTORY
    v1.00  29 Apr 1989  C. Lins:
      Initial implementation.
==============================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) NullHandler, ExitOnError, Raise;

FROM Items IMPORT
    (*--Cons*) NullItem,
    (*--Type*) Item, AssignProc, CompareProc, DisposeProc;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM StrEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
    (*--Proc*) AssignOf, DisposeOf, CompareOf;

    (*-----------------------*)

TYPE  Substring  = ARRAY StringSize OF Item;

TYPE  BoundedString = RECORD
        dataID : TypeID;      (*-- data type identifier *)
        size   : StringSize;  (*-- Maximum String Size *)
        length : CARDINAL;    (*-- Current String Length *)
        items  : Substring;   (*-- ARRAY[1..size] of Items *)
      END (*-- BoundedString *);

TYPE  String = POINTER TO BoundedString;


    (*---------------------------------*)
    (*--          EXCEPTIONS         --*)

VAR   stringError : Exceptions;
VAR   handler     : ARRAY Exceptions OF HandlerProc;

    (*-----------------------*)

PROCEDURE StringError () : Exceptions (*-- out   *);
BEGIN
  RETURN stringError;
END StringError;
(*----------------------------*)

PROCEDURE GetHandler  (    ofError   : Exceptions  (*-- in    *))
                                     : HandlerProc (*-- out   *);
BEGIN
  RETURN handler[ofError];
END GetHandler;
(*----------------------------*)

PROCEDURE SetHandler  (    ofError   : Exceptions  (*-- in    *);
                           toHandler : HandlerProc (*-- in    *));
BEGIN
  handler[ofError] := toHandler;
END SetHandler;
(*----------------------------*)

PROCEDURE RaiseErrIn  (    theRoutine : Operations    (*-- in    *);
                           theError   : Exceptions    (*-- in    *));
BEGIN
  stringError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handler[theError]);
END RaiseErrIn;
(*----------------------------*)


    (*---------------------------------*)
    (*--       LOCAL ROUTINES        --*)

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

(*-- Checks whether: fromPosition <= toPosition <= stringLength. Takes
  -- advantage of the fact that if toIndex is <= the stringLength and
  -- fromIndex is <= toIndex, then fromIndex must also be <= stringLength. *)

BEGIN
  RETURN (toIndex <= stringLength) & (fromIndex <= toIndex);
END FromToOK;
(*----------------------------*)


    (*---------------------------------*)
    (*--         CONSTRUCTORS        --*)

PROCEDURE Create    (    withSizeOf   : StringSize    (*-- in    *);
                         theDataType  : TypeID        (*-- in    *))
                                      : String        (*-- out   *);

CONST staticSize = SIZE(BoundedString)-SIZE(Substring);
CONST itemSize   = SIZE(Item);
VAR   newString  : String;

BEGIN
  IF (withSizeOf <= MAX(StringSize)) THEN
    Allocate(newString, staticSize + itemSize * withSizeOf);
    IF (newString # NIL) THEN
      WITH newString^ DO
        dataID := theDataType;
        size   := withSizeOf;
        length := 0;
      END (*--with*);
      RETURN newString;
    END (*--if*);
  END (*--if*);
  RaiseErrIn(create, overflow);
  RETURN NullString;
END Create;
(*----------------------------*)

PROCEDURE Destroy   (VAR theString    : String        (*-- inout *));

CONST staticSize = SIZE(BoundedString)-SIZE(Substring);
CONST itemSize   = SIZE(Item);

BEGIN
  Clear(theString);
  IF (stringError = noerr) THEN
    Deallocate(theString, staticSize + itemSize * theString^.size);
  END (*--if*);
END Destroy;
(*----------------------------*)

PROCEDURE Clear     (VAR theString    : String        (*-- inout *));

VAR   index : CARDINAL;
      free  : DisposeProc;

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      free := DisposeOf(dataID);
      FOR index := MIN(StringSize) TO length DO
        free(items[index]);
      END (*--for*);
      length := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*----------------------------*)

PROCEDURE Assign    (    fromString   : String        (*-- in    *);
                     VAR toString     : String        (*-- inout *));

VAR   index      : CARDINAL;
      assignment : AssignProc;

BEGIN
  stringError := noerr;
  IF (fromString = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (fromString # toString) THEN
    IF (toString = NIL) THEN
      toString := Create(fromString^.size, fromString^.dataID);
    ELSIF (fromString^.length <= toString^.size) THEN
      Clear(toString);
      toString^.dataID := fromString^.dataID;
    ELSE
      RaiseErrIn(assign, overflow);
    END (*--if*);
    IF (stringError = noerr) THEN
      WITH fromString^ DO
        assignment := AssignOf(dataID);
        FOR index := MIN(StringSize) TO length DO
          toString^.items[index] := assignment(items[index]);
        END (*--for*);
        toString^.length := length;
      END (*--with*);
    END (*--if*);
  END (*--if*);
END Assign;
(*----------------------------*)

PROCEDURE Prepend   (    theString    : String        (*-- in    *);
                     VAR toString     : String        (*-- inout *));
BEGIN
  Insert(theString, toString, 1);
END Prepend;
(*----------------------------*)

PROCEDURE Append    (    theString    : String        (*-- in    *);
                     VAR toString     : String        (*-- inout *));
BEGIN
  Insert(theString, toString, LengthOf(toString) + 1);
END Append;
(*----------------------------*)

PROCEDURE Insert    (    theString    : String        (*-- in    *);
                     VAR toString     : String        (*-- inout *);
                         theIndex     : Position      (*-- in    *));

VAR   newLength : CARDINAL;
      index     : CARDINAL;
      assignment: AssignProc;

BEGIN
  stringError := noerr;
  IF (theString # NIL) & (toString # NIL) THEN
    IF (theString^.dataID = toString^.dataID) THEN

      WITH toString^ DO
        newLength := theString^.length + length;

        IF (theIndex > length + 1) THEN
          RaiseErrIn(insert, positionerr);
        ELSIF (newLength > size) THEN
          RaiseErrIn(insert, overflow);
        ELSE

          assignment := AssignOf(dataID);

          FOR index := length TO theIndex BY -1 DO
            items[index + theString^.length] := items[index];
          END (*--for*);

          FOR index := MIN(StringSize) TO theString^.length DO
            items[theIndex + index - 1] := assignment(theString^.items[index]);
          END (*--for*);

          length := newLength;

        END (*--if*);
      END (*--with*);
    ELSE
      RaiseErrIn(insert, typeerror);
    END (*--if*);
  ELSE
    RaiseErrIn(insert, undefined);
  END (*--if*);
END Insert;
(*----------------------------*)

PROCEDURE Delete    (VAR theString    : String        (*-- inout *);
                         fromIndex    : Position      (*-- in    *);
                         toIndex      : Position      (*-- in    *));

VAR   index  : CARDINAL;
      offset : CARDINAL;
      free   : DisposeProc;

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF FromToOK(fromIndex, toIndex, length) THEN
        free   := DisposeOf(dataID);
        offset := toIndex - fromIndex + 1;
        FOR index := toIndex + 1 TO length DO
	  free(items[index - offset]);
          items[index - offset] := items[index];
        END (*--for*);
        DEC(length, offset);
      ELSE
        RaiseErrIn(delete, positionerr);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(delete, undefined);
  END (*--if*);
END Delete;
(*----------------------------*)

PROCEDURE Replace   (VAR theString    : String        (*-- inout *);
                         theIndex     : Position      (*-- in    *);
                         withString   : String        (*-- in    *));

VAR   endPosition : CARDINAL;
      index       : CARDINAL;
      free        : DisposeProc;
      assignment  : AssignProc;

BEGIN
  stringError := noerr;
  IF (theString # NIL) & (withString # NIL) THEN
    IF (theString^.dataID = withString^.dataID) THEN
      endPosition := theIndex + withString^.length - 1;
      WITH theString^ DO
        IF (theIndex <= length) & (endPosition <= size) THEN
          free := DisposeOf(theString^.dataID);
          assignment := AssignOf(theString^.dataID);
          FOR index := MIN(StringSize) TO withString^.length DO
	    free(items[theIndex + index - 1]);
            items[theIndex + index - 1] := assignment(withString^.items[index]);
          END (*--for*);

          IF (endPosition > length) THEN
            length := endPosition;
          END (*--if*);
        ELSE
          RaiseErrIn(replace, positionerr);
        END (*--if*);
      END (*--with*);
    ELSE
      RaiseErrIn(replace, typeerror);
    END (*--if*);
  ELSE
    RaiseErrIn(replace, undefined);
  END (*--if*);
END Replace;
(*----------------------------*)

PROCEDURE SetItem   (VAR theString  : String        (*-- inout *);
                         theIndex   : Position      (*-- in    *);
                         theItem    : Item          (*-- in    *));
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF (theIndex <= length) THEN
        items[theIndex] := theItem;
      ELSE
        RaiseErrIn(setitem, positionerr);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(setitem, undefined);
  END (*--if*);
END SetItem;
(*----------------------------*)

PROCEDURE Construct (VAR theString    : String        (*-- inout *);
			 theDataType  : TypeID	      (*-- in    *);
                     VAR theSubstring : ARRAY OF Item (*-- in    *));

VAR   index     : CARDINAL;
      lenSubstr : CARDINAL;
      newString : String;

BEGIN
  stringError := noerr;
  lenSubstr := LengthSubstr(theSubstring);

  IF (theString # NIL) THEN
    IF (lenSubstr = 0) THEN
      Clear(theString);
      RETURN;
    END (*--if*);

  ELSIF (lenSubstr = 0) THEN

    (*-- ERROR: cannot create string of length zero *)

    RaiseErrIn(construct, positionerr);
    RETURN;
  ELSE
    newString := Create(lenSubstr, theDataType);
    IF (stringError = noerr) THEN
      theString := newString;
    ELSE
      RETURN;
    END (*--if*);
  END (*--if*);

  WITH theString^ DO

    (*-- The minimum lenSubstr is one (1). *)

    index := MIN(Position);
    WHILE (index <= lenSubstr) & (theSubstring[index - 1] # NullItem) DO
      items[index] := theSubstring[index - 1];
      INC(index);
    END (*--while*);
    length := index - 1;
  END (*--with*);
END Construct;
(*----------------------------*)


    (*---------------------------------*)
    (*--          SELECTORS          --*)

PROCEDURE IsDefined  (    theString  : String      (*-- in    *))
                                     : BOOLEAN     (*-- out   *);
BEGIN
  RETURN (theString # NIL);
END IsDefined;
(*----------------------------*)

PROCEDURE IsEmpty    (    theString  : String      (*-- in    *))
                                     : BOOLEAN     (*-- out   *);
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    RETURN theString^.length = 0;
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*----------------------------*)

PROCEDURE LengthOf   (    theString  : String      (*-- in    *))
                                     : CARDINAL    (*-- out   *);
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    RETURN theString^.length;
  END (*--if*);
  RaiseErrIn(lengthof, undefined);
  RETURN 0;
END LengthOf;
(*----------------------------*)

PROCEDURE SizeOf     (    theString  : String      (*-- in    *))
                                     : CARDINAL    (*-- out   *);
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    RETURN theString^.size;
  END (*--if*);
  RaiseErrIn(sizeof, undefined);
  RETURN 0;
END SizeOf;
(*----------------------------*)

PROCEDURE TypeOf     (    theString  : String      (*-- in    *))
                                     : TypeID      (*-- out   *);
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    RETURN theString^.dataID;
  END (*--if*);
  RaiseErrIn(typeof, undefined);
  RETURN NullType;
END TypeOf;
(*----------------------------*)

PROCEDURE Compare    (    left       : String      (*-- in    *);
                          right      : String      (*-- in    *))
                                     : Relation    (*-- out   *);

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

      index := MIN(StringSize);
      WITH left^ DO
        LOOP
          IF (index > minLength) THEN
            EXIT (*--loop*);
          END (*--if*);
          relOrder := comparison(items[index], right^.items[index]);
          IF (relOrder # equal) THEN
            EXIT (*--loop*);
          END (*--if*);
          INC(index);
        END (*--loop*);
      END (*--with*);
    ELSE
      RaiseErrIn(compare, typeerror);
    END (*--if*);
  ELSE
    RaiseErrIn(compare, undefined);
  END (*--if*);
  RETURN relOrder;
END Compare;
(*----------------------------*)

PROCEDURE IsEqual    (    left       : String      (*-- in    *);
                          right      : String      (*-- in    *))
                                     : BOOLEAN     (*-- out   *);
BEGIN
  stringError := noerr;
  RETURN Compare(left, right) = equal;
END IsEqual;
(*----------------------------*)

PROCEDURE ItemOf     (    theString  : String      (*-- in    *);
                          theIndex   : Position    (*-- in    *))
                                     : Item        (*-- out   *);
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF (theIndex <= length) THEN
        RETURN items[theIndex];
      END (*--if*);
    END (*--with*);
    RaiseErrIn(itemof, positionerr);
  ELSE
    RaiseErrIn(itemof, undefined);
  END (*--if*);
  RETURN NullItem;
END ItemOf;
(*----------------------------*)

PROCEDURE SliceOf    (    theString  : String        (*-- in    *);
                          fromIndex  : Position      (*-- in    *);
                          toIndex    : Position      (*-- in    *);
                      VAR theSlice   : ARRAY OF Item (*-- out   *));

VAR   index     : CARDINAL;
      sliceSize : CARDINAL;

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF FromToOK(fromIndex, toIndex, length) THEN
        sliceSize := toIndex - fromIndex;
        IF (sliceSize <= HIGH(theSlice)) THEN
          FOR index := fromIndex TO toIndex DO
            theSlice[index - fromIndex] := items[index];
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

PROCEDURE SubstringOf(    theString  : String        (*-- in    *);
                      VAR toSubstring: ARRAY OF Item (*-- out   *));

VAR   index      : CARDINAL;
      copyLength : CARDINAL; (*-- # of items to copy into substring *)

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
      FOR index := MIN(StringSize) TO copyLength DO
        toSubstring[index - 1] := items[index];
      END (*--for*);
      IF (copyLength < HIGH(toSubstring)) THEN
        toSubstring[copyLength + 1] := NullItem;
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(substringof, undefined);
  END (*--if*);
END SubstringOf;
(*----------------------------*)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*~~ Module Initialization  ~~*)

BEGIN
  FOR stringError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(stringError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  stringError := noerr;
END StrSBMN.