IMPLEMENTATION MODULE StrSBUI;
(*==========================================================
    Version  : 1.00  29 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Monolithic Structure String (Opaque version)
               Sequential Bounded Unmanaged Iterator

    REVISION HISTORY
    v1.00  29 Apr 1989  C. Lins:
       Initial implementation. Derived from StringCSBMI module.
==========================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) NullHandler, ExitOnError, Raise;

FROM Items IMPORT
    (*--Cons*) NullItem,
    (*--Type*) Item, CompareProc, AccessProc, ChangeProc,
               LoopAccessProc, LoopChangeProc;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM StrEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;


    (*-----------------------*)


TYPE  Substring  = ARRAY StringSize OF Item;

TYPE  BoundedString = RECORD
	kompare: CompareProc; (*-- item comparison routine *)
        size   : StringSize;  (*-- Maximum String Size *)
        length : CARDINAL;    (*-- Current String Length *)
        items  : Substring;   (*-- ARRAY[1..TheSize] of Items *)
      END (*-- BoundedString *);

TYPE  String = POINTER TO BoundedString;

    (*-----------------------*)


VAR   stringError : Exceptions;
VAR   handler     : ARRAY Exceptions OF HandlerProc;

    (*-----------------------*)

PROCEDURE StringError ()              : Exceptions  (*-- out   *);
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


PROCEDURE LengthSubstr (VAR theSubstring : ARRAY OF Item (*-- in    *))
                                         : CARDINAL      (*-- out   *);
BEGIN
  IF (HIGH(theSubstring) = 0) & (theSubstring[0] = NullItem) THEN
    RETURN 0;
  END (*--if*);
  RETURN HIGH(theSubstring) + 1;
END LengthSubstr;
(*----------------------------*)

PROCEDURE FromToOK (    fromIndex    : Position   (*-- in    *);
                        toIndex      : Position   (*-- in    *);
                        stringLength : CARDINAL   (*-- in    *))
                                     : BOOLEAN    (*-- out   *);
BEGIN
  RETURN (toIndex <= stringLength) & (fromIndex <= toIndex);
END FromToOK;
(*----------------------------*)


PROCEDURE Create    (    theSize    : StringSize  (*--in   *);
		         compareItem: CompareProc (*--in   *))
                                    : String      (*--out  *);

CONST staticSize = SIZE(BoundedString) - SIZE(Substring);
CONST itemSize   = SIZE(Item);
VAR   newString  : String;

BEGIN
  stringError := noerr;
  Allocate(newString, staticSize + itemSize * theSize);
  IF (newString # NIL) THEN
    WITH newString^ DO
	  kompare:= compareItem;
      size   := theSize;
      length := 0;
    END (*--with*);
    RETURN newString;
  END (*--if*);
  RaiseErrIn(create, overflow);
  RETURN NullString;
END Create;
(*----------------------------*)

PROCEDURE Destroy   (VAR theString  : String     (*-- inout *));

CONST staticSize = SIZE(BoundedString) - SIZE(Substring);
CONST itemSize   = SIZE(Item);

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    Deallocate(theString, staticSize + itemSize * theString^.size);
  ELSE
    RaiseErrIn(destroy, undefined);
  END (*--if*);
END Destroy;
(*----------------------------*)

PROCEDURE Clear     (VAR theString  : String     (*-- inout *));
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    theString^.length := 0;
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*----------------------------*)

PROCEDURE Assign    (    theString  : String     (*-- in    *);
                     VAR toString   : String     (*-- inout *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    IF (theString # toString) THEN

      IF (toString # NIL) THEN
        IF (theString^.length > toString^.size) THEN
          RaiseErrIn(assign, overflow);
	ELSE
	  toString^.kompare := theString^.kompare;
        END (*--if*);
      ELSE
        toString := Create(theString^.size, theString^.kompare);
      END (*--if*);
      IF (stringError # noerr) THEN
        RETURN;
      END (*--if*);

      WITH theString^ DO
        FOR index := MIN(StringSize) TO length DO
          toString^.items[index] := items[index];
        END (*--for*);
        toString^.length := length;
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

VAR   newLength : CARDINAL;
      index     : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) & (toString # NIL) THEN

    WITH toString^ DO
      newLength := theString^.length + length;

      IF (theIndex > length + 1) THEN
        RaiseErrIn(insert, positionerr);
      ELSIF (newLength > size) THEN
        RaiseErrIn(insert, overflow);
      ELSE

        FOR index := length TO theIndex BY -1 DO
          items[index + theString^.length] := items[index];
        END (*--for*);

        FOR index := MIN(StringSize) TO theString^.length DO
          items[theIndex + index - 1] := theString^.items[index];
        END (*--for*);

        length := newLength;

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
      offset : CARDINAL; (*-- distance to move items down *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF FromToOK(fromIndex, toIndex, length) THEN
        offset := toIndex - fromIndex + 1;
        FOR index := toIndex + 1 TO length DO
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

PROCEDURE Replace   (VAR theString  : String     (*-- inout *);
                         theIndex   : Position   (*-- in    *);
                         withString : String     (*-- in    *));

VAR   endPosition : CARDINAL; (*-- new length of theString *)
      index       : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) & (withString # NIL) THEN
    endPosition := theIndex + withString^.length - 1;
    WITH theString^ DO
      IF (theIndex <= length) & (endPosition <= size) THEN
        FOR index := MIN(StringSize) TO withString^.length DO
          items[theIndex + index - 1] := withString^.items[index];
        END (*--for*);

        IF (endPosition > length) THEN
          length := endPosition;
        END (*--if*);
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

PROCEDURE Construct (VAR theString   : String        (*--inout*);
                         theSubstring: ARRAY OF Item (*--in   *);
			 compareItem : CompareProc   (*--in   *));

VAR   index     : CARDINAL; (*-- loop index over items *)
      lenSubstr : CARDINAL; (*-- # of chars in substring *)
      newString : String;

BEGIN
  stringError := noerr;
  lenSubstr := LengthSubstr(theSubstring);

  IF (theString # NIL) THEN
    IF (lenSubstr = 0) THEN
      Clear(theString);
      theString^.kompare := compareItem;
      RETURN;
    END (*--if*);
  ELSIF (lenSubstr = 0) THEN
    RaiseErrIn(construct, positionerr);
    RETURN;
  ELSE
    newString := Create(lenSubstr, compareItem);
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

PROCEDURE Compare    (    left       : String      (*-- in    *);
                          right      : String      (*-- in    *))
                                     : Relation    (*-- out   *);

VAR   index     : CARDINAL; (*-- Index into items arrays *)
      minLength : CARDINAL; (*-- Smaller of the two string lengths *)
      relOrder  : Relation; (*-- Most recent comparison result *)

BEGIN
  stringError := noerr;
  relOrder    := incomparable;
  IF (left # NIL) & (right # NIL) THEN

    IF (left^.kompare # right^.kompare) THEN
      RaiseErrIn(compare, typeerror);
      RETURN less;
    END (*--if*);

    WITH left^ DO
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
    LOOP
      IF (index > minLength) THEN
        EXIT (*--loop*);
      END (*--if*);

      WITH left^ DO
        relOrder := kompare(items[index], right^.items[index]);
      END (*--with*);
      IF (relOrder # equal) THEN
        EXIT (*--loop*);
      END (*--if*);

      INC(index);

    END (*--loop*);
  ELSE
    RaiseErrIn(compare, undefined);
  END (*--if*);
  RETURN relOrder;
END Compare;
(*----------------------------*)

PROCEDURE IsEqual    (    left       : String      (*-- in    *);
                          right      : String      (*-- in    *))
                                     : BOOLEAN     (*-- out   *);

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    WITH left^ DO
      IF (length = right^.length) THEN
        FOR index := MIN(StringSize) TO length DO
          IF (items[index] # right^.items[index]) THEN
            RETURN FALSE;
          END (*--if*);
        END (*--for*);
        RETURN TRUE;
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(isequal, undefined);
  END (*--if*);
  RETURN FALSE;
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

VAR   index     : CARDINAL; (*-- loop index over items *)
      sliceSize : CARDINAL; (*-- # items between from and to indexes *)

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


PROCEDURE LoopOver   (    theString : String         (*-- in    *);
                          theProcess: LoopAccessProc (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      FOR index := MIN(StringSize) TO length DO
        IF ~theProcess(items[index]) THEN
          RETURN;
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(loopover, undefined);
  END (*--if*);
END LoopOver;
(*----------------------------*)

PROCEDURE LoopChange (    theString : String         (*-- in    *);
                          theProcess: LoopChangeProc (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      FOR index := MIN(StringSize) TO length DO
        IF ~theProcess(items[index]) THEN
          RETURN;
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(loopchange, undefined);
  END (*--if*);
END LoopChange;
(*----------------------------*)


PROCEDURE Traverse   (    theString : String         (*-- in    *);
                          theProcess: AccessProc     (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      FOR index := MIN(StringSize) TO length DO
        theProcess(items[index]);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(traverse, undefined);
  END (*--if*);
END Traverse;
(*----------------------------*)

PROCEDURE TravChange (    theString : String         (*-- in    *);
                          theProcess: ChangeProc     (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      FOR index := MIN(StringSize) TO length DO
        theProcess(items[index]);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(travchange, undefined);
  END (*--if*);
END TravChange;
(*----------------------------*)


BEGIN
  FOR stringError := initfailed TO MAX(Exceptions) DO
    SetHandler(stringError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  stringError := noerr;
END StrSBUI.
