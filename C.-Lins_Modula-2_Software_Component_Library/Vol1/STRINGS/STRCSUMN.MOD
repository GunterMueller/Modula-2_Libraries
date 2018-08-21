(*
10.4		StringCSUMN Implementation
*)


IMPLEMENTATION MODULE StrCSUMN;
(*==========================================================
    Version  : 1.00  29 Apr 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2 Compiler
    Code Size: R-  bytes
    Component: Monolithic Structure String (Opaque version)
               Character Sequential Bounded Managed Non-Iterator

    REVISION HISTORY
    v1.00  29 Apr 1989  C. Lins:
        Initial JPI Modula-2 implementation.

    (C) Copyright 1989 Charles A. Lins
==========================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) NullHandler, ExitOnError, Raise;

FROM CharItems IMPORT
    (*--Cons*) NullItem,
    (*--Type*) Item;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM StrEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

    (*-----------------------*)


(*
10.4.1		Internal Unbounded String Representation

«Illustration Here»

Figure 10.1

For the unbounded string an internal representation is needed that does not require a size
specification when the string is created and allows the string length to vary dynamically
as items are inserted and removed.  An examination of the literature (Standish [4] or
Wiener and Sincovec [5], for example) reveals numerous possible representations each
different in terms of space and time efficiency.  Booch [1] demonstrates an elegant
solution that may be readily derived from the bounded string form, where one maintains
in the string header a pointer to the dynamically allocated array of items.  To support this
alternate structure requires an additional local routine delegated the responsibility for
adjusting the size of this array.  Then only cosmetic changes are needed throughout the
remainder of the module.  This routine has been named SetSize  in this module, the
details of which are described below with its implementation.

Like the internal representation for an unbounded stack, the representation for the
unbounded string begins with a header record dynamically allocated on the heap.  This
header is used to store two fields: (1) the current string length, and (2) a pointer to a
dynamically allocated array of items large enough to hold the current length of the string.

length is initialized to zero when a string is created, and this value represents an empty
string.  Furthermore, items is set to NIL when the length is zero.
*)

TYPE  Substring  = POINTER TO ARRAY Position OF Item;

TYPE  UnboundedString = RECORD
        size   : CARDINAL;    (*-- actual allocated size *)
        length : CARDINAL;    (*-- Current String Length *)
        items  : Substring;   (*-- ARRAY[1..length] of Items *)
      END (*-- UnboundedString *);

TYPE  String = POINTER TO UnboundedString;



(*
10.4.2		Exceptions

To support the exception handling mechanism two variables are needed.  The first,
stringError, is used to record the exception code from each operation; while handlers is an
array of exception handling procedures indexed by the exception code.

The routines StringError, GetHandler, and SetHandler have been previously described in
the definition module, and their operation should be readily apparent.  RaiseErrIn is a
local routine used to set the stringError variable and invoke the Raise  routine of the
ErrorHandling module.
*)

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



(*
10.4.3		Local Routines

There are two routines local to the string module used by other, exported routines.  They
are declared here following convention of having routines declared prior to their use.  This
is not required by Modula-2 (as it was for Pascal) but some single-pass compilers have
imposed restrictions in this regard and this technique facilitates porting to such compilers.

LengthSubstr determines that length of a substring  which for our purposes is a standard
Modula-2 string.  The substring parameter being tested is declared call-by-reference to
avoid the overhead involved in copying the open array implicit in call-by-value
parameters.  Note that we must first explicitly check for the special case of an empty
string, "", which is represented as a string containing only the string terminator character.

The second routine, FromToOK, checks whether the following precondition holds:

		fromIndex <= toIndex <= stringLength

which is required for routines such as Delete and SliceOf.  The routine takes advantage of
the fact that if the toIndex is less than or equal to the stringLength and the fromIndex is
less than or equal to the toIndex, then the fromIndex must also be less than or equal to the
stringLength.
*)

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


(*
10.4.4		SetSize

The routine SetSize manages changing the length of and the pointer to the dynamic array
of items for an unbounded string.  The input parameters are (1) the header record of the
string being changed, (2) the desired new length, and (3) whether the current item values
should be kept with the new item array.  SetSize must correctly account for the following
situations based on the desired and current lengths (using a decision table format, where an
X indicates a “don't care” value):

Desired		Current		Preserve
Length		Length		Value?	Action
0		X		X	deallocate existing items array
X		0		X	allocate new items array
greater than current            Yes	allocate new items array and copy item values
greater than current            No	allocate new items array and don't copy item values
less than current		X	do nothing

From the above table the reader can see that when the length of the string decreases we
leave the string unchanged and may wonder as to the reason for this.  Basically, we are
taking advantage of the semantics of the Deallocate operation which automatically
releases the correct amount of space that was originally allocated and futhermore sets the
pointer to NIL.  Therefore, we waste some space by not changing the items array's
physical length through reallocation of the array but instead choose to save the time spent
for this operation.

The local variable setSizeOK indicates, when false, failure of the reallocation step.  In all
other cases setSizeOK will be true (failure cannot occur when we deallocate an items
array).  Since the value for setSizeOK does not always have meaning as some states never
fail, it is better to use a local variable that can be checked when necessary instead of
returning a function result that would have to be processed by the caller in every case.

Let us examine each of the above cases from the decision table in further detail.  When
the new size to zero we must effectively clear the items array.  This is done by simply
deallocating the array and setting the string length to the desired value.

When the new size is non-zero and the string is already empty it is necessary to allocate a
new items array.  As there are no items to be preserved from an empty state we ignore the
valueOption  parameter.  If this allocation fails, setSizeOK is set to false, and SetSize is
aborted leaving the string unchanged.

Expansion of the items array occurs when the desired size is greater than the current
length.  A new items array is first allocated since it may be necessary to preserve the
items from the current items array.  If this allocation fails, setSizeOK is set to false, and
SetSize is aborted leaving the string unchanged.  Otherwise, if the valueOption has been
set to preservevalue  the individual items are copied to the new structure.  Once the value
has been preserved, if necessary, the current items array can be safely deallocated, and the
pointer to the new items array stored in its place.  Lastly, the string length field can be
updated.
*)

TYPE  SetOptions = (preservevalue, trashvalue);
VAR   setSizeOK  : BOOLEAN;

PROCEDURE SetSize   (VAR theString   : UnboundedString (*-- inout *);
                         totheSize   : CARDINAL        (*-- in    *);
                         valueOption : SetOptions      (*-- in    *));

CONST itemSize = SIZE(Item);
VAR   newItems : Substring;
      index    : CARDINAL; (*-- loop index over items *)

BEGIN
  setSizeOK := TRUE;
  WITH theString DO
    IF (totheSize = 0) THEN
      Deallocate(items, itemSize * size);
      size := 0;
    ELSIF (items = NIL) THEN
      Allocate(items, itemSize * totheSize);
      IF (items = NIL) THEN
        setSizeOK := FALSE;
        RETURN;
      END (*--if*);
      size := totheSize;

    ELSIF (totheSize > length) THEN
      Allocate(newItems, itemSize * totheSize);
      IF (newItems = NIL) THEN
        setSizeOK := FALSE;
        RETURN;
      END (*--if*);

      IF (valueOption = preservevalue) THEN
        FOR index := MIN(Position) TO length DO
          newItems^[index] := items^[index];
        END (*--for*);
      END (*--if*);

      Deallocate(items, itemSize * size);
      items := newItems;
      size  := totheSize;
    END (*--if*);
    length := totheSize;
  END (*--with*);
END SetSize;
(*----------------------------*)



(*
10.4.5		Constructors

Create begins by clearing the stringError  field under the assumption of a successful
result.

The header for the string must then be allocated in a local variable since the function
result cannot be manipulated but only returned.  The key to this allocation step is the
calculation of the number of bytes necessary based on the size of an individual item and
the number of items requested.  We must not forget the space for storing theSize and the
string length.  The constant expression staticSize  accomplishes this regardless of the
number and size of these “static” fields and is unaffected by changes that may come about
due to future maintenance.  If the unbounded string could not be allocated, the overflow
exception must be raised, and the NullString returned.

At this point, all possibility of failure has been avoided and the bounded string header can
be initialized to its empty state (length set to zero, items set to NIL).  Lastly, the new
string can be returned to the caller.
*)

PROCEDURE Create    ()              : String     (*-- out   *);

VAR   newString : String;

BEGIN
  stringError := noerr;
  Allocate(newString, SIZE(UnboundedString));
  IF (newString # NIL) THEN
    WITH newString^ DO
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


(*
Destroy simply needs to call the SetSize routine to deallocate the items array itself
followed by deallocation of the bounded string header which automatically sets theString
to the NullString.
*)

PROCEDURE Destroy   (VAR theString  : String     (*-- inout *));
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    SetSize(theString^, 0, trashvalue);
    Deallocate(theString, SIZE(UnboundedString));
  ELSE
    RaiseErrIn(destroy, undefined);
  END (*--if*);
END Destroy;
(*----------------------------*)


(*
Clear simply needs to call the SetSize routine to deallocate the items array itself and set
the length to zero, removing all of its items using the trashvalue  command.
*)

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


(*
The assignment for this implementation of unbounded string objects is simpler to
implement than other unbounded objects due to the opportunity for restricting overflow to
when the target object is (re-)created.  This is done by the SetSize routine described above. 
If the target object exists and is capable of holding all of the source object's items the
target can be safely cleared and its data type updated appropriately.  Otherwise, the
overflow exception is raised and the assignment operation is aborted.  When the target
object is in an undefined state it must then be created.  If overflow does not occur, the
actual assignment can commence, otherwise its suffices to exit (Create has already raised
the exception).

The assignment operator cannot be used to copy the whole items array as only a slice of
the array's index range was actually allocated and who knows what other dynamically
allocated objects follow it in memory.  Nor can assignment be used to copy individual
items as the data type of those items is unknown; using assignment for dynamically
allocated items would cause structural sharing of items, which is not desired.
*)

PROCEDURE Assign    (    theString  : String     (*-- in    *);
                     VAR toString   : String     (*-- inout *));

VAR   index : CARDINAL; (*-- loop index over items *)

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
        toString := Create();
        IF (stringError # noerr) THEN
          RETURN;
        END (*--if*);
      END (*--if*);

      WITH theString^ DO
        FOR index := MIN(Position) TO length DO
          toString^.items^[index] := items^[index];
        END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*----------------------------*)


(*
Prepend and Append simply make use of the Insert routine to add items to the front or
the back of the target string, respectively.
*)

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


(*
Insert adds the items of one string to an existing string at the given index position.  First
the target string is expanded to hold the new items using the SetSize routine with the new
length and the command to preserve the current items.  Once the items array of the string
has been made large enough, we make room for new string items by shifting items from
the insertion index to the right by the number of items being inserted.  Then we proceed
to insert the new string items into the vacated positions.  Remember that the modified
string's length was updated to reflect the newly inserted items by SetSize.
*)

PROCEDURE Insert    (    theString  : String     (*-- in    *);
                     VAR toString   : String     (*-- inout *);
                         theIndex   : Position   (*-- in    *));

VAR   oldLength : CARDINAL;
      newLength : CARDINAL;
      index     : CARDINAL; (*-- loop index over items *)

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

          FOR index := MIN(Position) TO theString^.length DO
            items^[theIndex + index - 1] := theString^.items^[index];
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


(*
Delete removes items from an existing string between the given index positions,
inclusive.  Invalid index positions raise positionerr  and abort the operation.  The
algorithm essentially shifts items above the toIndex down in the string into positions
beginning with the fromIndex.  It calculates the amount of this shift, called the offset,
loops through string items above toIndex, shifting each item into its new location.  After
moving the items the string's length is updated.
*)

PROCEDURE Delete    (VAR theString  : String     (*-- inout *);
                         fromIndex  : Position   (*-- in    *);
                         toIndex    : Position   (*-- in    *));

VAR   index  : CARDINAL; (*-- loop index over items *)
      offset : CARDINAL; (*-- distance to move items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF FromToOK(fromIndex, toIndex, length) THEN
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


(*
Replace deletes all items of the source string from the given index to the end of the string
and then inserts the replacement string at the end of the source.  If theString was expanded
by the replacement process, the string length is adjusted accordingly.
*)

PROCEDURE Replace   (VAR theString  : String     (*-- inout *);
                         theIndex   : Position   (*-- in    *);
                         withString : String     (*-- in    *));

VAR   endIndex : CARDINAL;
      index    : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (theString # NIL) & (withString # NIL) THEN
    WITH theString^ DO
      IF (theIndex <= length) THEN

        endIndex := theIndex + withString^.length - 1;
        IF (endIndex > length) THEN
          SetSize(theString^, endIndex, preservevalue);
          IF ~setSizeOK THEN
            RaiseErrIn(replace, overflow);
            RETURN;
          END (*--if*);
        END (*--if*);

        FOR index := MIN(Position) TO withString^.length DO
          items^[theIndex + index - 1] := withString^.items^[index];
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


(*
SetItem attempts to assign the given item to the string at the given index position.  The
index must be within the string's current length otherwise the positionerr exception is
raised avoiding assignment outside the string's current bounds.
*)

PROCEDURE SetItem   (VAR theString  : String     (*-- inout *);
                         theIndex   : Position   (*-- in    *);
                         theItem    : Item       (*-- in    *));
BEGIN
  stringError := noerr;
  IF (theString # NIL) THEN
    WITH theString^ DO
      IF (theIndex <= length) THEN
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


(*
Construct forms an unbounded string from a standard Modula-2 string.  If theString has
not yet been created then theSubstring must not be empty for compatibility with the
bounded form.  If theString does exist and theSubstring is empty it is sufficient to clear
theString.  Otherwise we simply loop through theSubstring copying items from there to
the target string and when done update the string's length.
*)

PROCEDURE Construct (VAR theString   : String        (*-- inout *);
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
    newString := Create();
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



(*
10.4.6		Selectors

IsDefined tests the given string for a non-NIL value.

IsEmpty tests the given string's length for zero which is the value used to represent the
empty string.  While LengthOf  returns the value of the given string's length.
*)

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


(*
Compare returns the ordering relation between two strings such that “left Relation right”
is True.  If either or both strings are undefined then the incomparable  relation is returned.

Initially we set up minLength with the smaller of the two string lengths, as reaching the
end of a string is one condition that terminates the comparison. We also set relOrder  to
the correct result based on length.  This is based on the fact that all other things being
equal, the smaller of the strings is less than the other.

One this initialization is done, the algorithm loops through the strings from the
beginning examining each character item for the relation between them.  The loop
continues as long as the strings are equal or until the end of the smaller string is reached. 
The instant that the left item at the current index is less than its counterpart in the right
string the loop is terminated and the relation less  is returned.  Likewise when the left
item is greater than the right item the relation greater  is returned.  For equal items the
index is incremented, advancing the algorithm towards the terminating condition.
*)

PROCEDURE Compare    (    left      : String   (*-- in    *);
                          right     : String   (*-- in    *))
                                    : Relation (*-- out   *);

VAR   index     : CARDINAL; (*-- Index into items arrays *)
      minLength : CARDINAL; (*-- Smaller of the two string lengths *)
      relOrder  : Relation; (*-- Most recent comparison result *)

BEGIN
  stringError := noerr;
  relOrder    := incomparable;
  IF (left # NIL) & (right # NIL) THEN

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

    index := MIN(Position);
    LOOP
      IF (index > minLength) THEN
        EXIT (*--loop*);
      END (*--if*);

      IF (left^.items^[index] < right^.items^[index]) THEN
        relOrder := less;
        EXIT (*--loop*);
      ELSIF (left^.items^[index] > right^.items^[index]) THEN
        relOrder := greater;
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


(*
IsEqual scans both strings looking for the first mismatch (inequality) which indicates the
strings are unequal, otherwise if the FOR loop completes the strings must be equal.  This
assumes that (1) the strings have been defined and (2) have the same length.  The first of
these assumptions, if unfounded, raises the undefined exception; while the second is a
simple determinant of inequality.
*)

PROCEDURE IsEqual    (    left      : String   (*-- in    *);
                          right     : String   (*-- in    *))
                                    : BOOLEAN  (*-- out   *);

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stringError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    WITH left^ DO
      IF (length = right^.length) THEN
        FOR index := MIN(Position) TO length DO
          IF (items^[index] # right^.items^[index]) THEN
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


(*
ItemOf attempts to return the item of the string at the given index position.  If theIndex
exceeds the length of the string the positionerr  exception is raised.  This exception and
also an undefined string causes the NullItem (0C) to be returned.
*)

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


(*
SliceOf extracts a portion of the given string returning the sequence of characters as a
standard Modula-2 string.  The portion of the string to be extracted is specified by the
range of index positions within the string, fromIndex  and toIndex.  We must take care
because the target slice is indexed from zero and so must shift items from their positions
within the source string into the appropriate positions in the target slice.  The
preconditions (1) fromIndex ≤ toIndex ≤ source string length, and (2) number of items
between the from and to indices, inclusive, ≤ target slice size, must both be met.  If not,
then the positionerr and overflow  exceptions are raised, respectively.  If necessary, the
string terminator is added to the end of the slice.

*)

PROCEDURE SliceOf   (    theString  : String        (*-- in    *);
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


(*
SubstringOf is similar to SliceOf, above, except that the whole string is returned. When
the Target substring is too small for all the items in the Source string, overflow is raised
and the target is filled with as many items that will fit.
*)

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


(*
10.2.7		Module Initialization

In the module initialization the local exception handlers array variables are set to default
handlers (ExitOnError) except for the noerr handler which is given the null handler. 
stringError is given the value noerr avoiding an undefined state.
*)

BEGIN
  SetHandler(noerr, NullHandler);
  FOR stringError := initfailed TO MAX(Exceptions) DO
    SetHandler(stringError, ExitOnError);
  END (*--for*);
  stringError := noerr;
END StrCSUMN.


(*
References

[1]	G. Booch, Software Components With Ada Structures, Tools, and Subsystems,
	Benjamin/Cummings, Menlo Park, CA, 1987, pp. 104-141.
[2]	D. Knuth, The Art of Computer Programming, Vol. 1, Fundamental Algorithms,
	Addison-Wesley, Reading, MA 1973.
[3]	R. Sedgewick, Algorithms, Addison-Wesley, Reading, MA 1983.
[4]	T. Standish, Data Structure Techniques, Addison-Wesley, Reading, MA 1980.
[5]	R. Wiener and R. Sincovec, Data Structures Using Modula-2, John Wiley & Sons, New
	York, NY 1986, pp. 461-469.
*)
