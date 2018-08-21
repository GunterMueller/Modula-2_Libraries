(*
9.5		StringCSBMN Implementation
*)


IMPLEMENTATION MODULE StrCSBMN;
(*==========================================================
    Version  : 1.00  29 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structure String (Opaque version)
               Character Sequential Bounded Managed Non-Iterator

    REVISION HISTORY
    v1.00  29 Apr 1989  C. Lins:
        Initial implementation.
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


(*	9.5.1	Internal Bounded String Representation

«Illustration Here»

Figure 9.1

Like the internal representation for a bounded stack, the representation for the bounded
string is a record dynamically allocated on the heap.  This record will be made just large
enough to hold the declared maximum size of the string.  Though the items array type
declaration covers the maximum allowed size range of a bounded string, only size entries
are actually allocated.  This technique allows great savings in the amount of space
actually used for each bounded string.

length is initialized to zero when a string is created, and this value represents an empty
string.  Furthermore, length may never exceed the value of size. Encountering this
condition is an indication of string overflow.

Representation Invariants:
  *	MIN(SizeRange) <= size <= MAX(SizeRange)
  *	MIN(SizeRange) <= length <= size
*)


TYPE  Substring  = ARRAY StringSize OF Item;

TYPE  BoundedString = RECORD
        size   : StringSize;  (*-- Maximum String Size *)
        length : CARDINAL;    (*-- Current String Length *)
        items  : Substring;   (*-- ARRAY[1..TheSize] of Items *)
      END (*-- BoundedString *);

TYPE  String = POINTER TO BoundedString;


(*	9.5.2			Exceptions

To support the exception handling mechanism two variables are needed.  The first,
stringError, is used to record the exception code from each operation; while handlers  is an
array of exception handling procedures indexed by the exception code.

The routines StringError, GetHandler, and SetHandler have been previously described in
the definition module, and their operation should be readily apparent.  RaiseErrIn  is a
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


(*	9.5.3			Local Routines

There are two routines local to the string module used by other, exported routines.  They
are declared here following convention of having routines declared prior to their use.  This
is not required by Modula-2 (as it was for Pascal) but some single-pass compilers have
imposed restrictions in this regard and this technique facilitates porting to such compilers.

LengthSubstr  determines that length of a substring  which for our purposes is a standard
Modula-2 string.  The substring parameter being tested is declared call-by-reference to
avoid the overhead involved in copying the open array implicit in call-by-value
parameters.  Note that we must first explicitly check for the special case of an empty
string, "", which is represented as a string containing only the string terminator character.

The second routine, FromToOK, checks whether the following precondition holds:

		fromIndex ≤ toIndex ≤ stringLength

which is required for routines such as Delete and SliceOf.  The routine takes advantage of
the fact that if the toIndex is less than or equal to the stringLength and the fromIndex is
less than or equal to the toIndex, then the fromIndex must also be less than or equal to the
stringLength.
*)


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


(*	9.5.4			Constructors

Create begins by clearing the stringError  field under the assumption of a successful
result.

The header for the string must then be allocated in a local variable since the function
result cannot be manipulated but only returned.  The key to this allocation step is the
calculation of the number of bytes necessary based on the size of an individual item and
the number of items requested.  We must not forget the space for storing theSize and the
string length.  The constant expression staticSize  accomplishes this regardless of the
number and size of these “static” fields and is unaffected by changes due to future
maintenance.  If the bounded string could not be allocated, the overflow exception must
be raised, and the NullString returned.

At this point, all possibility of failure has been avoided and the bounded string header can
be initialized to its empty state (length set to zero), and the size limit stored for this
bounded string.  Lastly, the new string can be returned to the caller.
*)

PROCEDURE Create    (    theSize    : StringSize (*-- in    *))
                                    : String     (*-- out   *);

CONST staticSize = SIZE(BoundedString) - SIZE(Substring);
CONST itemSize   = SIZE(Item);
VAR   newString  : String;

BEGIN
  stringError := noerr;
  Allocate(newString, staticSize + itemSize * theSize);
  IF (newString # NIL) THEN
    WITH newString^ DO
      size   := theSize;
      length := 0;
    END (*--with*);
    RETURN newString;
  END (*--if*);
  RaiseErrIn(create, overflow);
  RETURN NullString;
END Create;
(*----------------------------*)


(*
Destroy simply deallocates the bounded string header which automatically sets theString
to the NullString.
*)

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


(*
Clear simply sets the string length to zero, effectively removing all of its items.
*)

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


(*
Assignment for bounded objects is simpler to implement than their unbounded
counterparts as the opportunity for overflow is restricted to when the target object is
(re-)created.  The source string must be defined, otherwise the undefined exception is
raised.

If the target string exists, the source and target strings represent different string objects
(assignment of a string to itself is a useless operation) and the target string capable of
holding all of the source string's items the assignment can be safely accomplished. 
Otherwise, the overflow exception is raised and the operation aborted.  When the target
object is undefined it must be created using the size attribute of the source.  If overflow
does not occur, the actual assignment can commence, otherwise its suffices to exit (Create
has already raised the exception).

The assignment operator cannot be used to copy the whole items array as only a slice of
the array's index range was actually allocated and who knows what other dynamically
allocated objects follow it in memory.  Since our strings are formed from the basic data
type CHAR, the assignment operator can be used to copy individual items from one
string to another as there is no chance for structural sharing to occur.  This is done
through a simple loop over each of the items of the source.  Then the target string's
length can be updated to match that of the source string.
*)

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
        END (*--if*);
      ELSE
        toString := Create(theString^.size);
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
Insert adds the items of one string to an existing string at the given index position. 
Essentially, we must make room for new string items by shifting items from the
insertion index to the right by the number of items being inserted. Then we proceed to
insert the new string items into the vacated positions.  Finally, the modified string's
length is updated to reflect the newly inserted items.
*)

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


(*
Replace deletes all items of the source string from the given index to the end of the string
and then inserts the replacement string at the end of the source.  If theString was expanded
by the replacement process, the string length is adjusted accordingly.
*)

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


(*
Construct forms a bounded string from a standard Modula-2 string.  If theString has not
yet been created then theSubstring must not be empty since it is meaningless to create a
string with a maximum size of zero items.  If theString does exist and theSubstring is
empty it is sufficient to clear theString.  Otherwise we simply loop through theSubstring
copying items from there to the target string and when done update the string's length.
*)

PROCEDURE Construct (VAR theString   : String        (*-- inout *);
                         theSubstring: ARRAY OF Item (*-- in    *));

VAR   index     : CARDINAL; (*-- loop index over items *)
      lenSubstr : CARDINAL; (*-- # of chars in substring *)
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
    RaiseErrIn(construct, positionerr);
    RETURN;
  ELSE
    newString := Create(lenSubstr);
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



(*	9.5.5			Selectors

IsDefined returns true if the given string is not NIL, otherwise returning false.
*)

PROCEDURE IsDefined  (    theString  : String      (*-- in    *))
                                     : BOOLEAN     (*-- out   *);
BEGIN
  RETURN (theString # NIL);
END IsDefined;
(*----------------------------*)


(*
IsEmpty simply tests for a string length of zero, returning the appropriate logical value. 
As mentioned in the interface, an undefined string is considered empty.
*)

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


(*
SizeOf returns the size value for the given string.  An undefined string raises the
exception of the same name and returns a size of zero.
*)

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


(*
LengthOf returns the string's current length, which is the number of items in the string. 
An undefined string raises the exception of the same name and returns a length of zero.
*)

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


(*
Compare returns the ordering relation between two strings such that “left Relation right”
is True.  If either or both strings are undefined then the incomparable  relation is returned.

Initially we set up minLength with the smaller of the two string lengths, as reaching the
end of a string is one condition that terminates the comparison. We also preset relOrder 
to the correct result based on length.  This is based on the fact that all other things being
equal, the smaller of the strings is less than the other.

One this initialization is done, the algorithm loops through the strings from the
beginning examining each character item for the relation between them.  The loop
continues as long as the strings are equal or until the end of the smaller string is reached. 
The instant that the left item at the current index is less than its counterpart in the right
string the loop is terminated and the relation less  is returned.  Likewise when the left
item is greater than the right item the relation greater  is returned.  For equal items the
index is incremented, advancing the algorithm towards the terminating condition.
*)

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

      IF (left^.items[index] < right^.items[index]) THEN
        relOrder := less;
        EXIT (*--loop*);
      ELSIF (left^.items[index] > right^.items[index]) THEN
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


(*
ItemOf attempts to return the item of the string at the given index position.  If theIndex
exceeds the length of the string the positionerr  exception is raised.  This exception and
also an undefined string cause the NullItem (0C) to be returned.
*)

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


(*
SliceOf extracts a portion of the given string returning the sequence of characters as a
standard Modula-2 string.  The portion of the string to be extracted is specified by the
range of index positions within the string, fromIndex  and toIndex.  We must take care
that the target slice is indexed from zero and so must shift items from their positions
within the source string into the appropriate positions in the target slice.  The
preconditions (1) fromIndex <=toIndex <=source string length, and (2) number of items
between the from and to indices, inclusive, <=target slice size, must both be met.  If not,
then the positionerr and overflow  exceptions are raised, respectively.  If necessary, the
string terminator is added to the end of the slice.
*)

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


(*	9.5.6		Module Initialization

In the module initialization the local exception handlers array variables are set to default
handlers (ExitOnError) except for the noerr handler which is given the null handler. 
stringError is given the value noerr  avoiding an undefined state.
*)

BEGIN
  FOR stringError := initfailed TO MAX(Exceptions) DO
    SetHandler(stringError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  stringError := noerr;
END StrCSBMN.


(*
References

[1]	G. Booch, Software Components With Ada Structures, Tools, and Subsystems,
	Benjamin/Cummings, Menlo Park, CA, 1987, pp. 104-141.
[2]	D. Knuth, The Art of Computer Programming, Vol. 1, Fundamental Algorithms,
	Addison-Wesley, Reading, MA 1973.
[3]	R. Sedgewick, Algorithms, Addison-Wesley, Reading, MA 1983.
[4]	R. Wiener and R. Sincovec, Data Structures Using Modula-2, John Wiley & Sons, New
	York, NY 1986, pp. 461-469.
*)