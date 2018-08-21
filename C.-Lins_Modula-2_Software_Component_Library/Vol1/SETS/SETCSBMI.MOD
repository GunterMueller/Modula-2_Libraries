(*
14.2		SetCSBMI Implementation
*)


IMPLEMENTATION MODULE SetCSBMI;
(*==========================================================
    Version  : 1.00  30 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structure - Set
               Discrete Sequential Bounded Managed Iterator

    INTRODUCTION
    This module supports the abstract data type set for
    discrete values of CHARs.

    REVISION HISTORY
    v1.00  30 Apr 19898  C. Lins:
        Initial TopSpeed Modula-2 implementation.

    (C) Copyright 1989 Charles A. Lins
==========================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM CharItems IMPORT
    (*--Type*) Item, AccessProc, LoopAccessProc;

FROM SetEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) NullHandler, Raise, ExitOnError;

    (*-----------------------*)


(*
14.2.1		Internal Discrete Set Representation

«Illustration Here»

Figure 14.1

For the internal representation of the discrete (character) set we use a bit vector of items,
where an item at the appropriate index has a value of one if the item is a member of the
set and a value of zero otherwise.  In order to save space and enable the use of the standard
Modula-2 bitset operations, the bit vector is stored as an array of BITSETs.  So instead of
requiring 256 words per character set only 16 words are needed.  The array of bitsets is
indexed from zero to fifteen, inclusive, as shown in the calculation of the maximum value
for theBitsetsPerSet subrange.  In effect, this constant expression reduces to:

					(16 DIV 16) - 1 = (16 - 1) = 15.

The cost for the space savings is the increased execution time used in the calculation of an
item's bitset index and bit offset within the bitset.
*)

CONST bitsPerBitset = 16;
CONST maxSetSize    = MAX(SizeRange);
TYPE  BitsetsPerSet = [ 0 .. (maxSetSize DIV bitsPerBitset) - 1 ];
TYPE  BitIndex      = [ 0 .. bitsPerBitset - 1 ];

TYPE  ItemsArray = ARRAY BitsetsPerSet OF BITSET;
TYPE  DiscreteSet = RECORD
        items : ItemsArray;    (*-- Bit vector of items *)
      END (*-- DiscreteSet *);

TYPE  Set = POINTER TO DiscreteSet;

    (*-----------------------*)

VAR   theEmptySet : ItemsArray; (*-- Predefined set, initialized to ø *)


(*
14.2.2		Exceptions

To support the exception handling mechanism two variables are needed.  The first,
setError, is used to record the exception result from each operation; while handlers  is an
array of exception handling procedures indexed by the exception result.

The routines SetError, GetHandler, and SetHandler have been previously described in the
definition module, and their operation should be readily apparent.  RaiseErrIn is a local
routine used to set the setError variable and invoke the Raise routine of the
ErrorHandling module.
*)

VAR   setError   : Exceptions;
VAR   handlers   : ARRAY Exceptions OF HandlerProc;

    (*-----------------------*)

PROCEDURE SetError    ()             : Exceptions (*-- out   *);
BEGIN
  RETURN setError;
END SetError;
(*----------------------------*)

PROCEDURE GetHandler  (    ofError   : Exceptions  (*-- in    *))
                                     : HandlerProc (*-- out   *);
BEGIN
  RETURN handlers[ofError];
END GetHandler;
(*----------------------------*)

PROCEDURE SetHandler  (    ofError   : Exceptions  (*-- in    *);
                           toHandler : HandlerProc (*-- in    *));
BEGIN
  handlers[ofError] := toHandler;
END SetHandler;
(*----------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations    (*-- in    *);
                          theError   : Exceptions    (*-- in    *));
BEGIN
  setError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*----------------------------*)

PROCEDURE Recreate   (VAR theSet : Set     (*-- inout *))
                                 : BOOLEAN (*-- out   *);
BEGIN
  IF (theSet = NIL) THEN
    theSet := Create();
  END (*--if*);
  RETURN (theSet # NIL);
END Recreate;
(*----------------------------*)


(*
14.2.2		Constructors

Create simply allocates a new array of bitsets raising the overflow exception if unable to
do so.  Otherwise the newly created set is cleared using array assignment of the predefined
empty character set.
*)

PROCEDURE Create        ()           : Set         (*-- out   *);

VAR   newSet : Set;

BEGIN
  setError := noerr;
  Allocate(newSet, SIZE(DiscreteSet));
  IF (newSet # NIL) THEN
    newSet^.items := theEmptySet;
    RETURN newSet;
  END (*--if*);
  RaiseErrIn(create, overflow);
  RETURN NullSet;
END Create;
(*----------------------------*)


(*
Destroy takes advantage that Clear  sets setError  to noerr and raises the undefined 
exception.  So if Clear succeeds, Destroy releases the allocated set header.
*)

PROCEDURE Destroy       (VAR theSet    : Set         (*-- inout *));
BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    Deallocate(theSet, SIZE(theSet^));
  ELSE
    RaiseErrIn(destroy, undefined);
    theSet := NullSet;
  END (*--if*);
END Destroy;
(*----------------------------*)


(*
Clear sets setError to noerr and checks for an undefined set raising the undefined 
exception if necessary.  After asserting a valid set, it is sufficient to overwrite the existing
items with the predefined empty set.
*)

PROCEDURE Clear         (VAR theSet    : Set         (*-- inout *));
BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    theSet^.items := theEmptySet;
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*----------------------------*)


(*
Assign creates the target set if necessary and then uses array assignment to duplicate the
source bitset array within the target.
*)

PROCEDURE Assign        (    theSet  : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));
BEGIN 
  setError := noerr;
  IF (theSet # NIL) THEN
    IF Recreate(toSet) THEN
      toSet^.items := theSet^.items;
    END (*--if*);
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*----------------------------*)


(*
Include and Exclude simply calculate the bitset number and bit offset and use the
Modula-2 set inclusion and exclusion operations.
*)

PROCEDURE Include       (    theItem : Item        (*-- in    *);
                         VAR inSet   : Set         (*-- inout *));
BEGIN
  setError := noerr;
  IF (inSet # NIL) THEN
    INCL(inSet^.items[VAL(CARDINAL, ORD(theItem)) DIV bitsPerBitset],
         VAL(CARDINAL, ORD(theItem)) MOD bitsPerBitset);
  ELSE
    RaiseErrIn(include, undefined);
  END (*--if*);
END Include;
(*----------------------------*)

PROCEDURE Exclude       (    theItem : Item        (*-- in    *);
                         VAR fromSet : Set         (*-- inout *));
BEGIN
  setError := noerr;
  IF (fromSet # NIL) THEN
    EXCL(fromSet^.items[VAL(CARDINAL, ORD(theItem)) DIV bitsPerBitset],
         VAL(CARDINAL, ORD(theItem)) MOD bitsPerBitset);
  ELSE
    RaiseErrIn(exclude, undefined);
  END (*--if*);
END Exclude;
(*----------------------------*)


(*
Union, Intersection, Difference, and SymDifference all simply loop over the bitsets of
the left and right sets using the Modula-2 set operators to form the target set. 
Complement is similar except that it takes the difference of the universal set from the
given set.
*)

PROCEDURE Union         (    left   : Set         (*-- in    *);
                             right  : Set         (*-- in    *);
                         VAR toSet  : Set         (*-- inout *));

VAR   index : BitsetsPerSet; (*-- loop index over bitsets *)

BEGIN
  setError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF Recreate(toSet) THEN
      WITH toSet^ DO
        FOR index := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
          items[index] := left^.items[index] + right^.items[index];
        END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(union, undefined);
  END (*--if*);
END Union;
(*----------------------------*)

PROCEDURE Intersection  (    left   : Set         (*-- in    *);
                             right  : Set         (*-- in    *);
                         VAR toSet  : Set         (*-- inout *));

VAR   index : BitsetsPerSet; (*-- loop index over bitsets *)

BEGIN
  setError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF Recreate(toSet) THEN
      WITH toSet^ DO
        FOR index := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
          items[index] := left^.items[index] * right^.items[index];
        END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(intersection, undefined);
  END (*--if*);
END Intersection;
(*----------------------------*)

PROCEDURE Difference    (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   index : BitsetsPerSet; (*-- loop index over bitsets *)

BEGIN
  setError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF Recreate(toSet) THEN
      WITH toSet^ DO
        FOR index := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
          items[index] := left^.items[index] - right^.items[index];
        END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(difference, undefined);
  END (*--if*);
END Difference;
(*----------------------------*)

PROCEDURE SymDifference (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   index : BitsetsPerSet; (*-- loop index over bitsets *)

BEGIN
  setError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF Recreate(toSet) THEN
      WITH toSet^ DO
        FOR index := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
          items[index] := left^.items[index] / right^.items[index];
        END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(symdifference, undefined);
  END (*--if*);
END SymDifference;
(*----------------------------*)

PROCEDURE Complement    (    theSet  : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   index : BitsetsPerSet; (*-- loop index over bitsets *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    IF Recreate(toSet) THEN
      WITH theSet^ DO
        FOR index := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
          toSet^.items[index] := BITSET{0..15} - items[index];
        END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(complement, undefined);
  END (*--if*);
END Complement;
(*----------------------------*)



(*
14.2.3		Selectors

IsDefined returns true if the given set is not NIL and false otherwise, the simple test for a
defined set object.
*)

PROCEDURE IsDefined     (    theSet    : Set         (*-- in    *))
                                       : BOOLEAN     (*-- out   *);
BEGIN
  RETURN (theSet # NIL);
END IsDefined;
(*----------------------------*)


(*
IsEmpty simply loops through the bitsets returning false if any are found non-empty. 
Ideally, we would like to directly compare the given bitset array with the empty set array
but Modula-2 does not support array comparison.
*)

PROCEDURE IsEmpty       (    theSet    : Set         (*-- in    *))
                                       : BOOLEAN     (*-- out   *);

VAR   index : BitsetsPerSet; (*-- loop index over bitsets *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      FOR index := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
        IF (items[index] # BITSET{}) THEN
          RETURN FALSE;
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(isempty, undefined);
  END (*--if*);
  RETURN TRUE;
END IsEmpty;
(*----------------------------*)


(*
IsEqual loops over each of the set's bitset arrays returning false on the first inequality.  If
the loop completes without premature exit then the two sets must be equal.
*)

PROCEDURE IsEqual       (    left      : Set         (*-- in    *);
                             right     : Set         (*-- in    *))
                                       : BOOLEAN     (*-- out   *);

VAR   index : BitsetsPerSet;   (*-- loop index over bitsets *)

BEGIN
  setError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    WITH left^ DO
      FOR index := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
        IF (items[index] # right^.items[index]) THEN
          RETURN FALSE;
        END (*--if*);
      END (*--for*);
    END (*--with*);
    RETURN TRUE;
  ELSE
    RaiseErrIn(isequal, undefined);
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*----------------------------*)


(*
NumMembers calculates the number of member items of the set by looping over the
individual bitsets and summing to the number of “on” bits.  As simple BITSET
comparison with the empty bitset allows the routine to quickly skip over groups of
empty items.  As always, undefined sets cause zero to be returned.
*)

PROCEDURE NumMembers    (    theSet    : Set         (*-- in    *))
                                       : CARDINAL    (*-- out   *);

VAR   eachWord: BitsetsPerSet; (*-- loop index over bitsets *)
      eachBit : BitIndex;      (*-- loop index over bits  *)
      count   : CARDINAL;      (*-- working sum of items in the set *)

BEGIN
  setError := noerr;
  count    := 0;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      FOR eachWord := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
        IF (items[eachWord] # BITSET{}) THEN
          FOR eachBit := MIN(BitIndex) TO MAX(BitIndex) DO
            IF (eachBit IN items[eachWord]) THEN
              INC(count);
            END (*--if*);
          END (*--for*);
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(nummembers, undefined);
  END (*--if*);
  RETURN count;
END NumMembers;
(*----------------------------*)


(*
IsAMember calculates the bitset number and bit offset into the character set and simply
uses Modula-2 bitset inclusion to determine if the given item is a member of the set.  As
always, undefined sets cause false to be returned.
*)

PROCEDURE IsAMember     (    theItem   : Item        (*-- in    *);
                             theSet    : Set         (*-- in    *))
                                       : BOOLEAN     (*-- out   *);
BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    RETURN (VAL(CARDINAL, ORD(theItem)) MOD bitsPerBitset) IN
           theSet^.items[VAL(CARDINAL, ORD(theItem)) DIV bitsPerBitset];
  ELSE
    RaiseErrIn(ismember, undefined);
  END (*--if*);
  RETURN FALSE;
END IsAMember;
(*----------------------------*)


(*
IsSubset implementation takes advantage of the equivalence of A Í B with A Ï B = A, in
other words A - B = Ø.
*)


PROCEDURE IsSubset      (    left      : Set         (*-- in    *);
                             right     : Set         (*-- in    *))
                                       : BOOLEAN     (*-- out   *);

VAR   index : BitsetsPerSet; (*-- loop index over bitsets *)

BEGIN
  setError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    WITH left^ DO
      FOR index := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
        IF (items[index] - right^.items[index]) # BITSET{} THEN
          RETURN FALSE;
        END (*--if*);
      END (*--for*);
    END (*--with*);
    RETURN TRUE;
  ELSE
    RaiseErrIn(issubset, undefined);
  END (*--if*);
  RETURN FALSE;
END IsSubset;
(*----------------------------*)

PROCEDURE IsProperSubset (    left   : Set        (*-- in    *);
                              right  : Set        (*-- in    *))
                                     : BOOLEAN    (*-- out   *);
BEGIN
  RETURN IsSubset(left, right) & ~IsEqual(left, right);
END IsProperSubset;
(*----------------------------*)


(*
14.2.4		Iterators

LoopOver scans each bitset within the character set passing items on to the processing
procedure parameter.  Instead of examining every single bit, an optimization is done that
allows the routine to skip over a bitset if it is empty.  Only bitsets with at least one
member present are examined further to determine the individual items that are members.

Traverse operates in the same manner.
*)

PROCEDURE LoopOver      (    theSet  : Set            (*-- in    *);
                             process : LoopAccessProc (*-- in    *));

VAR   eachWord : BitsetsPerSet; (*-- loop index over bitsets *)
      eachBit  : BitIndex;      (*-- loop index over bits in bitset *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      FOR eachWord := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
        IF items[eachWord] # BITSET{} THEN
          FOR eachBit := MIN(BitIndex) TO MAX(BitIndex) DO
            IF (eachBit IN items[eachWord]) THEN
              IF ~process(CHR(eachWord * bitsPerBitset + eachBit)) THEN
                RETURN;
              END (*--if*);
            END (*--if*);
          END (*--for*);
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(loopover, undefined);
  END (*--if*);
END LoopOver;
(*----------------------------*)

PROCEDURE Traverse      (    theSet  : Set         (*-- in    *);
                             process : AccessProc  (*-- in    *));

VAR   eachWord : BitsetsPerSet; (*-- loop index over bitsets *)
      eachBit  : BitIndex;      (*-- loop index over bits in bitset *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      FOR eachWord := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
        IF items[eachWord] # BITSET{} THEN
          FOR eachBit := MIN(BitIndex) TO MAX(BitIndex) DO
            IF (eachBit IN items[eachWord]) THEN
              process(CHR(eachWord * bitsPerBitset + eachBit));
            END (*--if*);
          END (*--for*);
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(traverse, undefined);
  END (*--if*);
END Traverse;
(*----------------------------*)



(*
14.2.5		Module Initialization

In the module initialization, the predefined discrete set for the empty set (Ø) is filled with
empty values and the local exception handlers array variables are set to default handlers
(ExitOnError) except for the noerr handler which is given the null handler.  setError is
given the value noerr avoiding an undefined state.
*)

VAR   index : BitsetsPerSet; (*-- loop index over bitsets *)

BEGIN
  FOR index := MIN(BitsetsPerSet) TO MAX(BitsetsPerSet) DO
    theEmptySet[index] := BITSET{};
  END (*--for*);

  FOR setError := MIN(Exceptions) TO MAX(Exceptions) DO
    handlers[setError] := ExitOnError;
  END (*--for*);
  handlers[noerr] := NullHandler;
  setError := noerr;
END SetCSBMI.


(*
References

[1]	G. Booch, Software Components in Ada Structures, Tools and Subsystems,
	Benjamin/Cummings, Menlo Park, CA, 1987, pp. 40-43, 250-295.
[2]	R. Gleaves, Modula-2 for Pascal Programmers, Springer-Verlag, New York, NY, 1984,
	PowerSets Module, pg. 60.
[3]	R. Ford and R.S. Wiener, Modula-2: A Software Development Approach, John Wiley
	and Sons, New York, NY, 1985.
[4]	G.P. McKeown and V.J. Rayward-Smith, Mathematics for Computing, Halstead Press,
	Wokingham, England, 1982, Section 1.2, Set Theory, pp. 9-18.
[5]	Modula Corporation, Macintosh Modula-2 System Reference Manual, Version 4.1
	Supplement, Provo, UT, 1985, LongSets Module definition module.
[6]	N. Wirth, Programming in Modula-2, 3rd ed., Springer-Verlag, Berlin Heidelberg,
	1985.
*)