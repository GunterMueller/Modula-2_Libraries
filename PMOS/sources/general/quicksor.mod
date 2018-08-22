IMPLEMENTATION MODULE QuickSortModule;

	(********************************************************)
	(*							*)
	(*	In-memory sort using the QuickSort method	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	27 February 1995		*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* proc *)	ADR;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)	Copy,
    (* proc *)	AddOffset, SubtractOffset, Physical;

(************************************************************************)

TYPE
    EltPointer = ADDRESS;
    Array = RECORD
		location: ADDRESS;
		eltsize: CARDINAL;
		greaterorequal: CompareProc;
	    END (*RECORD*);

(************************************************************************)

PROCEDURE CmpPtr (pa, pb: EltPointer): INTEGER;

    (* Returns -1 if pa<pb, 0 if pa=pb, +1 if pa>pb.	*)

    VAR na, nb: LONGCARD;

    BEGIN
	na := Physical (pa);
	nb := Physical (pb);
	IF na < nb THEN RETURN -1;
	ELSIF na > nb THEN RETURN +1;
	ELSE RETURN 0;
	END (*IF*);
    END CmpPtr;

(************************************************************************)

PROCEDURE PtrDiff (high, low: ADDRESS): LONGCARD;

    (* Returns Physical(high) - Physical(low)	*)

    BEGIN
	RETURN Physical (high) - Physical (low);
    END PtrDiff;

(************************************************************************)

PROCEDURE Partition ( VAR (*INOUT*) A: Array;  low: EltPointer;
			VAR (*OUT*) mid: EltPointer;  high: EltPointer);

    (* By shuffling elements of A as necessary, ensures the property	*)
    (*		A[j] <= v	for low <= j < mid			*)
    (*		A[mid] = v						*)
    (*		A[j] >= v	for mid < j <= high			*)
    (* where v is some unspecified value chosen by the procedure.	*)
    (* Input assumption: high > low, i.e. more than one element.	*)
    (* Remark: for an array of <=3 elements, this procedure completely	*)
    (* sorts the array.							*)

    VAR up, down, temp: EltPointer;  N: CARDINAL;

    BEGIN
	down := low;  up := high;
	N := VAL(CARDINAL, PtrDiff (high, low) DIV VAL(LONGCARD,A.eltsize)) + 1;
	mid := AddOffset (low, A.eltsize * (N DIV 2));

	ALLOCATE (temp, A.eltsize);

	(* Pre-sort: first we put the first, middle and last elements	*)
	(* in their correct relative order.				*)
	(* To begin with, ensure that high^ >= low^.			*)

	IF NOT A.greaterorequal(high, low) THEN
	    Copy (low, temp, A.eltsize);
	    Copy (high, low, A.eltsize);
	    Copy (temp, high, A.eltsize);
	END (*IF*);
	IF N = 2 THEN
	    DEALLOCATE (temp, A.eltsize);  RETURN;
	END (*IF*);

	(* Load the middle element into temp^.  By swapping elements as	*)
	(* necessary, ensure that high^ >= temp^ >= low^.		*)

	Copy (mid, temp, A.eltsize);
	IF NOT A.greaterorequal (mid, low) THEN
	    Copy (low, temp, A.eltsize);
	    Copy (mid, low, A.eltsize);
	ELSIF NOT A.greaterorequal (high, mid) THEN
	    Copy (high, temp, A.eltsize);
	    Copy (mid, high, A.eltsize);
	END (*IF*);

	(* For an array of <=3 elements, the above pre-sort is actually	*)
	(* a complete sort.						*)

	IF N <= 3 THEN
	    Copy (temp, mid, A.eltsize);
	    DEALLOCATE (temp, A.eltsize);  RETURN;
	END (*IF*);

	(* v = temp^ *)

	LOOP
	    WHILE (CmpPtr (down, mid) < 0) AND A.greaterorequal (temp, down) DO
		down := AddOffset (down, A.eltsize);
	    END (*WHILE*);

	    (* All elements below down^ <= a			*)
	    (* ((down^ > v) AND (down < mid)) OR down >= mid	*)

	    IF CmpPtr (down, mid) < 0 THEN

		(* All elements below down^ <= v	*)
		(* (down^ > v) AND (down < mid)		*)

		Copy (down, mid, A.eltsize);
		mid := down;
		down := AddOffset (down, A.eltsize);

		(* hole at mid < down	*)

	    END (*IF*);

	    (* Note that down >= mid at this point.	*)

	    WHILE (CmpPtr (up, mid) > 0) AND A.greaterorequal (up, temp) DO
		up := SubtractOffset (up, A.eltsize);
	    END (*WHILE*);

	    (* All elements above up^ >= v			*)
	    (* ((up^ < v) AND (up > mid)) OR up <= mid		*)

	    IF CmpPtr (up, mid) <= 0 THEN EXIT(*LOOP*) END(*IF*);

	    Copy (up, mid, A.eltsize);
	    mid := up;
	    up := SubtractOffset (up, A.eltsize);

	    (* hole at mid > up *)

	END (*LOOP*);
	Copy (temp, mid, A.eltsize);
	DEALLOCATE (temp, A.eltsize);
    END Partition;

(************************************************************************)

PROCEDURE Sort ( VAR (*INOUT*) A: Array;  low, high: EltPointer);

    (* Sorts the subarray A[low..high] inclusive.	*)

    VAR mid: EltPointer;

    BEGIN
	WHILE CmpPtr (high, low) > 0 DO
	    Partition (A, low, mid, high);
	    IF CmpPtr (mid, low) > 0 THEN
		Sort (A, low, SubtractOffset (mid, A.eltsize));
	    END (*IF*);
	    low := AddOffset (mid, A.eltsize);
	END (*WHILE*);
    END Sort;

(************************************************************************)
(*			   THE END-USER VERSION				*)
(************************************************************************)

PROCEDURE QuickSort (VAR (*INOUT*) data: ARRAY OF BYTE;
				N, EltSize: CARDINAL;  GE: CompareProc);

    (* In-place sort of array data[0..N].  EltSize is the element size,	*)
    (* and GE is a user-supplied function to compare elements at two	*)
    (* specified addresses.						*)

    VAR A: Array;

    BEGIN
	WITH A DO
	    location := ADR (data);
	    eltsize := EltSize;
	    greaterorequal := GE;
	    Sort (A, location, AddOffset (location, N*EltSize));
	END (*WITH*);
    END QuickSort;

(************************************************************************)

END QuickSortModule.
