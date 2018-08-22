IMPLEMENTATION MODULE FileSort;

	(********************************************************)
	(*							*)
	(*	In-place file sort using the QuickSort method	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	27 February 1995		*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM FileSys IMPORT
    (* proc *)	SetPosition, ReadRecord, WriteRecord;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode;

FROM QuickSortModule IMPORT
    (* proc *)	QuickSort;

FROM LowLevel IMPORT
    (* proc *)	AddOffset, Copy;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST CacheSize = 2048;

TYPE
    Cache = RECORD
		first, last, dirty1, dirty2: RecordNumber;
		dp: ADDRESS;
	    END (*RECORD*);

    FileDescriptor = RECORD
			file: File;
			eltsize, offset: CARDINAL;
			greaterorequal: CompareProc;
			cache: Cache;
		     END (*RECORD*);

    EltPointer = ADDRESS;

(************************************************************************)
(*			BASIC I/O OPERATIONS				*)
(************************************************************************)

PROCEDURE ReadBlock (VAR (*IN*) FD: FileDescriptor;  N1, N2: RecordNumber;
							dest: EltPointer);

    (* Reads record numbers N1..N2, inclusive, into a buffer starting	*)
    (* at location 'dest'.						*)

    VAR status: ErrorCode;  actual: CARDINAL;

    BEGIN
	WITH FD DO
	    status := SetPosition (file, VAL(LONGCARD,offset)
					+ VAL(LONGCARD,eltsize)*N1);
	    status := ReadRecord (file, dest,
				eltsize*VAL(CARDINAL,N2-N1+1), actual);
	END (*WITH*);
    END ReadBlock;

(************************************************************************)

PROCEDURE WriteBlock (VAR (*IN*) FD: FileDescriptor; src: EltPointer;
						N1, N2: RecordNumber);

    (* Writes from a buffer starting at location 'src' to records	*)
    (* number N1 to N2 inclusive.					*)

    VAR status: ErrorCode;

    BEGIN
	WITH FD DO
	    status := SetPosition (file, VAL(LONGCARD,offset)
					+ VAL(LONGCARD,eltsize)*N1);
	    status := WriteRecord (file, src, eltsize*VAL(CARDINAL,N2-N1+1));
	END (*WITH*);
    END WriteBlock;

(************************************************************************)
(*			     CACHE MANAGEMENT				*)
(* The cache here is a "load on demand" cache rather than the more	*)
(* conventional cache which holds recently read records.  Our approach	*)
(* is to cache the low-numbered records of the subfile currently	*)
(* being sorted, which is consistent with an algorithm which, roughly	*)
(* speaking, sorts the bottom half of a range before starting work on	*)
(* the top half.							*)
(************************************************************************)

PROCEDURE FlushCache (VAR (*IN*) FD: FileDescriptor;  N: RecordNumber);

    (* Writes back any unwritten records, up to and including record	*)
    (* number N, back to disk.						*)

    BEGIN
	WITH FD DO
	    WITH cache DO
		IF N > dirty2 THEN
		    N := dirty2;
		END (*IF*);
		IF N >= dirty1 THEN
		    (* We must flush out records dirty1..N *)
		    WriteBlock (FD, AddOffset(dp,
			eltsize*VAL(CARDINAL,dirty1-first)), dirty1, N);
		    dirty1 := N+1;
		END (*IF*);
	    END (*WITH*);
	END (*WITH*);
    END FlushCache;

(************************************************************************)

PROCEDURE LoadCache (VAR (*IN*) FD: FileDescriptor;
					low, high: RecordNumber): BOOLEAN;

    (* Puts records low..high inclusive into the cache, if they will	*)
    (* fit.  If they don't fit, loads as many as possible starting from	*)
    (* record number low.  Returns TRUE iff everything fitted.		*)

    VAR result: BOOLEAN;

    BEGIN
	WITH FD DO

	    (* Work out whether everything will fit.  If not, adjust	*)
	    (* high downwards.						*)

	    IF VAL(LONGCARD,eltsize)*(high-low+1) <= CacheSize THEN
		result := TRUE;
	    ELSE
		result := FALSE;
		high := VAL(LONGCARD, CacheSize DIV eltsize) + low - 1;
	    END (*IF*);

	    (* Clean out everything below 'low' from the cache *)

	    IF low > 0 THEN
		FlushCache (FD, low-1);
	    END (*IF*);
	    WITH cache DO

		(* If the leading part of what we want is already in	*)
		(* the cache, move it to the beginning.			*)

		IF (first <= last) AND (low >= first) AND (low <= last) THEN
		    IF low > first THEN
			Copy (AddOffset(dp, eltsize*VAL(CARDINAL,low-first)),
				 dp, eltsize*VAL(CARDINAL,last-low+1));
			first := low;
		    END (*IF*);
		    low := last + 1;

		ELSE

		    (* In all other cases, empty the cache completely	*)
		    (* and make a fresh beginning.  Note that we don't	*)
		    (* bother to check for cases like low<first and	*)
		    (* high>first, because we know that data will flow	*)
		    (* through the cache only in one direction.		*)

		    FlushCache (FD, last);
		    last := 0;  first := 1;

		END (*IF*);

		(* At this stage, either the cache is empty or it	*)
		(* contains a part of what we want.  In either case,	*)
		(* the remainder is sure to fit.			*)

		IF first > last THEN
		    ReadBlock (FD, low, high, dp);
		    first := low;
		    last := high;
		ELSE
		    IF high >= low THEN
			ReadBlock (FD, low, high,
				AddOffset(dp,eltsize*VAL(CARDINAL,low-first)));
		    END (*IF*);
		    IF high > last THEN
			last := high;
		    END (*IF*);
		END (*IF*);
	    END (*WITH*);
	END (*WITH*);
	RETURN result;
    END LoadCache;

(************************************************************************)

PROCEDURE MarkDirty (VAR (*IN*) FD: FileDescriptor;  N: RecordNumber);

    (* Notes that record number N needs to be written back to disk	*)
    (* from the cache.							*)

    BEGIN
	WITH FD.cache DO
	    IF dirty2 < dirty1 THEN
		dirty1 := N;  dirty2 := N;
	    ELSIF N < dirty1 THEN dirty1 := N;
	    ELSIF N > dirty2 THEN dirty2 := N;
	    END (*IF*);
	END (*WITH*);
    END MarkDirty;

(************************************************************************)

PROCEDURE AddressInCache (VAR (*IN*) FD: FileDescriptor;
					index: RecordNumber): EltPointer;

    (* Returns the address in memory of element 'index', if it is in	*)
    (* the cache; otherwise returns NIL.				*)

    BEGIN
	WITH FD DO
	    WITH cache DO
		IF (first > last) OR (index < first) OR (index > last) THEN
		    RETURN NIL;
		ELSE
		    RETURN AddOffset (dp, eltsize*VAL(CARDINAL,index-first));
		END (*IF*);
	    END (*WITH*);
	END (*WITH*);
    END AddressInCache;

(************************************************************************)
(*			I/O THROUGH CACHE WHERE POSSIBLE		*)
(************************************************************************)

PROCEDURE GetRecord (VAR (*IN*) FD: FileDescriptor; index: RecordNumber;
							dest: EltPointer);

    (* Reads record number 'index' of the file into a buffer starting	*)
    (* at location 'dest'.						*)

    VAR p: EltPointer;

    BEGIN
	p := AddressInCache (FD, index);
	IF p <> NIL THEN
	    Copy (p, dest, FD.eltsize);
	ELSE
	    ReadBlock (FD, index, index, dest);
	END (*IF*);
    END GetRecord;

(************************************************************************)

PROCEDURE PutRecord (VAR (*IN*) FD: FileDescriptor; src: EltPointer;
						index: RecordNumber);

    (* Writes from a buffer starting at location 'src' to record	*)
    (* number 'index' of the file.					*)

    VAR p: EltPointer;

    BEGIN
	p := AddressInCache (FD, index);
	IF p <> NIL THEN
	    Copy (src, p, FD.eltsize);
	    MarkDirty (FD, index);
	ELSE
	    WriteBlock (FD, src, index, index);
	END (*IF*);
    END PutRecord;

(************************************************************************)
(*			THE OVERALL SORTING ALGORITHM			*)
(************************************************************************)

PROCEDURE Partition ( VAR (*IN*) FD: FileDescriptor;
				low: RecordNumber;
				VAR (*OUT*) mid: RecordNumber;
				high: RecordNumber);

    (* By shuffling elements as necessary, ensures the property		*)
    (*		R[j] <= v	for low <= j < mid			*)
    (*		R[mid] = v						*)
    (*		R[j] >= v	for mid < j <= high			*)
    (* where R[j] represents record number j of the file, mid is the	*)
    (* function result, and v is some unspecified value chosen by the	*)
    (* procedure.							*)

    VAR up, down: RecordNumber;
	ptemp, pmid: EltPointer;

    BEGIN
	down := low;  up := high;  mid := (down + up) DIV 2;

	ALLOCATE (ptemp, FD.eltsize);
	ALLOCATE (pmid, FD.eltsize);
	GetRecord (FD, mid, pmid);

	(* v is pmid^.  The following loop maintains the invariants:	*)
	(*	R[j] <= v	for low <= j < down			*)
	(*	R[j] >= v	for up < j <= high			*)
	(* We exit the outer loop when down >= mid and up <= mid.	*)
	(* Note that v=pmid^ is the value that should be stored as	*)
	(* R[mid], but to avoid redundant store and load operations as	*)
	(* mid changes we don't actually store this value back until	*)
	(* the final exit from the loop.  During loop execution, mid	*)
	(* refers to a "hole" in which a value has not yet been stored.	*)
	(* Note also that ptemp^ holds either R[down] or R[up],		*)
	(* depending on whether we're adjusting down or up at the time.	*)

	LOOP
	    GetRecord (FD, down, ptemp);
	    WHILE (down < mid) AND FD.greaterorequal (pmid, ptemp) DO
		INC (down);
		GetRecord (FD, down, ptemp);
	    END (*WHILE*);

	    IF down < mid THEN
		PutRecord (FD, ptemp, mid);
		mid := down;
		INC (down);
	    END (*IF*);

	    (* Note that down >= mid at this point.	*)

	    GetRecord (FD, up, ptemp);
	    WHILE (up > mid) AND FD.greaterorequal (ptemp, pmid) DO
		DEC (up);
		GetRecord (FD, up, ptemp);
	    END (*WHILE*);

	    IF up <= mid THEN EXIT(*LOOP*) END(*IF*);

	    PutRecord (FD, ptemp, mid);
	    mid := up;
	    DEC (up);

	END (*LOOP*);

	PutRecord (FD, pmid, mid);
	DEALLOCATE (pmid, FD.eltsize);
	DEALLOCATE (ptemp, FD.eltsize);

    END Partition;

(************************************************************************)

PROCEDURE SmallSort (VAR (*IN*) FD: FileDescriptor;  low, high: RecordNumber);

    (* Sorts the subfile of records low..high inclusive, where we are	*)
    (* guaranteed that the entire range is in the cache.		*)

    VAR p: EltPointer;

    BEGIN
	p := AddressInCache (FD, low);
	QuickSort (p^, VAL(CARDINAL,high-low), FD.eltsize, FD.greaterorequal);
	MarkDirty (FD, low);
	MarkDirty (FD, high);
    END SmallSort;

(************************************************************************)

PROCEDURE Sort ( VAR (*IN*) FD: FileDescriptor;  low, high: RecordNumber);

    (* Sorts the subfile of records low..high inclusive.	*)

    VAR mid: RecordNumber;

    BEGIN
	IF LoadCache (FD, low, high) THEN
	    SmallSort (FD, low, high);
	ELSE
	    Partition (FD, low, mid, high);
	    IF mid > low+1 THEN Sort (FD, low, mid-1) END(*IF*);
	    IF high > mid+1 THEN Sort (FD, mid+1, high) END(*IF*);
	END (*IF*);
	FlushCache (FD, high);
    END Sort;

(************************************************************************)
(*			   THE END-USER VERSION				*)
(************************************************************************)

PROCEDURE InplaceSort (f: File;  from, to: RecordNumber;
			EltSize, Offset: CARDINAL;  GE: CompareProc);

    (* In-place sort of part of a file.  We sort record numbers		*)
    (* from..to inclusive.  EltSize is the element size; Offset is the	*)
    (* number of bytes (zero, in most cases) before record number 0 in	*)
    (* the file; and GE is a user-supplied function to compare elements	*)
    (* at two specified addresses.					*)

    VAR FD: FileDescriptor;

    BEGIN
	WITH FD DO
	    file := f;
	    eltsize := EltSize;
	    offset := Offset;
	    greaterorequal := GE;
	    WITH cache DO
		first := MAX(RecordNumber);  last := 0;
		dirty1 := 1;  dirty2 := 0;
		ALLOCATE (dp, CacheSize);
	    END (*WITH*);
	END (*WITH*);
	Sort (FD, from, to);
	DEALLOCATE (FD.cache.dp, CacheSize);
    END InplaceSort;

(************************************************************************)

END FileSort.
