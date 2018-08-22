IMPLEMENTATION MODULE CircularBuffers;

	(********************************************************)
	(*							*)
	(*	Circular Buffers for passing character data	*)
	(*	between a pair of tasks.			*)
	(*							*)
	(*	Author:		P. Moylan			*)
	(*	Last edited:	21 February 1995		*)
	(*							*)
	(*	Status:		OK				*)
	(*							*)
	(********************************************************)

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE;

FROM Timer IMPORT
    (* proc *)	TimedWait;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateLock, Obtain, Release;

(************************************************************************)

TYPE
    BufferTemplate = RECORD
			SlotAvailable, DataPresent: Semaphore;
			lastlocation: CARDINAL;
			mutex: Lock;
			GetPlace, PutPlace: CARDINAL;
			data: ARRAY [0..0] OF CHAR
		     END;

	(* Remark: we do something slightly nonstandard here, because	*)
	(* we want circular buffers of various sizes and Modula-2 does	*)
	(* not support variable-length arrays.  The trick is to define	*)
	(* an array of one element, but then to extend it by calling	*)
	(* ALLOCATE to give us the array size we really need.  Of	*)
	(* course, this works only if we suppress "out of range" error	*)
	(* checking while compiling this module.  This is, admittedly,	*)
	(* an example of a programming style which probably should be	*)
	(* outlawed, but I couldn't think of a legal way to achieve the	*)
	(* desired result.						*)

    CircularBuffer = POINTER TO BufferTemplate;

(************************************************************************)

PROCEDURE CreateBuffer (VAR (*OUT*) B: CircularBuffer;  size: CARDINAL);

    (* Allocates space for a circular buffer, and initializes it.  The	*)
    (* caller specifies how many characters the buffer will hold.	*)

    BEGIN
	ALLOCATE (B, SIZE(BufferTemplate) + size - 1);
	WITH B^ DO
	    lastlocation := size - 1;
	    CreateSemaphore (SlotAvailable, size);
	    CreateSemaphore (DataPresent, 0);
	    CreateLock (mutex);
	    GetPlace := 0;  PutPlace := 0;
	END (*WITH*);
    END CreateBuffer;

(************************************************************************)

PROCEDURE PutBuffer (B: CircularBuffer; item: CHAR);

    (* Puts item into the circular buffer, waiting for space available	*)
    (* if necessary.							*)

    BEGIN
	WITH B^ DO
	    Wait (SlotAvailable);
	    (*# save, check(index => off) *)
	    data[PutPlace] := item;
	    (*# restore *)
	    IF PutPlace = lastlocation THEN PutPlace := 0
	    ELSE INC (PutPlace);
	    END (*IF*);
	    Signal (DataPresent);
	END (*WITH*);
    END PutBuffer;

(************************************************************************)

PROCEDURE PutBufferImpatient (B: CircularBuffer;  item: CHAR;
						TimeLimit: CARDINAL);

    (* Like PutBuffer, but waits no longer than TimeLimit milliseconds	*)
    (* for a buffer slot to become available.  If the time limit	*)
    (* expires, the oldest item in the buffer is overwritten by the	*)
    (* new data.							*)

    VAR TimedOut: BOOLEAN;

    BEGIN
	WITH B^ DO
	    TimedWait (SlotAvailable, TimeLimit, TimedOut);
	    IF TimedOut THEN
		Obtain (mutex);
		IF GetPlace = lastlocation THEN GetPlace := 0
		ELSE INC (GetPlace);
		END (*IF*);
		Release (mutex);
	    END (*IF*);
	    (*# save, check(index => off) *)
	    data[PutPlace] := item;
	    (*# restore *)
	    IF PutPlace = lastlocation THEN PutPlace := 0
	    ELSE INC (PutPlace);
	    END (*IF*);
	    IF NOT TimedOut THEN
		Signal (DataPresent);
	    END (*IF*);
	END (*WITH*);
    END PutBufferImpatient;

(************************************************************************)

PROCEDURE GetBuffer (B: CircularBuffer) : CHAR;

    (* Gets one character from the circular buffer, waiting when	*)
    (* necessary for a character to become available.			*)

    VAR result: CHAR;

    BEGIN
	WITH B^ DO
	    Wait (DataPresent);
	    Obtain (mutex);
	    (*# save, check(index => off) *)
	    result := data[GetPlace];
	    (*# restore *)
	    IF GetPlace = lastlocation THEN GetPlace := 0
	    ELSE INC (GetPlace);
	    END (*IF*);
	    Release (mutex);
	    Signal (SlotAvailable);
	END (*WITH*);
	RETURN result;
    END GetBuffer;

(************************************************************************)

PROCEDURE BufferEmpty (B: CircularBuffer): BOOLEAN;

    (* Returns TRUE iff the buffer is empty. *)

    VAR TimedOut: BOOLEAN;

    BEGIN
	WITH B^ DO
	    TimedWait (DataPresent, 0, TimedOut);
	    IF TimedOut THEN RETURN TRUE;
	    ELSE
		Signal (DataPresent);
		RETURN FALSE;
	    END (*IF*);
	END (*WITH*);
    END BufferEmpty;

(************************************************************************)

END CircularBuffers.
