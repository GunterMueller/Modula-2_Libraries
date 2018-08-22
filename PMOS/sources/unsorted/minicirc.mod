IMPLEMENTATION MODULE MiniCircularBuffers;

	(********************************************************)
	(*							*)
	(*	Circular Buffers for passing character data	*)
	(*	between a pair of tasks.			*)
	(*							*)
	(*	Author:		P. Moylan			*)
	(*	Last edited:	16 August 1992			*)
	(*							*)
	(*	Status:		OK				*)
	(*							*)
	(********************************************************)

FROM Storage IMPORT
    (* proc *)	ALLOCATE;

FROM MiniTrace IMPORT
    (* proc *)	InTrace, OutTrace;

FROM MiniKernel IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

(************************************************************************)

TYPE
    BufferTemplate = RECORD
			SlotAvailable, DataPresent: Semaphore;
			lastlocation: CARDINAL;
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
	InTrace ("CreateBuffer");
	ALLOCATE (B, SIZE(BufferTemplate) + size - 1);
	WITH B^ DO
	    lastlocation := size - 1;
	    CreateSemaphore (SlotAvailable, size);
	    CreateSemaphore (DataPresent, 0);
	    GetPlace := 0;  PutPlace := 0;
	END (*WITH*);
	OutTrace ("CreateBuffer");
    END CreateBuffer;

(************************************************************************)

PROCEDURE PutBuffer (B: CircularBuffer; item: CHAR);

    (* Puts item into the circular buffer, waiting for space available	*)
    (* if necessary.							*)

    BEGIN
	InTrace ("PutBuffer");
	WITH B^ DO
	    Wait (SlotAvailable);
	    data[PutPlace] := item;
	    IF PutPlace = lastlocation THEN PutPlace := 0
	    ELSE INC (PutPlace);
	    END (*IF*);
	    Signal (DataPresent);
	END (*WITH*);
	OutTrace ("PutBuffer");
    END PutBuffer;

(************************************************************************)

PROCEDURE GetBuffer (B: CircularBuffer) : CHAR;

    (* Gets one character from the circular buffer, waiting when	*)
    (* necessary for a character to become available.			*)

    VAR result: CHAR;

    BEGIN
	InTrace ("GetBuffer");
	WITH B^ DO
	    Wait (DataPresent);
	    result := data[GetPlace];
	    IF GetPlace = lastlocation THEN GetPlace := 0
	    ELSE INC (GetPlace);
	    END (*IF*);
	    Signal (SlotAvailable);
	END (*WITH*);
	OutTrace ("GetBuffer");
	RETURN result;
    END GetBuffer;

(************************************************************************)

END MiniCircularBuffers.
