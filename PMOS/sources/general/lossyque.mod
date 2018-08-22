IMPLEMENTATION MODULE LossyQueues;

	(********************************************************)
	(*							*)
	(*	Implementation of lossy queue structures	*)
	(*							*)
	(*	Author:		P. Moylan			*)
	(*	Last edited:	27 February 1995		*)
	(*	Status:		Working				*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* proc *)	ADR;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateLock, Obtain, Release;

FROM LowLevel IMPORT
    (* proc *)	Copy;

(************************************************************************)

TYPE
    (* Most of the fields below are self-explanatory, but note that:	*)
    (* (a) lastlocation, GetPlace, and PutPlace are measured as byte	*)
    (*	   offsets from the beginning of the data array;		*)
    (* (b) EltSize is the size, in bytes, of one element.		*)

    QueueTemplate = RECORD
			BufferFull: BOOLEAN;
			DataPresent: Semaphore;
			update: Lock;
			lastlocation, EltSize: CARDINAL;
			GetPlace, PutPlace: CARDINAL;
			data: ARRAY [0..0] OF BYTE;
		    END;

	(* Remark: we do something slightly nonstandard here, because	*)
	(* we want circular queues of various sizes and Modula-2 does	*)
	(* not support variable-length arrays.  The trick is to define	*)
	(* an array of one element, but then to extend it by calling	*)
	(* ALLOCATE to give us the array size we really need.  Of	*)
	(* course, this works only if we suppress "out of range" error	*)
	(* checking while compiling this module.  This is, admittedly,	*)
	(* an example of a programming style which probably should be	*)
	(* outlawed, but I couldn't think of a legal way to achieve the	*)
	(* desired result.						*)

    LossyQueue = POINTER TO QueueTemplate;

(************************************************************************)

PROCEDURE CreateQueue (VAR (*OUT*) B: LossyQueue;
					capacity, elementsize: CARDINAL);

    (* Allocates space for a lossy queue, and initializes it.  The	*)
    (* caller specifies how many elements (assumed to be of equal size)	*)
    (* the queue will hold, and the size in bytes of each element.	*)

    VAR size: CARDINAL;

    BEGIN
	size := capacity*elementsize;
	ALLOCATE (B, SIZE(QueueTemplate) + size - 1);
	WITH B^ DO
	    BufferFull := FALSE;
	    lastlocation := size - 1;
	    EltSize := elementsize;
	    CreateSemaphore (DataPresent, 0);
	    CreateLock (update);
	    GetPlace := 0;  PutPlace := 0;
	END (*WITH*);
    END CreateQueue;

(************************************************************************)

PROCEDURE PutQueue (B: LossyQueue;  item: ARRAY OF BYTE): BOOLEAN;

    (* If space is available, puts item at the tail of the queue and	*)
    (* returns TRUE.  Returns FALSE if the new item would not fit.	*)
    (* Note: it is assumed that SIZE(item) matches the element size	*)
    (* declared when the queue was created.				*)

    BEGIN
	WITH B^ DO
	    IF BufferFull THEN
		RETURN FALSE;
	    END (*IF*);
	    (*# save, check(index=>off) *)
	    Copy (ADR(item), ADR(data[PutPlace]), EltSize);
	    (*# restore *)

	    Obtain (update);
	    INC (PutPlace, EltSize);
	    IF PutPlace > lastlocation THEN PutPlace := 0; END(*IF*);
	    BufferFull := PutPlace = GetPlace;
	    Release (update);

	    Signal (DataPresent);
	    RETURN TRUE;
	END (*WITH*);
    END PutQueue;

(************************************************************************)

PROCEDURE GetQueue (B: LossyQueue;  VAR (*OUT*) item: ARRAY OF BYTE);

    (* Takes one item from the head of the queue, waiting if necessary. *)

    BEGIN
	WITH B^ DO
	    Wait (DataPresent);
	    (*# save, check(index=>off) *)
	    Copy (ADR(data[GetPlace]), ADR(item), EltSize);
	    (*# restore *)

	    Obtain (update);
	    INC (GetPlace, EltSize);
	    IF GetPlace > lastlocation THEN GetPlace := 0 END (*IF*);
	    BufferFull := FALSE;
	    Release (update);

	END (*WITH*);
    END GetQueue;

(************************************************************************)

END LossyQueues.
