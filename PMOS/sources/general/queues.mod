IMPLEMENTATION MODULE Queues;

	(********************************************************)
	(*							*)
	(*		Generic queue module.			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:		Working.			*)
	(*							*)
	(********************************************************)

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, DestroySemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateLock, DestroyLock, Obtain, Release;

(************************************************************************)

TYPE

    (* A QueueLink is a pointer from one queue element to the next.	*)

    QueueLink = POINTER TO QueueElement;

    (* The actual queue element contains a pointer to the next queue	*)
    (* element, and a pointer to the enqueued data.			*)

    QueueElement =  RECORD
			next: QueueLink;
			DataPtr: ADDRESS;
		    END (*RECORD*);

    (* Every queue has a header, which contains pointers to the head	*)
    (* and tail of the queue; an access lock for critical section	*)
    (* protection; and a counting semaphore which keeps track of the	*)
    (* size of the queue.						*)

    QueueHeader =   RECORD
			head, tail: QueueLink;
			access: Lock;
			count: Semaphore;
		    END (*RECORD*);

    (* Finally, a Queue is defined via a pointer to its header.		*)

    Queue = POINTER TO QueueHeader;

(************************************************************************)

PROCEDURE CreateQueue (VAR (*OUT*) Q: Queue);

    (* Creates a new queue, initially empty.	*)

    BEGIN
	NEW (Q);
	WITH Q^ DO
	    head := NIL;  tail := NIL;
	    CreateLock (access);
	    CreateSemaphore (count, 0);
	END (*WITH*);
    END CreateQueue;

(************************************************************************)

PROCEDURE DestroyQueue (Q: Queue);

    (* Destroys queue Q, thus freeing up the space it occupied.  Any	*)
    (* data still on the queue are lost.  After this call, no further	*)
    (* operations should be performed on the queue.			*)

    VAR following: QueueLink;

    BEGIN
	WITH Q^ DO
	    Obtain (access);
	    WHILE head <> NIL DO
		following := head^.next;
		DISPOSE (head);
		head := following;
	    END (*WHILE*);
	    DestroyLock (access);
	    DestroySemaphore (count);
	END (*WITH*);
	DISPOSE (Q);
    END DestroyQueue;

(************************************************************************)

PROCEDURE AddToQueue (Q: Queue;  DataPointer: ADDRESS);

    (* Places a new element at the tail of queue Q.  The caller has an	*)
    (* obligation to ensure that DataPointer^ remains in existence	*)
    (* for as long as it remains on the queue.				*)

    VAR element: QueueLink;

    BEGIN
	NEW (element);
	WITH element^ DO
	    next := NIL;  DataPtr := DataPointer;
	END (*WITH*);
	WITH Q^ DO
	    Obtain (access);
	    IF head = NIL THEN
		head := element;
	    ELSE
		tail^.next := element;
	    END (*IF*);
	    tail := element;
	    Release (access);
	    Signal (count);
	END (*WITH*);
    END AddToQueue;

(************************************************************************)

PROCEDURE TakeFromQueue (Q: Queue): ADDRESS;

    (* Removes and returns a pointer to the datum at the head of the	*)
    (* queue.								*)

    VAR result: ADDRESS;
	second: QueueLink;

    BEGIN
	WITH Q^ DO
	    Wait (count);
	    Obtain (access);
	    second := head^.next;  result := head^.DataPtr;
	    DISPOSE (head);
	    head := second;
	    IF head = NIL THEN
		tail := NIL;
	    END (*IF*);
	    Release (access);
	END (*WITH*);
	RETURN result;
    END TakeFromQueue;

(************************************************************************)

PROCEDURE Empty (Q: Queue): BOOLEAN;

    (* Returns TRUE iff Q is empty.	*)

    VAR result: BOOLEAN;

    BEGIN
	WITH Q^ DO
	    Obtain (access);
	    result := head = NIL;
	    Release (access);
	END (*WITH*);
	RETURN result;
    END Empty;

(************************************************************************)

END Queues.
