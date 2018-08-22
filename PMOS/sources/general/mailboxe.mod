IMPLEMENTATION MODULE Mailboxes;

	(********************************************************)
	(*							*)
	(*	Mailboxes for intertask communication		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	27 February 1995		*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM Timer IMPORT
    (* proc *)	TimedWait;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateLock, Obtain, Release;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)	Copy;

(************************************************************************)

CONST GuardConst = 3579;

TYPE
    MessagePtr = POINTER TO Message;

    Message = RECORD
		  next: MessagePtr;
		  size: CARDINAL;
		  content: ARRAY [0..0] OF BYTE;
	      END (*RECORD*);

    Mailbox = POINTER TO
		    RECORD
			guard: CARDINAL;
			limit: CARDINAL;
			mutex: Lock;
			count: Semaphore;
			head, tail: MessagePtr;
		    END (*RECORD*);

(************************************************************************)

PROCEDURE CreateMailbox (LengthLimit: CARDINAL): Mailbox;

    (* Creates a new mailbox.  LengthLimit is the maximum number of	*)
    (* characters in a single message.  (A limit is needed so that a	*)
    (* task reading the mailbox knows how much space to allocate.)	*)

    VAR result: Mailbox;

    BEGIN
	NEW (result);
	WITH result^ DO
	    guard := GuardConst;
	    limit := LengthLimit;
	    CreateLock (mutex);
	    CreateSemaphore (count, 0);
	    head := NIL;  tail := NIL;
	END (*WITH*);
	RETURN result;
    END CreateMailbox;

(************************************************************************)

PROCEDURE SendMessage (MB: Mailbox;  messageptr: ADDRESS;
					length: CARDINAL): BOOLEAN;

    (* Copies a string, specified by its address and length, into the	*)
    (* specified mailbox.  Returns TRUE if successful, and FALSE if the	*)
    (* message is too long or the mailbox does not exist.		*)

    VAR pnew: MessagePtr;

    BEGIN

	(* Check for invalid mailbox. *)

	IF (MB = NIL) OR (MB^.guard <> GuardConst)
			OR (length > MB^.limit) THEN
	    RETURN FALSE;
	END (*IF*);

	(* Create a new queue element containing the message. *)

	NEW (pnew);
	ALLOCATE (pnew, SIZE(Message) + length - 1);
	WITH pnew^ DO
	    next := NIL;  size := length;
	    IF length > 0 THEN
		Copy (messageptr, ADR(content), length);
	    END (*IF*);
	END (*WITH*);

	(* Insert the new entry into the mailbox queue. *)

	WITH MB^ DO
	    Obtain (mutex);
	    IF head = NIL THEN
		head := pnew;
	    ELSE
		tail^.next := pnew;
	    END (*IF*);
	    tail := pnew;
	    Release (mutex);
	    Signal (count);
	END (*WITH*);
	RETURN TRUE;

    END SendMessage;

(************************************************************************)

PROCEDURE ReceiveMessage (MB: Mailbox;  VAR (*OUT*) message: ARRAY OF CHAR;
					TimeLimit: CARDINAL): CARDINAL;

    (* Returns the next message (after waiting if necessary) from	*)
    (* mailbox MB.  TimeLimit is a timeout value in milliseconds.	*)
    (* (Specify TimeLimit=0 for infinite patience.)  The function	*)
    (* return value is the message length; this is zero if no message	*)
    (* was obtained, either because of a faulty mailbox or because of	*)
    (* timeout.  Note: it is also possible to have a genuine message of	*)
    (* zero length.							*)

    VAR length: CARDINAL;  TimedOut: BOOLEAN;
	second: MessagePtr;

    BEGIN

	(* Check for invalid mailbox.	*)

	IF (MB = NIL) OR (MB^.guard <> GuardConst) THEN
	    RETURN 0;
	END (*IF*);

	WITH MB^ DO

	    (* Wait no longer than TimeLimit for a message to arrive. *)

	    IF TimeLimit = 0 THEN
		Wait (count);
	    ELSE
		TimedWait (count, TimeLimit, TimedOut);
		IF TimedOut THEN RETURN 0 END(*IF*);
	    END (*IF*);

	    (* If we reach here, at least one message is available. *)

	    Obtain (mutex);

	    (* Copy the message. *)

	    WITH head^ DO
		second := next;  length := size;
		IF length > 0 THEN
		    Copy (ADR(content), ADR(message), length);
		END (*IF*);
	    END (*WITH*);
	    DEALLOCATE (head, SIZE(Message) + length - 1);

	    (* Update the queue. *)

	    head := second;
	    IF head = NIL THEN
		tail := NIL;
	    END (*IF*);
	    Release (mutex);
	END (*WITH*);
	RETURN length;
    END ReceiveMessage;

(************************************************************************)

END Mailboxes.
