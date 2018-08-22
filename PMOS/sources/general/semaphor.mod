IMPLEMENTATION MODULE Semaphores;

	(********************************************************)
	(*							*)
	(*	Implements the Wait and Signal operations on	*)
	(*	semaphores.					*)
	(*							*)
	(*	Programmer:	P. Moylan			*)
	(*	Last edited:	27 February 1995		*)
	(*	Status:		Working				*)
	(*							*)
	(********************************************************)

FROM TaskControl IMPORT
    (* type *)	TaskQueue,
    (* proc *)	CreateQueue, MarkAsReady, QueueAndSwitchTasks,
		QueueWithTimeout;

FROM InnerKernel IMPORT
    (* proc *)	EnterKernel, LeaveKernel;

FROM Storage IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM TerminationControl IMPORT
    (* proc *)	Crash;

(************************************************************************)

TYPE Semaphore = POINTER TO RECORD
				value	:	INTEGER;
				blockedlist :	TaskQueue
			    END;

(************************************************************************)

PROCEDURE CreateSemaphore (VAR (*OUT*) s: Semaphore;
					InitialValue: CARDINAL);

    (* Creates semaphore s, with the given initial value and an empty	*)
    (* queue.								*)

    VAR savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();
	NEW(s);
	s^.value := InitialValue;
	CreateQueue (s^.blockedlist);
	LeaveKernel (savedPSW);
    END CreateSemaphore;

(************************************************************************)

PROCEDURE DestroySemaphore (VAR (*INOUT*) s: Semaphore);

    (* Reclaims any space used by semaphore s.  Remark:  It is not at	*)
    (* all obvious what should be done with any tasks which happen to	*)
    (* be blocked on this semaphore (should they be unblocked, or	*)
    (* killed?).  At present we take the easy way out and assume that	*)
    (* there are no pending operations on s at the time that it is	*)
    (* destroyed.							*)

    VAR savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();
	DISPOSE (s);
	LeaveKernel (savedPSW);
    END DestroySemaphore;

(************************************************************************)

PROCEDURE Wait (VAR (*INOUT*) s: Semaphore);

    (* Decrements the semaphore value.  If the value goes negative, the	*)
    (* calling task is blocked and there is a task switch.		*)

    VAR savedPSW: CARDINAL;

    BEGIN
	IF s = NIL THEN Crash ("Wait on nonexistent semaphore"); END(*IF*);
	savedPSW := EnterKernel();
	DEC (s^.value);
	IF s^.value < 0 THEN
	    QueueAndSwitchTasks (s^.blockedlist)
	END (*IF*);
	LeaveKernel (savedPSW);
    END Wait;

(************************************************************************)

PROCEDURE TimedWaitT (VAR (*INOUT*) s: Semaphore;  TimeLimit: INTEGER;
			VAR (*OUT*) TimedOut: BOOLEAN);

    (* Like procedure Wait, except that it returns with TimedOut TRUE	*)
    (* if the corresponding Signal does not occur within TimeLimit	*)
    (* clock ticks.							*)

    VAR savedPSW: CARDINAL;

    BEGIN
	IF s = NIL THEN
	    Crash ("Timed Wait on nonexistent semaphore");
	END(*IF*);
	savedPSW := EnterKernel();
	TimedOut := FALSE;
	DEC (s^.value);
	IF s^.value < 0 THEN
	    IF TimeLimit <= 0 THEN
		TimedOut := TRUE;
	    ELSE
		TimedOut := QueueWithTimeout (s^.blockedlist, TimeLimit);
	    END (*IF*);

	    IF TimedOut THEN
		INC (s^.value);
	    END (*IF*);

	END (*IF s^.value < 0 *);
	LeaveKernel (savedPSW);

    END TimedWaitT;

(************************************************************************)

PROCEDURE Signal (VAR (*INOUT*) s: Semaphore);

    (* Increments the semaphore value.  Unblocks one task, if there was	*)
    (* one waiting on this semaphore.					*)

    VAR savedPSW: CARDINAL;

    BEGIN
	IF s = NIL THEN Crash ("Signal on nonexistent semaphore"); END(*IF*);
	savedPSW := EnterKernel();
	INC (s^.value);
	IF s^.value <= 0 THEN

	    (* If the semaphore initial value is nonnegative, the test	*)
	    (* (s^.value<=0) is equivalent to the more obvious test	*)
	    (* for nonempty blocked list.  Negative initial values are	*)
	    (* not permitted in this implementation.			*)

	    MarkAsReady (s^.blockedlist);

	END (*IF*);
	LeaveKernel (savedPSW);
    END Signal;

END Semaphores.
