IMPLEMENTATION MODULE DummySemaphores;

	(********************************************************)
	(*							*)
	(*	Defines the semaphore data type, and the two	*)
	(*	basic operations on a semaphore.		*)
	(*							*)
	(*	Programmer:	P. Moylan			*)
	(*	Last edited:	14 August 1992			*)
	(*							*)
	(*	Status:		OK.				*)
	(*							*)
	(*	NOTE: This is a dummy version; it does not	*)
	(*	actually do anything with the "semaphores".	*)
	(*	It is purely for use in single-task		*)
	(*	applications.					*)
	(*							*)
	(********************************************************)

TYPE Semaphore = POINTER TO CHAR;

VAR Dummy: CHAR;

(************************************************************************)

PROCEDURE CreateSemaphore (VAR (*OUT*) s: Semaphore; InitialValue: CARDINAL);

    (* Creates semaphore s, with the given initial value and an empty	*)
    (* queue.								*)

    BEGIN
	s := ADR(Dummy);
    END CreateSemaphore;

(************************************************************************)

PROCEDURE DestroySemaphore (VAR (*INOUT*) s: Semaphore);

    (* Reclaims any space used by semaphore s.  Remark:  It is not at	*)
    (* all obvious what should be done with any tasks which happen to	*)
    (* be blocked on this semaphore (should they be unblocked, or	*)
    (* killed?).  At present we take the easy way out and assume that	*)
    (* there are no pending operations on s at the time that it is	*)
    (* destroyed.							*)

    BEGIN
	s := NIL;
    END DestroySemaphore;

(************************************************************************)

PROCEDURE Wait (VAR (*INOUT*) s: Semaphore);

    (* Decrements the semaphore value.  If the value goes negative,	*)
    (* the calling task is blocked and there is a task switch.		*)

    BEGIN
    END Wait;

(************************************************************************)

PROCEDURE TimedWaitT (VAR (*INOUT*) s: Semaphore;
			TimeLimit: INTEGER;  VAR (*OUT*) TimedOut: BOOLEAN);

    (* Like procedure Wait, except that it returns with TimedOut TRUE	*)
    (* if the corresponding Signal does not occur within TimeLimit	*)
    (* clock ticks.  Note that this procedure is not recommended for	*)
    (* general use, because "clock ticks" is not a convenient unit of	*)
    (* time for most callers.  For a more useful version, see procedure	*)
    (* TimedWait in module Timer.					*)

    BEGIN
    END TimedWaitT;

(************************************************************************)

PROCEDURE Signal (VAR (*INOUT*) s: Semaphore);

    (* Increments the semaphore value.  Unblocks one waiting task,	*)
    (* if there was one.						*)

    BEGIN
    END Signal;

(************************************************************************)

END DummySemaphores.
