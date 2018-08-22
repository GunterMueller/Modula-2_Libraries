IMPLEMENTATION MODULE Storage1;

	(********************************************************)
	(*							*)
	(*  Storage allocation with critical section protection	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	15 March 1995			*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

IMPORT Storage;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateLock, Obtain, Release;

(************************************************************************)

VAR L: Lock;

(************************************************************************)

PROCEDURE ALLOCATE (VAR (*OUT*) p: ADDRESS;  size: CARDINAL);

    (* Allocates a block of size bytes. *)

    BEGIN
	Obtain (L);
	Storage.ALLOCATE (p, size);
	Release (L);
    END ALLOCATE;

(************************************************************************)

PROCEDURE DEALLOCATE (VAR (*INOUT*) p: ADDRESS;  size: CARDINAL);

    (* Deallocates memory that was allocated by ALLOCATE. *)

    VAR PSW: CARDINAL;

    BEGIN
	Obtain (L);
	Storage.DEALLOCATE (p, size);
	Release (L);
    END DEALLOCATE;

(************************************************************************)

BEGIN
    CreateLock (L);
END Storage1.
