IMPLEMENTATION MODULE Keyboard;

	(****************************************************************)
	(*								*)
	(*	Keyboard input module for use where we want to		*)
	(*	bypass the normal PMOS keyboard driver - e.g. for	*)
	(*	running the simple multitasking demonstration, or	*)
	(*	for cases where we need to avoid conflicts with		*)
	(*	the debugger.						*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	26 July 1994				*)
	(*  Status:		Working					*)
	(*								*)
	(****************************************************************)

IMPORT IO;

(************************************************************************)

PROCEDURE KeyPressed(): BOOLEAN;

    (* Reports whether a character is available, but does not fetch it.	*)

    BEGIN
	RETURN IO.KeyPressed();
    END KeyPressed;

(************************************************************************)

PROCEDURE InKey (): CHAR;

    (* Reads a single character code from the keyboard.	*)

    BEGIN
	RETURN IO.RdKey();
    END InKey;

(************************************************************************)

PROCEDURE PutBack (ch: CHAR);

    (* A dummy procedure; feature not available in this version.	*)

    BEGIN
    END PutBack;

(************************************************************************)

PROCEDURE SetLocks (code: CARDINAL);

    (* A dummy procedure; feature not available in this version.	*)

    BEGIN
    END SetLocks;

(************************************************************************)

PROCEDURE LockStatus (): CARDINAL;

    (* A dummy procedure; feature not available in this version.	*)

    BEGIN
	RETURN 0;
    END LockStatus;

(************************************************************************)

PROCEDURE HotKey (FunctionKey: BOOLEAN;  code: CHAR;  S: Semaphore);

    (* A dummy procedure; feature not available in this version.	*)

    BEGIN
    END HotKey;

(************************************************************************)

PROCEDURE StuffKeyboardBuffer (ch: CHAR);

    (* A dummy procedure; feature not available in this version.	*)

    BEGIN
    END StuffKeyboardBuffer;

(************************************************************************)

END Keyboard.
