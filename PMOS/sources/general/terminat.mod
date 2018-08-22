IMPLEMENTATION MODULE TerminationControl;

	(********************************************************)
	(*							*)
	(*	Support for program termination procedures.	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM Storage IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

(*<TopSpeed*)
FROM Lib IMPORT
    (* proc *)	Terminate;
(*>*)

(*<FST
FROM System IMPORT
    (* proc *)	TermProcedure, Terminate;
>*)

FROM InnerKernel IMPORT
    (* proc *)	PrepareForShutdown;

FROM MiscPMOS IMPORT
    (* proc *)	CopyString;

(************************************************************************)

TYPE
    (* We have one ListElement for each termination procedure.	*)

    ListPointer = POINTER TO ListElement;
    ListElement =   RECORD
			Procedure: PROC;
			next: ListPointer;
		    END (*RECORD*);

VAR
    (* ListHead and ListTail point to the first and last elements,	*)
    (* respectively, of the list of termination procedures.		*)

    ListHead, ListTail: ListPointer;

    (* The termination handler which was present before we installed	*)
    (* our own.								*)

    OriginalHandler: PROC;

    (* Text message supplied by caller of procedure Crash.	*)

    CrashMessage: ARRAY [0..79] OF CHAR;

    (* MessagePresent = TRUE iff procedure Crash has been called.	*)

    MessagePresent: BOOLEAN;

    (* Flag to say whether termination processing has commenced.	*)

    TerminationInProgress: BOOLEAN;

(************************************************************************)
(*		UPDATING THE LIST OF TERMINATION PROCEDURES		*)
(************************************************************************)

PROCEDURE SetTerminationProcedure (TP: PROC);

    (* Adds TP to the list of procedures which will be called just	*)
    (* before program termination.  The list is ordered such that the	*)
    (* last procedure added will be the first one called.  Exception:	*)
    (* if termination is already in progress when this procedure is	*)
    (* called, then TP will not be called until all of the existing	*)
    (* termination procedures have been called.  This rule permits	*)
    (* multi-pass termination processing, where necessary, by letting	*)
    (* termination procedures themselves install more termination	*)
    (* procedures.							*)

    VAR OldHead, NewTail: ListPointer;

    BEGIN
	IF TerminationInProgress THEN

	    (* Add the new list element to the tail of the list. *)

	    NEW (NewTail);
	    WITH NewTail^ DO
		Procedure := TP;
		next := NIL;
	    END (*WITH*);
	    IF ListTail = NIL THEN
		ListHead := NewTail;
	    ELSE
		ListTail^.next := NewTail;
	    END (*IF*);
	    ListTail := NewTail;

	ELSE

	    (* Termination not already in progress.  Add the new item	*)
	    (* to the head of the list, to give the desired LIFO order. *)

	    OldHead := ListHead;
	    NEW (ListHead);
	    WITH ListHead^ DO
		Procedure := TP;
		next := OldHead;
	    END (*WITH*);
	    IF OldHead = NIL THEN
		ListTail := ListHead;
	    END (*IF*);

	END (*IF*);

    END SetTerminationProcedure;

(************************************************************************)
(*			THE ACTUAL TERMINATION HANDLER			*)
(************************************************************************)

PROCEDURE TerminationHandler;

    (* This is the procedure which is called on program termination.	*)
    (* It then calls all of the procedures which the user wants called.	*)

    VAR OldHead: ListPointer;  UserProc: PROC;

    BEGIN
	TerminationInProgress := TRUE;

	(* Work through the list of termination procedures.  Note that	*)
	(* it's important to remove the termination handler from the	*)
	(* list before calling it, to avoid recursive calls in case the	*)
	(* handler itself triggers another termination.			*)

	WHILE ListHead <> NIL DO
	    UserProc := ListHead^.Procedure;
	    OldHead := ListHead;  ListHead := ListHead^.next;
	    DISPOSE (OldHead);
	    UserProc;
	END (*WHILE*);

	(* Finally, call the system-level termination handler.	*)
	
	PrepareForShutdown;
	(*<TopSpeed*) OriginalHandler(); (*>*)
	(*<FST Terminate(0); >*)
	
    END TerminationHandler;

(************************************************************************)
(*			RAISING AN ERROR CONDITION			*)
(************************************************************************)

PROCEDURE Crash (message: ARRAY OF CHAR);

    (* Terminates the program with an error report.	*)

    BEGIN
	CopyString (message, CrashMessage);
	MessagePresent := TRUE;
	TerminationHandler;
    END Crash;

(************************************************************************)
(*			USER-CALLABLE ERROR REPORTING			*)
(************************************************************************)

PROCEDURE TerminationMessage (VAR (*OUT*) message: ARRAY OF CHAR): BOOLEAN;

    (* Returns the message supplied by the caller of the Crash		*)
    (* procedure.  The function result is TRUE if such a message	*)
    (* exists, and FALSE if Crash was never called.			*)

    BEGIN
	IF MessagePresent THEN
	    CopyString (CrashMessage, message);
	    RETURN TRUE;
	ELSE
	    RETURN FALSE;
	END (*IF*);
    END TerminationMessage;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

BEGIN
    ListHead := NIL;  ListTail := NIL;
    TerminationInProgress := FALSE;  MessagePresent := FALSE;
    (*<TopSpeed*) Terminate (TerminationHandler, OriginalHandler); (*>*)
    (*<FST TermProcedure (TerminationHandler); >*)
END TerminationControl.
