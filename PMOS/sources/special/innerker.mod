IMPLEMENTATION MODULE InnerKernel;

	(****************************************************************)
	(*								*)
	(*	This is a "generic" version of the InnerKernel module.	*)
	(*	These procedures would normally be written in assembly	*)
	(*	language, but for those without access to a suitable	*)
	(*	assembler the present version can be substituted for	*)
	(*	the assembly language version.  We try to stick to	*)
	(*	"standard" Modula-2 as much as possible, so that this	*)
	(*	module can be ported to another machine or compiler	*)
	(*	with the minimum of fuss (see notes on porting below).	*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	16 March 1995				*)
	(*  Status:		Working					*)
	(*	Interrupt handling is not as efficient as I would like,	*)
	(*	 but there seems to be no way to improve this given the	*)
	(*	 limitations of the Wirth-style IOTRANSFER operation.	*)
	(*								*)
	(*	Comment: now that I've come up with a better approach	*)
	(*	 to killing an interrupt task, I might be able to	*)
	(*	 simplify the handling of interrupt tasks by stripping	*)
	(*	 off the "shell" part.  This needs more thought.	*)
	(*								*)
	(****************************************************************)
	(*								*)
	(*  NOTES ON PORTING THIS MODULE TO ANOTHER MACHINE/COMPILER:	*)
	(*								*)
	(*   1.	The SYSTEM procedures here are the Wirth-style		*)
	(*	coroutine support as described in "Programming in	*)
	(*	Modula-2" by Niklaus Wirth.  The coroutine support in	*)
	(*	the new ISO standard is significantly different, and	*)
	(*	requires a different approach.				*)
	(*	A version of this module which follows the ISO standard	*)
	(*	is planned but has not yet been developed.		*)
	(*								*)
	(*   2.	If speed is a major issue, you will ultimately have to	*)
	(*	replace this module with an assembly language version.	*)
	(*	The present version is intended to be an interim	*)
	(*	solution, although for many uses it may well turn out	*)
	(*	to be a satisfactory permanent solution.		*)
	(*								*)
	(*   3.	With some compilers you will need to change the IMPORT	*)
	(*	declarations, since the imported procedures are		*)
	(*	sometimes found in SYSTEM and sometimes in some other	*)
	(*	module.  Check the documentation for your own libraries.*)
	(*								*)
	(*   4.	It is vital that all procedures in this module be	*)
	(*	executed with interrupts disabled.  The caller normally	*)
	(*	guarantees this by calls to EnterKernel, but beware of	*)
	(*	adding things like BIOS calls which would re-enable	*)
	(*	interrupts.						*)
	(*								*)
	(*   5.	A potential trouble spot with some compiler/library	*)
	(*	packages is that the ALLOCATE and DEALLOCATE procedures	*)
	(*	might not be re-entrant.  If this is the case on your	*)
	(*	system, you will have to write your own version of	*)
	(*	module Storage.						*)
	(*								*)
	(*   6.	The present version was developed and tested using	*)
	(*	TopSpeed Modula-2, DOS version 3.10.  It works well	*)
	(*	with that package except for one shortcoming: because	*)
	(*	of the way in which TopSpeed implements module		*)
	(*	priorities, the TopSpeed versions of TRANSFER and	*)
	(*	IOTRANSFER treat the interrupt masks at ports 21H and	*)
	(*	A1H as per-thread resources, rather than as global	*)
	(*	resources as one would normally require for		*)
	(*	multitasking applications.  You therefore cannot	*)
	(*	reliably disable specific devices by setting a bit in	*)
	(*	the appropriate mask register, since a task switch	*)
	(*	could lead to that bit being cleared again.  The only	*)
	(*	place in PMOS where this is known to be a problem is	*)
	(*	in module Printer.  The problem could be solved by	*)
	(*	putting code to save/restore the mask registers around	*)
	(*	every call to TRANSFER and IOTRANSFER, but I have	*)
	(*	preferred to avoid complicating this module with a fix	*)
	(*	to a problem which is specific to just one compiler,	*)
	(*	given that a superior assembly language version of this	*)
	(*	module is already available to TopSpeed users.		*)
	(*								*)
	(****************************************************************)

FROM SYSTEM IMPORT
    (* proc *)	NEWPROCESS, TRANSFER, IOTRANSFER, EI, DI;

FROM LowLevel IMPORT
    (* proc *)	MakePointer;

FROM Storage IMPORT
    (* proc *)	ALLOCATE;

FROM MiscPMOS IMPORT
    (* proc *)	EnterCriticalSection, LeaveCriticalSection;

FROM ConfigurationOptions IMPORT
    (* const *)	StackSize;

FROM TerminationControl IMPORT
    (* proc *)	Crash;

(************************************************************************)

TYPE
    InterruptNumber = CARDINAL;

    (* The tight restriction on the maximum number of concurrent	*)
    (* threads is a limitation of the TopSpeed run-time system.  It	*)
    (* allows up to 32 threads (approx. - I might be out by one or two)	*)
    (* and we have to distribute these between tasks and interrupt	*)
    (* shell threads.  With a different compiler you might be able to	*)
    (* increase the limit.						*)

    ThreadNumber = [0..20];

    GlobalParameterBlock =  RECORD
				maincode, endcode: PROC;
				IntEnable: BOOLEAN;
				selector: TaskSelector;
			    END (*RECORD*);

VAR
    (* GlobalParameters is needed when creating a new task; we need to	*)
    (* transfer information to the task as it starts up, and the only	*)
    (* way to do this is via global variables.  This is safe in the	*)
    (* present instance because we do the creation with interrupts	*)
    (* disabled, and because the first thing we do after creating a	*)
    (* task is an immediate transfer to that task to let it take its	*)
    (* own copy of the necessary information.				*)

    GlobalParameters: GlobalParameterBlock;

    (* BackgroundSelector is a selector for the interrupted task, in	*)
    (* the case where an interrupt task is running.  Task switching to	*)
    (* and from interrupt tasks is done directly by this module, not by	*)
    (* the higher-level part of the kernel.  Interrupt tasks are always	*)
    (* allowed to "run to completion" - that is, to their next call to	*)
    (* IOTransfer - and nested interrupt handlers are not supported.	*)
    (* (To support them, we would have to make BackgroundSelector a	*)
    (* stack rather than a simple variable, and this would impair the	*)
    (* efficiency of interrupt handlers.)  To ensure this, interrupt	*)
    (* tasks must never enable interrupts.				*)

    (* When creating a new task, BackgroundSelector is temporarily used	*)
    (* to identify the creator.  (It would be unwise, for a variety of	*)
    (* reasons, to create a new task from inside an interrupt task, so	*)
    (* we don't bother to support that possibility.)			*)

    BackgroundSelector:	TaskSelector;

    (* ThreadTable is an array of selectors for threads that are	*)
    (* created during module initialisation.  See the comments in the	*)
    (* initialisation code to see why we need this.			*)

    ThreadTable: ARRAY ThreadNumber OF TaskSelector;

    (* NextThreadNumber is the first available entry in ThreadTable.	*)

    NextThreadNumber: [0..MAX(ThreadNumber)+1];

(************************************************************************)
(*			THE INTERRUPT TABLE				*)
(************************************************************************)

CONST
    IntTableLimit = 9;
    NoInterrupt = IntTableLimit;

TYPE IntTableIndex = [0..IntTableLimit];

VAR
    (* TableLoc records the identity of the currently running interrupt	*)
    (* task, if any.  If the currently running task is not an interrupt	*)
    (* task, then TableLoc = NoInterrupt.  If it is an interrupt task,	*)
    (* TableLoc holds the index for the entry in the interrupt table	*)
    (* which belongs to that task.					*)

    TableLoc: IntTableIndex;

    (* Each entry in the interrupt table describes one interrupt task.	*)
    (* The "selector" field is for the actual interrupt task, and the	*)
    (* "ShellSelector" field is for the extra task which we have to	*)
    (* wrap around the real interrupt task in order to catch the first	*)
    (* interrupt which arrives after the actual interrupt task has shut	*)
    (* down.  (This is needed because there is no way to cancel an	*)
    (* IOTRANSFER.)  The "oldhandler" field is for saving the original	*)
    (* contents of the interrupt vector.				*)

    IntTable: ARRAY IntTableIndex OF
		    RECORD
			IntNumber: CARDINAL;
			selector, ShellSelector: TaskSelector;
			oldhandler: PROC;
		    END (*RECORD*);
		
(************************************************************************)

PROCEDURE ClearIntTable;

    (* Clears the interrupt table. *)

    VAR k: IntTableIndex;

    BEGIN
	FOR k := 0 TO IntTableLimit DO
	    WITH IntTable[k] DO
		IntNumber := MAX(CARDINAL);
		selector := NIL;
		oldhandler := NULLPROC;
		ShellSelector := NIL;
	    END (*WITH*);
	END (*FOR*);
    END ClearIntTable;
    		
(************************************************************************)

PROCEDURE LocateIntTableEntry (TS: TaskSelector): IntTableIndex;
    	
    (* Returns the Interrupt table index for the entry which belongs	*)
    (* to the given task selector.					*)
    		
    VAR k: IntTableIndex;
    	
    BEGIN
	k := 0;
	WHILE (k < IntTableLimit) AND (IntTable[k].selector <> TS) DO
	    INC (k);
	END (*WHILE*);
	RETURN k;
    END LocateIntTableEntry;
    		
(************************************************************************)
(*			INTERRUPT HANDLING				*)
(************************************************************************)

PROCEDURE GenericInterruptHandler;

    (* We create one instance of this thread for each interrupt number.	*)
    (* Each time an interrupt occurs, the thread is activated and	*)
    (* switches control to the "real" interrupt task.  That is, this	*)
    (* thread acts as the intermediary between the Modula-2 standard	*)
    (* IOTRANSFER operation and our IOTransfer operation.  The reason	*)
    (* why we have to wrap an extra thread around the user's thread is	*)
    (* that the Wirth-style IOTRANSFER does not provide any way to	*)
    (* reset an interrupt vector when killing an interrupt handler; so	*)
    (* we must provide a "live" interrupt-handling thread, whose stack	*)
    (* is guaranteed to remain allocated up to and including the time	*)
    (* of program termination, to handle the first interrupt to arrive	*)
    (* after the "real" interrupt task has been killed off.		*)

    VAR k: IntTableIndex;

    BEGIN
	(* Startup code: the global variable TableLoc tells us which	*)
	(* entry in the interrupt table belongs to us.			*)

	k := TableLoc;

	(* The first time this thread is invoked is during module	*)
	(* initialisation.  At this stage all we have to do is disable	*)
	(* processor interrupts (for this thread) and then switch back	*)
	(* to the initialisation code.					*)

	DI();
	TRANSFER (IntTable[k].ShellSelector, BackgroundSelector);

	LOOP
	    (* Go to the "real" interrupt task. *)

	    WITH IntTable[k] DO
		TRANSFER (ShellSelector, selector);
	    END (*WITH*);

	    (* We get back here after the interrupt task has executed	*)
	    (* its IOTransfer.  Go back to the interrupted task. 	*)

	    TableLoc := NoInterrupt;
	    WITH IntTable[k] DO
		IOTRANSFER (ShellSelector, BackgroundSelector, IntNumber);
	    END (*WITH*);

	    (* We wake up here after the interrupt. *)

	    TableLoc := k;

	END (*LOOP*);

    END GenericInterruptHandler;

(************************************************************************)
(*		    THE "MAIN PROGRAM" FOR ALL TASKS			*)
(************************************************************************)

PROCEDURE RunTask;

    (* We run one invocation of this for each task. *)

    VAR MainProc, ExitProc: PROC;

    BEGIN
	(* The first time this thread is invoked is during module	*)
	(* initialisation.  At this stage all we have to do is disable	*)
	(* processor interrupts (for this thread) and then switch back	*)
	(* to the initialisation code.					*)

	DI();
	TRANSFER (ThreadTable[NextThreadNumber], BackgroundSelector);

	(* The next time we get control is where a user task is		*)
	(* assigned to this thread.  Copy the global parameters		*)
	(* immediately, and then go back to the creator of the task.	*)

	WITH GlobalParameters DO
	    MainProc := maincode;
	    ExitProc := endcode;
	    IF IntEnable THEN EI() ELSE DI() END(*IF*);
	    TRANSFER (selector, BackgroundSelector);
	END (*WITH*);

	(* When we get the processor back, execute the body of the	*)
	(* task code.							*)

	MainProc();

	(* Task done, execute its exit code.	*)

	ExitProc();

    END RunTask;

(************************************************************************)
(*		    THE EXTERNALLY CALLABLE PROCEDURES			*)
(************************************************************************)

PROCEDURE EnterKernel (): CARDINAL;

    (* Saves the processor flags word, including the current "interrupt	*)
    (* enable" status, and returns with interrupts disabled.		*)
    (* NOTE: this procedure and the following one should be used as a	*)
    (* matched pair.							*)

    BEGIN
	RETURN EnterCriticalSection();
    END EnterKernel;

(************************************************************************)

PROCEDURE LeaveKernel (PSW: CARDINAL);

    (* Restores the processor flags word, including the "interrupt	*)
    (* enable" status.  NOTE: this procedure and the one above should	*)
    (* be used as a matched pair.					*)

    BEGIN
	LeaveCriticalSection (PSW);
    END LeaveKernel;

(************************************************************************)

PROCEDURE NPXsave (selector: FloatSaveSelector);

    (* This procedure is a dummy in the present version; we rely	*)
    (* on the library TRANSFER and IOTRANSFER to save the floating	*)
    (* point state.							*)

    BEGIN
    END NPXsave;

(************************************************************************)

PROCEDURE NPXrestore (selector: FloatSaveSelector);

    (* This procedure is a dummy in the present version; we rely	*)
    (* on the library TRANSFER and IOTRANSFER to save the floating	*)
    (* point state.							*)

    BEGIN
    END NPXrestore;

(************************************************************************)

PROCEDURE TaskInit (StackBase: ADDRESS;  StackSize: CARDINAL;
			EnableInterrupts: BOOLEAN;
			TaskExit, StartAddress: PROC): TaskSelector;

    (* Initialises the stack for a new task.  Parameter StackBase	*)
    (* points to a block of memory which can be used to hold the stack	*)
    (* (note that this is a pointer to the start of the memory block,	*)
    (* not to the bottom of the stack); and StackSize is the size of	*)
    (* this block.  The next parameter specifies whether processor	*)
    (* interrupts should be enabled when the task is started.		*)
    (* StartAddress and TaskExit are the start address of the task code	*)
    (* and the start address of the code to execute when the task	*)
    (* terminates.  The value returned is a selector for the new task.	*)

    (* Note: in this version the StackBase and StackSize parameters are	*)
    (* ignored, since we've had to pre-create all threads.  That leads	*)
    (* to a waste of memory; but that seems to be the price of		*)
    (* portability, unless someone can come up with a better way to	*)
    (* create threads (using the more-or-less portable Wirth model)	*)
    (* without leaving the kernel critical sections unprotected.	*)

    (* Idea: maybe I should go back to my earlier approach, but add	*)
    (* interrupt mask protection to the tricky sections.		*)

    VAR result: TaskSelector;

    BEGIN
	IF NextThreadNumber > MAX(ThreadNumber) THEN
	    Crash ("Too many tasks created.")
	END (*IF*);
	WITH GlobalParameters DO
	    maincode := StartAddress;
	    endcode := TaskExit;
	    IntEnable := EnableInterrupts;

	    (* Run the new task just far enough to ensure that the	*)
	    (* GlobalParameters are copied over.			*)

	    selector := ThreadTable[NextThreadNumber];
	    INC (NextThreadNumber);
	    TRANSFER (BackgroundSelector, selector);
	    RETURN selector;

	END (*WITH*);

    END TaskInit;

(************************************************************************)

PROCEDURE InitMainTask (): TaskSelector;

    (* This procedure is a dummy in the present version; we rely	*)
    (* on the library TRANSFER and IOTRANSFER to save the floating	*)
    (* point state.							*)

    BEGIN
	RETURN NIL;
    END InitMainTask;

(************************************************************************)

PROCEDURE MakeFloatSaveSelector (selector: TaskSelector): FloatSaveSelector;

    (* This procedure is a dummy in the present version; we rely	*)
    (* on the library TRANSFER and IOTRANSFER to save the floating	*)
    (* point state.							*)

    BEGIN
	RETURN 0;
    END MakeFloatSaveSelector;

(************************************************************************)

PROCEDURE Transfer (VAR (*OUT*) source: TaskSelector;
					destination: TaskSelector);

    (* Performs a task switch to the destination task, at the same time	*)
    (* saving a selector for the outgoing task in variable "source".	*)
    (* This allows a subsequent call to Transfer to resume the		*)
    (* original task.  By the time this procedure has returned to the	*)
    (* caller, then, we are again executing the calling task.		*)

    (* Special case: if this procedure is called by an interrupt task,	*)
    (* the call is interpreted as a requiring a task switch from the	*)
    (* interrupted task - i.e. the source parameter must specify the	*)
    (* interrupted task - to the destination task.  In this case the	*)
    (* actual switch to the destination task does not happen until the	*)
    (* interrupt task makes its next call to IOTransfer.  The reason	*)
    (* for this interpretation is that task switching to and from	*)
    (* interrupt tasks is managed internally by this module; the	*)
    (* occurrence of an interrupt is not something that can be		*)
    (* controlled by the caller.					*)

    BEGIN
	IF TableLoc = NoInterrupt THEN
	    TRANSFER (source, destination);
	ELSE
	    source := BackgroundSelector;
	    BackgroundSelector := destination;
	END (*IF*);
    END Transfer;

(************************************************************************)

PROCEDURE IOTransfer;

    (* May be called only from an interrupt task.  Performs a task	*)
    (* switch from the current interrupt task to the task which it	*)
    (* interrupted.  Unlike Transfer, no parameters are required	*)
    (* because (a) the selector for the destination task is already	*)
    (* known to this module, having been saved at the time of the	*)
    (* interrupt; and (b) selectors for interrupt tasks are maintained	*)
    (* directly by this module rather than by the caller.		*)

    BEGIN
	WITH IntTable[TableLoc] DO
	    TRANSFER (selector, ShellSelector);
	END (*WITH*);

	(* We return from the TRANSFER when the interrupt occurs. *)

    END IOTransfer;

(************************************************************************)

PROCEDURE StartInterruptTask (TS: TaskSelector; InterruptNumber: CARDINAL);

    (* Starts an interrupt task by running its initialisation section	*)
    (* - i.e. everything up to the first IOTransfer - and arranging	*)
    (* that from then on it will be activated by the given interrupt.	*)

    VAR StackBase: ADDRESS;
	p: POINTER TO PROC;

    BEGIN
	TableLoc := LocateIntTableEntry (NIL);
	IF TableLoc = NoInterrupt THEN
	    Crash ("Too many interrupt tasks.")
	END (*IF*);
	WITH IntTable[TableLoc] DO
	    IntNumber := InterruptNumber;
	    selector := TS;
	    p := MakePointer (0, 4*IntNumber);
	    oldhandler := p^;
	    TRANSFER (BackgroundSelector, ShellSelector);
	END (*WITH*);

	(* When we get back here, the "shell" task has completed its	*)
	(* initialisation, including priming the interrupt vector; and	*)
	(* the interrupt task has run to its first call to IOTransfer.	*)

    END StartInterruptTask;

(************************************************************************)

PROCEDURE DisconnectFromInterrupt (TS: TaskSelector);

    (* Restores the interrupt vector to which TS was connected to its	*)
    (* state before TS was established as an interrupt task.  (N.B. The	*)
    (* result could be chaotic if there was no previous call to		*)
    (* ConnectToInterrupt.)						*)

    VAR p: POINTER TO PROC;  k: IntTableIndex;

    BEGIN
	k := LocateIntTableEntry(TS);
	WITH IntTable[k] DO
	    p := MakePointer (0, 4*IntNumber);
	    p^ := oldhandler;
	END (*WITH*);
    END DisconnectFromInterrupt;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

PROCEDURE CreateThreads;

    (* A shortcoming of the NEWPROCESS procedure is that it creates a	*)
    (* thread with interrupts initially enabled.  If we created tasks	*)
    (* at the obvious time - i.e. inside procedure TaskInit - this	*)
    (* would create a security hole in the kernel, where processor	*)
    (* interrupts would be momentarily enabled at a time where we're	*)
    (* executing kernel code that is supposed to be indivisible.	*)
    (* (Believe me, I've tried the more obvious approach, and it	*)
    (* crashes the system in a disastrous way.)  Our solution is to	*)
    (* create all threads and get past the "interrupts enabled" glitch	*)
    (* _before_ the multitasking kernel is started.  This admittedly is	*)
    (* a complex solution with all the air of being a kludge, but I	*)
    (* couldn't think of a better solution (apart from rewriting	*)
    (* NEWPROCESS, which is effectively what I've done in the assembly	*)
    (* language version of this module).				*)

    CONST GIHStackSize = 2048;

    VAR StackBase: ADDRESS;

    BEGIN
	FOR NextThreadNumber := 0 TO MAX(ThreadNumber) DO
	    ALLOCATE (StackBase, StackSize);
	    NEWPROCESS (RunTask, StackBase, StackSize,
					ThreadTable[NextThreadNumber]);

	    (* Run the new thread just far enough to ensure that it	*)
	    (* disables its interrupts.					*)

	    TRANSFER (BackgroundSelector, ThreadTable[NextThreadNumber]);
	END (*FOR*);
	NextThreadNumber := 0;

	(* Now do the same for the interrupt handler shells. *)

	FOR TableLoc := 0 TO MAX(IntTableIndex)-1 DO
	    ALLOCATE (StackBase, GIHStackSize);
	    NEWPROCESS (GenericInterruptHandler, StackBase,
				StackSize, IntTable[TableLoc].ShellSelector);
	    TRANSFER (BackgroundSelector, IntTable[TableLoc].ShellSelector);
	END (*FOR*);
	TableLoc := NoInterrupt;

    END CreateThreads;

(************************************************************************)

BEGIN
    ClearIntTable;
    CreateThreads;
END InnerKernel.
