IMPLEMENTATION MODULE Timer;
(*  disable # debug(vid=>off) *)

	(********************************************************)
	(*							*)
	(*		   Clock tick handler			*)
	(*							*)
	(*	Author:		P. Moylan			*)
	(*	Last edited:	17 March 1995			*)
	(*	Description:					*)
	(*	  This module contains the clock interrupt	*)
	(*	  handler.  It checks whether the current task	*)
	(*	  has used its time quota, also keeps track of	*)
	(*	  delayed tasks and timeouts.			*)
	(*							*)
	(*	Status:		OK				*)
	(*							*)
	(********************************************************)

FROM LowLevel IMPORT
    (*<ChainTimerInt*)
    (* proc *)	INCV, MakePointer,
    (*>*)
    (* proc *)	LowByte, HighByte, OutByte;

FROM MiscPMOS IMPORT
    (* proc *)	EnterCriticalSection, LeaveCriticalSection;

FROM Semaphores IMPORT
    (*<ChainTimerInt*)
    (* proc *)	CreateSemaphore, Wait, Signal,
    (*>*)
    (* proc *)	TimedWaitT;

FROM TaskControl IMPORT
    (*<ChainTimerInt*)
    (* proc *)	CreateTask, NotUsingFloatingPoint,
    (*>*)
    (* proc *)	CreateInterruptTask, KillInterruptTask, WaitForInterrupt,
    		Delay, CheckSleepers, TimeSliceCheck;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

(************************************************************************)

CONST MillisecondsPerTick = 20;

    (* It is convenient to choose milliseconds as the system-wide unit	*)
    (* for things like time delays (or, at least, for time delays	*)
    (* controlled by this module), on the grounds that a resolution	*)
    (* finer than one millisecond could make the handling of timer	*)
    (* interrupts a major source of system overhead.			*)
    (* Hardware limitations restrict the value of MillisecondsPerTick	*)
    (* to be in the range [1..109].  We could in principle go as low as	*)
    (* 1.7 microseconds per tick by using real arithmetic, but that	*)
    (* would be fooling ourselves - the timer can go that fast, but the	*)
    (* processor would not keep up.					*)

CONST channel0 = 040H;  TimerControlPort = 043H;

    (* I/O port definitions for the 8254 timer chip.	*)

    (* The following variables are used only in the case where we want	*)
    (* to enable the timer interrupt handler that was active before	*)
    (* this program started running.  There's some overhead in doing	*)
    (* this, but in some environments it's necessary in order to avoid	*)
    (* killing background stuff.					*)

(*<ChainTimerInt*)
(*# save, call(interrupt=>on, reg_param=>(), same_ds=>off, near_call=>off) *)
(*# data(near_ptr => off) *)
TYPE IntProc = PROCEDURE();
     IntProcPointer = POINTER TO IntProc;
VAR OriginalHandler: IntProc;
(*# restore *)

VAR
    ClockCount, CountPerInterrupt: CARDINAL;
    CounterWrap: Semaphore;
(*>*)

(************************************************************************)
(*			"PUT-ME-TO-SLEEP" PROCEDURE			*)
(************************************************************************)

PROCEDURE Sleep (milliseconds: CARDINAL);

    (* Puts the caller to sleep for approximately the given number of	*)
    (* milliseconds.  The time is not guaranteed to be precise, because	*)
    (* (a) after the specified time has expired, the caller is made	*)
    (* ready, but will not run immediately unless it has a higher	*)
    (* priority than the task which is running at that time, and (b) we	*)
    (* do not necessarily run the hardware timer with millisecond	*)
    (* resolution anyway.  High resolution just adds to the system	*)
    (* overhead created by the timer interrupts.  For an application	*)
    (* which genuinely needs high-precision delays, it makes more sense	*)
    (* to have a separate hardware timer dedicated just to that job.	*)

    BEGIN
	Delay (milliseconds DIV MillisecondsPerTick);
    END Sleep;

(************************************************************************)
(*			SEMAPHORE WAIT WITH TIMEOUT			*)
(************************************************************************)

PROCEDURE TimedWait (VAR (*INOUT*) s: Semaphore;  TimeLimit: INTEGER;
					VAR (*OUT*) TimedOut: BOOLEAN);

    (* Like a semaphore Wait, except that it returns with TimedOut TRUE	*)
    (* if the corresponding Signal does not occur within TimeLimit	*)
    (* milliseconds.							*)

    BEGIN
	TimedWaitT (s,
	    (TimeLimit + MillisecondsPerTick DIV 2) DIV MillisecondsPerTick,
	    TimedOut);
    END TimedWait;

(************************************************************************)
(*			    THE INTERRUPT TASK				*)
(************************************************************************)

PROCEDURE ClockInterruptTask;

    (* Activated periodically by the hardware clock.  In between ticks,	*)
    (* we become dormant via a call to WaitForInterrupt.  This task	*)
    (* has two functions:						*)
    (* 1.  Check whether a sleeping task needs to be woken up, and wake	*)
    (*     it up if so.							*)
    (* 2.  Check whether the running task has used its time quota, and	*)
    (*     if so deal with that situation.				*)
    (* To spread the load on the system, we perform the above functions	*)
    (* on alternate clock ticks.					*)

    BEGIN
	LOOP (*FOREVER*)

	    (* On the first interrupt, check the time allotment of	*)
	    (* the interrupted task.  (This does nothing unless		*)
	    (* timeslicing is enabled in module TaskControl.)		*)

	    WaitForInterrupt;
	    TimeSliceCheck;

	    (*<ChainTimerInt*)
	    IF INCV (ClockCount, CountPerInterrupt) THEN
		Signal (CounterWrap);
	    END (*IF*);
	    (*>*)

	    (* On the next interrupt, check for sleeping tasks.	*)

	    WaitForInterrupt;

	    (*<ChainTimerInt*)
	    IF INCV (ClockCount, CountPerInterrupt) THEN
		(* Important note: this Signal must come BEFORE the	*)
		(* call to CheckSleepers, because of complications	*)
		(* related to the way the kernel handles time-outs.	*)
		Signal (CounterWrap);
	    END (*IF*);
	    (*>*)

	    CheckSleepers;

	END (*LOOP*);
    END ClockInterruptTask;

(************************************************************************)
(*		EXECUTING THE ORIGINAL TIMER INTERRUPT ROUTINE		*)
(************************************************************************)

(*<ChainTimerInt*)

PROCEDURE LookAfterOriginalInterruptRoutine;

    (* This is a separate task which calls the original interrupt	*)
    (* routine (i.e. the one that was active before this module took	*)
    (* over) each time the timer count wraps around.			*)

    (* You're probably wondering why we use this roundabout method of	*)
    (* calling the routine.  The answer is that there's a conflict	*)
    (* between the PMOS design conventions and the BIOS approach.	*)
    (* PMOS requires that interrupt tasks be non-interruptible; but	*)
    (* BIOS interrupt handlers often re-enable interrupts.  Thus, we	*)
    (* cannot safely execute a BIOS routine from inside an interrupt	*)
    (* task.								*)

    BEGIN
	NotUsingFloatingPoint ();
	LOOP
	    Wait (CounterWrap);
	    OriginalHandler;
	END (*LOOP*);
    END LookAfterOriginalInterruptRoutine;

(*>*)

(************************************************************************)
(*			CLEANUP ON PROGRAM TERMINATION			*)
(************************************************************************)

PROCEDURE SetCount (count: CARDINAL);

    (* Set up timer 0 to generate a square wave of period proportional	*)
    (* to parameter 'count'.  Note: There are three timers on the timer	*)
    (* chip, but only timer 0 is available for our present purposes;	*)
    (* the other two are hardwired to do sound generation and dynamic	*)
    (* RAM refresh.							*)

    BEGIN
	OutByte (TimerControlPort, 036H);
	OutByte (channel0, LowByte(count));
	OutByte (channel0, HighByte(count));
    END SetCount;

(************************************************************************)

PROCEDURE Cleanup;

    (* Restores the timer 0 frequency to that used by MS-DOS, and	*)
    (* de-installs the interrupt handler.				*)

    VAR savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterCriticalSection();
	SetCount (0);
	KillInterruptTask (8);
	LeaveCriticalSection (savedPSW);
    END Cleanup;

(************************************************************************)
(*			      INITIALISATION				*)
(************************************************************************)

PROCEDURE InitialiseClock;

    (* Performs the appropriate hardware initialisation to (a) set up	*)
    (* the hardware timer to interrupt at the desired rate, and (b) set	*)
    (* up the interrupt vector to point to the interrupt routine.	*)

    VAR savedPSW: CARDINAL;
	(*<ChainTimerInt*) p: IntProcPointer; (*>*)

    BEGIN
	savedPSW := EnterCriticalSection();

	(* The input frequency to the hardware timer is 1.19318 MHz,	*)
	(* and the actual interrupt frequency is this frequency divided	*)
	(* by variable "count".  There is an extra factor of 2 caused	*)
	(* by the fact that our interrupt task effectively divides	*)
	(* the interrupt frequency by 2.  The end result is the		*)
	(* following formula:						*)

	(*<~ChainTimerInt
	SetCount ((59659 * MillisecondsPerTick + 50) DIV 100);
	>*)

	(*<ChainTimerInt*)

	(* In the case where we're chaining the timer interrupt, we	*)
	(* have to initialise some extra variables and create a new	*)
	(* task to handle the job.					*)

	CountPerInterrupt := (59659 * MillisecondsPerTick + 50) DIV 100;
	SetCount (CountPerInterrupt);
	ClockCount := 0;
	CreateSemaphore (CounterWrap, 0);
	p := MakePointer (0, 8*4);
	OriginalHandler := p^;

	(* The priority assigned in the next call may need to be varied	*)
	(* depending on the application.  With a Microsoft mouse driver	*)
	(* it is essential that the priority be >1; otherwise the mouse	*)
	(* driver crashes the entire system.  With some communications	*)
	(* drivers we might want to make the priority even higher.	*)

	CreateTask (LookAfterOriginalInterruptRoutine, 2, "Old timer int");
	(*>*)

	(* Connect the interrupt routine to the interrupt source. *)

	CreateInterruptTask (8, ClockInterruptTask, "Heartbeat tick");

	LeaveCriticalSection (savedPSW);
    END InitialiseClock;

(************************************************************************)

BEGIN
    SetTerminationProcedure (Cleanup);
    InitialiseClock;
END Timer.
