IMPLEMENTATION MODULE TaskControl;

	(****************************************************************)
	(*								*)
	(*	    The dispatcher of the operating system, and		*)
	(*	     related procedures and data structures.		*)
	(*								*)
	(*	    This version supports priority inheritance.		*)
	(*								*)
	(*	This is the portable part of the PMOS task control	*)
	(*	software.  The non-portable part is in module		*)
	(*	InnerKernel.						*)
	(*								*)
	(*	Programmer:	P. Moylan				*)
	(*	Last edited:	16 March 1995				*)
	(*	Status:		Working					*)
	(*								*)
	(*  Description:						*)
	(*	See individual procedures.  This is the module which	*)
	(*	does all task switching, also task creation.		*)
	(*	The kernel of the operating system consists of this	*)
	(*	module, module InnerKernel, and module Semaphores.  The	*)
	(*	module called Timer is also intimately linked to the	*)
	(*	kernel; it is technically not part of the kernel, but	*)
	(*	does most of its work by calls to kernel procedures.	*)
	(*								*)
	(*	The present kernel implementation allows a task to	*)
	(*	be in one of the following (mutually exclusive) states:	*)
	(*	    1. Active						*)
	(*		- running					*)
	(*		- ready						*)
	(*		- blocked waiting for a Lock			*)
	(*	    2. Inactive						*)
	(*		- sleeping					*)
	(*		- waiting for a semaphore Signal		*)
	(*		- waiting for a Signal with a timeout		*)
	(*		- dead						*)
	(*	In most of these cases, the state is defined by the	*)
	(*	kernel list on which the task is placed.  (All active	*)
	(*	tasks are on the same "Ready" structure, but we have	*)
	(*	enough other information to distinguish among the three	*)
	(*	active states.)  The exception is where a task is	*)
	(*	performing a timed semaphore wait, since there the	*)
	(*	task is simultaneously on two lists.  When such a task	*)
	(*	is woken up, it is run immediately in order to make	*)
	(*	indivisible the operation of removing the task from	*)
	(*	the two lists.  If this causes a higher-priority task	*)
	(*	to be preempted, a second task switch reinstates this	*)
	(*	higher-priority task before we leave the kernel.	*)
	(*	(Procedure RecheckPriorities looks after this detail.)	*)
	(*								*)
	(*	The priority of a task is normally a constant; but the	*)
	(*	priority can be increased in the case of an active	*)
	(*	task holding a Lock, where a higher-priority task is	*)
	(*	waiting to obtain that Lock.  In such cases the task's	*)
	(*	dynamic priority is equal to the priority of the	*)
	(*	highest-priority task which it is blocking.  As it	*)
	(*	turns out, there is no need to record the dynamic	*)
	(*	priorities for each active task; it is sufficient to	*)
	(*	maintain a global variable CurrentPriority, which holds	*)
	(*	the dynamic priority of the currently running task.	*)
	(*								*)
	(*	Interrupt tasks follow their own set of rules and do	*)
	(*	not fit into the above classification.  An interrupt	*)
	(*	task should never obtain a Lock,  perform a semaphore	*)
	(*	Wait (although a Signal is permitted), or Sleep.  The	*)
	(*	only possible states for an interrupt task are running	*)
	(*	(responding to an interrupt) or idle (waiting for an	*)
	(*	interrupt).  When an interrupt task is running, the	*)
	(*	variables CurrentTask and CurrentPriority continue to	*)
	(*	refer to the interrupted task.				*)
	(*								*)
	(****************************************************************)

FROM SYSTEM IMPORT
    (* proc *)	ADR;

FROM ConfigurationOptions IMPORT
    (* const*)	TimeSlicingEnabled, MaxTaskNumber, StackSize;

FROM InnerKernel IMPORT
    (* type *)	TaskSelector, FloatSaveSelector,
    (* proc *)	EnterKernel, LeaveKernel, NPXsave, NPXrestore,
		InitMainTask, MakeFloatSaveSelector,
		TaskInit, Transfer, IOTransfer, StartInterruptTask,
		DisconnectFromInterrupt;

	(* Procedure Transfer is the low-level primitive for the	*)
	(* stack switch operation, which is the most crucial part of	*)
	(* a task switch.  Procedures NPXsave and NPXrestore look after	*)
	(* saving and restoring the state of the NPX (Numeric Processor	*)
	(* Extension, i.e. the floating point hardware) coprocessor.	*)
	(* TaskInit introduces a new task into the system.		*)

FROM Storage IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure, Crash;

(************************************************************************)

TYPE
    TaskNumber = [0..MaxTaskNumber];
    InterruptType = CARDINAL;
    Lock = POINTER TO LockRecord;
    TaskPointer = POINTER TO TaskDescriptor;
    Task = TaskPointer;

    (* The kernel maintains a number of queues, which for the most part	*)
    (* are FIFO queues.  (Very occasionally, we need to remove a task	*)
    (* from the middle of a queue.)  Mainly because of the limitations	*)
    (* of the Modula-2 opaque export, we choose to represent a queue by	*)
    (* a single "head" pointer, and to implement a doubly linked list	*)
    (* structure with the aid of "previous" and "next" pointers in the	*)
    (* task descriptors.						*)

    TaskQueue = TaskPointer;

    (* Each task in the system has a task descriptor.			*)
    (* The fields in a task descriptor are:				*)
    (*	previous, next:	pointers to preceding and following task	*)
    (*		descriptors on the queue (i.e. whatever system queue	*)
    (*		this task descriptor happens to be on).			*)
    (*	name:	a character string describing the task.  Used only for	*)
    (*		diagnostic purposes.					*)
    (*	priority: task priority.  This is the task's base priority.	*)
    (*		Because of priority inheritance, the task may at times	*)
    (*		be running at a higher priority, but we don't need to	*)
    (*		record this in the task descriptor; it turns out to be	*)
    (*		sufficient to keep a single global variable (see	*)
    (*		CurrentPriority below) to hold the dynamic priority of	*)
    (*		the currently running task.				*)
    (*	WaitingFor: the Lock, if any, on which this task is blocked.	*)
    (*  FirstLock: the first in the list of Locks held by this task.	*)
    (*	selector: a selector for the task state of this task.  In the	*)
    (*		current implementation, most of the state information	*)
    (*		(register contents, etc.) is saved on the task's stack	*)
    (*		when a task switch occurs, and the selector field is	*)
    (*		used to save the stack pointer.				*)
    (*  FPselector: used for floating point save/restore.		*)
    (*	StackBase: the low address of the memory segment set aside for	*)
    (*		the stack. This is needed only when we kill a task, and	*)
    (*		wish to reclaim the memory used by its stack.		*)
    (*	TimeLeft: for a running task, this records how much of its	*)
    (*		current time slice the task still has left.  For a	*)
    (*		sleeping task, it shows how long to keep the task	*)
    (*		asleep (but not in terms of absolute time - see		*)
    (*		procedure Delay).					*)
    (*	InterruptHandler: true iff this task is an interrupt task.	*)
    (*	FloatingPointUser:  true iff this task is a user (or potential	*)
    (*		user) of the numeric coprocessor.			*)
    (*	dead:	true iff this is an unused descriptor or a descriptor	*)
    (*		for a task which has terminated.			*)
    (*	sleeping: true iff this is a task which is on the sleep queue.	*)
    (*	InterruptNo: interrupt number, used only for interrupt tasks.	*)
    (*	nextsleeper:	pointer to next task descriptor on the sleep	*)
    (*		queue.  Unused except when this task is a sleeping task	*)
    (*		or one which is timing out.				*)

    TaskDescriptor = RECORD
			previous, next	: Task;
			name		: NameString;
			priority	: PriorityLevel;
			WaitingFor,
			FirstLock	: Lock;
			selector	: TaskSelector;
			FPselector	: FloatSaveSelector;
			StackBase	: ADDRESS;
			TimeLeft	: INTEGER;
			InterruptHandler,
			FloatingPointUser,
			dead, sleeping	: BOOLEAN;
			InterruptNo	: InterruptType;
			nextsleeper	: Task;
		     END;

    (* A LockRecord holds all the data for a lock.  Note that we do	*)
    (* not maintain the traditional blocked list; in the more standard	*)
    (* implementations of a binary semaphore this would be needed to	*)
    (* know which task to unblock when the lock is released, but the	*)
    (* present algorithm leaves everything unblocked, relying on a	*)
    (* re-check at the time of a task switch.				*)
    (* The "next" and "previous" fields link together all locks held	*)
    (* by a single task.  This list would not be needed if we assumed	*)
    (* perfectly disciplined users of the locks; but we need to be	*)
    (* able to deal with things like tasks which exit while still	*)
    (* holding locks.							*)

    LockRecord = RECORD
		     Locked: BOOLEAN;
		     Holder: Task;
		     next, previous: Lock;
		 END (*RECORD*);

(************************************************************************)

VAR
    (* TaskTable is an array of task descriptors.  After procedure	*)
    (* InitialiseTaskControl returns, there will be two tasks in the	*)
    (* system: the initialisation task, and the null task.  The		*)
    (* remaining task descriptors will be on UnusedDescriptors,		*)
    (* available for the creation of new tasks.  A task descriptor is	*)
    (* allocated by procedure CreateTask, and is returned to		*)
    (* UnusedDescriptors by procedure TaskExit.				*)

    TaskTable: ARRAY TaskNumber OF TaskDescriptor;

    (* CurrentTask is the current non-interrupt task.  (Note: when an	*)
    (* interrupt task is running, CurrentTask continues to refer to the	*)
    (* interrupted task.)  LastTask is the task which was running just	*)
    (* before CurrentTask got control.  Most of the time LastTask is	*)
    (* irrelevant to us; but it is needed when dealing with some	*)
    (* complications which arise when waking a sleeping task.		*)

    LastTask, CurrentTask: Task;

    (* CurrentPriority is the _dynamic_ priority of the currently	*)
    (* running task.  If no other task is blocked on a Lock held by the	*)
    (* current task, then CurrentPriority is the current task's base	*)
    (* priority.  Otherwise, CurrentPriority is equal to the maximum	*)
    (* base priority of all tasks which are blocked (directly or	*)
    (* indirectly) by the current task as a result of their waiting to	*)
    (* obtain a Lock.							*)

    CurrentPriority: PriorityLevel;

    (* Because of the way we do priority inheritance, our treatment	*)
    (* of the "ready list" is somewhat unconventional.  This list holds	*)
    (* not only tasks which are ready, but also				*)
    (*		- the currently running task, and			*)
    (*		- all tasks waiting to obtain a Lock.			*)
    (* (Do not confuse obtaining a Lock with waiting on a general	*)
    (* semaphore.  Tasks doing a semaphore Wait are not on the ready	*)
    (* list.)  When we choose a task on the ready list and discover	*)
    (* that it is blocked, we switch to its blocker instead, with the	*)
    (* blocker inheriting the priority of the blocked task.		*)
		
    (* Note: priority level 0 is special - there is normally only one	*)
    (* task, namely the null task, at priority 0.			*)

    ReadyList: ARRAY PriorityLevel OF TaskQueue;

    (* InterruptTaskActive = TRUE iff an interrupt task is running.	*)
    	
    InterruptTaskActive: BOOLEAN;

    (* UnusedDescriptors is a queue of all task descriptors which are	*)
    (* unused, i.e. which are available for use as descriptors of newly	*)
    (* created tasks.							*)

    UnusedDescriptors: TaskQueue;

    (* The timequota array holds the time-slice quota, in units of	*)
    (* clock ticks, for each priority level.  Quotas are based on a	*)
    (* task's base priority, not on its inherited priority.		*)

    timequota: ARRAY PriorityLevel OF CARDINAL;

(************************************************************************)
(*			  KERNEL QUEUE OPERATIONS			*)
(************************************************************************)

PROCEDURE CreateQueue (VAR (*OUT*) KQ: TaskQueue);

    (* Creates an initially empty kernel queue.	*)

    BEGIN
	KQ := NIL;
    END CreateQueue;

(*************************************************************************)

PROCEDURE AddToQueue (VAR (*INOUT*) Q: TaskQueue;  T: Task);

    (* Adds task T to the tail of queue Q.	*)

    VAR tail: TaskPointer;

    BEGIN
	IF Q = NIL THEN
	    Q := T;
	    WITH T^ DO
		previous := T;  next := T;
	    END (*WITH*);
	ELSE
	    WITH Q^ DO
		tail := previous;  previous := T;
	    END (*WITH*);
	    tail^.next := T;
	    WITH T^ DO
		previous := tail;  next := Q;
	    END (*WITH*);
	END (*IF*);
    END AddToQueue;

(************************************************************************)

PROCEDURE TakeFromQueue (VAR (*INOUT*) Q: TaskQueue): Task;

    (* Removes the first entry from queue Q and returns it.		*)
    (* Assumption: the caller has already verified that the queue is	*)
    (* nonempty.							*)

    VAR result: Task;  tail: TaskPointer;

    BEGIN
	result := Q;  tail := Q^.previous;
	IF tail = Q THEN
	    Q := NIL;
	ELSE
	    Q := Q^.next;  Q^.previous := tail;  tail^.next := Q;
	END (*IF*);
	WITH result^ DO
	    previous := NIL;  next := NIL;
	END (*WITH*);
	RETURN result;
    END TakeFromQueue;

(************************************************************************)

PROCEDURE LeaveQueue (VAR (*INOUT*) KQ: TaskQueue);

    (* Removes the current task from queue KQ.  The caller is		*)
    (* responsible for knowing that the current task really is on KQ.	*)

    BEGIN
	WITH CurrentTask^ DO
	    IF next = CurrentTask THEN
		KQ := NIL;
	    ELSE
		previous^.next := next;  next^.previous := previous;
		IF KQ = CurrentTask THEN
		    KQ := next;
		END (*IF*);
	    END (*IF*);
	    previous := NIL;  next := NIL;
	END (*WITH*);
    END LeaveQueue;

(************************************************************************)
(*			      THE NULL TASK				*)
(************************************************************************)

PROCEDURE NullTask;

    (* The only function of the null task is to soak up processor time	*)
    (* when no other task is able to run.				*)

    BEGIN
	NotUsingFloatingPoint;
	LOOP (* Do nothing *) END (*LOOP*);
    END NullTask;

(************************************************************************)
(*			    	DISPATCHER				*)
(************************************************************************)

PROCEDURE TaskSwitch (T: Task);

    (* Performs a task switch from the current task to task T.  It is	*)
    (* assumed that the current task has already been moved to the	*)
    (* appropriate kernel queue.					*)
    (* This is, in principle, a system-dependent procedure, whose	*)
    (* detailed implementation depends on the processor architecture	*)
    (* and on compiler conventions.  To keep this module portable, a	*)
    (* separate procedure InnerKernel.Transfer is called to handle the	*)
    (* system-dependent part.						*)

    (* Special case: if an interrupt task is running, the actual task	*)
    (* switch is deferred until the interrupt task has finished		*)
    (* processing the current interrupt and has reached its next call	*)
    (* to WaitForInterrupt.  Procedure Transfer also looks after this	*)
    (* detail.								*)

    BEGIN
	IF T^.dead THEN
	    Crash ("Task switch to nonexistent task");
	END (*IF*);
	LastTask := CurrentTask;

	IF T <> LastTask THEN

	    CurrentTask := T;

	    (* Save and restore the floating point state if necessary.	*)
	    (* Notice that if an interrupt task is running (i.e. if	*)
	    (* InterruptTaskActive = TRUE), the saved state is that of	*)
	    (* the interrupted task, not the interrupt task.  This is	*)
	    (* the appropriate action since there is a system-wide	*)
	    (* assumption that interrupt handlers may not perform	*)
	    (* floating point operations.				*)

	    IF LastTask^.FloatingPointUser THEN
		NPXsave (LastTask^.FPselector);
	    END (*IF*);
	    IF T^.FloatingPointUser THEN
		NPXrestore (T^.FPselector);
	    END (*IF*);

	    Transfer (LastTask^.selector, T^.selector);

	    (* By the time we get back to here, one or more other	*)
	    (* tasks have been running; and we resume at this point	*)
	    (* by virtue of another task having executed Transfer.	*)

	END (*IF*);
	
    END TaskSwitch;

(************************************************************************)

PROCEDURE RunNextTask;

    (* Performs a task switch to the first task on the ready list	*)
    (* whose dynamic priority is CurrentPriority, if that task is not	*)
    (* blocked.  If it is blocked, we switch to its blocker instead.	*)

    VAR T: Task;  L: Lock;

    BEGIN
	T := ReadyList[CurrentPriority];
	LOOP
	    L := T^.WaitingFor;
	    IF (L = NIL) OR NOT L^.Locked THEN EXIT(*LOOP*) END(*IF*);
	    T := L^.Holder;
	END (*LOOP*);
	IF TimeSlicingEnabled THEN
	    T^.TimeLeft := timequota[T^.priority];
	END (*IF*);
	TaskSwitch (T);
    END RunNextTask;

(************************************************************************)

PROCEDURE SelectAnotherTask;

    (* Performs a task switch from the current task to the next ready	*)
    (* task.  It is assumed that the current task has already been	*)
    (* removed from the active list.  Notice that there is always at	*)
    (* least one ready task, namely the null task.			*)

    BEGIN
	WHILE ReadyList[CurrentPriority] = NIL DO
	    DEC (CurrentPriority);
	END (*WHILE*);
	RunNextTask;
    END SelectAnotherTask;

(************************************************************************)

PROCEDURE QueueAndSwitchTasks (VAR (*INOUT*) KQ: TaskQueue);

    (* Puts the current task on the tail of list KQ, and gives control	*)
    (* to the highest-priority ready task.				*)

    BEGIN
	IF CurrentTask^.FirstLock <> NIL THEN
	    Crash ("Task became inactive while holding a lock.");
	END (*IF*);
	LeaveQueue (ReadyList[CurrentTask^.priority]);
	AddToQueue (KQ, CurrentTask);
	SelectAnotherTask;
    END QueueAndSwitchTasks;

(************************************************************************)

PROCEDURE MarkAsReady (VAR (*INOUT*) FromQ: TaskQueue);

    (* Removes the first task from queue FromQ and makes it ready.  If	*)
    (* the task has a higher priority than the currently running task,	*)
    (* then we perform an immediate task switch.  Otherwise, the new	*)
    (* task descriptor is placed on the ready list.			*)
    (* Special case: if the incoming task is a sleeping task, it must	*)
    (* be a timing-out task which has been reactivated before its	*)
    (* timeout period has expired.  We run such a task immediately,	*)
    (* but don't yet put it on the list of active tasks.  The task runs	*)
    (* just far enough to clean up its kernel data structures, after	*)
    (* which procedure RecheckPriorities decides whether to switch back	*)
    (* to the preempted task or to legitimise the task switch.		*)

    VAR thispriority: PriorityLevel;
	T: Task;

    BEGIN
	T := TakeFromQueue (FromQ);
	IF T^.sleeping THEN
	    TaskSwitch (T);
	ELSE
	    thispriority := T^.priority;
	    AddToQueue (ReadyList[thispriority], T);
	    IF thispriority > CurrentPriority THEN
		CurrentPriority := thispriority;
		IF TimeSlicingEnabled THEN
		    T^.TimeLeft := timequota[thispriority];
		END (*IF*);
		TaskSwitch (T);
	    END (*IF*);
	END (*IF*);
    END MarkAsReady;

(************************************************************************)

PROCEDURE TimeSliceCheck;

    (* Called from the clock interrupt routine.  We check whether the	*)
    (* running task has used its time quota, and if so switch to the	*)
    (* next ready task.  Remark: it will sometimes happen that the	*)
    (* current task has used up its time quota, but that all ready	*)
    (* tasks have a lower priority than the current task.  In that	*)
    (* case, the end result of the "task switch" will be that the	*)
    (* current task gets its time quota refreshed and continues running.*)

    (* This procedure does nothing is time-slicing is disabled.		*)

    VAR savedPSW: CARDINAL;

    BEGIN
	IF TimeSlicingEnabled THEN
	    savedPSW := EnterKernel();
	    WITH CurrentTask^ DO
		DEC (TimeLeft);
		IF TimeLeft <= 0 THEN
		    ReadyList[priority] := ReadyList[priority]^.next;
		    RunNextTask;
		END (*IF*);
	    END (*WITH*);
	    LeaveKernel (savedPSW);
	END (*IF*);
    END TimeSliceCheck;

(************************************************************************)
(*			    TASK TERMINATION				*)
(************************************************************************)

PROCEDURE TaskExit;

    (* Removes the currently running task from the system, and performs	*)
    (* a task switch to the next ready task.				*)
    (* There is normally no need for a task to call this procedure,	*)
    (* because it is automatically called when the task code "falls out	*)
    (* the bottom" by executing its final procedure return.  The stack	*)
    (* is set up, at the time a task is created, in such a way that	*)
    (* TaskExit will be entered at that time.				*)

    VAR savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();
	WITH CurrentTask^ DO
	    IF FirstLock <> NIL THEN
		ReleaseAllLocks;
	    END (*IF*);

	    (* The following section is temporarily commented out,	*)
	    (* because what is needed is a little more complex than	*)
	    (* what is shown here.  This is just a crude first		*)
	    (* approximation to remind me to look at this issue later.	*)
	    (* If this code is run by an interrupt task then		*)
	    (* CurrentTask doesn't even identify its descriptor.	*)
	    (* The best solution might turn out to be to convert the	*)
	    (* interrupt task to a non-interrupt task (i.e. disconnect	*)
	    (* it from the interrupt mechanism) and then put it on the	*)
	    (* ready list with an arrangement to call the "normal" exit	*)
	    (* procedure when next it runs.				*)
(*
	    IF InterruptHandler THEN
		DisconnectFromInterrupt (selector);
		InterruptHandler := FALSE;
	    END (*IF*);
*)
	    dead := TRUE;
	    LeaveQueue (ReadyList[priority]);
	END (*WITH*);
	AddToQueue (UnusedDescriptors, CurrentTask);

	(* Note that the current task is now not on any of the active	*)
	(* kernel queues, and it never will be again.  Therefore, there	*)
	(* will never be a return from the SelectAnotherTask call below.*)
	(* This, plus the fact that we are still inside the kernel and	*)
	(* cannot be interrupted, justifies our deallocation of the	*)
	(* stack space even though the stack is about to be used to	*)
	(* push the return address for the SelectAnotherTask call.	*)
	(* (But this approach should perhaps be reviewed, since it does	*)
	(* rely on assumptions about how DEALLOCATE works.)		*)

	IF CurrentTask^.StackBase = NIL THEN
	    Crash ("TaskExit called from main program");
	ELSE
	    DEALLOCATE (CurrentTask^.StackBase, StackSize);
	    SelectAnotherTask;
	END (*IF*);

	(* We never reach this point.	*)

    END TaskExit;

(************************************************************************)
(*			    TASK INITIALISATION				*)
(************************************************************************)

PROCEDURE UsingFloatingPoint;

    (* Tells the kernel that this task is one which uses the floating	*)
    (* point processor.							*)

    BEGIN
	IF InterruptTaskActive THEN
	    Crash ("Interrupt task may not use floating point");
	END (*IF*);
	CurrentTask^.FloatingPointUser := TRUE;
    END UsingFloatingPoint;

(************************************************************************)

PROCEDURE NotUsingFloatingPoint;

    (* Tells the kernel that this task is one which does not use the	*)
    (* floating point processor.  Calling this (optional) procedure	*)
    (* speeds up task switching slightly, but it does put the onus on	*)
    (* the caller to be certain that no floating point operations will	*)
    (* be executed.							*)

    BEGIN
	CurrentTask^.FloatingPointUser := FALSE;
    END NotUsingFloatingPoint;

(************************************************************************)

PROCEDURE MakeTask (tptr: Task;  StartAddress: PROC;
			taskpriority: PriorityLevel;
			AllowInterrupts: BOOLEAN;  taskname: NameString);

    (* Initialises the stack for a new task, and fills in the fields of	*)
    (* its task descriptor tptr^.  The remaining parameters are the	*)
    (* start address of the task code; the priority of the new task;	*)
    (* a flag which is TRUE iff the task is to be started with		*)
    (* interrupts enabled; and a textual name for diagnostic purposes.	*)

    (* Remark:  An important function of this procedure is to set up	*)
    (* initial conditions for the task so that it looks as if it	*)
    (* entered the ready list in the same way as, for example, a	*)
    (* blocked task which has just become unblocked.  Procedure		*)
    (* InnerKernel.TaskInit looks after most of that detail.		*)

    BEGIN
	WITH tptr^ DO

	    (* Create the task stack, and give it the necessary		*)
	    (* initial contents.					*)

	    ALLOCATE (StackBase, StackSize);
	    selector := TaskInit (StackBase, StackSize, AllowInterrupts,
						TaskExit, StartAddress);
	    FPselector := MakeFloatSaveSelector (selector);

	    (* Set up the remaining task descriptor values.  Some of	*)
	    (* the assignments below are redundant, but I believe that	*)
	    (* filling in most of the fields is desirable in that it	*)
	    (* makes this module more readable and guards against	*)
	    (* strange bugs being introduced in future revisions.	*)

	    name := taskname;
	    priority := taskpriority;
	    WaitingFor := NIL;  FirstLock := NIL;
	    IF TimeSlicingEnabled THEN
		TimeLeft := timequota[priority];
	    ELSE
		TimeLeft := 0;
	    END (*IF*);
	    InterruptHandler := FALSE;  dead := FALSE;  sleeping := FALSE;
	    nextsleeper := NIL;
	    FloatingPointUser := TRUE;
	    InterruptNo := 0;

	    (* Save the creator's floating point state in the save area	*)
	    (* of the new task, to ensure that the new task starts with	*)
	    (* a meaningful floating point state.  Unfortunately the	*)
	    (* NPXsave seems to reset the floating point hardware	*)
	    (* to an undesired default, so we must restore the state	*)
	    (* again to get back where we were.				*)

	    NPXsave (FPselector);  NPXrestore (FPselector);

	END (*WITH*);

    END MakeTask;

(************************************************************************)

PROCEDURE CreateTask (StartAddress: PROC;  taskpriority: PriorityLevel;
						taskname: NameString);

    (* Must be called to introduce a task to the system. The first	*)
    (* parameter, which should be the name of a procedure containing	*)
    (* the task code, gives the starting address.  The second parameter	*)
    (* is the task's base priority.  If this task has a higher priority	*)
    (* than its creator, it will run immediately.  Otherwise, it	*)
    (* becomes ready.							*)
    (* The effective priority of a task can be higher than its base	*)
    (* priority, as the result of priority inheritance.  This happens	*)
    (* when the task holds a lock on which a higher-priority task is	*)
    (* waiting.								*)

    VAR tptr: Task;
	savedPSW: CARDINAL;
	
    BEGIN
	savedPSW := EnterKernel();
	tptr := UnusedDescriptors;
	IF tptr = NIL THEN
	    Crash ("Too many tasks created");
	END (*IF*);
	MakeTask (tptr, StartAddress, taskpriority, TRUE, taskname);
	tptr^.FloatingPointUser := TRUE;
	MarkAsReady (UnusedDescriptors);
	LeaveKernel (savedPSW);
    END CreateTask;

(************************************************************************)
(*		PROCEDURES TO DEAL WITH INTERRUPT TASKS			*)
(************************************************************************)

PROCEDURE CreateInterruptTask (InterruptNumber: CARDINAL;
				StartAddress: PROC;  taskname: NameString);

    (* Introduces an interrupt task to the system.  The first parameter	*)
    (* is the hardware-defined interrupt number, and the second is the	*)
    (* address of the procedure whose code is the interrupt handler.	*)
    (* An interrupt task differs from an ordinary task in that, when it	*)
    (* is not running, it is idle rather than ready, and the dispatcher	*)
    (* does not consider it to be among the tasks eligible to run.	*)
    (* (It is run by a task switch which is made directly by the	*)
    (* assembly language code which fields the interrupt).  When the	*)
    (* interrupt task has responded to the interrupt, it must call	*)
    (* procedure WaitForInterrupt to put itself back in the idle state.	*)
    (* On the next interrupt, it will continue from just after the call	*)
    (* to WaitForInterrupt.  Normally, therefore, the interrupt task	*)
    (* will be written as an infinite loop.  If for any reason the	*)
    (* interrupt task exits by falling out of the bottom of its code,	*)
    (* it will be destroyed in the same way as a normal task which	*)
    (* terminates.  That could be fatal, unless steps have been taken	*)
    (* to reset the interrupt vector.					*)

    VAR ITptr: Task;  savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();

	IF UnusedDescriptors = NIL THEN
	    Crash ("Too many tasks created");
	END (*IF*);

	ITptr := TakeFromQueue (UnusedDescriptors);
	MakeTask (ITptr, StartAddress, MAX(PriorityLevel), FALSE, taskname);
	WITH ITptr^ DO
	    InterruptHandler := TRUE;
	    FloatingPointUser := FALSE;
	    InterruptNo := InterruptNumber;
	END (*WITH*);

	(* Now start the interrupt task and connect it to the desired	*)
	(* interrupt.  This operation includes executing the task's	*)
	(* initialisation code, i.e. everything up to its first call to	*)
	(* procedure WaitForInterrupt.					*)

	InterruptTaskActive := TRUE;
	StartInterruptTask (ITptr^.selector, InterruptNumber);

	(* When we get back to here, the interrupt task has run to its	*)
	(* first call to WaitForInterrupt, and InterruptTaskActive is	*)
	(* FALSE again.							*)

	LeaveKernel (savedPSW);
	
    END CreateInterruptTask;

(************************************************************************)

PROCEDURE KillInterruptTask (InterruptNumber: CARDINAL);

    (* Removes an interrupt task from the system. *)

    VAR k: TaskNumber;  found: BOOLEAN;

    BEGIN
	k := MaxTaskNumber;
	REPEAT
	    WITH TaskTable[k] DO
		found := InterruptHandler AND NOT dead
				AND (InterruptNo = InterruptNumber);
	    END (*WITH*);
	    DEC (k);
	UNTIL found OR (k = 0);
	IF found THEN
	    INC (k);
	    WITH TaskTable[k] DO
		DisconnectFromInterrupt (selector);
		InterruptHandler := FALSE;  dead := TRUE;
		DEALLOCATE (StackBase, StackSize);
	    END (*WITH*);
	    AddToQueue (UnusedDescriptors, ADR(TaskTable[k]));
	END (*IF*);
    END KillInterruptTask;

(************************************************************************)

PROCEDURE WaitForInterrupt;

    (* Called by an interrupt task, to make itself dormant until the	*)
    (* next interrupt comes along.  It is not necessary to specify	*)
    (* the interrupt number, since this was fixed at the time the	*)
    (* interrupt task was created.					*)

    VAR savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();
	IF NOT InterruptTaskActive THEN
	    Crash ("Normal task has called WaitForInterrupt");
	END (*IF*);

	(* Procedure IOTransfer performs a task switch from the		*)
	(* interrupt task to the interrupted task, whose state was	*)
	(* saved at the time the interrupt occurred.			*)

	InterruptTaskActive := FALSE;
	IOTransfer;

	(* We return to this point after the next interrupt.	*)
	(* Return to the interrupt task.			*)
	
	InterruptTaskActive := TRUE;
	LeaveKernel (savedPSW);

    END WaitForInterrupt;

(************************************************************************)
(*			CREATING AND DESTROYING LOCKS			*)
(************************************************************************)

PROCEDURE CreateLock (VAR (*OUT*) L: Lock);

    (* Creates a new lock.	*)

    VAR savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();
	NEW (L);
	WITH L^ DO
	    Locked := FALSE;  Holder := NIL;
	    next := NIL;  previous := NIL;
	END (*WITH*);
	LeaveKernel (savedPSW);
    END CreateLock;

(************************************************************************)

PROCEDURE DestroyLock (VAR (*INOUT*) L: Lock);

    (* Disposes of a lock.	*)

    VAR savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();

	(* It is legitimate for a task to obtain L before destroying	*)
	(* it, in which case we must remove L from the list of locks	*)
	(* held by this task.  However we leave the Locked field set in	*)
	(* case some task still has a dangling pointer to this lock.	*)

	WITH L^ DO
	    IF Holder <> NIL THEN
		IF previous = NIL THEN Holder^.FirstLock := next
		ELSE previous^.next := next;
		END (*IF*);
		IF next <> NIL THEN
		next^.previous := previous;
		END (*IF*);
	    END (*IF*);
	    Locked := TRUE;
	END (*WITH*);
	DISPOSE (L);
	LeaveKernel (savedPSW);
    END DestroyLock;

(************************************************************************)
(*			     OBTAINING A LOCK				*)
(************************************************************************)

PROCEDURE Obtain (L: Lock);

    (* Obtains lock L, waiting if necessary.	*)

    VAR Blocker: Task;  M: Lock;
	savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();
	IF L^.Locked THEN

	    (* Work out the blocker of the current task.  Note: in an	*)
	    (* earlier version of this module I was able to avoid the	*)
	    (* loop below by keeping a BlockedBy field in each task	*)
	    (* descriptor; but it turned out to be more trouble than it	*)
	    (* was worth to update the BlockedBy information when a	*)
	    (* lock was released.  It's less expensive to incur the	*)
	    (* penalty here and keep the Release code simple.  In any	*)
	    (* case the penalty is not severe - it would take a really	*)
	    (* baroque example to slow us down here.			*)

	    M := L;
	    REPEAT
		Blocker := M^.Holder;  M := Blocker^.WaitingFor;
	    UNTIL (M = NIL) OR NOT M^.Locked;

	    IF Blocker = CurrentTask THEN
		Crash ("Deadlock detected");
	    END (*IF*);

	    CurrentTask^.WaitingFor := L;
	    IF TimeSlicingEnabled THEN
		Blocker^.TimeLeft := timequota[Blocker^.priority];
	    END (*IF*);
	    TaskSwitch (Blocker);

	    (* When execution resumes at this point (via	*)
	    (* RunNextTask), L^.Locked will be FALSE.		*)

	    CurrentTask^.WaitingFor := NIL;

	END (*IF*);

	WITH L^ DO
	    Locked := TRUE;  Holder := CurrentTask;

	    (* Add L to the list of locks held by CurrentTask. *)

	    previous := NIL;
	    next := CurrentTask^.FirstLock;
	    IF next <> NIL THEN
		next^.previous := L;
	    END (*IF*);
	    CurrentTask^.FirstLock := L;

	END (*WITH*);
	LeaveKernel (savedPSW);

    END Obtain;

(************************************************************************)
(*			     RELEASING A LOCK				*)
(************************************************************************)

PROCEDURE Release (L: Lock);

    (* Releases lock L.  This implicitly changes the "blocked by"	*)
    (* status of all tasks blocked by the current task, but it turns	*)
    (* out to be faster to get procedure RunNextTask to recheck the	*)
    (* blocking status than to keep track of "blocked by" information.	*)
    (* All we need to know here is whether we've inherited a priority	*)
    (* greater than our base priority.  If so, we must have been	*)
    (* blocking another task which should run now - we can trust	*)
    (* RunNextTask to work out the identity of that task.		*)

    VAR savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();
	WITH L^ DO

	    (* Remove L from the list of locks held by CurrentTask. *)

	    IF previous = NIL THEN CurrentTask^.FirstLock := next
	    ELSE previous^.next := next;
	    END (*IF*);
	    IF next <> NIL THEN
		next^.previous := previous;
	    END (*IF*);

	    (* Mark the lock as free. *)
	    Locked := FALSE;  Holder := NIL;

	END (*WITH*);

	(* If we have inherited an augmented priority, a task of higher	*)
	(* priority might just have become unblocked.			*)

	IF CurrentTask^.priority < CurrentPriority THEN
	    RunNextTask;
	END (*IF*);

	LeaveKernel (savedPSW);
    END Release;

(************************************************************************)

PROCEDURE ReleaseAllLocks;

    (* Releases all locks held by the current task.  Application-level	*)
    (* tasks normally won't need to call this procedure; it is		*)
    (* provided to support the system shutdown function and for things	*)
    (* like "emergency abort" operations.				*)

    VAR savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();
	WHILE CurrentTask^.FirstLock <> NIL DO
	    Release (CurrentTask^.FirstLock);
	END (*WHILE*);
	LeaveKernel (savedPSW);
    END ReleaseAllLocks;

(************************************************************************)
(*		PROCEDURES TO DEAL WITH SLEEPING TASKS			*)
(************************************************************************)

VAR SleepQueueHead: Task;

    (* SleepQueueHead points to a priority queue of descriptors for	*)
    (* tasks which are sleeping.  The first element of the list has the	*)
    (* time-to-go in its TimeLeft field.  For subsequent elements,	*)
    (* TimeLeft records the difference between its time-to-go and that	*)
    (* of the preceding list element.  There are some situations in	*)
    (* which the delay specification cannot be met exactly, so it is	*)
    (* possible for the TimeLeft field to go negative.			*)

(************************************************************************)
(*			SLEEP QUEUE INITIALISATION			*)
(************************************************************************)

PROCEDURE InitialiseSleepQueue;

    (* Creates an initially empty queue of sleeping tasks.	*)

    BEGIN
	SleepQueueHead := NIL;
    END InitialiseSleepQueue;

(************************************************************************)
(*			'PUT-ME-TO-SLEEP' PROCEDURES			*)
(************************************************************************)

PROCEDURE InsertCurrentTaskInSleepQueue (sleeptime: INTEGER);

    (* Inserts the current task into the sleep queue.  This queue is a	*)
    (* linear list.  The time to sleep is stored in incremental form:	*)
    (* for every task on the list except the first, the TimeLeft field	*)
    (* in the task descriptor holds the difference between the time to	*)
    (* go for this task and that of the task ahead of it in the list.	*)
    (* This means that the clock interrupt routine need only decrement	*)
    (* the TimeLeft field of the first sleeping task.			*)

    VAR previous, following: Task;

    BEGIN
	CurrentTask^.sleeping := TRUE;

	(* Find the correct place in the sleep queue. *)

	following := SleepQueueHead;  previous := NIL;
	LOOP
	    IF following = NIL THEN EXIT(*LOOP*) END (*IF*);
	    IF sleeptime < following^.TimeLeft THEN EXIT(*LOOP*)
	    END (*IF*);
	    previous := following;  following := following^.nextsleeper;
	    DEC (sleeptime, previous^.TimeLeft);
	END (*LOOP*);

	(* Insert the task descriptor for this task between	*)
	(* previous^ and following^.				*)

	IF previous = NIL THEN
	    SleepQueueHead := CurrentTask;
	ELSE
	    previous^.nextsleeper := CurrentTask;
	END (*IF*);
	CurrentTask^.nextsleeper := following;

	(* Fix the TimeLeft fields for the current and following tasks.	*)

	CurrentTask^.TimeLeft := sleeptime;
	IF following <> NIL THEN
	    DEC (following^.TimeLeft, sleeptime);
	END (*IF*);

    END InsertCurrentTaskInSleepQueue;

(************************************************************************)

PROCEDURE RemoveCurrentFromSleepQueue;

    (* Removes the current task from the sleep queue.	*)

    VAR previous, current: Task;

    BEGIN
	previous := NIL;  current := SleepQueueHead;
	WHILE current <> CurrentTask DO
	    previous := current;  current := current^.nextsleeper;
	END (*WHILE*);
	IF current^.nextsleeper <> NIL THEN
	    INC (current^.nextsleeper^.TimeLeft, current^.TimeLeft);
	END (*IF*);
	IF previous = NIL THEN
	    SleepQueueHead := current^.nextsleeper;
	ELSE
	    previous^.nextsleeper := current^.nextsleeper;
	END (*IF*);
	CurrentTask^.sleeping := FALSE;
    END RemoveCurrentFromSleepQueue;

(************************************************************************)

PROCEDURE RecheckPriorities;

    (* This procedure is called, just before leaving the kernel, by a	*)
    (* sleeping task which has been woken up.  The task is running but	*)
    (* has not yet been put on the list of active tasks.  If this task	*)
    (* has a high enough priority, we put it on the active list and let	*)
    (* it continue to run.  Otherwise, we mark it as ready but switch	*)
    (* back to the task which it preempted.				*)

    VAR thispriority: PriorityLevel;

    BEGIN
	thispriority := CurrentTask^.priority;
	IF thispriority > CurrentPriority THEN
	    AddToQueue (ReadyList[thispriority], CurrentTask);
	    CurrentPriority := thispriority;
	    IF TimeSlicingEnabled THEN
		CurrentTask^.TimeLeft := timequota[thispriority];
	    ELSE
		CurrentTask^.TimeLeft := 0;
	    END (*IF*);
	ELSE
	    AddToQueue (ReadyList[thispriority], CurrentTask);
	    TaskSwitch (LastTask);
	END (*IF*);
    END RecheckPriorities;

(************************************************************************)

PROCEDURE Delay (sleeptime: INTEGER);

    (* Puts the calling task to sleep for approximately the specified	*)
    (* number of clock ticks.  The time delay is necessarily		*)
    (* approximate, because (a) the clock might tick over when we are	*)
    (* in the middle of this procedure; (b) after the expiration of the	*)
    (* delay period, the sleeping task is made ready, but this does not	*)
    (* guarantee that it will run immediately.				*)

    VAR savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();
	IF CurrentTask^.FirstLock <> NIL THEN
	    Crash ("Task sleeping while holding a lock");
	END (*IF*);
	LeaveQueue (ReadyList[CurrentTask^.priority]);
	InsertCurrentTaskInSleepQueue (sleeptime);
	SelectAnotherTask;

	(* We resume at this point after waking up.		*)

	RecheckPriorities;
	LeaveKernel (savedPSW);

    END Delay;

(************************************************************************)
(*		    CLOCK TICK HANDLER FOR SLEEPING TASKS		*)
(************************************************************************)

PROCEDURE CheckSleepers;

    (* Called from the clock interrupt task.  We check whether a	*)
    (* sleeping task needs to be woken up, and wake it up if so.	*)

    (* Note: this procedure typically needs no explicit critical	*)
    (* section protection.  It is called from the clock interrupt	*)
    (* handler, therefore runs with interrupts disabled.  We prefer,	*)
    (* however, to stick to the rule that all kernel procedures are	*)
    (* protected via EnterKernel and LeaveKernel, since it makes the	*)
    (* kernel easier to check for correctness.  This convention also	*)
    (* helps portability.  It means, for example, that this procedure	*)
    (* will still work in the multiprocessor case, or in the case where	*)
    (* the clock interrupt routine does not disable all interrupts.	*)

    VAR	first, second: Task;
	savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();
	first := SleepQueueHead;
	IF first <> NIL THEN

	    (* update sleeping time of head of sleep queue *)

	    DEC (first^.TimeLeft);

	    (* Is it time to wake up first sleeping task? *)

	    IF first^.TimeLeft <= 0 THEN

		(* Yes.  Correct sleeping time of second sleeper. *)

		second := first^.nextsleeper;
		SleepQueueHead := second;
		IF second <> NIL THEN
		    INC (second^.TimeLeft, first^.TimeLeft);
		END (* IF second <> NIL *);

		(* Wake up the first sleeping task.  Because of		*)
		(* complications when this task is still on a semaphore	*)
		(* blocked list, we must run the task immediately	*)
		(* rather than simply make it ready.  The task is not	*)
		(* yet put on the list of active tasks, because it	*)
		(* might be preempting a task of higher priority.	*)
		(* Before the newly awakened task leaves the kernel, it	*)
		(* will call procedure RecheckPriorities to clean up	*)
		(* such details.					*)

		first^.sleeping := FALSE;
		TaskSwitch (first);

	    END (* IF sleeping time has expired *)

	END (* IF sleep queue not empty *);

	LeaveKernel (savedPSW);

    END CheckSleepers;

(************************************************************************)
(*			TASK QUEUEING WITH TIMEOUT			*)
(************************************************************************)

PROCEDURE QueueWithTimeout (VAR (*INOUT*) KQ: TaskQueue;
					TimeLimit: INTEGER): BOOLEAN;

    (* Like procedure QueueAndSwitchTasks, this procedure puts the	*)
    (* current task on the tail of list KQ, and gives control to the	*)
    (* highest-priority ready task.  The difference is that we allow	*)
    (* this task to remain on KQ for at most TimeLimit timer ticks.	*)
    (* If the task is removed from KQ before the time limit expires,	*)
    (* we return a result of FALSE.  If the time limit expires first,	*)
    (* we remove the queued task from KQ anyway, and make it runnable,	*)
    (* and return a result of TRUE when it does run.			*)
    (* Note: this procedure may be called only from inside the kernel.	*)
    (* While being timed out, a task is simultaneously on two kernel	*)
    (* queues: the specified KQ, and the Sleep queue.  It will be	*)
    (* removed from one of these - which one depends, of course, on	*)
    (* whether the time limit expires - by some other kernel action,	*)
    (* and at that stage we must remove it from the other queue before	*)
    (* it leaves the kernel.  If we did not guarantee indivisibility	*)
    (* of the operation of removing the task from the two queues, we	*)
    (* would run the risk of putting the same task onto the ready	*)
    (* structure twice.							*)

    VAR TimedOut: BOOLEAN;

    BEGIN
	InsertCurrentTaskInSleepQueue (TimeLimit);
	QueueAndSwitchTasks (KQ);

	(* When execution of this task resumes at this point, we have	*)
	(* been activated either by being removed from KQ or by a timer	*)
	(* event (but never both simultaneously).  That is, we have	*)
	(* been removed from one queue, and must now remove ourself	*)
	(* from the other.						*)

	TimedOut := NOT CurrentTask^.sleeping;
	IF TimedOut THEN
	    LeaveQueue (KQ);
	ELSE
	    RemoveCurrentFromSleepQueue;
	END (*IF*);

	(* Additional complication: the kernel procedures have so far	*)
	(* given the newly awakened task preferential treatment, to	*)
	(* allow it to remove itself from both queues before leaving	*)
	(* the kernel.  If this has caused a higher-priority task to be	*)
	(* preempted, we must give the processor back.			*)

	RecheckPriorities;
	RETURN TimedOut;

    END QueueWithTimeout;

(************************************************************************)
(*				PMOS SHUTDOWN				*)
(************************************************************************)

PROCEDURE ShutDown;

    (* Kills all remaining interrupt tasks, and partially inhibits	*)
    (* multitasking by giving the current task - which is running the	*)
    (* program termination code - the maximum possible priority.  This	*)
    (* still allows the possibility of a task switch caused by		*)
    (* something like a semaphore Wait; but it does ensure that the	*)
    (* mere unblocking of another task, for example by releasing a	*)
    (* Lock, will not cause a task switch.  Notice too that time	*)
    (* slicing is irrelevant beyond this point, because the timer	*)
    (* interrupt task will be disabled.					*)

    VAR k: TaskNumber;  savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterKernel();

	(* Remove any interrupt handlers which might still be active.	*)
	(* The reason for doing this backwards is that this is more	*)
	(* likely to work in the case of multiple handlers for the	*)
	(* same interrupt number.  At some time I should look for a	*)
	(* more robust solution for this case.				*)

	FOR k := MaxTaskNumber TO 0 BY -1 DO
	    WITH TaskTable[k] DO
		IF InterruptHandler AND NOT dead THEN
		    DisconnectFromInterrupt (selector);
		    InterruptHandler := FALSE;  dead := TRUE;
		    DEALLOCATE (StackBase, StackSize);
		    AddToQueue (UnusedDescriptors, ADR(TaskTable[k]));
		END (*IF*);
	    END (*WITH*);
	END (*FOR*);

	(* Increase the priority of the current task. *)

	LeaveQueue (ReadyList[CurrentTask^.priority]);
	CurrentPriority := MAX(PriorityLevel);
	CurrentTask^.priority := CurrentPriority;
	AddToQueue (ReadyList[CurrentPriority], CurrentTask);
	LeaveKernel (savedPSW);

    END ShutDown;

(************************************************************************)
(*			    MODULE INITIALISATION			*)
(************************************************************************)

PROCEDURE InitialiseTaskControl;

    (* Must be called before any tasks are created, to initialise the	*)
    (* task control data structures.					*)

    VAR p: PriorityLevel;  j: TaskNumber;
	k: InterruptType;
	quota: CARDINAL;

    BEGIN
	InterruptTaskActive := FALSE;

	(* Initialise the timequota array. *)

	IF TimeSlicingEnabled THEN
	    quota := 1;
	    FOR p := MaxPriority TO 1 BY -1 DO
		timequota[p] := quota;  quota := 2*quota;
	    END (*FOR*);
	    timequota[0] := 32767;
	END (*IF*);

	(* Collect all the unused task descriptors into a linear list.	*)

	CreateQueue (UnusedDescriptors);
	FOR j := 0 TO MaxTaskNumber DO
	    TaskTable[j].dead := TRUE;
	    TaskTable[j].sleeping := FALSE;
	    AddToQueue (UnusedDescriptors, ADR (TaskTable[j]) );
	END (*FOR*);

	(* The active list and the sleep queue are initially empty.  *)

	FOR p := 0 TO MaxPriority DO
	    CreateQueue (ReadyList[p]);
	END (*FOR*);
	InitialiseSleepQueue;

	(* Create a descriptor for the current task, i.e. for the task	*)
	(* which called this procedure.  We give it a low priority	*)
	(* because this task is often running the background jobs.  We	*)
	(* also set its StackBase to NIL because the stack for this	*)
	(* task already exists, i.e. it's not created by this module.	*)
	
	LastTask := NIL;
	CurrentTask := TakeFromQueue (UnusedDescriptors);
	CurrentPriority := 1;
	WITH CurrentTask^ DO
	    name := "Main program";
	    priority := CurrentPriority;
	    WaitingFor := NIL;  FirstLock := NIL;
	    FPselector := MakeFloatSaveSelector (InitMainTask());
	    StackBase := NIL;
	    InterruptHandler := FALSE;  dead := FALSE;
	    FloatingPointUser := TRUE;  sleeping := FALSE;
	    InterruptNo := 0;
	END (*WITH*);
	AddToQueue (ReadyList[CurrentPriority], CurrentTask);
	
	(* Create the null task.  It will not run immediately, because	*)
	(* the current (initialisation) task has higher priority.	*)

	CreateTask (NullTask, 0, "Null task");

	(* We have a working operating system.  It is now safe to	*)
	(* enable clock interrupts, and to call CreateTask to introduce	*)
	(* more tasks to the system.  Device interrupts can be enabled	*)
	(* when the device drivers are installed.			*)

    END InitialiseTaskControl;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

BEGIN
    InitialiseTaskControl;
    SetTerminationProcedure (ShutDown);
END TaskControl.
