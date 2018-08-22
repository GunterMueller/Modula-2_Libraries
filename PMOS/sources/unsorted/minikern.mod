IMPLEMENTATION MODULE MiniKernel;

	(****************************************************************)
	(*								*)
	(*	A stripped-down demonstration version of an operating	*)
	(*			system kernel.				*)
	(*								*)
	(*	Programmer:	P. Moylan				*)
	(*	Last edited:	16 August 1992				*)
	(*	Status:		OK					*)
	(*		Have had to disable kernel tracing, since it	*)
	(*		causes infinite recursion.  In future must	*)
	(*		find a new mechanism for handling this.		*)
	(*								*)
	(****************************************************************)

FROM SYSTEM IMPORT
    (* type *)	ADDRESS,
    (* proc *)	ADR;

FROM SYSTEM IMPORT
    (* proc *)	NEWPROCESS, TRANSFER;

FROM Storage IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;
(*
FROM GlassTTY IMPORT
    (* proc *)	WriteString, WriteLn;
*)
(*
FROM Trace IMPORT
    (* proc *)	InTrace, OutTrace;
*)

(************************************************************************)

CONST
    MaxTaskNumber = 31;
    MaxPriority = 3;
    StackSize = 1024;

TYPE
    TaskNumber = [0..MaxTaskNumber];
    PriorityType = [0..MaxPriority];
    TaskPointer = POINTER TO TaskDescriptor;

	(****************************************************************)
	(*								*)
	(* Each task in the system has a task descriptor.		*)
	(* The fields in a task descriptor are:				*)
	(*	next:	pointer to next task descriptor on the queue.	*)
	(*		(i.e. whatever system queue this task		*)
	(*		descriptor happens to be on).			*)
	(*	priority: task priority.				*)
	(*	selector: a selector for the task state of this task.	*)
	(*	StackBase: the low address of the memory segment set	*)
	(*		aside for the stack. This is needed only when	*)
	(*		we kill a task, and wish to reclaim the memory	*)
	(*		used by its stack.				*)
	(*								*)
	(****************************************************************)

    TaskDescriptor = RECORD
			next		: TaskPointer;
			priority	: PriorityType;
			selector	: ADDRESS;
			StackBase	: ADDRESS;
		     END;

	(* The system maintains a number of queues.  Each of these has	*)
	(* a header which points to the first and last elements on the	*)
	(* queue.  For an empty queue, both pointers are NIL.		*)

    KernelQueue =   RECORD
			head, tail: TaskPointer;
		    END;

    Semaphore = POINTER TO RECORD
				value	:	INTEGER;
				blockedlist :	KernelQueue
			   END;

(************************************************************************)

VAR

    TaskTable: ARRAY TaskNumber OF TaskDescriptor;

	(* TaskTable is an array of task descriptors.  After procedure	*)
	(* InitialiseTaskControl returns, there will be two tasks in	*)
	(* the system: the initialisation task, and the null task.  The	*)
	(* remaining task descriptors will be on the UnusedDescriptors	*)
	(* list, available for the creation of new tasks.  A task	*)
	(* descriptor is allocated by procedure CreateTask, and is	*)
	(* returned to UnusedDescriptors by procedure TaskExit.		*)

    CurrentTaskPointer: TaskPointer;

	(* CurrentTaskPointer points to the running task.  Note: the	*)
	(* currently running task is not considered to be "ready", and	*)
	(* its task descriptor is not on any of the kernel queues.	*)

    UnusedDescriptors: KernelQueue;

	(* UnusedDescriptors is a queue of all task descriptors which	*)
	(* are unused, i.e. which are available for use as descriptors	*)
	(* of newly created tasks.					*)

    ReadyList: ARRAY PriorityType OF KernelQueue;

	(* ReadyList holds task descriptors for all the ready tasks.	*)
	(* It is an array of linear lists, one list for each priority	*)
	(* level.  Priority level 0 is special: only the null task has	*)
	(* priority zero, and the null task is always ready except when	*)
	(* it is running.						*)

(************************************************************************)
(*			    KERNEL LIST OPERATIONS			*)
(************************************************************************)

PROCEDURE CreateQueue (VAR (*OUT*) KQ: KernelQueue);

    (* Creates an initially empty kernel queue.	*)

    BEGIN
	KQ.head := NIL;  KQ.tail := NIL;
    END CreateQueue;

(*************************************************************************)

PROCEDURE AddToList (VAR (*INOUT*) Q: KernelQueue; TaskPtr: TaskPointer);

    (* Adds the task descriptor pointed to by TaskPtr to the tail of	*)
    (* queue Q.								*)

    BEGIN
	TaskPtr^.next := NIL;
	IF Q.head = NIL THEN Q.head := TaskPtr
	ELSE Q.tail^.next := TaskPtr
	END (*IF*);
	Q.tail := TaskPtr;
    END AddToList;

(************************************************************************)

PROCEDURE TakeFromList (VAR (*INOUT*) Q: KernelQueue): TaskPointer;

    (* Removes the first entry, which is a pointer to a task		*)
    (* descriptor, from queue Q and returns it.  Assumption: the caller	*)
    (* has already verified that the queue is nonempty.			*)

    VAR result: TaskPointer;

    BEGIN
	result := Q.head;
	Q.head := result^.next;
	IF Q.head = NIL THEN Q.tail := NIL  END (*IF*);
	RETURN result;
    END TakeFromList;

(************************************************************************)
(*				THE NULL TASK				*)
(************************************************************************)

PROCEDURE NullTask;

    (* The only function of the null task is to soak up processor time	*)
    (* when no other task is able to run.				*)

    BEGIN
	(*InTrace ("NullTask");*)
	LOOP (* Do nothing *) END (*LOOP*);
    END NullTask;

(************************************************************************)
(*			    	DISPATCHER				*)
(************************************************************************)

PROCEDURE TaskSwitch (T: TaskPointer);

    (* Performs a task switch from the current task to task T.  It is	*)
    (* assumed that the current task has already been moved to the	*)
    (* appropriate kernel queue.					*)

    VAR OldTaskPointer: TaskPointer;

    BEGIN
	(*InTrace ("TaskSwitch");*)
	IF CurrentTaskPointer <> T THEN
	    OldTaskPointer := CurrentTaskPointer;  CurrentTaskPointer := T;
	    TRANSFER (OldTaskPointer^.selector, CurrentTaskPointer^.selector);
	END (*IF*);
	(*OutTrace ("TaskSwitch");*)
    END TaskSwitch;

(************************************************************************)

PROCEDURE SelectAnotherTask;

    (* Performs a task switch from the current task to the next ready	*)
    (* task.  It is assumed that the current task has already been put	*)
    (* on the appropriate kernel queue.  Notice that there is always at	*)
    (* least one ready task, namely the null task.  The only time that	*)
    (* the ready structure can become totally empty is when the null	*)
    (* task is running.  In that case, this procedure will be called	*)
    (* only after the null task has been put back on the ready list.	*)

    VAR p: PriorityType;

    BEGIN
	(*InTrace ("SelectAnotherTask");*)
	p := MaxPriority;
	WHILE ReadyList[p].head = NIL DO
	    DEC (p);
	END (*WHILE*);
	TaskSwitch (TakeFromList (ReadyList[p]));
	(*OutTrace ("SelectAnotherTask");*)
    END SelectAnotherTask;

(************************************************************************)

PROCEDURE QueueAndSwitchTasks (VAR (*INOUT*) KQ: KernelQueue);

    (* Puts the current task on the tail of list KQ, and gives control	*)
    (* to the highest-priority ready task.				*)

    BEGIN
	(*InTrace ("QueueAndSwitchTasks");*)
	AddToList (KQ, CurrentTaskPointer);
	SelectAnotherTask;
	(*OutTrace ("QueueAndSwitchTasks");*)
    END QueueAndSwitchTasks;

(************************************************************************)

PROCEDURE MarkAsReady (VAR (*INOUT*) FromQ: KernelQueue);

    (* Removes the first task from queue FromQ and makes it ready.  If	*)
    (* the task has a higher priority than the currently running task,	*)
    (* then we perform an immediate task switch.  Otherwise, the new	*)
    (* task descriptor is placed on the ready list.			*)

    VAR thispriority, currentpriority: PriorityType;
	T: TaskPointer;

    BEGIN
	(*InTrace ("MarkAsReady");*)
	currentpriority := CurrentTaskPointer^.priority;
	T := TakeFromList (FromQ);
	thispriority := T^.priority;
	IF (thispriority > currentpriority) THEN
	    AddToList (ReadyList[currentpriority], CurrentTaskPointer);
	    TaskSwitch (T);
	ELSE AddToList (ReadyList[thispriority], T);
	END (*IF*);
	(*OutTrace ("MarkAsReady");*)
    END MarkAsReady;

(************************************************************************)
(*			    TASK TERMINATION				*)
(************************************************************************)

PROCEDURE TaskExit;

    (* Removes the currently running task from the system, and performs	*)
    (* a task switch to the next ready task.				*)

    BEGIN
	(*InTrace ("TaskExit");*)
	AddToList (UnusedDescriptors, CurrentTaskPointer);

	(* Note that the current task is now not on any of the active	*)
	(* kernel queues, and it never will be again.  Therefore, there	*)
	(* will never be a return from the SelectAnotherTask call below.*)

	DEALLOCATE (CurrentTaskPointer^.StackBase, StackSize);
	SelectAnotherTask;

	(* We never reach this point.	*)

    END TaskExit;

(************************************************************************)
(*			    TASK INITIALISATION				*)
(************************************************************************)

PROCEDURE CreateTask (StartAddress: PROC; taskpriority: CARDINAL);

    (* Must be called to introduce a task to the system. The first	*)
    (* parameter, which should be the name of a procedure containing	*)
    (* the task code, gives the starting address.  The second parameter	*)
    (* is the task priority.  If this task has a higher priority than	*)
    (* its creator, it will run immediately.  Otherwise, it becomes	*)
    (* ready.								*)

    BEGIN
	(*InTrace ("CreateTask");*)
	WITH UnusedDescriptors.head^ DO
	    priority := taskpriority;
	    ALLOCATE (StackBase, StackSize);
	    NEWPROCESS (StartAddress, StackBase, StackSize, selector);
	END (*WITH*);
	MarkAsReady (UnusedDescriptors);
	(*OutTrace ("CreateTask");*)
    END CreateTask;

(************************************************************************)
(*				SEMAPHORES				*)
(************************************************************************)

PROCEDURE CreateSemaphore (VAR (*OUT*) s: Semaphore; InitialValue: INTEGER);

    (* Creates semaphore s, with the given initial value and an empty	*)
    (* queue.								*)

    BEGIN
	(*InTrace ("CreateSemaphore");*)
	NEW(s);
	s^.value := InitialValue;
	CreateQueue (s^.blockedlist);
	(*OutTrace ("CreateSemaphore");*)
    END CreateSemaphore;

(************************************************************************)

PROCEDURE Wait (VAR (*INOUT*) s: Semaphore);

    (* Decrements the semaphore value.  If the value goes negative,	*)
    (* the calling task is blocked and there is a task switch.		*)

    BEGIN
	(*InTrace ("Wait");*)
	DEC (s^.value);
	IF s^.value < 0 THEN
	    QueueAndSwitchTasks (s^.blockedlist)
	END (*IF*);
	(*OutTrace ("Wait");*)
    END Wait;

(************************************************************************)

PROCEDURE Signal (VAR (*INOUT*) s: Semaphore);

    (* Increments the semaphore value.  Unblocks one waiting task,	*)
    (* if there was one.						*)

    BEGIN
	(*InTrace ("Signal");*)
	INC (s^.value);
	IF s^.value <= 0 THEN

	    (* Note: the test (s^.value <= 0) is equivalent to	*)
	    (* the test (s^.blockedlist.head <> NIL).		*)

	    MarkAsReady (s^.blockedlist);

	END (*IF*);
	(*OutTrace ("Signal");*)
    END Signal;

(************************************************************************)
(*			    MODULE INITIALISATION			*)
(************************************************************************)

PROCEDURE InitialiseTaskControl;

    (* Must be called before any tasks are created, to initialise the	*)
    (* task control data structures.					*)

    VAR p: PriorityType;  j: TaskNumber;

    BEGIN
	(*InTrace ("InitialiseTaskControl");*)

	(* Collect all the unused task descriptors into a linear list.	*)

	CreateQueue (UnusedDescriptors);
	FOR j := 0 TO MaxTaskNumber DO
	    AddToList (UnusedDescriptors, ADR (TaskTable[j]) );
	END (*FOR*);

	(* The ready list structure is initially empty.  *)

	FOR p := 0 TO MaxPriority DO
	    CreateQueue (ReadyList[p]);
	END (*FOR*);

	(* Create a descriptor for the current task, i.e. for the task	*)
	(* which called this procedure.  We give it a low priority	*)
	(* because this task is often running the background jobs.	*)

	CurrentTaskPointer := TakeFromList (UnusedDescriptors);
	CurrentTaskPointer^.priority := 1;

	(* Create the null task.  It will not run immediately, because	*)
	(* the current (initialisation) task has higher priority.	*)

	CreateTask (NullTask, 0);

	(* We have a working operating system.  It is now safe to	*)
	(* call CreateTask to introduce more tasks to the system.	*)

	(*OutTrace ("InitialiseTaskControl");*)
    END InitialiseTaskControl;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

BEGIN
    InitialiseTaskControl;
END MiniKernel.
