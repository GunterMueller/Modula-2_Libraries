MODULE SemaTest;

	(********************************************************)
	(*							*)
	(*	Test program to check that the semaphore	*)
	(*	operations work.				*)
	(*							*)
	(*	Programmer:	P. Moylan			*)
	(*	Last edited:	9 February 1995    	      	*)
	(*							*)
	(*	Description:	Runs a pair of tasks in a	*)
	(*		simple producer/consumer relationship.	*)
	(*					                *)
	(*	Status:						*)
	(*		Working, except for a bug in		*)
	(*		tracing: attempting to trace kernel	*)
	(*		causes infinite recursion.  For now	*)
	(*		kernel tracing has been removed, but	*)
	(*		given the intended use of this program	*)
	(*		we should find a mechanism for kernel	*)
	(*		tracing.				*)
	(*							*)
	(********************************************************)


FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteLn, Write,
		PressAnyKey, ReadChar;

FROM Trace IMPORT
    (* proc *)	TraceOn;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* proc *)	CreateTask, TaskExit;

FROM CircularBuffers IMPORT
    (* type *)	CircularBuffer,
    (* proc *)	CreateBuffer, PutBuffer, GetBuffer;

(************************************************************************)

CONST Esc = CHR(01BH);

VAR

    (* The consumer and producer tasks will communicate through a	*)
    (* circular buffer called "pipe".					*)

    pipe: CircularBuffer;

    (* logwindow is a screen window used for status messages.		*)

    logwindow: Window;

    (* The "finished" semaphore is needed so the main task will know	*)
    (* when the consumer and producer tasks have both exited.		*)

    finished: Semaphore;

(************************************************************************)
(*			THE PRODUCER AND CONSUMER TASKS			*)
(************************************************************************)

PROCEDURE Producer;

    (* The producer task.  Takes input from the keyboard, sends it to	*)
    (* the intertask buffer where it will be picked up by the consumer	*)
    (* task.  Exits when a second Esc key is received (the first is	*)
    (* sent to the consumer task).					*)

    VAR datum: CHAR;  EscSent: BOOLEAN;
	pwindow: Window;

    BEGIN
	OpenWindow (pwindow, red, green, 4, 11, 0, 39, simpleframe, nodivider);
	WriteString (pwindow, "Starting producer task.");
	WriteLn (pwindow);
	EscSent := FALSE;

	LOOP
	    ReadChar (pwindow, datum);
	    IF datum = Esc THEN
		IF EscSent THEN EXIT(*LOOP*) END (*IF*);
		EscSent := TRUE;
	    END (*IF*);
	    PutBuffer (pipe, datum);
	END (*LOOP*);

	WriteString (pwindow, "Producer task is now terminating.");
	WriteLn (pwindow);
	CloseWindow (pwindow);
	Signal (finished);
	TaskExit;
    END Producer;

(************************************************************************)

PROCEDURE Consumer;

    (* The consumer task.  Takes input from the intertask buffer and	*)
    (* prints it, changing lower case letters to upper case.		*)
    (* Exits when an Esc character has been received.			*)

    VAR datum: CHAR;
	cwindow: Window;

    BEGIN
	OpenWindow (cwindow, blue, cyan, 7,14, 30,69, simpleframe, nodivider);
	WriteString (cwindow, "Starting consumer task.");
	WriteLn (cwindow);
	LOOP
	    datum := GetBuffer(pipe);
	    IF (datum >= 'a') AND (datum <= 'z') THEN
		datum := CHR (ORD(datum)-ORD('a')+ORD('A'))
	    END (*IF*);
	    IF datum = Esc THEN EXIT(*LOOP*) END(*IF*);
	    Write (cwindow, datum);
	END (*LOOP*);
	WriteLn (cwindow);
	WriteString (cwindow, "Consumer task is now terminating.");
	WriteLn (cwindow);
	CloseWindow (cwindow);
	Signal (finished);
	TaskExit;
    END Consumer;

(************************************************************************)
(*			    THE TEST PROCEDURE				*)
(************************************************************************)

PROCEDURE RunTheTest;

    CONST ProducerPriority = 1; ConsumerPriority = 1;

    VAR taskcount: CARDINAL;

    BEGIN
	CreateBuffer (pipe, 8);
	CreateSemaphore (finished, 0);
	taskcount := 0;

	CreateTask (Consumer, ConsumerPriority, "Consumer");
	INC (taskcount);
	WriteString (logwindow, "Have created consumer task.");
	WriteLn (logwindow);

	CreateTask (Producer, ProducerPriority, "Producer");
	INC (taskcount);
	WriteString (logwindow, "Have created producer task.");
	WriteLn (logwindow);

	WHILE taskcount > 0 DO
	    Wait (finished);  DEC(taskcount);
	END (*WHILE*);

	WriteString (logwindow,
			"All tasks terminated - back in RunTheTest.");
	WriteLn (logwindow);
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

    BEGIN
	(*TraceOn (10, 24, 0, 79, 50);*)
	OpenWindow (logwindow, white, black, 0, 3, 0, 49,
					noframe, nodivider);
	WriteString (logwindow, "Test of semaphore operations.");
	WriteLn (logwindow);
	RunTheTest;
	WriteString (logwindow, "End of test.");
	PressAnyKey (logwindow);
	CloseWindow (logwindow);
    END SemaTest.
