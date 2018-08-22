IMPLEMENTATION MODULE SoundEffects;

	(********************************************************)
	(*							*)
	(*	Procedures to produce audible output.		*)
	(*							*)
	(*	Programmer:	P. Moylan			*)
	(*	Last edited:	9 March 1995			*)
	(*	Status:		Working				*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* proc *)	ADR;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)	OutByte, LowByte, HighByte;

FROM Queues IMPORT
    (* type *)	Queue,
    (* proc *)	CreateQueue, AddToQueue, TakeFromQueue, Empty;

FROM Semaphores IMPORT
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* proc *)	CreateTask, NotUsingFloatingPoint;

FROM Timer IMPORT
    (* proc *)	Sleep;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

(************************************************************************)

CONST
    OffCode = 0CH;  OnCode = 0FH;

TYPE

    (* The following declaration is the closest we can come in Modula-2	*)
    (* to specifying the generic type "ARRAY OF Note".  It means, of	*)
    (* course, that we have to disable array bound checking when	*)
    (* compiling this module.						*)

    NoteArray = ARRAY [0..0] OF Note;

    NoteArrayPointer = POINTER TO NoteArray;

    SemaphorePointer = POINTER TO Semaphore;

    QueuePointer = POINTER TO QueueElement;

    (* The music waiting to be played is ordered by having a queue of	*)
    (* waiting entries.  Each element of the queue is a record with a	*)
    (* pointer to an array of music data, the last subscript of that	*)
    (* array (the first subscript is always assumed to be zero), and a	*)
    (* pointer to the semaphore on which we will perform a Signal to	*)
    (* indicate that we have finished working on this section of data.	*)
    (* Notice that the queue is not directly a queue of data.  Rather, 	*)
    (* it is a queue of pointers to data.				*)

    QueueElement =  RECORD
			dataptr: NoteArrayPointer;
			lastsubscript: CARDINAL;
			CompletionSemaphoreAddress: SemaphorePointer;
		    END (*RECORD*);

(************************************************************************)

VAR

    (* PlayQueue is the actual queue.	*)

    PlayQueue: Queue;

    (* beep holds the data to be used to produce a "beep" noise.	*)

    beep: ARRAY [0..1] OF Note;

    (* The completion semaphore used when doing the beep.	*)

    BeepSem: Semaphore;

(************************************************************************)

PROCEDURE Play (VAR (*IN*) playdata: ARRAY OF Note;
			VAR (*INOUT*) done: Semaphore);

    (* Adds the array to the list of music queued up waiting to be	*)
    (* played.  The actual playing is handled by a separate task - see	*)
    (* PlayerTask later in this module.  On return from this procedure,	*)
    (* the playing is not necessarily over.  The caller must perform a	*)
    (* Wait(done) to know when the array playdata is no longer in use.	*)

    VAR elementpointer: QueuePointer;

    BEGIN
	NEW (elementpointer);
	WITH elementpointer^ DO
	    dataptr := ADR (playdata);  lastsubscript := HIGH (playdata);
	    CompletionSemaphoreAddress := ADR(done);
	END (*WITH*);
	AddToQueue (PlayQueue, elementpointer);
    END Play;

(************************************************************************)

PROCEDURE Beep;

    (* Produces a short "beep" noise.	*)

    BEGIN
	Wait (BeepSem);
	Play (beep, BeepSem);
    END Beep;

(************************************************************************)

PROCEDURE PlayerTask;

    (* This is the procedure which does all the real work.  It runs as	*)
    (* a separate task, which typically spends most of its time		*)
    (* blocked while waiting for something to play.  Even while it is	*)
    (* playing something, it spends most of its time sleeping, because	*)
    (* the timer hardware does most of its work for it.			*)

    (* A duration code of 0 indicates the end of the data, in cases	*)
    (* where the data do not fill the entire array.			*)
    (* A period code of 1, with a nonzero duration, indicates a rest.	*)

    CONST TimerControlPort = 67;  channel2 = 66;

    VAR arrayptr: NoteArrayPointer;  j, top, Period, Duration: CARDINAL;
	doneaddress: SemaphorePointer;  qptr: QueuePointer;

    BEGIN
	NotUsingFloatingPoint;
	LOOP	(* forever *)

	    (* Note that this task will remain blocked inside procedure	*)
	    (* TakeFromQueue while there is nothing to play.		*)

	    qptr := TakeFromQueue (PlayQueue);
	    WITH qptr^ DO
		arrayptr := dataptr;
		top := lastsubscript;
		doneaddress := CompletionSemaphoreAddress;
	    END (*WITH*);
	    DISPOSE (qptr);

	    (* Turn on the speaker.  This is done by setting the two	*)
	    (* low-order bits of port 97.				*)

	    OutByte (97, OnCode);

	    j := 0;

	    LOOP
		(*# save, check(index=>off) *)
		Duration := arrayptr^[j].duration;
		Period := arrayptr^[j].period;
		(*# restore *)

		(* A duration code of 0 indicates the end of the data.	*)

		IF Duration = 0 THEN
		    EXIT(*LOOP*);
		END (*IF*);

		IF Period = 1 THEN

		    (* A rest has been requested.  Turn off the speaker	*)
		    (* and sleep for a while, then turn on the speaker.	*)

		    OutByte (97, OffCode);
		    Sleep (Duration);
		    OutByte (97, OnCode);

		ELSE

		    (* We have a normal note to play.  The control byte	*)
		    (* sent to the timer control port specifies: timer	*)
		    (* #2, two count bytes to follow, and mode 3	*)
		    (* (square wave).					*)

		    OutByte (TimerControlPort, 0B6H);
		    OutByte (channel2, LowByte(Period));
		    OutByte (channel2, HighByte(Period));

		    Sleep (Duration);

		END (*IF*);

		IF j = top THEN EXIT(*LOOP*) END(*IF*);
		INC (j);

	    END (*LOOP*);

	    (* Turn off the speaker.	*)

	    OutByte (97, OffCode);

	    (* Tell the user task that we have finished with this	*)
	    (* buffer-full of data.					*)

	    Signal (doneaddress^);

	END (*LOOP*);
    END PlayerTask;

(************************************************************************)
(*			MODULE TERMINATION				*)
(************************************************************************)

PROCEDURE CloseDown;

    (* Brings the module to an orderly halt.	*)

    VAR qptr: QueuePointer;

    BEGIN
	WHILE NOT Empty(PlayQueue) DO
	    qptr := TakeFromQueue (PlayQueue);
	    Signal (qptr^.CompletionSemaphoreAddress^);
	    DISPOSE (qptr);
	END (*WHILE*);

	(* Turn off the speaker. *)

	OutByte (97, OffCode);

    END CloseDown;

(************************************************************************)
(*			MODULE INITIALISATION				*)
(************************************************************************)

BEGIN

    (* Create an initially empty queue of music to be played.	*)

    CreateQueue (PlayQueue);

    (* Define the parameters of a "beep" noise.	*)

    beep[0].period := 1000;
    beep[0].duration := 40;
    beep[1].period := 1;
    beep[1].duration := 20;
    CreateSemaphore (BeepSem, 1);

    SetTerminationProcedure (CloseDown);

    (* Create the task which plays the music.	*)

    CreateTask (PlayerTask, 10, "Sound effects");

END SoundEffects.
