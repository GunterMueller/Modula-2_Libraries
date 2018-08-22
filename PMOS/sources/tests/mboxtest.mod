MODULE MBoxTest;

	(********************************************************)
	(*							*)
	(*		Test of the mailbox facility		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 June 1993			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM Mailboxes IMPORT
    (* type *)	Mailbox,
    (* proc *)	CreateMailbox, SendMessage, ReceiveMessage;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteLn, ReadChar;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* proc *)	CreateTask;

(************************************************************************)

CONST MessageLimit = 20;
      Esc = CHR(27);

TYPE MessageBuffer = ARRAY [0..MessageLimit-1] OF CHAR;

VAR MB: Mailbox;
    TaskDone: Semaphore;

(************************************************************************)

PROCEDURE ReadString (w: Window;  VAR (*OUT*) buffer: ARRAY OF CHAR;
					VAR (*OUT*) length: CARDINAL);

    (* Reads a string from the keyboard, and reports its length.	*)
    (* Returns when a carriage return or escape is read, or if the	*)
    (* buffer is full.  If an escape character is read, the preceding	*)
    (* input is abandoned and we return with the escape as a		*)
    (* one-character string.						*)

    VAR ch: CHAR;

    BEGIN
	length := 0;
	LOOP
	    ReadChar (w, ch);
	    IF ch = CHR(13) THEN EXIT(*LOOP*) END(*IF*);
	    buffer[length] := ch;  INC(length);
	    IF ch = Esc THEN
		buffer[0] := Esc;  length := 1;
		EXIT (*LOOP*);
	    END(*IF*);
	    IF length > HIGH(buffer) THEN EXIT(*LOOP*) END(*IF*);
	END (*LOOP*);
    END ReadString;

(************************************************************************)

PROCEDURE Producer;

    VAR w: Window;  buffer: MessageBuffer;  length: CARDINAL;

    BEGIN
	OpenWindow (w, blue, cyan, 8, 16, 0, 39, simpleframe, nodivider);
	REPEAT
	    ReadString (w, buffer, length);
	    IF NOT SendMessage (MB, ADR(buffer), length) THEN
		WriteString (w, "Send failure!");
		WriteLn (w);
	    END (*IF*);
	UNTIL buffer[0] = Esc;
	CloseWindow (w);
	Signal (TaskDone);
    END Producer;

(************************************************************************)

PROCEDURE Consumer;

    VAR w: Window;  buffer: MessageBuffer;  length: CARDINAL;

    BEGIN
	OpenWindow (w, white, magenta, 8, 16, 40, 79, simpleframe, nodivider);
	LOOP
	    WriteLn (w);
	    length := ReceiveMessage (MB, buffer, 5000);
	    IF length = 0 THEN
		WriteString (w, "Empty message received!");
	    ELSIF buffer[0] = Esc THEN
		EXIT (*LOOP*);
	    END (*IF*);
	    IF length < MessageLimit THEN
		buffer[length] := CHR(0);
	    END (*IF*);
	    WriteString (w, buffer);
	END (*LOOP*);
	CloseWindow (w);
	Signal (TaskDone);
    END Consumer;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    MB := CreateMailbox (MessageLimit);
    CreateSemaphore (TaskDone, 0);
    CreateTask (Producer, 1, "Producer");
    CreateTask (Consumer, 1, "Consumer");
    Wait (TaskDone);  Wait (TaskDone);
END MBoxTest.
