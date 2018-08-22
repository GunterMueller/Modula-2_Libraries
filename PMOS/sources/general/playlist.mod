IMPLEMENTATION MODULE PlayList;

	(********************************************************)
	(*							*)
	(*	Playing sounds from a list of buffers		*)
	(*							*)
	(*  Programmer:		P. Moylan, based on ideas	*)
	(*			developed by T. Channon		*)
	(*  Last edited:	27 February 1995		*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE;

FROM Types IMPORT
    (* type *)	FarPointer;

FROM PlayBuff IMPORT
    (* proc *)	BufferAddress, StartPlaying, Synch0, Synch1, StopPlaying;

FROM Keyboard IMPORT
    (* proc *)	KeyPressed;

FROM LowLevel IMPORT
    (* proc *)	FarAddOffset, FarCopy, Far;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

(************************************************************************)
(*			    TYPE DEFINITIONS				*)
(************************************************************************)

TYPE
    (* A BufferList is a linked list of buffers. *)

    ListPointer = POINTER TO BlockHeader;
    BlockHeader =   RECORD
			next: ListPointer;
			dataptr: OutputBufferPointer;
		    END (*RECORD*);

    BufferList = POINTER TO RECORD
		     head, tail: ListPointer;
		 END (*RECORD*);

(************************************************************************)
(*			  LIST MANIPULATION				*)
(************************************************************************)

PROCEDURE CreateList (VAR (*OUT*) L: BufferList);

    (* Creates an empty list. *)

    BEGIN
	NEW (L);
	WITH L^ DO
	    head := NIL;  tail := NIL;
	END (*WITH*);
    END CreateList;

(************************************************************************)

PROCEDURE AddToList (VAR (*INOUT*) L: BufferList;  p: OutputBufferPointer);

    (* Appends p^ to L. *)

    VAR elt: ListPointer;

    BEGIN
	NEW (elt);  elt^.next := NIL;  elt^.dataptr := p;
	IF L^.head = NIL THEN L^.head := elt
	ELSE L^.tail^.next := elt
	END (*IF*);
	L^.tail := elt;
    END AddToList;

(************************************************************************)

PROCEDURE DiscardList (VAR (*INOUT*) L: BufferList);

    (* Returns the whole of L to free storage. *)

    VAR next: ListPointer;

    BEGIN
	WHILE L^.head <> NIL DO
	    DISPOSE (L^.head^.dataptr);
	    next := L^.head^.next;
	    DISPOSE (L^.head);
	    L^.head := next;
	END (*WHILE*);
	DISPOSE (L);
    END DiscardList;

(************************************************************************)
(*		    PLAYING FROM A LIST OF BUFFERS			*)
(************************************************************************)

PROCEDURE PlayFromList (L: BufferList);

    (* Real sound play. *)

    VAR LP: ListPointer;
	OutPtr0, OutPtr1: FarPointer;

    BEGIN
	(* We have two output buffers, which we fill alternately.  The	*)
	(* following two pointers point to the two buffers.		*)

	OutPtr0 := BufferAddress();
	OutPtr1 := FarAddOffset (OutPtr0, OutputBufferSize);

	(* Copy the first data block to buffer 0, then start playing.	*)

	LP := L^.head;
	IF LP = NIL THEN RETURN END(*IF*);
	FarCopy (Far(LP^.dataptr), OutPtr0, OutputBufferSize);
	LP := LP^.next;
	StartPlaying;

	LOOP
	    (* Wait for buffer 1 to become available, then fill it.	*)

	    Synch1;
	    IF LP = NIL THEN EXIT END(*IF*);
	    FarCopy (Far(LP^.dataptr), OutPtr1, OutputBufferSize);
	    LP := LP^.next;

	    (* Wait for buffer 0 to become available, then fill it.	*)

	    Synch0;
	    IF (LP = NIL) OR KeyPressed() THEN EXIT END(*IF*);
	    FarCopy (Far(LP^.dataptr), OutPtr0, OutputBufferSize);
	    LP := LP^.next;

	END (*LOOP*);

	StopPlaying;

    END PlayFromList;

(************************************************************************)

END PlayList.
