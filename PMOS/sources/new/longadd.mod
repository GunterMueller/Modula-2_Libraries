MODULE LongAdd;

    (* Quick prototype, crudely tested, undocumented.  PJM 31/8/94. *)

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE;

FROM InOut IMPORT
    (* proc *)	Write;

(************************************************************************)

TYPE
    Digit = [0..9];

    DigitPtr = POINTER TO DigitRecord;
    DigitRecord = RECORD
			value: Digit;
			previous, next: DigitPtr;
		  END (*RECORD*);

    LongNumber = POINTER TO RECORD
				head, tail: DigitPtr;
			    END (*RECORD*);

(************************************************************************)

PROCEDURE CreateLongNumber(): LongNumber;

    VAR result: LongNumber;

    BEGIN
	NEW (result);
	WITH result^ DO
	    head := NIL;  tail := NIL;
	END (*WITH*);
	RETURN result;
    END CreateLongNumber;

(************************************************************************)

PROCEDURE InsertAtHead (newelt: DigitPtr;  number: LongNumber);

    (* Inserts a new element at the head of "number"; the caller is	*)
    (* responsible for filling in the value.				*)

    BEGIN
	WITH newelt^ DO
	    previous := NIL;  next := number^.head;
	END (*WITH*);
	WITH number^ DO
	    head := newelt;
	    IF tail = NIL THEN tail := head END(*IF*);
	END (*WITH*);
    END InsertAtHead;

(************************************************************************)

PROCEDURE InsertAtTail (newelt: DigitPtr;  number: LongNumber);

    (* Inserts a new element at the tail of "number"; the caller is	*)
    (* responsible for filling in the value.				*)

    BEGIN
	WITH newelt^ DO
	    previous := number^.tail;  next := NIL;
	END (*WITH*);
	WITH number^ DO
	    tail := newelt;
	    IF head = NIL THEN head := tail END(*IF*);
	END (*WITH*);
    END InsertAtTail;

(************************************************************************)

PROCEDURE StringToLongNumber (s: ARRAY OF CHAR): LongNumber;

    CONST Nul = CHR(0);

    VAR result: LongNumber;
	j: CARDINAL;  current: DigitPtr;

    BEGIN
	result := CreateLongNumber();
	j := 0;
	WHILE (j <= HIGH(s)) AND (s[j] <> Nul) DO
	    NEW (current);
	    current^.value := ORD(s[j]) - ORD("0");
	    InsertAtTail (current, result);
	    INC (j);
	END (*WHILE*);
	RETURN result;
    END StringToLongNumber;

(************************************************************************)

PROCEDURE WriteLongNumber (num: LongNumber);

    VAR current: DigitPtr;

    BEGIN
	current := num^.head;
	IF current = NIL THEN Write ("0")
	ELSE
	    WHILE current <> NIL DO
		Write (CHR(current^.value+ORD("0")));
		current := current^.next;
	    END (*WHILE*);
	END (*IF*);
    END WriteLongNumber;

(************************************************************************)

PROCEDURE DigitAdd (x, y: Digit;  sum: DigitPtr;
					CarryIn: BOOLEAN): BOOLEAN;

    (* Single digit addition.  The function result is the carry out.	*)

    VAR result: CARDINAL;

    BEGIN
	result := x + y;
	IF CarryIn THEN INC(result) END(*IF*);
	IF result < 10 THEN
	    sum^.value := result;
	    RETURN FALSE;
	ELSE
	    sum^.value := result - 10;
	    RETURN TRUE;
	END (*IF*);
    END DigitAdd;

(************************************************************************)

PROCEDURE Add (first, second: LongNumber): LongNumber;

    (* Adds two numbers.  A bit too complicated, we ought to be able	*)
    (* to use some hindsight to simplify this.				*)

    VAR result: LongNumber;  shorter, longer, current: DigitPtr;
	carry: BOOLEAN;

    BEGIN
	result := CreateLongNumber();
	longer := first^.tail;
	shorter := second^.tail;
	carry := FALSE;
	WHILE shorter <> NIL DO
	    IF longer = NIL THEN
		longer := shorter;  shorter := NIL;
	    ELSE
		NEW (current);
		InsertAtHead (current, result);
		carry := DigitAdd (shorter^.value, longer^.value,
						current, carry);
		shorter := shorter^.previous;  longer := longer^.previous;
	    END (*IF*);
	END (*WHILE*);

	(* We've now traversed the shorter input list, but there	*)
	(* might be some of the longer still left.			*)

	WHILE longer <> NIL DO
	    NEW (current);
	    InsertAtHead (current, result);
	    carry := DigitAdd (0, longer^.value, current, carry);
	    longer := longer^.previous;
	END (*WHILE*);

	(* Allow for a final carry. *)

	IF carry THEN
	    NEW (current);
	    InsertAtHead (current, result);
	    current^.value := 1;
	END (*IF*);

	RETURN result;

    END Add;

(************************************************************************)

VAR first, second, sum: LongNumber;

BEGIN

    (* Test program. *)

    first := StringToLongNumber ("54321");
    second := StringToLongNumber ("42");
    sum := Add (first, second);
    WriteLongNumber (sum);

END LongAdd.
