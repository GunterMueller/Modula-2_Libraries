IMPLEMENTATION MODULE Keyboard;

	(****************************************************************)
	(*								*)
	(*			Keyboard Input				*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	9 March 1995				*)
	(*  Status:							*)
	(*	Basic operations are working.				*)
	(*  Known shortcomings:						*)
	(*	Some special combinations - e.g. break, printscreen,	*)
	(*	 scroll lock - not yet handled.				*)
	(*	The F11 and F12 keys of the extended keyboard are not	*)
	(*	 yet handled.						*)
	(*  Known faults:						*)
	(*	When several keys are hit together, spurious characters	*)
	(*	 can result, e.g. the <shift>ER combination produces a	*)
	(*	 spurious "|".  Tests so far suggest that this is a	*)
	(*	 fault in the keyboard controller rather than in my	*)
	(*	 software.						*)
	(*	On some machines, get a spurious NMI (INT 2) on startup.*)
	(*								*)
	(****************************************************************)

(************************************************************************)
(*									*)
(*  This module is rather more complicated than a typical keyboard	*)
(*  input routine, because the keyboard I'm using does not directly	*)
(*  return standard character codes.  Rather, it returns codes known as	*)
(*  "scan codes", and each keypress generates two codes, one on		*)
(*  pressing the key and one on releasing it.  Module KBdriver picks up	*)
(*  the scan codes, the present module translates scan codes to ASCII	*)
(*  codes.  In principle we can detect things like several keys held	*)
(*  down together, but in fact this module does not bother dealing with	*)
(*  such cases except when one of the keys is a shift key or something	*)
(*  similar.  (Software which wants to give special meanings to		*)
(*  multi-key combinations should bypass this module and call KBdriver	*)
(*  to get the scan codes directly).					*)
(*									*)
(*  The code for a key release is the same as the code for a key press	*)
(*  except that the high order bit is set.  (In fact, it's slightly	*)
(*  more complicated than that; the keyboard generates a two-byte code	*)
(*  for a key release, but the keyboard controller turns this into a	*)
(*  one-byte code before the software gets to see it.)  We can		*)
(*  afford to ignore all key releases except for those for the shift,	*)
(*  alt, and control keys.  The Caps Lock and Num Lock keys also	*)
(*  require special handling.						*)
(*									*)
(*  To allow the keyboard user to type ahead, this module contains a	*)
(*  task which puts characters into a circular buffer, where they are	*)
(*  kept until picked up by a call to InKey.  (There are already	*)
(*  type-ahead facilities in module KBdriver, and also in the keyboard	*)
(*  hardware itself; but we might as well be generous to the user, and	*)
(*  in any case having a separate keyboard task instead of doing the	*)
(*  scan code translations in the user task simplifies some aspects of	*)
(*  the software design, because of the way that getting one character	*)
(*  can involve looking at several successive scan codes).		*)
(*									*)
(*  As a protection against deadlock, there is a timeout on the		*)
(*  "circular buffer full" condition.  If the buffer remains full for	*)
(*  too long, the oldest character in the buffer is discarded to make	*)
(*  room for the newest character.					*)
(*									*)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE;

FROM LowLevel IMPORT
    (* type *)	IAND, IOR;

FROM CircularBuffers IMPORT
    (* type *)	CircularBuffer,
    (* proc *)	CreateBuffer, PutBufferImpatient, GetBuffer, BufferEmpty;

FROM KBdriver IMPORT
    (* const *)	ScrollLockLED, NumLockLED, CapsLockLED,
    (* proc *)	GetScanCode, ClearLED, ToggleLED;

(*FROM SoundEffects IMPORT
    (* const *)	Beep;
*)

FROM Semaphores IMPORT
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* proc *)	CreateTask, NotUsingFloatingPoint;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure, Crash;

(************************************************************************)
(*			    TRANSLATION TABLES				*)
(************************************************************************)

    (* Note: in all of these tables a code of 0 means "no code".  Some	*)
    (* of the keys in question are handled as special cases which do	*)
    (* not require table lookup.  Others are simply cases where no	*)
    (* function has been assigned to the key in question.		*)

CONST
    F1code = BYTE(3BH);		(* Scan code for F1 function key	*)
    Delcode = BYTE(53H);	(* Scan code for Del on numeric keypad	*)

TYPE
    (* NormalCodeRange is the range of scan codes handled by our main	*)
    (* translation tables.  Scan codes outside this range can in fact	*)
    (* occur, but they are treated separately.				*)

    NormalCodeRange = [BYTE(0)..BYTE(035H)];

    Table1 = ARRAY NormalCodeRange OF BYTE;
    Table2 = ARRAY [F1code..Delcode] OF BYTE;

    (* Remark: the declarations below are not standard Modula-2.  In	*)
    (* fact I'm not sure how to declare an array of constants in	*)
    (* standard Modula-2, but the compiler I use accepts this.		*)

CONST LowerCase = Table1 (
	(* Nul Esc 123456	*)	 00, 27, 49, 50, 51, 52, 53, 54,
	(* 7890-= Bsp Tab	*)	 55, 56, 57, 48, 45, 61, 08, 09,
	(* qwertyui		*)	113,119,101,114,116,121,117,105,
	(* op[] Enter Ctrl as	*)	111,112, 91, 93, 13, 00, 97,115,
	(* dfghjkl;		*)	100,102,103,104,106,107,108, 59,
	(* '` Lshift \zxcv	*)	 39, 96,  0, 92,122,120, 99,118,
	(* bnm,./		*)	 98,110,109, 44, 46, 47 );

CONST UpperCase = Table1 (
	(* Nul Esc !@#$%^	*)	 00, 27, 33, 64, 35, 36, 37, 94,
	(* &*()_+ Bsp Tab	*)	 38, 42, 40, 41, 95, 43, 08, 09,
	(* QWERTYUI		*)	 81, 87, 69, 82, 84, 89, 85, 73,
	(* OP{} Enter Ctrl AS	*)	 79, 80,123,125, 10, 00, 65, 83,
	(* DFGHJKL:		*)	 68, 70, 71, 72, 74, 75, 76, 58,
	(* "~ Lshift |ZXCV	*)	 34,126,  0,124, 90, 88, 67, 86,
	(* BNM<>?		*)	 66, 78, 77, 60, 62, 63 );

    (* The following three tables handle the function keys F1..F10 and	*)
    (* the numeric keypad keys.  They do not cover F11 and F12, which	*)
    (* exist only on some keyboards.					*)

CONST KeypadTable = Table2 (
	(*       F1F2F3F4F5		*)		 59,60,61,62,63,
	(* F6F7F8F9F10 Num Scr Home	*)	64,65,66,67,68,00,00,71,
	(* Up Pgup - Lft Mid Rgt + End	*)	72,73,45,75,76,77,43,79,
	(* Dn Pgdn Ins Del		*)	80,81,82,83);

CONST ShiftedKeypadTable = Table2 (
	(*       F1F2F3F4F5		*)		 84,85,86,87,88,
	(* F6F7F8F9F10 Num Scr 7	*)	89,90,91,92,93,00,00,55,
	(* 89-456+1			*)	56,57,45,52,53,54,43,49,
	(* 230.				*)	50,51,48,46);

CONST CtrlKeypadTable = Table2 (
	(*       F1F2F3F4F5		*)		 94,95,96,97,98,
	(* F6F7F8F9F10 Num Scr 7	*)	99,100,101,102,103,00,00,119,
	(* 89-456+1			*)	00,132,45,115,00,116,43,117,
	(* 230.				*)	00,118,00,00);

    (* Some of the entries in this last table are, I admit, somewhat	*)
    (* illogical.  My only excuse is that I have tried to be compatible	*)
    (* with existing software for this machine.				*)

(************************************************************************)
(*			THE 'HOT KEY' TABLES				*)
(************************************************************************)

TYPE CharSet = SET OF CHAR;

VAR HotKeys, HotFunctionKeys: CharSet;
    HotKeySemaphore: ARRAY CHAR OF Semaphore;
    HotFunctionKeySemaphore: ARRAY CHAR OF Semaphore;

(************************************************************************)
(*			THE CHARACTER BUFFER				*)
(************************************************************************)

CONST
    CharBufferSize = 8;

VAR
    (* CharBuffer is a circular buffer holding characters.	*)

    CharBuffer:	CircularBuffer;

    (* The state of the three "lock" conditions.  *)

    CapsLock, NumLock, ScrollLock: BOOLEAN;

    (* A semaphore to signal the Ctrl/Alt/Del combination.	*)

    CtrlAltDel: Semaphore;

(************************************************************************)
(*			THE TRANSLATION TASK				*)
(************************************************************************)

PROCEDURE PutCode (FunctionKey: BOOLEAN;  code: CHAR);

    BEGIN
	IF FunctionKey THEN
	    IF code IN HotFunctionKeys THEN
		Signal (HotFunctionKeySemaphore[code]);
	    ELSE
		PutBufferImpatient (CharBuffer, CHR(0), 2000);
		PutBufferImpatient (CharBuffer, code, 2000);
	    END (*IF*);
	ELSE
	    IF code IN HotKeys THEN
		Signal (HotKeySemaphore[code]);
	    ELSE
		PutBufferImpatient (CharBuffer, code, 2000);
	    END (*IF*);
	END (*IF*);
    END PutCode;

(************************************************************************)

PROCEDURE TranslationTask;

    (* This procedure, which is run as a separate task, picks up the	*)
    (* scan codes and turns them into ASCII codes.			*)

    CONST
	(* Scan codes for some special keys.	*)

	SpaceCode = BYTE(39H);		(* space bar *)
	KeypadStarCode = BYTE(37H);	(* "*" on numeric keypad *)
	LeftShiftCode = BYTE(2AH);	(* left shift key *)
	RightShiftCode = BYTE(36H);	(* right shift key *)
	LeftShiftRelease = BYTE(0AAH);	(* left shift key release *)
	RightShiftRelease = BYTE(0B6H);	(* right shift key release *)
	CtrlCode = BYTE(1DH);		(* ctrl key *)
	CtrlRelease = BYTE(9DH);	(* ctrl key release *)
	AltCode = BYTE(38H);		(* alt key *)
	AltRelease = BYTE(0B8H);	(* alt key release *)
	CapsLockCode = BYTE(3AH);	(* caps lock key *)
	CapsLockRelease = BYTE(0BAH);	(* caps lock key release *)
	NumLockCode = BYTE(45H);	(* num lock key *)
	NumLockRelease = BYTE(0C5H);	(* num lock key release *)
	InsCode = BYTE(52H);		(* Ins key *)
	LostCode = BYTE(0FFH);		(* lost keypress *)

    TYPE CharSet = SET OF CHAR;

    VAR scancode: BYTE;  result: CHAR;
	ShiftKeyIsDown, CtrlKeyIsDown, AltKeyIsDown: BOOLEAN;
	CapsLockKeyIsDown, NumLockKeyIsDown: BOOLEAN;

    (********************************************************************)
    (*		The main body of TranslationTask starts here		*)
    (********************************************************************)

    BEGIN
	NotUsingFloatingPoint;
	ShiftKeyIsDown := FALSE;  CtrlKeyIsDown := FALSE;
	AltKeyIsDown := FALSE;  CapsLockKeyIsDown := FALSE;
	NumLockKeyIsDown := FALSE;

	LOOP	(*FOREVER*)
	    scancode := GetScanCode();
	    IF scancode = LostCode THEN
		(*Beep*);

	    (* Check for the shift, ctrl, and alt keys.	*)

	    ELSIF (scancode = LeftShiftCode)
			OR (scancode = RightShiftCode) THEN
		ShiftKeyIsDown := TRUE;
	    ELSIF (scancode = LeftShiftRelease)
			OR (scancode = RightShiftRelease) THEN
		ShiftKeyIsDown := FALSE;
	    ELSIF scancode = CtrlCode THEN
		CtrlKeyIsDown := TRUE;
	    ELSIF scancode = CtrlRelease THEN
		CtrlKeyIsDown := FALSE;
	    ELSIF scancode = AltCode THEN
		AltKeyIsDown := TRUE;
	    ELSIF scancode = AltRelease THEN
		AltKeyIsDown := FALSE;

	    (* The Caps Lock key is a bit harder to deal with.  If the	*)
	    (* user holds it down it will auto-repeat, and we don't	*)
	    (* want this repeating to toggle the Caps Lock state.	*)

	    ELSIF scancode = CapsLockCode THEN
		IF NOT CapsLockKeyIsDown THEN
		    CapsLock := NOT CapsLock;
		    CapsLockKeyIsDown := TRUE;
		    ToggleLED (CapsLockLED);
		END (*IF*);
	    ELSIF scancode = CapsLockRelease THEN
		CapsLockKeyIsDown := FALSE;

	    (* The Num Lock key is treated similarly to Caps Lock.	*)

	    ELSIF scancode = NumLockCode THEN
		IF NOT NumLockKeyIsDown THEN
		    NumLock := NOT NumLock;
		    NumLockKeyIsDown := TRUE;
		    ToggleLED (NumLockLED);
		END (*IF*);
	    ELSIF scancode = NumLockRelease THEN
		NumLockKeyIsDown := FALSE;

	    (* All other codes with the high order bit set can be	*)
	    (* ignored, as they are either key releases which we don't	*)
	    (* need to know about, or they are the "E0" or "E1" codes	*)
	    (* for the extended keyboard, which are not given special	*)
	    (* treatment in this version.				*)

	    ELSIF scancode >= BYTE(80H) THEN
		(* Do nothing *)

	    (* The space bar and the keypad "*" are treated separately,	*)
	    (* because their scan codes fall outside the range of our	*)
	    (* translation tables.					*)

	    ELSIF scancode = SpaceCode THEN
		PutCode (FALSE, " ");
	    ELSIF scancode = KeypadStarCode THEN
		PutCode (FALSE, "*");

	    (* The "normal" case is where the scan code is within the	*)
	    (* range of our translation tables.  In this case we do a	*)
	    (* table lookup, although the result will be affected by	*)
	    (* the state of the various shift keys.			*)

	    ELSIF scancode <= BYTE(MAX(NormalCodeRange)) THEN

		IF ShiftKeyIsDown THEN
		    result := UpperCase[scancode];
		    IF CapsLock AND (result IN CharSet{"A".."Z"}) THEN
			result := LowerCase[scancode];
		    END (*IF*);
		ELSE
		    result := LowerCase[scancode];
		    IF CapsLock AND (result IN CharSet{"a".."z"}) THEN
			result := UpperCase[scancode];
		    END (*IF*);
		END (*IF*);

		(* If the Ctrl key is pressed, strip off the three high	*)
		(* order bits of the character code.			*)

		IF CtrlKeyIsDown THEN
		    result := CHR (IAND(ORD(result), 1FH));
		END (*IF*);

		(* If the Alt key is pressed, set the high order bit of	*)
		(* the character code.  In this we differ from the	*)
		(* convention used by other software for this machine;	*)
		(* but it seems to me to be a useful facility, in that	*)
		(* it gives a simple way of generating the extended	*)
		(* characters supported by the screen hardware.		*)

		IF AltKeyIsDown THEN
		    result := CHR (IOR(ORD(result), 80H));
		END (*IF*);

		PutCode (FALSE, result);

	    (* Check for the numeric keypad and function keys.  In the	*)
	    (* unshifted state, these produce a two-byte result, of	*)
	    (* which the first byte is 0.  However, the result is	*)
	    (* affected by the Ctrl, Shift, and Num Lock keys.		*)
	    (* Special case: the Ctrl/Alt/Del combination is a reset.	*)
	    (* To make this work under OS/2 (which intercepts this	*)
	    (* combination before we get to see it, and which uses it	*)
	    (* to cause a total system crash), we allow Ctrl/Alt/Ins	*)
	    (* as an alternative.					*)

	    ELSIF (scancode >= F1code) AND (scancode <= Delcode) THEN
		IF CtrlKeyIsDown THEN
		    IF ((scancode = Delcode) OR (scancode = InsCode))
						AND AltKeyIsDown THEN
			Signal (CtrlAltDel);  result := CHR(0);
		    ELSE
			result := CtrlKeypadTable[scancode]
		    END (*IF*);
		ELSIF NumLock = ShiftKeyIsDown THEN
		    result := KeypadTable[scancode];
		ELSE
		    result := ShiftedKeypadTable[scancode]
		END (*IF*);

		(* If the result is a numeric or punctuation character,	*)
		(* we produce a simple one-byte result.  The keys which	*)
		(* give a two-byte code always have the second byte	*)
		(* greater than or equal to ";".  Key combinations	*)
		(* which do not produce anything are given a code of 0	*)
		(* in the tables.					*)

		IF result >= ";" THEN
		    PutCode (TRUE, result);
		ELSIF result <> CHR(0) THEN
		    PutCode (FALSE, result);
		END (*IF*);

	    (* In the case of a code not known to us, return a "?" to	*)
	    (* indicate that something unexpected happened.		*)

	    ELSIF scancode >= BYTE(MAX(NormalCodeRange)) THEN
		PutCode (FALSE, "?");

	    END (*IF*);
	END (*LOOP*);
    END TranslationTask;

(************************************************************************)
(*			    THE PUTBACK BUFFER				*)
(************************************************************************)

MODULE PutBackBuffer;

    (* Implementation of the PutBack procedure poses some awkward	*)
    (* problems, to the point where it would not be worth implementing	*)
    (* if it were not such a useful operation.  The obvious solution,	*)
    (* of stuffing characters back into the character buffer, creates	*)
    (* deadlock if we try to avoid losing characters, and creates some	*)
    (* critical section problems even if we accept the risk of losing	*)
    (* characters.  The critical section problems can easily be solved,	*)
    (* but only at the cost of making input less efficient, and this	*)
    (* is hard to justify given that PutBack operations will typically	*)
    (* be infrequent.  (That is, it is undesirable to cripple the	*)
    (* "normal" case just for the sake of supporting a special case	*)
    (* which accounts for just a small proportion of total operations).	*)
    (* The solution adopted in this version is to have a separate data	*)
    (* structure to hold the characters which are put back.  These	*)
    (* characters are held in a "lossy stack" - we discard the oldest	*)
    (* datum whenever the stack is going to overflow.			*)

    EXPORT
	(* var  *)  NoCharsSaved,
	(* proc *)  Push, Pop;

    CONST
	stacksize = 8;

    VAR
	NoCharsSaved: BOOLEAN;
	stackptr: [0..stacksize];
	stack: ARRAY [1..stacksize] OF CHAR;

    (********************************************************************)

    PROCEDURE Push (ch: CHAR);

	(* Pushes ch onto the stack.  If the stack is already full, the	*)
	(* character at the bottom of the stack is lost.		*)

	VAR j: [1..stacksize];

	BEGIN
	    IF stackptr = stacksize THEN
		FOR j := 1 TO stacksize-1 DO
		    stack[j] := stack[j+1];
		END (*FOR*);
		stack[stacksize] := ch;
	    ELSE
		INC (stackptr);  stack[stackptr] := ch;
		NoCharsSaved := FALSE;
	    END (*IF*);
	END Push;

    (********************************************************************)

    PROCEDURE Pop(): CHAR;

	(* Returns the character from the top of the stack.	*)

	VAR result: CHAR;

	BEGIN
	    result := stack[stackptr];  DEC(stackptr);
	    NoCharsSaved := stackptr = 0;
	    RETURN result;
	END Pop;

    (********************************************************************)

    BEGIN
	NoCharsSaved := TRUE;
	stackptr := 0;
    END PutBackBuffer;

(************************************************************************)
(*		THE EXTERNALLY CALLABLE INPUT PROCEDURES		*)
(************************************************************************)

PROCEDURE KeyPressed(): BOOLEAN;

    (* Returns TRUE iff a character is available. *)

    BEGIN
	IF NoCharsSaved THEN RETURN NOT BufferEmpty (CharBuffer)
	ELSE RETURN TRUE
	END (*IF*);
    END KeyPressed;

(************************************************************************)

PROCEDURE InKey(): CHAR;

    (* Reads one key from the circular buffer, or from the putback	*)
    (* buffer if any characters have been put back.			*)

    BEGIN
	IF NoCharsSaved THEN RETURN GetBuffer (CharBuffer)
	ELSE RETURN Pop()
	END (*IF*);
    END InKey;

(************************************************************************)

PROCEDURE PutBack (ch: CHAR);

    (* This is an "un-read" operation, i.e. the character ch will	*)
    (* re-appear on the next call to InKey.  This facility is provided	*)
    (* for the use of software which can overshoot by one character	*)
    (* when reading its input - a situation which can often occur.	*)

    BEGIN
	Push (ch);
    END PutBack;

(************************************************************************)

PROCEDURE StuffKeyboardBuffer (ch: CHAR);

    (* Stores ch as if it had come from the keyboard, so that a		*)
    (* subsequent InKey() will pick it up.				*)

    BEGIN
	PutBufferImpatient (CharBuffer, ch, 2000);
    END StuffKeyboardBuffer;

(************************************************************************)

PROCEDURE SetLocks (code: CARDINAL);

    (* Set/clear the caps lock, num lock, and scroll lock conditions.	*)
    (* The code is defined in KBDRIVER.DEF.				*)

    BEGIN
	CapsLock := ORD(IAND (code, CapsLockLED)) <> 0;
	NumLock := ORD(IAND (code, NumLockLED)) <> 0;
	ScrollLock := ORD(IAND (code, ScrollLockLED)) <> 0;
	ClearLED (CapsLockLED+NumLockLED+ScrollLockLED);
	ToggleLED (BYTE(code));
    END SetLocks;

(************************************************************************)

PROCEDURE LockStatus (): CARDINAL;

    (* Returns the current state of the caps lock, num lock, and scroll	*)
    (* lock conditions, using the code defined in KBDRIVER.DEF.		*)

    VAR result: CARDINAL;

    BEGIN
	result := 0;
	IF CapsLock THEN INC (result, CapsLockLED) END(*IF*);
	IF NumLock THEN INC (result, NumLockLED) END(*IF*);
	IF ScrollLock THEN INC (result, ScrollLockLED) END(*IF*);
	RETURN result;
    END LockStatus;

(************************************************************************)

PROCEDURE HotKey (FunctionKey: BOOLEAN;  code: CHAR;  S: Semaphore);

    (* After this procedure is called, typing the key combination for	*)
    (* 'code' will cause a Signal(S).  Set FunctionKey=TRUE to trap one	*)
    (* of the two-character special function keys, and FALSE otherwise.	*)
    (* The character is consumed; if it should be passed on, then the	*)
    (* user's hot key handler can do a PutBack().  Note: there is no	*)
    (* provision for having multiple hot key handlers for the same key;	*)
    (* any existing hot key mapping will be overridden.			*)

    BEGIN
	IF FunctionKey THEN
	    INCL (HotFunctionKeys, code);
	    HotFunctionKeySemaphore[code] := S;
	ELSE
	    INCL (HotKeys, code);
	    HotKeySemaphore[code] := S;
	END (*IF*);
    END HotKey;

(************************************************************************)
(*				SHUTDOWN				*)
(************************************************************************)

PROCEDURE StopProgram;

    (* This is a task whose only function is to execute a HALT when	*)
    (* the Ctrl/Alt/Del combination is pressed.  We handle this in a	*)
    (* separate task, rather than in the code which detects the		*)
    (* Ctrl/Alt/Del, because we want to keep the keyboard software	*)
    (* alive until after higher-level modules have executed their	*)
    (* termination procedures.						*)

    BEGIN
	NotUsingFloatingPoint;
	Wait (CtrlAltDel);
	Crash ("Ctrl/Alt/Del termination");
    END StopProgram;

(************************************************************************)

PROCEDURE ShutDown;

    (* Final cleanup of this module at program exit. *)

    BEGIN
	SetLocks (0);
    END ShutDown;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

BEGIN
    SetLocks (0);
    HotKeys := CharSet{};
    HotFunctionKeys := CharSet{};
    SetTerminationProcedure (ShutDown);
    CreateBuffer (CharBuffer, CharBufferSize);
    CreateTask (TranslationTask, 8, "Keyboard main");
    CreateSemaphore (CtrlAltDel, 0);
    CreateTask (StopProgram, 15, "CtrlAltDel check");
END Keyboard.
