IMPLEMENTATION MODULE Piano;

	(********************************************************)
	(*							*)
	(*		Play notes from the keyboard		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	15 March 1993			*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

FROM Keyboard IMPORT
    (* proc *)	InKey;

FROM Music IMPORT
    (* proc *)	SetNoteDuration, PlayMusic;

FROM Windows IMPORT
    (* type *)	Window,
    (* proc *)	OpenSimpleWindow, CloseWindow, WriteString, WriteLn;

(************************************************************************)

TYPE ZeroToTwenty = [0..20];
     KeyTable = ARRAY ["a".."z"] OF ZeroToTwenty;
     NoteTable = ARRAY ZeroToTwenty OF ARRAY [0..1] OF CHAR;

CONST
    CodeTable = KeyTable (
			(* ABCDEFGH *)		2, 0, 0, 5, 0, 7, 9, 11,
			(* IJKLMNOP *)		13, 12, 14, 16, 0, 0, 15, 0,
			(* QRSTUVWX *)		1, 6, 4, 8, 0, 0, 3, 0,
			(* YZ       *)		10, 0);

    OutTable = NoteTable (  "R ", "C#", "D ", "D#", "E ", "F ", "F#", "G ",
			    "G#", "A ", "A#", "B ", "C ", "C#", "D ", "D#",
			    "E ", "F ", "F#", "G ", "G#" );

(************************************************************************)

PROCEDURE Code (ch: CHAR): ZeroToTwenty;

    (* Translates a keyboard character into a ZeroToTwenty code. *)

    TYPE CharSet = SET OF CHAR;

    BEGIN
	IF ch IN CharSet {"a".."z"} THEN RETURN CodeTable[ch]
	ELSIF ch = ";" THEN RETURN 17
	ELSIF ch = "[" THEN RETURN 18
	ELSIF ch = "'" THEN RETURN 19
	ELSIF ch = "]" THEN RETURN 20
	ELSE RETURN 0;
	END (*IF*);
    END Code;

(************************************************************************)

PROCEDURE PlayPiano;

    CONST Esc = CHR(27);

    VAR ch: CHAR;  note, lastnote: ZeroToTwenty;  w: Window;

    BEGIN
	lastnote := 12;
	SetNoteDuration (150);
	OpenSimpleWindow (w, 0, 3, 49, 79);
	WriteString (w, "Use the Esc key to return the");  WriteLn (w);
	WriteString (w, "keyboard to normal operation.");
	LOOP
	    ch := InKey();
	    IF ch = Esc THEN EXIT(*LOOP*);
	    ELSIF ch = "b" THEN PlayMusic ("d")
	    ELSIF ch = "n" THEN PlayMusic ("u")
	    ELSE
		note := Code (ch);
		IF note <> 0 THEN

		    IF INTEGER (note) - INTEGER(lastnote) > 6 THEN
			PlayMusic ("u");
		    ELSIF INTEGER(lastnote) - INTEGER(note) > 6 THEN
			PlayMusic ("d");
		    END (*IF*);

		    PlayMusic (OutTable[note]);
		    lastnote := note;
		END (*IF*);
	    END (*IF*);
	END (*LOOP*);
	CloseWindow (w);
    END PlayPiano;

(************************************************************************)

END Piano.
