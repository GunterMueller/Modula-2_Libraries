IMPLEMENTATION MODULE Play3S;

	(********************************************************)
	(*							*)
	(*		    3-part music			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(*	Needs to be customised for the processor	*)
	(*	model and speed.  See the comments on		*)
	(*	"Tuning parameters" below.			*)
	(*							*)
	(*	Note: won't work satisfactorily under OS/2.	*)
	(*							*)
	(********************************************************)

FROM Keyboard IMPORT
    (* proc *)	KeyPressed;

FROM MiscPMOS IMPORT
    (* proc *)	EnterCriticalSection, LeaveCriticalSection;

FROM LowLevel IMPORT
    (* proc *)	HighByte, MakeWord, OutByte;

FROM MATHLIB IMPORT
    (* proc *)	Sin;

(************************************************************************)
(*			     TUNING PARAMETERS				*)
(*									*)
(*	The constant "LowestInterval" may need to be adjusted to	*)
(*	suit your processor speed.  It controls the frequency of the	*)
(*	lowest note played.  Adjust it upwards if all notes sound	*)
(*	too low, or downwards if all notes sound too high.  Note that	*)
(*	making it too high will make the very highest notes		*)
(*	unplayable because of aliasing effects.  On no account should	*)
(*	this constant be set greater than 900.				*)
(*									*)
(*	The constant TimeScale controls the note durations.  It also	*)
(*	needs to be adjusted to compensate for processor speed.		*)
(*									*)
(*	The values I've found that sound about right are:		*)
(*									*)
(*				LowestInterval		TimeScale	*)
(*		33MHz 486:	   60.0			   60		*)
(*		DX2/50:		   60.0 (?)		  110		*)
(*									*)
(*	These are rough values only.  I've made no attempt to tune to	*)
(*	any sort of absolute reference.  Note that the "best" value	*)
(*	for LowestInterval depends not only on processor speed but	*)
(*	also on what sort of speaker is installed on the machine.	*)
(*	A certain amount of tweaking is needed simply because the	*)
(*	speaker was not really designed for playing music.		*)
(*									*)
(************************************************************************)

CONST
    LowestInterval = 60.0;
    TimeScale = 110;

TYPE Note = SHORTCARD [0..63];

(************************************************************************)
(*				GLOBAL DATA				*)
(************************************************************************)

VAR
    (* NoteToIntervalMap takes a note code and translates it to the	*)
    (* step amount to be used in stepping through a WaveForm array.	*)

    NoteToIntervalMap: ARRAY Note OF CARDINAL;

    (* CurrentDuration says how long to play each note. *)

    CurrentDuration: CARDINAL;

    (* Table defining one period of the waveform to be played. *)

    Wave: ARRAY SHORTCARD OF SHORTCARD;

    (* The following three variables are subscripts into the waveform	*)
    (* table.  To be more precise, the high-order byte is used as a	*)
    (* subscript, with the low-order byte acting as a non-integer	*)
    (* part of the subscript.						*)

    Pointer1, Pointer2, Pointer3: CARDINAL;

(************************************************************************)
(*			SOME TECHNICAL DETAILS				*)
(*									*)
(* The speaker is controlled by the two low-order bits of port 97.	*)
(* Bit 0 enables the timer output to the speaker, and bit 1 turns the	*)
(* speaker on.  This module does not use the timer, because all that	*)
(* can do is create fixed-frequency square waves, and we want more	*)
(* flexible waveform control.  What we do instead is control the	*)
(* speaker by direct manipulation of bit 1.  By using pulse-width	*)
(* modulation techniques, we achieve the equivalent of 3-bit analogue	*)
(* output.  (The choice of 3 bits is the result of a compromise.  More	*)
(* bits give better resolution, but reduce the sampling rate to the	*)
(* point where spurious high-frequency components in the signal become	*)
(* too obvious.  I've tried this with 4 bits, and my subjective		*)
(* judgment is that the result is not quite as good as with 3 bits.)	*)
(*									*)
(************************************************************************)

PROCEDURE Play3Notes (note1, note2, note3: SHORTCARD);

    (* Plays the three notes simultaneously. *)

    CONST off = 0CH;  on = off + 2;

    VAR savedPSW, interval1, interval2, interval3: CARDINAL;
	time, t2: CARDINAL;  level: SHORTCARD;

    BEGIN
	interval1 := NoteToIntervalMap[note1];
	interval2 := NoteToIntervalMap[note2];
	interval3 := NoteToIntervalMap[note3];

	savedPSW := EnterCriticalSection();

	FOR t2 := 0 TO TimeScale DO
	    FOR time := 0 TO CurrentDuration DO

		(* In the following incrementations, integer overflow	*)
		(* is not an error - we want the pointers to wrap.	*)

		(*# save, check(overflow => off) *)
		INC (Pointer1, interval1);
		INC (Pointer2, interval2);
		INC (Pointer3, interval3);
		(*# restore *)

		level := Wave[HighByte(Pointer1)] + Wave[HighByte(Pointer2)]
				+ Wave[HighByte(Pointer3)];

		(* Now for the tricky part.  What we would like to do	*)
		(* is to put out this 8-bit level to the speaker; but	*)
		(* the speaker accepts only a 1-bit output, so we have	*)
		(* to compromise.  In the present version, we put out a	*)
		(* PWM signal whose average is based on the high-order	*)
		(* three bits of the level.  If we do this in assembly	*)
		(* language we can go to four or five significant bits,	*)
		(* but my experience has been that the assembly		*)
		(* language version does not sound any better.		*)

		CASE level DIV 32 OF
		|  0:	OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
		|  1:	OutByte (97, on);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
		|  2:	OutByte (97, on);
			OutByte (97, on);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
		|  3:	OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
		|  4:	OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
		|  5:	OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, off);
			OutByte (97, off);
			OutByte (97, off);
		|  6:	OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, off);
			OutByte (97, off);
		|  7:	OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, on);
			OutByte (97, off);
		END (*CASE*);

	    END (*FOR*);
	END (*FOR*);

	LeaveCriticalSection (savedPSW);

    END Play3Notes;

(************************************************************************)

PROCEDURE Play (VAR (*IN*) A: ARRAY OF BYTE);

    (* Plays the music encoded in array A.  The encoding is explained	*)
    (* in the comments in the definition module.			*)

    VAR j: CARDINAL;  datum: SHORTCARD;

    BEGIN
	j := 0;
	LOOP
	    IF (j > HIGH(A)) OR KeyPressed() THEN EXIT(*LOOP*) END(*IF*);
	    datum := A[j];  INC(j);
	    IF datum < 64 THEN
		Play3Notes (datum, A[j], A[j+1]);
		INC (j, 2);
	    ELSE
		CASE datum OF
		  |  253:	(* change duration relative *)
			CurrentDuration := CurrentDuration*CARDINAL(A[j])
						DIV CARDINAL(A[j+1]);
			INC (j, 2);
		  |  254:	(* change duration absolute *)
			CurrentDuration := MakeWord (A[j+1], A[j]);
			INC (j, 2);
		  |  255:	(* end of music *)
			EXIT (*LOOP*);
		  |  ELSE
			INC (j, 2);

		END (*CASE*);
	    END (*IF*);
	END (*LOOP*);

	(* Turn off the speaker.	*)

	OutByte (97, 0CH);

    END Play;

(************************************************************************)
(*				INITIALISATION				*)
(************************************************************************)

PROCEDURE SetupArrays;

    (* Initialises the global data. *)

    CONST PI = 3.141592653589793240;
	  freqstep = 1.057298094605;	(* 2^(1/12) *)

    VAR step: LONGREAL;
	j: SHORTCARD;  note: Note;

    BEGIN
	(* Create the note-to-interval map. *)

	step := LowestInterval;
	NoteToIntervalMap[0] := 0;
	FOR note := 1 TO MAX(Note) DO
	    NoteToIntervalMap[note] := VAL(CARDINAL, step);
	    step := step*freqstep;
	END (*FOR*);

	(* Create the waveform array. *)

	(* Use this code if a sine wave is desired. *)
(*
	FOR j := 0 TO 255 DO
	    Wave[j] := VAL(SHORTCARD,
			42.0*(1.0 + Sin(VAL(LONGREAL,j)*PI/128.0)));
	END (*FOR*);
*)
	(* Alternative: use this to get a square wave. *)

	FOR j := 0 TO 127 DO
	    Wave[j] := 85;
	END (*FOR*);

	FOR j := 128 TO 255 DO
	    Wave[j] := 0;
	END (*FOR*);

    END SetupArrays;

(************************************************************************)

BEGIN
    CurrentDuration := 100;
    Pointer1 := 0;  Pointer2 := 0;  Pointer3 := 0;
    SetupArrays;
END Play3S.
