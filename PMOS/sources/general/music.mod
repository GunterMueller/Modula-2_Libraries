IMPLEMENTATION MODULE Music;

	(********************************************************)
	(*							*)
	(*		Module to play music.			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	26 July 1994			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

IMPORT Keyboard;		(* To enable Ctrl/Alt/Del detection *)

FROM SoundEffects IMPORT
    (* type *)	Note,
    (* proc *)	Play;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

(************************************************************************)

CONST
    Rest = 1;			(* code for no sound *)
    irrelevantnote = 12;	(* used when remembering last note	*)

TYPE
    buffersubscript = [0..10];
    ZeroToEleven = [0..11];
    Octave = [0..8];
    buffernumber = [0..1];

VAR
    (* CurrentDuration and CurrentOctave are the current duration of a	*)
    (* note and the octave in which we are playing, respectively.	*)

    CurrentDuration: CARDINAL;
    CurrentOctave: Octave;

    (* NoteTable performs the mapping C -> 0, D -> 2, E -> 4, F -> 5,	*)
    (* etc.  The gaps in this sequence are accounted for by sharps and	*)
    (* flats, which are handled elsewhere.				*)

    NoteTable: ARRAY ["A".."G"] OF ZeroToEleven;

    (* LastNote is either the ZeroToEleven code for the last note	*)
    (* played, or it has the special value "irrelevantnote".  It is	*)
    (* used when deciding when to change octaves.			*)

    LastNote: CARDINAL;

    (* PeriodTable converts the ZeroToEleven note code into a period	*)
    (* code as required by module SoundEffects.				*)

    PeriodTable: ARRAY ZeroToEleven OF CARDINAL;

    (* The arrays buffer[0] and buffer[1] each hold data to be send to	*)
    (* module SoundEffects.  We use them alternately, so that one can	*)
    (* be filling up while the other is being emptied.  This frees the	*)
    (* caller from having to worry about synchronisation problems, and	*)
    (* lets the caller stay far enough ahead of the actual playing to	*)
    (* make it unlikely that there will be gaps in the sound output due	*)
    (* to delays in the caller task.					*)
    (* We have to make the buffers global, even though they are used by	*)
    (* only one procedure in this module, because they remain in use	*)
    (* between calls to PlayMusic.					*)

    buffer: ARRAY buffernumber OF ARRAY buffersubscript OF Note;

    (* Variable currentbuffer keeps track of whether we are currently	*)
    (* putting data in buffer[0] or buffer[1].				*)

    currentbuffer: buffernumber;

    (* FreeBuffer is a counting semaphore, which keeps track of how	*)
    (* many empty output buffers are currently available.		*)

    FreeBuffer: Semaphore;

(************************************************************************)

PROCEDURE SetNoteDuration (D: CARDINAL);

    (* Sets the duration of each of the following notes, until further	*)
    (* notice, to D milliseconds.  The precision of this setting is	*)
    (* limited by the clock interrupt frequency used in module Timer;	*)
    (* the resolution can be as poor as 1/9 second.  The duration can	*)
    (* subsequently be modified by the * and / options (see PlayMusic),	*)
    (* or by another call to SetNoteDuration.				*)

    BEGIN
	CurrentDuration := D;
    END SetNoteDuration;

(************************************************************************)

PROCEDURE Translate (VAR (*IN*) notes: ARRAY OF CHAR;
			VAR (*INOUT*) place: CARDINAL;
			VAR (*OUT*) period: CARDINAL);

    (* Translates a note code in the conventional notation (C,D,etc.),	*)
    (* as stored in notes[place], into the numeric period code expected	*)
    (* by module SoundEffects.  Parameter place is updated, so that on	*)
    (* return notes[place] is the first character not processed by this	*)
    (* procedure.							*)

    VAR note: ZeroToEleven;
	j: Octave;

    BEGIN
	IF notes[place] = "R" THEN
	    period := Rest;  INC (place);
	ELSE
	    note := NoteTable[notes[place]];
	    INC (place);
	    IF place <= HIGH(notes) THEN

		(* Check for sharp or flat. *)

		IF notes[place] = "#" THEN
	    	    INC(note);  INC(place);
		ELSIF notes[place] = "b" THEN
	    	    DEC(note);  INC(place);
		END (*IF*);

	    END (*IF*);

	    (* We now have the note in the form of a ZeroToEleven code,	*)
	    (* but we still have to decide the octave.  The criterion	*)
	    (* we use is to minimise the distance from the last note	*)
	    (* played.  This can be overridden by the "u" and "d" codes.*)

	    IF LastNote <> irrelevantnote THEN
		IF INTEGER(note)-INTEGER(LastNote) > 6 THEN
		    IF CurrentOctave > 0 THEN DEC(CurrentOctave) END(*IF*);
		ELSIF INTEGER(LastNote) - INTEGER(note) > 6 THEN
		    IF CurrentOctave < MAX(Octave) THEN
			INC(CurrentOctave)
		    END (*IF*);
		END (*IF*);
	    END (*IF*);
	    LastNote := note;
	    period := PeriodTable[note];
	    FOR j := 1 TO CurrentOctave DO
		period := period DIV 2;
	    END (*FOR*);
	END (*IF*);
    END Translate;

(************************************************************************)

PROCEDURE PlayMusic (notes: ARRAY OF CHAR);

    (* Plays the tune specified in array "notes".  The playing is done	*)
    (* asynchronously; that is, this procedure returns before the music	*)
    (* is over.  However, a return from this procedure does imply that	*)
    (* array "notes" can be re-used or destroyed; the notes might not	*)
    (* yet have been played, but the data necessary to play them have	*)
    (* been processed and the necessary information stored.		*)

    VAR	inplace: CARDINAL;  outplace: buffersubscript;

    BEGIN
	inplace := 0;

	(* On entry to this procedure, currentbuffer is the number of	*)
	(* the last buffer which was last sent to procedure Play.  The	*)
	(* Wait(FreeBuffer) which we are about to execute is, because	*)
	(* we are using a double-buffered approach, a wait for the	*)
	(* second-last buffer which was sent to Play.  After the Wait,	*)
	(* we can be sure that that alternate buffer is available.	*)

	Wait (FreeBuffer);
	currentbuffer := 1 - currentbuffer;  outplace := 0;

	(* The following loop translates from letter codes in array	*)
	(* "notes" to numeric codes in array buffer[currentbuffer],	*)
	(* calling SoundEffects.Play each time the output buffer fills	*)
	(* up.  At such times, we switch to the other buffer (after	*)
	(* waiting for it to be emptied).				*)

	REPEAT

	    IF notes[inplace] = " " THEN INC(inplace)	(* ignore spaces *)

	    (* Check for duration changes.	*)

	    ELSIF notes[inplace] = "*" THEN
		CurrentDuration := 2*CurrentDuration;  INC(inplace);
	    ELSIF notes[inplace] = "3" THEN
		CurrentDuration := CurrentDuration DIV 3;  INC(inplace);
	    ELSIF notes[inplace] = "/" THEN
		CurrentDuration := CurrentDuration DIV 2;  INC(inplace);

	    (* Check for octave changes.	*)

	    ELSIF notes[inplace] = "u" THEN
		IF CurrentOctave < MAX(Octave) THEN
		    INC(CurrentOctave);
		END (*IF*);
		INC (inplace);
	    ELSIF notes[inplace] = "d" THEN
		IF CurrentOctave > 0 THEN DEC(CurrentOctave) END(*IF*);
		INC (inplace);
	    ELSE

		(* We have a note to play.			*)
		(* N.B. procedure Translate updates inplace.	*)

		Translate (notes, inplace,
				buffer[currentbuffer][outplace].period);
		buffer[currentbuffer][outplace].duration := CurrentDuration;

		IF outplace = HIGH(buffer[currentbuffer]) THEN

		    (* The current buffer is full, so send that batch	*)
		    (* of data to procedure Play.			*)

		    Play (buffer[currentbuffer], FreeBuffer);

		    (* Wait, if necessary, for the other buffer to	*)
		    (* become available, and switch to using it.	*)

		    Wait (FreeBuffer);
		    currentbuffer := 1 - currentbuffer;  outplace := 0;

		ELSE
		    INC (outplace);
		END (*IF*);

	    END (*IF*);
	UNTIL (inplace > HIGH(notes)) OR (notes[inplace] = CHR(0));

	(* On exit from the above loop, we still have a partially	*)
	(* filled final buffer to send to Play.  There is a special	*)
	(* case where the "partially filled buffer" is in fact empty,	*)
	(* but Play can handle that case.				*)

	buffer[currentbuffer][outplace].duration := 0;(* to mark end of data*)
	Play (buffer[currentbuffer], FreeBuffer);

    END PlayMusic;

(************************************************************************)

PROCEDURE WaitForMusicFinished;

    (* Blocks the calling task until there is no more music playing.	*)
    (* This is a guard against things like premature task termination.	*)

    BEGIN

	(* Put in a claim for both of the output buffers, i.e. wait	*)
	(* until they are both unused.					*)

	Wait (FreeBuffer);  Wait (FreeBuffer);

	(* Now release them, because we didn't really want to use them.	*)

	Signal (FreeBuffer);  Signal (FreeBuffer);

    END WaitForMusicFinished;

(************************************************************************)
(*			MODULE INITIALISATION				*)
(************************************************************************)

PROCEDURE Initialise;

    (* Sets the initial duration and octave, and sets up the tables	*)
    (* for translating from music notation to duration codes.		*)

    CONST scale = 0.943874313;		(*  2^(-1/12)	*)

    VAR period: REAL;  j: ZeroToEleven;

    BEGIN
	CurrentDuration := 3;  CurrentOctave := 4;
	LastNote := irrelevantnote;

	(* NoteTable translates from letter codes into a numeric code	*)
	(* consistent with a well-tempered tuning.			*)

	NoteTable["C"] := 0;  NoteTable["D"] := 2;
	NoteTable["E"] := 4;  NoteTable["F"] := 5;
	NoteTable["G"] := 7;  NoteTable["A"] := 9;
	NoteTable["B"] := 11;

	(* For PeriodTable, we give the lowest (octave 0) C a frequency	*)
	(* which is the lowest frequency which can be handled, and then	*)
	(* derive all other frequencies by successive scaling by the	*)
	(* twelfth root of 2.  Our base frequency is about one note out	*)
	(* when compared with the standard A4 = 440Hz tuning, but in	*)
	(* this version I take that as an acceptable imperfection.	*)
	(* To repair this flaw, we would have to take into account the	*)
	(* fact that C in octave 0 cannot be played, which suggests	*)
	(* that an improved version of this module would need to be	*)
	(* "tuned" to a key other than C major.				*)

	period := 65535.0;
	PeriodTable[0] := 65535;
	FOR j := 1 TO 11 DO
	    period := scale*period;
	    PeriodTable[j] := TRUNC(period + 0.5);
	END (*FOR*);

	(* We initially have two empty output buffers, so initialise	*)
	(* semaphore FreeBuffer to record this fact; and arbitrarily	*)
	(* choose one of the buffers as the initially active buffer.	*)

	CreateSemaphore (FreeBuffer, 2);
	currentbuffer := 0;

    END Initialise;

(************************************************************************)

BEGIN
    Initialise;
END Music.
