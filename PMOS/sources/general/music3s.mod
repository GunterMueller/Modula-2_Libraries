IMPLEMENTATION MODULE Music3S;

	(********************************************************)
	(*							*)
	(*		Translates 3-part music from		*)
	(*		musical notation to the form		*)
	(*		expected by module Play3S.		*)
	(*							*)
	(*	This version is for slow processors.  The	*)
	(*	version called Music3 is superior, but		*)
	(*	requires a reasonably fast processor.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		OK except for fault noted below	*)
	(*							*)
	(*	Fault: In procedure Insert, the timing		*)
	(*	calculation goes wrong when we meet a "254"	*)
	(*	code.  I'll need to re-think how to do this.	*)
	(*							*)
	(*	Minor puzzling fault: I'm getting a redundant	*)
	(*	set of "252" codes inserted at the beginning	*)
	(*	of the music array.  This doesn't cause any	*)
	(*	problems, but it does suggest that I need to	*)
	(*	ask why this is happening.			*)
	(*							*)
	(********************************************************)

FROM Play3S IMPORT
    (* proc *)	Play;

FROM Rationals IMPORT
    (* type *)	Rational,
    (* proc *)	Zero, Unity, Subtract, Multiply, Divide, Compare;

FROM LowLevel IMPORT
    (* proc *)	HighByte, LowByte;

(************************************************************************)

CONST
    irrelevantnote = 0;		(* used when remembering last note	*)

TYPE
    ZeroToTwelve = SHORTCARD [0..12];
    Octave = SHORTCARD [0..8];
    Triple = ARRAY VoiceNumber OF SHORTCARD;

VAR

    (* NoteTable performs the mapping C -> 1, D -> 3, E -> 5, F -> 6,	*)
    (* etc.  The gaps in this sequence are accounted for by sharps and	*)
    (* flats, which are handled elsewhere.				*)

    NoteTable: ARRAY ["A".."G"] OF ZeroToTwelve;

    (* MusicBuffer is where the result of the translation is kept.	*)

    MusicBuffer: ARRAY [0..9000] OF Triple;

    (* EndPointer is the position within MusicBuffer where the		*)
    (* last triple - holding the "end of music" code - resides.		*)

    EndPointer: CARDINAL;

    (* VoiceData keeps information about where we are up to in the	*)
    (* translation of each voice.  It is needed because the caller	*)
    (* is allowed to specify a few notes for one voice, then a few	*)
    (* notes for a second voice, and then perhaps to switch back to	*)
    (* the first voice, etc.						*)

    (* The distinction between the SrcDuration and the CodedDuration	*)
    (* fields is that SrcDuration is the duration, as supplied by the	*)
    (* caller, of the last note processed; whereas CodedDuration is	*)
    (* the duration of the data at MusicBuffer[position]; there is no	*)
    (* direct relationship between these two, because for example a	*)
    (* single note specified by the caller might be split into two	*)
    (* or more buffer entries of shorter duration.			*)

    (* Notice, by the way, that these durations are in "relative" form,	*)
    (* i.e. they are relative to the absolution duration as set by	*)
    (* procedure SetDuration.						*)

    VoiceData: ARRAY VoiceNumber OF
		    RECORD
			position: CARDINAL;
			SrcDuration, CodedDuration: Rational;
			CurrentOctave: Octave;
			LastNote: ZeroToTwelve;
			Enabled: BOOLEAN;
		    END (*RECORD*);

(************************************************************************)
(*			DELETIONS FROM MusicBuffer			*)
(************************************************************************)

PROCEDURE Delete (place: CARDINAL);

    (* Deletes the existing entry MusicBuffer[place], moving subsequent	*)
    (* data backwards to fill the hole.					*)

    VAR j: CARDINAL;  voice: VoiceNumber;

    BEGIN
	FOR j := place TO EndPointer-1 DO
	    MusicBuffer[j] := MusicBuffer[j+1];
	END (*FOR*);
	DEC (EndPointer);
	FOR voice := 1 TO MAX(VoiceNumber) DO
	    WITH VoiceData[voice] DO
		IF position > place THEN
		    DEC(position);
		END (*IF*);
	    END (*WITH*);
	END (*FOR*);
    END Delete;

(************************************************************************)

PROCEDURE Compactify (place: CARDINAL);

    (* Scans the MusicBuffer array from "place" onwards, combining	*)
    (* adjacent "change duration" codes where possible.  (It's possible	*)
    (* in principle to avoid these redundancies while doing the		*)
    (* translation, but permitting them reduces some of the translation	*)
    (* complexity, so we gain some simplicity by stripping out the	*)
    (* rubbish in a separate pass.  The array rarely gets so large that	*)
    (* we need to be concerned about the execution time overhead.)	*)

    VAR f1, f2, product: Rational;

    BEGIN
	WHILE MusicBuffer[place][1] <> 255 DO
	    IF (MusicBuffer[place][1] = 253)
			AND (MusicBuffer[place+1][1] = 253) THEN
		f1.num := INTEGER(MusicBuffer[place][2]);
		f1.denom := CARDINAL(MusicBuffer[place][3]);
		f2.num := INTEGER(MusicBuffer[place+1][2]);
		f2.denom := CARDINAL(MusicBuffer[place+1][3]);
		Delete (place+1);
		product := Multiply (f1, f2);
		IF Compare (product, Unity()) <> 0 THEN
		    MusicBuffer[place][2] := SHORTCARD(product.num);
		    MusicBuffer[place][3] := SHORTCARD(product.denom);
		ELSE
		    Delete (place);
		END (*IF*);
	    ELSE
		INC (place);
	    END (*IF*)
	END (*WHILE*);
    END Compactify;

(************************************************************************)
(*			INSERTIONS INTO MusicBuffer			*)
(************************************************************************)

PROCEDURE CreateInsertionPoint (place: CARDINAL);

    (* Inserts an empty element at MusicBuffer[place]. *)

    VAR j: CARDINAL;  voice: VoiceNumber;

    BEGIN
	INC (EndPointer);
	FOR j := EndPointer TO place+1 BY -1 DO
	    MusicBuffer[j] := MusicBuffer[j-1]
	END (*FOR*);
	FOR voice := 1 TO MAX(VoiceNumber) DO
	    MusicBuffer[place][voice] := 0;
	    WITH VoiceData[voice] DO
		IF position > place THEN
		    INC(position);
		END (*IF*);
	    END (*WITH*);
	END (*FOR*);
    END CreateInsertionPoint;

(************************************************************************)

PROCEDURE NewDurationSetting (place: CARDINAL;
			oldduration, newduration: Rational): BOOLEAN;

    (* Inserts the code for a new note duration at MusicBuffer[place].	*)
    (* The function result is TRUE iff we had to actually insert a new	*)
    (* array element, as distinct from updating an existing one.	*)

    VAR change: Rational;  inserted: BOOLEAN;

    BEGIN
	change := Divide (newduration, oldduration);
	IF Compare (change, Unity()) = 0 THEN RETURN FALSE
	ELSIF MusicBuffer[place][1] = 253 THEN
	    oldduration.num := INTEGER(MusicBuffer[place][2]);
	    oldduration.denom := CARDINAL(MusicBuffer[place][3]);
	    change := Multiply (change, oldduration);
	    inserted := FALSE;
	ELSE
	    CreateInsertionPoint (place);
	    MusicBuffer[place][1] := 253;
	    inserted := TRUE;
	END (*IF*);
	MusicBuffer[place][2] := SHORTCARD(change.num);
	MusicBuffer[place][3] := SHORTCARD(change.denom);
	RETURN inserted;
    END NewDurationSetting;

(************************************************************************)

PROCEDURE Split (place: CARDINAL;  d1, d2: Rational);

    (* This procedure is called when the existing data at		*)
    (* MusicBuffer[place] has a duration of d2, and we want to add a	*)
    (* note in a new voice with duration d1 < d2.  The existing entry	*)
    (* is split into two portions of duration d1 and (d2-d1).		*)

    (* To avoid confusing the timing for data already present, it's	*)
    (* important to do insertions ahead of the existing data rather	*)
    (* than letting place catch up to the position for new insertions	*)
    (* after existing data.  This explains the slightly non-obvious	*)
    (* order in which we make the insertions.				*)

    VAR diff, lastchange: Rational;  dummy: BOOLEAN;

    BEGIN
	IF NewDurationSetting (place, d2, d1) THEN INC (place) END(*IF*);
	CreateInsertionPoint (place);
	CreateInsertionPoint (place);
	MusicBuffer[place] := MusicBuffer[place+2];  INC (place);
	MusicBuffer[place] := MusicBuffer[place+1];  INC (place);
	diff := Subtract (d2, d1);
	lastchange := Divide (d2, diff);
	MusicBuffer[place][1] := 253;
	MusicBuffer[place][2] := SHORTCARD(lastchange.num);
	MusicBuffer[place][3] := SHORTCARD(lastchange.denom);
	dummy := NewDurationSetting (place-1, d1, diff);
    END Split;

(************************************************************************)

PROCEDURE Insert (V: VoiceNumber;  note: SHORTCARD;  duration: Rational);

    (* Inserts a new note for voice V into the MusicBuffer array.	*)

    VAR place: CARDINAL;  ExpectedDuration, factor: Rational;

    BEGIN
	WITH VoiceData[V] DO
	    place := position;
	    ExpectedDuration := CodedDuration;
	END (*WITH*);
	WHILE duration.num > 0 DO
	    IF place = EndPointer THEN

		(* Insertion at the end of the array. *)

		IF ExpectedDuration <> duration THEN
		    IF NewDurationSetting (place, ExpectedDuration, duration)
					THEN INC (place) END(*IF*);
		    ExpectedDuration := duration;
		END (*IF*);
		CreateInsertionPoint(place);
		MusicBuffer[place][V] := note;
		INC (place);
		duration := Zero();

	    ELSIF MusicBuffer[place][1] = 252 THEN

		(* Skip over any "change waveform" commands. *)

		INC (place);

	    ELSIF MusicBuffer[place][1] = 253 THEN

		(* "Change relative duration" code found.  We take note	*)
		(* of it and step past it.  This could lead to		*)
		(* redundant strings of "change duration" entries, but	*)
		(* I can live with that for the sake of clearer code.	*)

		factor.num := INTEGER(MusicBuffer[place][2]);
		factor.denom := CARDINAL(MusicBuffer[place][3]);
		ExpectedDuration := Multiply (ExpectedDuration, factor);
		INC (place);

	    ELSIF MusicBuffer[place][1] = 254 THEN

		(* "Change absolute duration" code found.  Since this	*)
		(* module deals only with the "relative duration" part	*)
		(* of the timing, the only effect here is that we need	*)
		(* to start with a new "relative duration" base.	*)

		duration := Unity();
		ExpectedDuration := duration;
		VoiceData[V].SrcDuration := duration;
		INC (place);

	    ELSIF Compare (duration, ExpectedDuration) >= 0 THEN

		MusicBuffer[place][V] := note;
		INC (place);
		duration := Subtract (duration, ExpectedDuration);

	    ELSE

		(* The new note is shorter than notes already there for	*)
		(* the other voices.  We have to split up the existing	*)
		(* data.						*)

		Split (place, duration, ExpectedDuration);

	    END (*IF*);
	END (*WHILE*);

	WITH VoiceData[V] DO
	    position := place;
	    CodedDuration := ExpectedDuration;
	END (*WITH*);

    END Insert;

(************************************************************************)
(*			INTERPRETING MUSICAL NOTATION			*)
(************************************************************************)

PROCEDURE Translate (VAR (*IN*) notes: ARRAY OF CHAR;
			VAR (*INOUT*) place: CARDINAL): ZeroToTwelve;

    (* Translates a note code in the conventional notation (C,D,etc.),	*)
    (* as stored in notes[place], into the numeric code expected by	*)
    (* module Play3.  Parameter place is updated, so that on return	*)
    (* notes[place] is the first character not yet processed.		*)

    VAR result: ZeroToTwelve;

    BEGIN
	IF notes[place] = "R" THEN
	    result := 0;  INC (place);
	ELSE
	    result := NoteTable[notes[place]];
	    INC (place);
	    IF place <= HIGH(notes) THEN

		(* Check for sharp or flat. *)

		IF notes[place] = "#" THEN
	    	    INC(result);  INC(place);
		ELSIF notes[place] = "b" THEN
	    	    DEC(result);  INC(place);
		END (*IF*);

	    END (*IF*);
	END (*IF*);

	RETURN result;

    END Translate;

(************************************************************************)

PROCEDURE Convert (voice: VoiceNumber;  notes: ARRAY OF CHAR);

    (* Converts the notes specified in array "notes".  The result is	*)
    (* inserted into global variable MusicBuffer.			*)

    TYPE CharSet = SET OF CHAR;

    CONST Two = Rational (2, 1);  Three = Rational (3, 1);
	Digits = CharSet {'0'..'9'};

    VAR	inplace, start: CARDINAL;
	CurrentDuration, factor: Rational;
	CurrentOctave: Octave;
	note, LastNote: SHORTCARD;

    BEGIN
	start := VoiceData[voice].position;
	CurrentDuration := VoiceData[voice].SrcDuration;
	CurrentOctave := VoiceData[voice].CurrentOctave;
	LastNote := VoiceData[voice].LastNote;

	inplace := 0;

	(* The following loop translates from letter codes in array	*)
	(* "notes" to numeric codes in array MusicBuffer.		*)

	REPEAT

	    IF notes[inplace] = " " THEN INC(inplace)	(* ignore spaces *)

	    (* Check for duration changes.	*)

	    ELSIF notes[inplace] = "*" THEN
		INC(inplace);
		factor := Two;
		IF notes[inplace] IN Digits THEN
		    factor.num := ORD(notes[inplace]) - ORD("0");
		    INC (inplace);
		END (*IF*);
		CurrentDuration := Multiply (factor, CurrentDuration);
	    ELSIF notes[inplace] = "/" THEN
		INC(inplace);
		factor := Two;
		IF notes[inplace] IN Digits THEN
		    factor.num := ORD(notes[inplace]) - ORD("0");
		    INC (inplace);
		END (*IF*);
		CurrentDuration := Divide (CurrentDuration, factor);
	    ELSIF notes[inplace] = "3" THEN
		CurrentDuration := Divide (CurrentDuration, Three);
		INC(inplace);

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

		(* We have a note to translate.			*)
		(* N.B. procedure Translate updates inplace.	*)

		note := Translate (notes, inplace);

		IF note = 0 THEN
		    Insert (voice, 0, CurrentDuration);
		ELSE

		    (* We now have the note in the form of a		*)
		    (* ZeroToTwelve code, but we still have to decide	*)
		    (* the octave.  The criterion we use is to minimise	*)
		    (* the distance from the last note played.  This	*)
		    (* can be overridden by the "u" and "d" codes.	*)

		    IF LastNote <> irrelevantnote THEN
			IF INTEGER(note)-INTEGER(LastNote) > 6 THEN
			    IF CurrentOctave > 0 THEN
				DEC(CurrentOctave);
			    END(*IF*);
			ELSIF INTEGER(LastNote) - INTEGER(note) > 6 THEN
			    IF CurrentOctave < MAX(Octave) THEN
				INC(CurrentOctave)
			    END (*IF*);
			END (*IF*);
		    END (*IF*);
		    LastNote := note;
		    Insert (voice, VAL(SHORTCARD, 12*CurrentOctave + note),
						CurrentDuration);
		END (*IF*);

	    END (*IF*);

	UNTIL (inplace > HIGH(notes)) OR (notes[inplace] = CHR(0));

	VoiceData[voice].SrcDuration := CurrentDuration;
	VoiceData[voice].CurrentOctave := CurrentOctave;
	VoiceData[voice].LastNote := LastNote;
	Compactify (start);

    END Convert;

(************************************************************************)

PROCEDURE Voice1 (notes: ARRAY OF CHAR);

    (* Appends more notes to Voice #1. *)

    BEGIN
	IF VoiceData[1].Enabled THEN
	    Convert (1, notes);
	END (*IF*);
    END Voice1;

(************************************************************************)

PROCEDURE Voice2 (notes: ARRAY OF CHAR);

    (* Appends more notes to Voice #2. *)

    BEGIN
	IF VoiceData[2].Enabled THEN
	    Convert (2, notes);
	END (*IF*);
    END Voice2;

(************************************************************************)

PROCEDURE Voice3 (notes: ARRAY OF CHAR);

    (* Appends more notes to Voice #3. *)

    BEGIN
	IF VoiceData[3].Enabled THEN
	    Convert (3, notes);
	END (*IF*);
    END Voice3;

(************************************************************************)

PROCEDURE EnableVoice (V: VoiceNumber;  enable: BOOLEAN);

    (* If enable is FALSE then the following data for voice V are	*)
    (* ignored.  (This is for testing, when you want to suppress a	*)
    (* voice in order to listen to the others more closely.)  The	*)
    (* default condition is that all voices are enabled.		*)

    BEGIN
	VoiceData[V].Enabled := enable;
    END EnableVoice;

(************************************************************************)

PROCEDURE SetWaveform (V: VoiceNumber;  N: WaveformNumber);

    (* Changes the waveform for voice V. *)

    VAR place: CARDINAL;

    BEGIN
	place := VoiceData[V].position;
	CreateInsertionPoint (place);
	MusicBuffer[place][1] := 252;
	MusicBuffer[place][2] := BYTE(V);
	MusicBuffer[place][3] := BYTE(N);
    END SetWaveform;

(************************************************************************)

PROCEDURE SetEnvelope (V: VoiceNumber;  attack, decay, sustain: CARDINAL);

    (* Does nothing - this procedure is present for compatibility with	*)
    (* other versions, but the present version does not have the	*)
    (* capability of controlling the waveform envelope.			*)

    BEGIN
    END SetEnvelope;

(************************************************************************)

PROCEDURE SetInstrument (N: WaveformNumber;
				attack, decay, sustain: CARDINAL);

    (* Changes the waveform for all voices.  The last four parameters	*)
    (* are ignored.							*)

    VAR V: VoiceNumber;

    BEGIN
	FOR V := 1 TO MAX(VoiceNumber) DO
	    SetWaveform (V, N);
	END (*FOR*);
    END SetInstrument;

(************************************************************************)

PROCEDURE SetDuration (value: CARDINAL);

    (* Sets the duration for the following notes (all voices).  The	*)
    (* change takes place at the point immediately following all data	*)
    (* entered so far - i.e. at the "current time" for the voice for	*)
    (* which most data has been entered so far.				*)

    VAR voice: VoiceNumber;  place: CARDINAL;

    BEGIN
	(* Work out where to put the "set duration" code.	*)

	place := 0;
	FOR voice := 1 TO MAX(VoiceNumber) DO
	    WITH VoiceData[voice] DO
		IF position > place THEN
		    place := position;
		END (*IF*);
	    END (*WITH*);
	END (*FOR*);

	(* Insert the code. *)

	IF MusicBuffer[place][1] <> 254 THEN
	    CreateInsertionPoint (place);
	    MusicBuffer[place][1] := 254;
	END (*IF*);
	MusicBuffer[place][2] := LowByte(value);
	MusicBuffer[place][3] := HighByte(value);

    END SetDuration;

(************************************************************************)

PROCEDURE PlayTheMusic;

    (* Plays the music that has been stored so far. *)

    BEGIN
	Play (MusicBuffer);
    END PlayTheMusic;

(************************************************************************)
(*			MODULE INITIALISATION				*)
(************************************************************************)

PROCEDURE Initialise;

    (* Sets the initial duration and octave, and sets up the tables	*)
    (* for translating from music notation to the output codes.		*)

    VAR voice: VoiceNumber;

    BEGIN
	(* MusicBuffer is where our final result goes.  Initially we	*)
	(* fill it with the "end of music" code.			*)

	MusicBuffer[0][1] := 255;
	MusicBuffer[0][2] := 0;
	MusicBuffer[0][3] := 0;
	EndPointer := 0;

	FOR voice := 1 TO MAX(VoiceNumber) DO
	    WITH VoiceData[voice] DO
		position := 0;
		SrcDuration := Unity();
		CodedDuration := Unity();
		CurrentOctave := 3;
		LastNote := irrelevantnote;
		Enabled := TRUE;
	    END (*WITH*);
	END (*FOR*);

	(* NoteTable translates from letter codes into a numeric code	*)
	(* consistent with a well-tempered tuning.			*)

	NoteTable["C"] := 1;  NoteTable["D"] := 3;
	NoteTable["E"] := 5;  NoteTable["F"] := 6;
	NoteTable["G"] := 8;  NoteTable["A"] := 10;
	NoteTable["B"] := 12;

    END Initialise;

(************************************************************************)

BEGIN
    Initialise;
END Music3S.
