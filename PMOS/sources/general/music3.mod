IMPLEMENTATION MODULE Music3;

	(********************************************************)
	(*							*)
	(*		Translates 3-part music from		*)
	(*		musical notation to the form		*)
	(*		expected by module Play3B.		*)
	(*							*)
	(*  Programmer:		P. Moylan, T Channon		*)
	(*  Last edited:	21 March 1995			*)
	(*  Changes:            Signed wavetables.		*)
	(*                      Precomputed amplitudes		*)
	(*                      Precomputed envelopes		*)
	(*  Status:		Working				*)
	(*							*)
	(*	This version is TC's version of Midsummer's	*)
	(*	Day (i.e. the winter solstice), with some	*)
	(*	modifications by PJM.				*)
	(*							*)
	(*	Latest modifications:				*)
	(*	    Precomputed scaling factors			*)
	(*	    Slight change in the way gains are		*)
	(*		calculated				*)
	(*	    Integer rather than shortint wave tables	*)
	(*							*)
	(********************************************************)

(************************************************************************)
(*			IMPLEMENTATION NOTES				*)
(*									*)
(* The work done by this module falls into two distinct phases.  In the	*)
(* first phase we are receiving data, in more-or-less standard musical	*)
(* notation, from the caller for the three voices separately.  We do	*)
(* a little processing of this data, to simplify the job of the second	*)
(* phase, but basically we are just storing the caller's data in three	*)
(* big arrays.  The amount of pre-processing we can do is limited by	*)
(* the fact that full processing (to produce a result in the form of an	*)
(* actual waveform) would require arrays that would overflow the	*)
(* 64 Kbyte segment size limit.						*)
(*									*)
(* The second phase starts when procedure PlayTheMusic is called.  We	*)
(* handle this in the separate module Play3B.				*)
(*									*)
(************************************************************************)

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM Play3 IMPORT
    (* const*)	CycleCount,
    (* type *)	WaveNumber, EnvelopeNumber, EnvelopePtr,
		EnvelopeArrayIndex, GainType, WaveGroup,
    (* proc *)	DefineWaveform, DefineEnvelope, Play;

FROM MATHLIB IMPORT
    (* proc *)	Sin;

(************************************************************************)

CONST
    (* The constant LowestInterval is a scaling factor for note		*)
    (* frequencies.  In the present we've set it rather arbitrarily.	*)
    (* It could be adjusted more precisely to give an A440 tuning or	*)
    (* similar, but so far I haven't done the necessary calculations.	*)

    LowestInterval = 4.375*LONGREAL(CycleCount);

    irrelevantnote = 0;		(* used when remembering last note	*)
    DefaultDuration = 200;

TYPE
    ZeroToTwelve = SHORTCARD [0..12];
    Octave = SHORTCARD [0..8];
    Note = SHORTCARD [0..63];
    CodeSubscript = [0..4000];

VAR

    (* Waveform tables. *)

    Wave: ARRAY WaveNumber OF WaveGroup;

    (* NoteToIntervalMap takes a note code and translates it to the	*)
    (* step amount to be used in stepping through a WaveForm array.	*)

    NoteToIntervalMap: ARRAY Note OF CARDINAL;

    (* NoteTable performs the mapping C -> 1, D -> 3, E -> 5, F -> 6,	*)
    (* etc.  The gaps in this sequence are accounted for by sharps and	*)
    (* flats, which are handled elsewhere.				*)

    NoteTable: ARRAY ["A".."G"] OF ZeroToTwelve;

    (* We define a number of different attack/decay envelopes.		*)

    EnvelopeData: ARRAY EnvelopeNumber OF
		    RECORD
			Attack, Decay, Sustain: CARDINAL;
			shapeptr: EnvelopePtr;
		    END (*RECORD*);

    (* VoiceData keeps information about where we are up to in the	*)
    (* translation of each voice.  It is needed because the caller	*)
    (* is allowed to specify a few notes for one voice, then a few	*)
    (* notes for a second voice, and then perhaps to switch back to	*)
    (* the first voice, etc.						*)

    VoiceData: ARRAY VoiceNumber OF
		    RECORD
			position: CARDINAL;
			LastDuration, NextDuration: CARDINAL;
			CurrentOctave: Octave;
			LastNote: ZeroToTwelve;
			Enabled: BOOLEAN;
			Code: ARRAY CodeSubscript OF CARDINAL;
		    END (*RECORD*);

(************************************************************************)
(*			APPENDING DATA TO THE BUFFERS			*)
(************************************************************************)

PROCEDURE DurationSetting (V: VoiceNumber;  newduration: CARDINAL);

    (* Sets a new absolute duration for voice V.  *)

    BEGIN
	WITH VoiceData[V] DO
	    Code[position] := 65534;  INC (position);
	    Code[position] := newduration;  INC (position);
	    LastDuration := newduration;
	    NextDuration := newduration;
	END (*WITH*);
    END DurationSetting;

(************************************************************************)

PROCEDURE AddNote (V: VoiceNumber;  note: Note;  duration: CARDINAL);

    (* Appends a new note to the stored data for voice V.	*)

    BEGIN
	WITH VoiceData[V] DO
	    IF duration <> LastDuration THEN
		DurationSetting (V, duration);
	    END (*IF*);
	    Code[position] := NoteToIntervalMap[note];
	    INC (position);
	END (*WITH*);
    END AddNote;

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
    (* appended to the global coded note data.				*)

    TYPE CharSet = SET OF CHAR;

    CONST Digits = CharSet {'0'..'9'};

    VAR	inplace: CARDINAL;
	CurrentDuration, factor: CARDINAL;
	CurrentOctave: Octave;
	note, LastNote: Note;

    BEGIN
	CurrentDuration := VoiceData[voice].NextDuration;
	CurrentOctave := VoiceData[voice].CurrentOctave;
	LastNote := VoiceData[voice].LastNote;

	inplace := 0;

	(* The following loop translates from letter codes in array	*)
	(* "notes" to numeric codes in array VoiceData[voice].Code.	*)

	REPEAT

	    IF notes[inplace] = " " THEN INC(inplace)	(* ignore spaces *)

	    (* Check for duration changes.	*)

	    ELSIF notes[inplace] = "*" THEN
		INC(inplace);
		factor := 2;
		IF notes[inplace] IN Digits THEN
		    factor := ORD(notes[inplace]) - ORD("0");
		    INC (inplace);
		END (*IF*);
		CurrentDuration := CurrentDuration*factor;
	    ELSIF notes[inplace] = "/" THEN
		INC(inplace);
		factor := 2;
		IF notes[inplace] IN Digits THEN
		    factor := ORD(notes[inplace]) - ORD("0");
		    INC (inplace);
		END (*IF*);
		CurrentDuration := CurrentDuration DIV factor;
	    ELSIF notes[inplace] = "3" THEN
		CurrentDuration := CurrentDuration DIV 3;
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
		    AddNote (voice, 0, CurrentDuration);
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
		    AddNote (voice, VAL(Note, 12*CurrentOctave + note),
						CurrentDuration);
		END (*IF*);

	    END (*IF*);

	UNTIL (inplace > HIGH(notes)) OR (notes[inplace] = CHR(0));

	VoiceData[voice].NextDuration := CurrentDuration;
	VoiceData[voice].CurrentOctave := CurrentOctave;
	VoiceData[voice].LastNote := LastNote;

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

    BEGIN
	WITH VoiceData[V] DO
	    Code[position] := 65532;  INC (position);
	    Code[position] := N;  INC (position);
	END (*WITH*);
    END SetWaveform;

(************************************************************************)
(*			ENVELOPE CALCULATIONS				*)
(************************************************************************)

PROCEDURE EnvelopeModel0 (p: EnvelopePtr;  Attack, Decay, Sustain: LONGREAL);

    (* Calculates the envelope array using an attack/decay/sustain model.*)

    CONST scaletogain = LONGREAL(MAX(GainType));

    VAR level: LONGREAL;
	phase: [0..1];  j: EnvelopeArrayIndex;

    BEGIN
	Decay := 1.0 - Decay;
	phase := 0;  level := 0.0;
	FOR j := 0 TO MAX(EnvelopeArrayIndex) DO
	    IF phase = 0 THEN

		(* Attack phase: ramp up linearly to maximum. *)

		level := level + Attack;
		IF level > 1.0 THEN
		    level := 1.0;  INC(phase);
		END(*IF*);

	    ELSE

		(* Decay phase: we're using an exponential decay. *)

		level := Sustain + Decay*(level - Sustain);

	    END (*IF*);

	    p^[j] := VAL(GainType, scaletogain*level);

	END (*FOR*);

    END EnvelopeModel0;

(************************************************************************)

PROCEDURE CreateEnvelope (attack, decay, sustain: CARDINAL): EnvelopeNumber;

    (* Returns the envelope number corresponding to the parameters, if	*)
    (* such an envelope already exists; or creates a new envelope and	*)
    (* returns its number.						*)

    CONST scale = 1.0/LONGREAL(MAX(CARDINAL));

    VAR E: EnvelopeNumber;

    (********************************************************************)

    PROCEDURE Match (E: EnvelopeNumber;
			attack, decay, sustain: CARDINAL): BOOLEAN;

	(* Returns TRUE iff envelope number E matches the parameters.	*)

	BEGIN
	    WITH EnvelopeData[E] DO
		RETURN (Attack = attack) AND (Decay = decay)
			AND (Sustain = sustain);
	    END (*WITH*);
	END Match;

    (********************************************************************)

    BEGIN
	E := 0;
	LOOP
	    IF EnvelopeData[E].shapeptr = NIL THEN EXIT(*LOOP*)
	    ELSIF Match (E, attack, decay, sustain) THEN RETURN E
	    ELSIF E < MAX(EnvelopeNumber) THEN INC(E)
	    ELSE
		(* We've run out of envelopes, steam an old one open.	*)
		DISPOSE(EnvelopeData[E].shapeptr);
		EXIT(*LOOP*);
	    END(*IF*);
	END (*LOOP*);

	(* If we reach this point, then we haven't found a match with	*)
	(* an existing envelope and have to create a new one.		*)

	(* This section is still experimental. *)

	WITH EnvelopeData[E] DO
	    Attack := attack;  Decay := decay;  Sustain := sustain;
	    NEW (shapeptr);
	    EnvelopeModel0 (shapeptr, scale*LONGREAL(attack),
			scale*LONGREAL(decay), scale*LONGREAL(sustain));
	    DefineEnvelope (E, shapeptr);
	END (*WITH*);
	RETURN E;

    END CreateEnvelope;

(************************************************************************)

PROCEDURE SetEnvelope (V: VoiceNumber;  attack, decay, sustain: CARDINAL);

    (* Use this to simulate different instruments.  Parameters attack	*)
    (* and decay are in effect rates of change.  The sustain parameter	*)
    (* is a steady-state amplitude (range [0..65535]) after the		*)
    (* "decay" phase.							*)

    VAR E: EnvelopeNumber;

    BEGIN
	E := CreateEnvelope (attack, decay, sustain);
	WITH VoiceData[V] DO
	    Code[position] := 65531;  INC (position);
	    Code[position] := E;  INC (position);
	END (*WITH*);
    END SetEnvelope;

(************************************************************************)

PROCEDURE SetInstrument (N: WaveformNumber;
				attack, decay, sustain: CARDINAL);

    (* Like SetEnvelope and SetWaveform, but affects all voices. *)

    VAR V: VoiceNumber;

    BEGIN
	FOR V := 1 TO MAX(VoiceNumber) DO
	    SetWaveform (V, N);
	    SetEnvelope (V, attack, decay, sustain);
	END (*FOR*);
    END SetInstrument;

(************************************************************************)

PROCEDURE SetDuration (value: CARDINAL);

    (* Sets the duration for the following notes (all voices).	*)

    VAR V: VoiceNumber;

    BEGIN
	FOR V := 1 TO MAX(VoiceNumber) DO
	    DurationSetting (V, value);
	END (*FOR*);
    END SetDuration;

(************************************************************************)

PROCEDURE PlayTheMusic;

    (* Plays the music that has been stored so far. *)

    VAR V: VoiceNumber;

    BEGIN
	FOR V := 1 TO MAX(VoiceNumber) DO
	    WITH VoiceData[V] DO
		Code[position] := 65535;
	    END (*WITH*);
	END (*FOR*);
	Play (VoiceData[1].Code, VoiceData[2].Code, VoiceData[3].Code);
    END PlayTheMusic;

(************************************************************************)
(*			MODULE INITIALISATION				*)
(************************************************************************)

PROCEDURE Initialise;

    (* Sets the initial duration and octave, and sets up the tables	*)
    (* for translating from music notation to the output codes.		*)

    CONST
	PI = 3.141592653589793240;
	freqstep = 1.057298094605;	(* 2^(1/12) *)
	offset = LONGREAL(128*CycleCount-1) / 3.0;
	bound = 2.0*offset;		(* limit on peak-to-peak value	*)

    VAR step, scale: LONGREAL;
	j, break: SHORTCARD;  note: Note;
	voice: VoiceNumber;  w: WaveNumber;  E: EnvelopeNumber;
	Gain: ARRAY GainType OF LONGREAL;
	g: GainType;

    (********************************************************************)

    PROCEDURE WaveValue (w: WaveNumber;  pos: BYTE;  val: LONGREAL);

	(* Is passed the value for maximum sound, fills in the tables	*)
	(* with all the scaled values.					*)

	VAR g: GainType;

	BEGIN
	    FOR g := 0 TO MAX(GainType) DO
		Wave[w][g]^[pos] := VAL(INTEGER, val*Gain[g]);
	    END;
	END WaveValue;

    (********************************************************************)

    BEGIN

	FOR g := 0 TO MAX(GainType) DO
	    Gain[g] := LONGREAL(g)/LONGREAL(MAX(GainType));
	    FOR w := 0 TO HIGH(Wave) DO
		NEW (Wave[w][g]);
	    END (*FOR*);
	END (*FOR*);

	(* NoteTable translates from letter codes into a numeric code	*)
	(* consistent with a well-tempered tuning.			*)

	NoteTable["C"] := 1;  NoteTable["D"] := 3;
	NoteTable["E"] := 5;  NoteTable["F"] := 6;
	NoteTable["G"] := 8;  NoteTable["A"] := 10;
	NoteTable["B"] := 12;

	(* Create the note-to-interval map. *)

	step := LowestInterval;
	NoteToIntervalMap[0] := 0;
	FOR note := 1 TO MAX(Note) DO
	    NoteToIntervalMap[note] := VAL(CARDINAL, step);
	    step := step*freqstep;
	END (*FOR*);

	(* Create the waveform arrays. *)

	(* Waveform #0: sine wave. *)
	(* Waveform #1: sine wave + 3rd harmonic. *)

	FOR j := 0 TO 255 DO
	    WaveValue (0, j, 0.5*bound*Sin(VAL(LONGREAL,j)*PI/128.0));
	    WaveValue (1, j, 0.56*bound*(Sin(VAL(LONGREAL,j)*PI/128.0)
				+ 0.18*Sin(3.0*VAL(LONGREAL,j)*PI/128.0)));
	END (*FOR*);

	(* Waveforms #2, 3, 4: rectangular waves.	*)

	FOR w := 2 TO 4 DO
	    break := 56*SHORTCARD(w) + 16;
	    FOR j := 0 TO break-1 DO
		WaveValue (w, j, offset);
	    END (*FOR*);
	    FOR j := break TO 255 DO
		WaveValue (w, j, -offset);
	    END (*FOR*);
	END (*FOR*);

	(* Waveforms #5, 6, 7: triangular waves.	*)

	FOR w := 5 TO 6 DO
	    break := VAL(SHORTCARD, 64*w - 192);
	    scale := bound/LONGREAL(break);
	    FOR j := 0 TO break-1 DO
		WaveValue (w,j, scale*LONGREAL(j)-offset);
	    END (*FOR*);
	    scale := bound/LONGREAL(256-CARDINAL(break));
	    FOR j := break TO 255 DO
		WaveValue (w, j, scale*LONGREAL(256-CARDINAL(j))-offset);
	    END (*FOR*);
	END (*FOR*);
	FOR j := 0 TO 255 DO
	    WaveValue (7, j, bound*LONGREAL(j)/256.0-offset);
	END (*FOR*);

	(* Pass the waveforms to the subsidiary module. *)

	FOR w := 0 TO MAX(WaveNumber) DO
	    DefineWaveform (w, Wave[w]);
	END (*FOR*);

	(* Initialise the EnvelopeData array. *)

	FOR E := 0 TO MAX(EnvelopeNumber) DO
	    WITH EnvelopeData[E] DO
		Attack := 0;  Decay := 0;  Sustain := 0;
		shapeptr := NIL;
	    END (*WITH*);
	END (*FOR*);

	(* Set initial conditions for the voice data. *)

	FOR voice := 1 TO MAX(VoiceNumber) DO
	    WITH VoiceData[voice] DO
		position := 0;
		LastDuration := DefaultDuration;
		NextDuration := DefaultDuration;
		CurrentOctave := 3;
		LastNote := irrelevantnote;
		Enabled := TRUE;
	    END (*WITH*);
	END (*FOR*);

    END Initialise;

(************************************************************************)

BEGIN
    Initialise;
END Music3.
