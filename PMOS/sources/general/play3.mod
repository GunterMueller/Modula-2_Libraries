IMPLEMENTATION MODULE Play3;

	(********************************************************)
	(*							*)
	(*		    3-part music			*)
	(*							*)
	(*  Programmer:		P. Moylan, T. Channon		*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(*	This is the version that uses a precomputed	*)
	(*	envelope and precomputed amplitude.		*)
	(*							*)
	(*	This is the module that deals with the data	*)
	(*	format used by Music3.  It takes three data	*)
	(*	arrays produced by Music3, and converts the	*)
	(*	data in real time to sample values which are	*)
	(*	then sent to the speaker by the subsidiary	*)
	(*	module PlayBuff.				*)
	(*							*)
	(********************************************************)

FROM PlayBuff IMPORT
    (* const *)	OutputBufferSize,
    (* proc *)	BufferAddress, SetCycleCount, StartPlaying,
		Synch0, Synch1, StopPlaying;

FROM IO IMPORT
    (* proc *)	KeyPressed;

FROM LowLevel IMPORT
    (* proc *)	AddOffset, SubtractOffset, FarAddOffset;

(************************************************************************)
(*				GLOBAL DATA				*)
(************************************************************************)

CONST
    (* TimeScale is an overall scaling factor for note durations.	*)
    (* This saves the caller the trouble of re-scaling for different	*)
    (* versions of this module.  It also contains an adjustment for	*)
    (* changes in CycleCount.						*)

    TimeScale = 1664 DIV CycleCount;

    (* DefaultDuration is the initial note duration.  Its value is	*)
    (* not particularly important, since typically this will be		*)
    (* overridden by a duration specification by the caller.		*)

    DefaultDuration = 200;

TYPE
    OutputBufferSubscript = [0..OutputBufferSize-1];
    (*# save, data(near_ptr=>off) *)
    OutputBufferPointer = POINTER TO ARRAY OutputBufferSubscript OF SHORTINT;
    (*# restore *)
    VoiceSet = SET OF VoiceNumber;
    WaveGroupPointer = POINTER TO WaveGroup;

VAR

    (* We have two output buffers, which we fill alternately.  The	*)
    (* following two pointers point to the two buffers.			*)

    OutPtr0, OutPtr1: OutputBufferPointer;

    (* Flag to say that playing should stop. *)

    ExitFlag: BOOLEAN;

    (* Pointers to tables defining one period of the waveform to be played.*)

    WaveTable: ARRAY WaveNumber OF WaveGroup;

    (* Pointers to tables defining the attack/decay envelopes. *)

    EnvelopePointer: ARRAY EnvelopeNumber OF EnvelopePtr;

    (* Tables defining the current state of each voice.  The fields	*)
    (* have the following meanings.					*)
    (*	Duration	The time duration of a note.  This must be	*)
    (*			multiplied by TimeScale to get a value in units	*)
    (*			of sampling instants.				*)
    (*	SamplesLeft	The number of sampling instants remaining for	*)
    (*			the note that is currently playing.		*)
    (*	Step		The high-order byte of this variable gives the	*)
    (*			step size when stepping through the waveform	*)
    (*			table.  (The low-order byte is in effect the	*)
    (*			fractional part.)  This defines the frequency	*)
    (*			of the note.					*)
    (*	EnvIndex	Where we're up to in the envelope array.	*)
    (*	EnvPtr		A pointer to the envelope array.		*)
    (*	SamplePos	Where we're up to in the waveform table.	*)
    (*	WaveGroupPtr	Points to a set of waveform tables.		*)
    (*	WavePtr		Pointer to the current waveform table.		*)
    (*	InPtr		Where we're up to in the input data.		*)

    VoiceData: ARRAY VoiceNumber OF
		    RECORD

			(* Information about the current state of play. *)

			Duration: CARDINAL;
			SamplesLeft: CARDINAL;
			Step: CARDINAL;

			(* Attack/decay shaping. *)

			EnvIndex: EnvelopeArrayIndex;
			EnvPtr: EnvelopePtr;

			(* References to the waveform tables. *)

			SamplePos:  RECORD
					CASE :BOOLEAN OF
					   | FALSE:	w: CARDINAL;
					   | TRUE:	lo, hi: BYTE;
					END (*CASE*);
				    END (*RECORD*);
			WaveGroupPtr: WaveGroupPointer;
			WavePtr: WaveTablePointer;

			(* The "source" from which we're working. *)

			InPtr: POINTER TO CARDINAL;

		    END (*RECORD*);

    (* The set of voices which have finished playing. *)

    VoicesFinished: VoiceSet;

(************************************************************************)
(*			STARTING A NEW NOTE				*)
(************************************************************************)

PROCEDURE LoadNewNote (V: VoiceNumber);

    (* Sets up the conditions needed to start playing a new note for	*)
    (* voice V.								*)

    BEGIN
	ExitFlag := KeyPressed();
	WITH VoiceData[V] DO
	    Step := InPtr^;  InPtr := AddOffset (InPtr, 2);
	    WHILE Step >= 65531 DO
		CASE Step OF
		  | 65531:	(* Set new envelope number. *)
				EnvPtr := EnvelopePointer[InPtr^];
				InPtr := AddOffset (InPtr, 2);
				Step := InPtr^;  InPtr := AddOffset (InPtr, 2);

		  | 65532:	(* Set new waveform. *)
				WaveGroupPtr := ADR(WaveTable[InPtr^]);
				InPtr := AddOffset (InPtr, 2);
				Step := InPtr^;  InPtr := AddOffset (InPtr, 2);

		  | 65533:	(* Obsolete, should not occur. *)
				Step := InPtr^;  InPtr := AddOffset (InPtr, 2);

		  | 65534:	(* New duration. *)
				Duration := InPtr^;
				InPtr := AddOffset (InPtr, 2);
				Step := InPtr^;  InPtr := AddOffset (InPtr, 2);

		  | 65535:	(* End of data. *)
				INCL (VoicesFinished, V);
				ExitFlag := ExitFlag OR
				  (VoicesFinished = VoiceSet{1..MAX(VoiceNumber)});
				Step := 0;
				InPtr := SubtractOffset (InPtr, 2);

		END (*CASE*);

	    END (*WHILE*);

	    SamplesLeft := TimeScale*Duration;
	    WavePtr := WaveGroupPtr^[0];
	    EnvIndex := 0;

	END (*WITH*);

    END LoadNewNote;

(************************************************************************)
(*		   LOADING DATA INTO THE OUTPUT BUFFER			*)
(************************************************************************)

PROCEDURE FillBuffer (p: OutputBufferPointer);

    (* Produces the next N bytes of output data. *)

    CONST bias = 128*CycleCount+255;

    TYPE cast = RECORD
		    CASE :BOOLEAN OF
		      | FALSE:	int: INTEGER;
		      | TRUE:	lo, hi: SHORTINT;
		    END (*CASE*);
		END (*RECORD*);

    VAR place: OutputBufferSubscript;  sum: cast;

    BEGIN
	FOR place := 0 TO MAX(OutputBufferSubscript) DO

	    (* If we've come to the end of the current note	*)
	    (* for any voice, set up a new one.			*)

	    IF VoiceData[1].SamplesLeft = 0 THEN
		LoadNewNote (1);
	    END (*IF*);
	    IF VoiceData[2].SamplesLeft = 0 THEN
		LoadNewNote (2);
	    END (*IF*);
	    IF VoiceData[3].SamplesLeft = 0 THEN
		LoadNewNote (3);
	    END (*IF*);

	    (* Pick up new levels from the waveform tables,	*)
	    (* and add them to the value in the output buffer.	*)
	    (* Note that SamplePos must be permitted to wrap.	*)

	    (*# save, check(overflow => off) *)
	    INC (VoiceData[1].SamplePos.w, VoiceData[1].Step);
	    INC (VoiceData[2].SamplePos.w, VoiceData[2].Step);
	    INC (VoiceData[3].SamplePos.w, VoiceData[3].Step);
	    (*# restore *)

	    DEC (VoiceData[1].SamplesLeft);
	    DEC (VoiceData[2].SamplesLeft);
	    DEC (VoiceData[3].SamplesLeft);

            sum.int := VoiceData[1].WavePtr^[VoiceData[1].SamplePos.hi]
		+ VoiceData[2].WavePtr^[VoiceData[2].SamplePos.hi]
		+ VoiceData[3].WavePtr^[VoiceData[3].SamplePos.hi] + bias;
	    p^[place] := sum.hi;

	END (*FOR*);
	
	(* To save on processor time, we do envelope updating only	*)
	(* once per buffer.  This version uses a precomputed envelope.	*)
	(* Note that WaveGroupPtr points to a collection of waveform	*)
	(* tables, one for each gain level.  What we are doing here, in	*)
	(* effect, is updating the gain.				*)

	WITH VoiceData[1] DO
	    WavePtr := WaveGroupPtr^[EnvPtr^[EnvIndex]];
	    IF EnvIndex < MAX(EnvelopeArrayIndex) THEN
		INC (EnvIndex);
	    END (*IF*);
	END (*WITH*);
	WITH VoiceData[2] DO
	    WavePtr := WaveGroupPtr^[EnvPtr^[EnvIndex]];
	    IF EnvIndex < MAX(EnvelopeArrayIndex) THEN
		INC (EnvIndex);
	    END (*IF*);
	END (*WITH*);
	WITH VoiceData[3] DO
	    WavePtr := WaveGroupPtr^[EnvPtr^[EnvIndex]];
	    IF EnvIndex < MAX(EnvelopeArrayIndex) THEN
		INC (EnvIndex);
	    END (*IF*);
	END (*WITH*);

    END FillBuffer;

(************************************************************************)
(*	   THE EXTERNALLY CALLABLE PROCEDURES FOR PLAYING MUSIC		*)
(************************************************************************)

PROCEDURE DefineWaveform (N: WaveNumber;  VAR (*IN*) data: WaveGroup);

    (* Defines a new waveform for wave number N. *)

    BEGIN
	WaveTable[N] := data;
    END DefineWaveform;

(************************************************************************)

PROCEDURE DefineEnvelope (E: EnvelopeNumber;  shapeptr: EnvelopePtr);

    (* Defines one of the attack/decay envelopes. *)

    BEGIN
	EnvelopePointer[E] := shapeptr;
    END DefineEnvelope;

(************************************************************************)

PROCEDURE Play (VAR (*IN*) A1, A2, A3: ARRAY OF CARDINAL);

    (* Plays the music encoded in arrays A1, A2, A3.  The encoding is	*)
    (* explained in the comments in the definition module.		*)

    VAR voice: VoiceNumber;  p: OutputBufferPointer;
        i : CARDINAL;

    BEGIN
	(* Set up pointers to the user data.	*)

	VoiceData[1].InPtr := ADR(A1);
	VoiceData[2].InPtr := ADR(A2);
	VoiceData[3].InPtr := ADR(A3);

	(* Set the state of all voices to suitable initial conditions.	*)

	FOR voice := 1 TO MAX(VoiceNumber) DO
	    WITH VoiceData[voice] DO
		Duration := DefaultDuration;
		SamplesLeft := 0;
		Step := 0;
		EnvPtr := EnvelopePointer[0];
		SamplePos.w := 0;
		WaveGroupPtr := ADR(WaveTable[0]);
		WavePtr := WaveGroupPtr^[0];
	    END (*WITH*);
	END (*FOR*);
	VoicesFinished := VoiceSet{};
	ExitFlag := FALSE;

	(* Now for the actual playing. *)

	FillBuffer(OutPtr0);
	StartPlaying;
	WHILE NOT ExitFlag DO
	    Synch1;
	    FillBuffer(OutPtr1);
	    Synch0;
	    FillBuffer(OutPtr0);
	END (*WHILE*);
	StopPlaying;

    END Play;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

PROCEDURE SetDefaults;

    (* Gives initial conditions to some of the global variables.  Note	*)
    (* that a lot of the initialisation is done in procedure Play, and	*)
    (* various variables are given values by commands embedded in the	*)
    (* user data.  We do however have to cover the possibility that	*)
    (* the caller neglects to specify some options.			*)

    VAR w: WaveNumber;  E: EnvelopeNumber;
	NilGroup: WaveGroup;  g: GainType;

    BEGIN

	FOR g := 0 TO MAX(GainType) DO
	    NilGroup[g] := NIL;
	END (*FOR*);

	FOR w := 0 TO MAX(WaveNumber) DO
	    WaveTable[w] := NilGroup;
	END (*FOR*);

	FOR E := 0 TO MAX(EnvelopeNumber) DO
	    EnvelopePointer[E] := NIL;
	END (*FOR*);

    END SetDefaults;

(************************************************************************)

BEGIN
    OutPtr0 := BufferAddress();
    OutPtr1 := FarAddOffset (OutPtr0, OutputBufferSize);
    SetCycleCount (CycleCount);
    SetDefaults;
END Play3.
