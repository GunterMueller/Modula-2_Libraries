MODULE Plink;

	(********************************************************)
	(*							*)
	(*	  Allows the user to test the effect of		*)
	(*	adjustable music parameters on the sound,	*)
	(*	  by trial and error from the keyboard.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		Working				*)
	(*	The "model 1" approach needs major improvements	*)
	(*							*)
	(********************************************************)

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, SetCursor, WriteLn, WriteString, ColourSwap;

FROM NumericIO IMPORT
    (* proc *)	WriteRJCard, EditCardinal;

FROM Keyboard IMPORT
    (* proc *)	InKey, PutBack;

FROM Play3 IMPORT
    (* const*)	CycleCount,
    (* type *)	WaveNumber, GainType, WaveGroup, EnvelopeArrayIndex,
    (* proc *)	DefineWaveform, DefineEnvelope, Play;

FROM MATHLIB IMPORT
    (* proc *)	Sin, Cos, Exp;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE;

(************************************************************************)

TYPE
    VarRange = [0..6];

VAR
    (* Waveform table pointers. *)

    Wave: ARRAY WaveNumber OF WaveGroup;

    (* Array to define the attack/decay envelope. *)

    Envelope: ARRAY EnvelopeArrayIndex OF GainType;

    (* Where the music is stored.  We also use part of the MainVoice	*)
    (* array for temporary storage of data.				*)

    MainVoice: ARRAY [0..7] OF CARDINAL;
    NullVoice: ARRAY [0..0] OF CARDINAL;

    (* Some variables whose values will be adjusted from the keyboard.	*)

    attack, decay, sustain: CARDINAL;

    (* Envelope generation model. *)

    Model: [0..1];

    (* Map that tells us how to access the variables that can be	*)
    (* changed from the keyboard.					*)

    VarMap: ARRAY VarRange OF
		RECORD
		    address: POINTER TO CARDINAL;
		    max: CARDINAL;
		END (*RECORD*);

(************************************************************************)
(*			ENVELOPE CALCULATIONS				*)
(************************************************************************)

PROCEDURE EnvelopeModel0 (Attack, Decay, Sustain: LONGREAL);

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

	    Envelope[j] := VAL(GainType, scaletogain*level);

	END (*FOR*);

    END EnvelopeModel0;

(************************************************************************)

PROCEDURE OldEnvelopeModel1 (alpha, beta, steady: LONGREAL);

    (* Calculates the envelope array using a second order dynamic model.*)
    (* This model is not yet working well, apparently because I'm not	*)
    (* working with a useful range of parameters.			*)

    CONST small = 1.0E-20;

    VAR state: ARRAY [0..1] OF LONGREAL;
	nextstate: LONGREAL;  j: EnvelopeArrayIndex;
	Result: ARRAY EnvelopeArrayIndex OF LONGREAL;
	min, max, scale: LONGREAL;

    BEGIN
	min := 0.0;  max := 0.0;

	(* Map alpha and beta to the stable range: alpha from -2.0 to	*)
	(* +2.0, and beta from -1.0 to 1.0-ABS(alpha).			*)

	alpha := 4.0*alpha - 2.0;
	beta := (2.0 - ABS(alpha))*beta - 1.0;

	state[0] := -steady;  state[1] := -steady;
	FOR j := 0 TO MAX(EnvelopeArrayIndex) DO
	    Result[j] := state[1] + steady;
	    IF Result[j] < min THEN min := Result[j]
	    ELSIF Result[j] > max THEN max := Result[j]
	    END (*IF*);
	    nextstate := alpha*state[1] + beta*state[0];
	    state[0] := state[1];
	    state[1] := nextstate;
	END (*FOR*);

	(* Now scale the results to fit into the desired range. *)

	IF ABS(max-min) < small THEN
	    Envelope[0] := 0;
	    FOR j := 1 TO MAX(EnvelopeArrayIndex) DO
		Envelope[j] := MAX (GainType);
	    END (*FOR*);
	ELSE
	    scale := LONGREAL(MAX(GainType))/(max - min);
	    FOR j := 0 TO MAX(EnvelopeArrayIndex) DO
		Envelope[j] := VAL (GainType, scale*(Result[j] - min));
	    END (*FOR*);
	END (*IF*);

    END OldEnvelopeModel1;

(************************************************************************)

PROCEDURE EnvelopeModel1 (A, D, steady: LONGREAL);

    (* Calculates the envelope array using a second order dynamic model.*)
    (* This model is not yet working well, apparently because I'm not	*)
    (* working with a useful range of parameters.			*)

    CONST small = 1.0E-20;
	scaleA = 10.0;  scaleD = 0.5;

    VAR j: EnvelopeArrayIndex;
	Result: ARRAY EnvelopeArrayIndex OF LONGREAL;
	min, max, scale: LONGREAL;
	t, deltat: LONGREAL;
	K1, K2, K3, alpha: LONGREAL;

    BEGIN
	min := 0.0;  max := 0.0;
	K1 := steady;  K2 := -K1;
	deltat := 0.2;
	alpha := scaleD*D;
	K3 := scaleA*A + alpha*K2;
	t := 0.0;

	FOR j := 0 TO MAX(EnvelopeArrayIndex) DO
	    Result[j] := K1 + Exp(-alpha*t)*(K2*Cos(t) + K3*Sin(t));
	    IF Result[j] < min THEN min := Result[j]
	    ELSIF Result[j] > max THEN max := Result[j]
	    END (*IF*);
	    t := t + deltat;
	END (*FOR*);

	(* Now scale the results to fit into the desired range. *)

	scale := LONGREAL(MAX(GainType))/(max-min);
	FOR j := 0 TO MAX(EnvelopeArrayIndex) DO
	    Envelope[j] := VAL (GainType, scale*(Result[j]-min));
	END (*FOR*);

    END EnvelopeModel1;

(************************************************************************)

PROCEDURE UpdateEnvelope;

    (* Turns the current envelope parameters into an envelope array.	*)

    CONST scaletoreal = 1.0/LONGREAL(MAX(CARDINAL));

    VAR Attack, Decay, Sustain: LONGREAL;

    BEGIN
	Attack := scaletoreal*LONGREAL(attack);
	Decay := scaletoreal*LONGREAL(decay);
	Sustain := scaletoreal*LONGREAL(sustain);

	IF Model = 0 THEN EnvelopeModel0 (Attack, Decay, Sustain);
	ELSE EnvelopeModel1 (Attack, Decay, Sustain);
	END (*IF*);

    END UpdateEnvelope;

(************************************************************************)

PROCEDURE ShowEnvelope (w: Window);

    (* Displays the current envelope. *)

    VAR j: EnvelopeArrayIndex;

    BEGIN
	SetCursor (w, 2, 1);
	FOR j := 0 TO MAX(EnvelopeArrayIndex) DO
	    WriteRJCard (w, Envelope[j], 6);
	END (*FOR*);
    END ShowEnvelope;

(************************************************************************)
(*			   SCREEN EDITING				*)
(************************************************************************)

PROCEDURE EditValue (w: Window;  N: VarRange;  fieldsize: CARDINAL);

    (* Allows direct entry of the value of variable number N from the	*)
    (* keyboard.							*)

    VAR value: CARDINAL;

    BEGIN
	WITH VarMap[N] DO
	    value := address^;
	    EditCardinal (w, value, fieldsize);
	    IF value > max THEN value := max END(*IF*);
	    address^ := value;
	END (*WITH*);
    END EditValue;

(************************************************************************)

PROCEDURE AdjustValue (N: VarRange;  amount: INTEGER);

    (* Changes the value of variable number N by the given amount, with	*)
    (* wraparound.  We clip "amount" in the case where the value	*)
    (* specified is inappropriately large relative to the maximum	*)
    (* allowed value of variable number N.				*)

    VAR value, max, absamount: CARDINAL;  halfmax: INTEGER;

    BEGIN
	max := VarMap[N].max;
	IF max < MAX(CARDINAL) THEN
	    IF max = 1 THEN halfmax := 1
	    ELSE halfmax := max DIV 2;
	    END (*IF*);
	    IF amount > halfmax THEN amount := halfmax
	    ELSIF amount < -halfmax THEN amount := -halfmax
	    END (*IF*);
	END (*IF*);
	value := VarMap[N].address^;
	IF amount > 0 THEN
	    absamount := amount;
	    IF value <= max - absamount THEN
		value := value + absamount;
	    ELSE
		value := value - (max - absamount + 1);
	    END (*IF*);
	ELSE
	    absamount := -amount;
	    IF value >= absamount THEN
		value := value - absamount;
	    ELSE
		value := value + (max - absamount + 1);
	    END (*IF*);
	END (*IF*);
	VarMap[N].address^ := value;
    END AdjustValue;

(************************************************************************)

PROCEDURE DoTheTests;

    TYPE CharSet = SET OF CHAR;

    CONST
	Esc = CHR(27);  Digits = CharSet {'0'..'9'};
	userrow = 6;  colbase = 10;  fieldwidth = 5;  gap = 4;

    VAR w, EW: Window;  ch: CHAR;
	N: VarRange;  value: CARDINAL;

    (********************************************************************)

    PROCEDURE Highlight;

	(* Swaps the screen colour for field N. *)

	BEGIN
	    ColourSwap (w, userrow, colbase + (fieldwidth+gap)*N, fieldwidth);
	END Highlight;

    (********************************************************************)

    PROCEDURE PositionCursor (N: VarRange);

	(* Sets the screen cursor to the position of variable N.	*)

	BEGIN
	    SetCursor (w, userrow, colbase + (fieldwidth+gap)*N);
	END PositionCursor;

    (********************************************************************)

    PROCEDURE WriteValue (N: VarRange);

	(* Writes the value for field N to the screen. *)

	BEGIN
	    PositionCursor (N);
	    WriteRJCard (w, VarMap[N].address^, fieldwidth);
	END WriteValue;

    (********************************************************************)

    BEGIN
	OpenWindow (EW, black, white, 17, 24, 0, 79, simpleframe, nodivider);
	WriteString (EW, "Envelope:");
	OpenWindow (w, black, green, 8, 16, 0, 79, noframe, nodivider);
	WriteLn (w);
	WriteString (w, "        Cursor right/left to select variable,");
	WriteString (w, " up/down to change value,");
	WriteLn (w);
	WriteString (w, "       PgUp/PgDown for coarse changes, Space to");
	WriteString (w, " play note, Esc to exit");
	SetCursor (w, userrow-2, colbase-1);
	WriteString (w, "Duration Frequency Waveform Attack   Decay");
	WriteString (w, "   Sustain    Model");
	SetCursor (w, userrow, colbase);
	FOR N := 0 TO 6 DO
	    WriteValue (N);
	END (*FOR*);
	N := 0;
	LOOP
	    UpdateEnvelope;  ShowEnvelope (EW);
	    Highlight;
	    ch := InKey();
	    Highlight;
	    IF ch = Esc THEN
		EXIT (*LOOP*);
	    ELSIF ch = " " THEN
		Play (MainVoice, NullVoice, NullVoice);
	    ELSIF ch IN Digits THEN
		PositionCursor (N);
		PutBack (ch);
		EditValue (w, N, fieldwidth);
		WriteValue (N);
	    ELSIF ch = CHR(0) THEN
		ch := InKey();
		IF ch = "H" THEN		(* cursor up *)
		    AdjustValue (N, 1);
		    WriteValue (N);
		ELSIF ch = "P" THEN		(* cursor down *)
		    AdjustValue (N, -1);
		    WriteValue (N);
		ELSIF ch = "I" THEN		(* page up *)
		    AdjustValue (N, 250);
		    WriteValue (N);
		ELSIF ch = "Q" THEN		(* page down *)
		    AdjustValue (N, -250);
		    WriteValue (N);
		ELSIF ch = "M" THEN		(* cursor right *)
		    IF N = MAX(VarRange) THEN N := 0
		    ELSE INC(N);
		    END (*IF*);
		ELSIF ch = "K" THEN		(* cursor left *)
		    IF N = 0 THEN N := MAX(VarRange)
		    ELSE DEC(N);
		    END (*IF*);
		END (*IF*);

	    END (*IF*);

	END (*LOOP*);

    END DoTheTests;

(************************************************************************)
(*			MODULE INITIALISATION				*)
(************************************************************************)

PROCEDURE Initialise;

    CONST
	PI = 3.141592653589793240;
	offset = LONGREAL(128*CycleCount-1) / 3.0;
	bound = 2.0*offset;		(* limit on peak-to-peak value	*)

    VAR scale: LONGREAL;
	j, break: SHORTCARD;
	w: WaveNumber;
	v: VarRange;
	g: GainType;
	Gain: ARRAY GainType OF LONGREAL;

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
	(* Create the waveform arrays. *)

	FOR g := 0 TO MAX(GainType) DO
	    Gain[g] := LONGREAL(g)/LONGREAL(MAX(GainType));
	    FOR w := 0 TO HIGH(Wave) DO
		NEW (Wave[w][g]);
	    END (*FOR*);
	END (*FOR*);

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

	(* Pass the waveforms and envelope address to the subsidiary	*)
	(* module.							*)

	FOR w := 0 TO 7 DO
	    DefineWaveform (w, Wave[w]);
	END (*FOR*);
	DefineEnvelope (0, ADR(Envelope));

	(* Put some code in the Voice arrays.	*)

	MainVoice[0] := 65534;
	MainVoice[1] := 300;		(* Duration *)
	MainVoice[2] := 65532;
	MainVoice[3] := 2;		(* Waveform number *)
	MainVoice[4] := 65531;
	MainVoice[5] := 0;		(* Envelope number *)
	MainVoice[6] := 2750;		(* note code *)
	MainVoice[7] := 65535;
	NullVoice[0] := 65535;

	(* Set up the VarMap array. *)

	FOR v := 0 TO MAX(VarRange) DO
	    VarMap[v].max := MAX(CARDINAL);
	END (*FOR*);
	VarMap[2].max := MAX(WaveNumber);
	VarMap[6].max := 1;

	VarMap[0].address := ADR(MainVoice[1]);		(* Duration *)
	VarMap[1].address := ADR(MainVoice[6]);		(* Note code *)
	VarMap[2].address := ADR(MainVoice[3]);		(* Waveform number *)
	VarMap[3].address := ADR(attack);		(* attack *)
	VarMap[4].address := ADR(decay);		(* decay *)
	VarMap[5].address := ADR(sustain);		(* sustain *)
	VarMap[6].address := ADR(Model);		(* model *)

	(* Give initial values to some other global variables. *)

	Model := 1;
	attack := 65535;
	decay := 2000;
	sustain := 32768;

    END Initialise;

(************************************************************************)

BEGIN
    Initialise;
    DoTheTests;
END Plink.
