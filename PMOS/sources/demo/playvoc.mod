MODULE PlayVOC;

	(********************************************************)
	(*							*)
	(*		Plays sound from a VOC file		*)
	(*		    to the PC speaker			*)
	(*							*)
	(*  Programmer:		T. Channon			*)
	(*		with some modifications by P. Moylan	*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(*	This module has been reorganised a little	*)
	(*	by PM, but it's still basically TC's code.	*)
	(*							*)
	(*	Changes:					*)
	(*	  - some code lifted out of lower-level		*)
	(*	    modules.					*)
	(*	  - declarations shifted around to make them	*)
	(*	    as local as possible.			*)
	(*	  - sample rate doubling for input data with	*)
	(*	    slow sampling.				*)
	(*							*)
	(********************************************************)

IMPORT Lib, Str;

FROM FileSys IMPORT
    (* type *)	File,
    (* proc *)	OpenFile, SetPosition, ReadByte, ReadRecord, CloseFile;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode,
    (* proc *)	TranslateErrorCode;

FROM PlayList IMPORT
    (* type *)	BufferList, OutputBufferPointer,
    (* proc *)	CreateList, AddToList, PlayFromList, DiscardList;

FROM PlayBuff IMPORT
    (* const*)	OutputBufferSize,
    (* proc *)	SetCycleCount;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, WriteString, WriteChar, WriteLn, PressAnyKey,
		SetCursor;

FROM NumericIO IMPORT
    (* proc *)	WriteCard, WriteRJCard;

FROM LowLevel IMPORT
    (* proc *)	Far, Copy, BlockFill;

(************************************************************************)

CONST
    (* Size of an input buffer. *)

    SIZESOUNDBUFFER = 16384;

    (* Sampling rate below which we decide to make the output rate	*)
    (* twice the input rate.						*)

    SlowRate = 11000;  (* Hz *)

TYPE
    SoundBufferType = ARRAY [0..SIZESOUNDBUFFER-1] OF BYTE;

    HeadType =  RECORD
		    name : ARRAY [0..12H] OF CHAR;
		    eof  : CHAR;
		    ofs  : CARDINAL;
		    vers : CARDINAL;
		    verx : CARDINAL;
		END (*RECORD*);

VAR
    (* The screen window used for log messages. *)

    log: Window;

    (* PWM span - this depends on sampling rate. *)

    span: SHORTCARD; (* PWM span *)

(************************************************************************)
(*			MESSAGES TO SCREEN				*)
(************************************************************************)

PROCEDURE Err(s : ARRAY OF CHAR);

    BEGIN
	WriteLn (log);   WriteString (log, 'Error: ');
	WriteString (log, s);
	PressAnyKey (log);
	HALT;
    END Err;

(************************************************************************)
(*			SETTING THE SAMPLING RATE			*)
(************************************************************************)

PROCEDURE SetSampleRate (Hz : CARDINAL);

    (* Sets internal rate, module defaults to 13 kHz. *)

    CONST
	(* MAXPWM sets minimum allowed sample rate.  Timer rate is	*)
	(* 1.19318MHz, 200 is approx 6khz.  MINPWM sets max allowed	*)
	(* rate, would stress computer, 54 is 22.05kHz ie. half CD rate *)

	MAXPWM = 200;  MINPWM = 54;

    BEGIN
	span := SHORTCARD (1.1931817E6 / VAL(LONGREAL, Hz));
	IF (span >= MINPWM) & (span < MAXPWM) THEN
	    (* If error use last rate, default if never previously set. *)
	    SetCycleCount(span);
	END;
    END SetSampleRate;

(************************************************************************)
(*			DATA FORMAT CONVERSION				*)
(************************************************************************)

PROCEDURE ScaledValue (val: SHORTCARD;  scale: CARDINAL): SHORTINT;

    (* Requantizes val to suit the desired output range.  The basic	*)
    (* operation is to scale the value up and then take the high-order	*)
    (* bits of the product, adding some random noise for dithering.	*)
    (* The final step is a "round up" based on the most significant	*)
    (* of the ignored bits.						*)

    VAR ti: INTEGER;

    BEGIN
	ti := INTEGER(CARDINAL(val) * scale)
					+ INTEGER(Lib.RANDOM(256))-128;
	RETURN SHORTINT(ti DIV 128 + (ti MOD 128) DIV 64);
    END ScaledValue;

(************************************************************************)

PROCEDURE LoadBlock (f: File;  dlen: LONGCARD;  RepeatCount: CARDINAL;
				VAR (*INOUT*) OutList:BufferList);

    (* Converts one block of input data to the form we want, appends	*)
    (* the result to OutList.  Parameter dlen specifies the number of	*)
    (* input samples.  RepeatCount specifies how many output samples	*)
    (* will be produced for each input sample.  We pad out the data at	*)
    (* the end, if necessary, to give an integral number of output	*)
    (* buffers.								*)

    VAR scale: CARDINAL;
	OutBuffer: OutputBufferPointer;
	OutIndex: [0..OutputBufferSize-1];
	OutCount: CARDINAL;
	InData: POINTER TO SoundBufferType;
	InIndex: [0..SIZESOUNDBUFFER-1];
	InCount: CARDINAL;
	rptcount: CARDINAL;
	status: ErrorCode;

    BEGIN
	NEW (InData);  InIndex := 0;  rptcount := RepeatCount;
	NEW (OutBuffer);  OutIndex := 0;  OutCount := 1;
	SetCursor (log, 3, 1);
	WriteString (log, "     1 output buffers");
	scale := CARDINAL(span DIV 2)-2; (*reduce amplitude slightly: dither *)
	WHILE dlen > 0 DO
	    IF dlen > LONGCARD(SIZESOUNDBUFFER-1) THEN
		status := ReadRecord(f, ADR(InData^[InIndex]),
					SIZESOUNDBUFFER, InCount);
	    ELSE
		status := ReadRecord (f, ADR(InData^[InIndex]),
					CARDINAL(dlen), InCount);
	    END(*IF*);
	    LOOP
		IF dlen = 0 THEN EXIT END(*IF*);
		OutBuffer^[OutIndex] := ScaledValue (InData^[InIndex], scale);
		IF OutIndex = OutputBufferSize-1 THEN
		    AddToList (OutList, OutBuffer);
		    NEW (OutBuffer);  OutIndex := 0;
		    INC (OutCount);
		    SetCursor (log, 3, 1);
		    WriteRJCard (log, OutCount, 6);
		ELSE
		    INC (OutIndex);
		END (*IF*);
		DEC (rptcount);
		IF rptcount = 0 THEN
		    DEC(dlen);  rptcount := RepeatCount;
		    IF InIndex < (SIZESOUNDBUFFER-1) THEN
			INC (InIndex);
		    ELSE
			InIndex := 0;  EXIT;
		    END(*IF*);
		END (*IF*);
	    END(*LOOP*);
	END(*WHILE*);
	IF OutIndex < OutputBufferSize-1 THEN
	    BlockFill (Far(ADR(OutBuffer^[OutIndex])),
	    			OutputBufferSize - 1 - OutIndex, span DIV 2);
	END (*IF*);
	AddToList (OutList, OutBuffer);
	WriteLn (log);
	DISPOSE (InData);
    END LoadBlock;

(************************************************************************)
(*			THE MAIN PROCEDURE				*)
(************************************************************************)

PROCEDURE Play (f: File);

    (* Assumption: file f is already open.  Takes all the input data	*)
    (* from file f, converts it to our output format, and calls the	*)
    (* player procedure.						*)

    VAR blocktype : SHORTCARD;
	sam       : SHORTCARD;
	rl        : CARDINAL;
	dlen : LONGCARD;
	samplerate: CARDINAL; (* Hz *)
	RepeatCount: CARDINAL;
	comp: SHORTCARD;
	OutList: BufferList;
	status: ErrorCode;

    BEGIN
	CreateList (OutList);
	LOOP
	    IF (ReadRecord(f, ADR(blocktype), 1, rl) <> OK)
				OR (blocktype = 0) THEN EXIT END(*IF*);
	    CASE blocktype OF
	      | 1:	dlen := 0;
			status := ReadRecord (f, ADR(dlen), 3, rl);
			DEC (dlen, 2);
			RepeatCount := 1;
			sam := ReadByte (f);
			samplerate := VAL(CARDINAL,
				1.0E6 / VAL(LONGREAL, 256-CARDINAL(sam)));
			IF samplerate < SlowRate THEN
			    RepeatCount := 2;
			    samplerate := 2*samplerate;
			END (*IF*);
			SetSampleRate (samplerate);
			WriteString (log, 'Sample rate ');
			WriteCard (log, samplerate);
			WriteString (log, ' kHz');  WriteLn (log);
			comp := ReadByte (f);
			LoadBlock (f, dlen, RepeatCount, OutList);
	       ELSE
			Err('Unknown block header byte');
	    END(*CASE*);
	END(*LOOP*);
	PlayFromList (OutList);
	DiscardList (OutList);
    END Play;

(************************************************************************)

PROCEDURE Init(): File;

    CONST DEFAULTPWM = 13000; (* 13kHz *)

    VAR s: ARRAY [0..79] OF CHAR;
	rl: CARDINAL;
	head: HeadType;
	f: File;
	status: ErrorCode;

    BEGIN
	SetSampleRate (DEFAULTPWM);
	OpenWindow (log, black, green, 8, 16, 10, 70, simpleframe, nodivider);
	IF Lib.ParamCount() = 0 THEN
	    Err ("Missing file name");
	ELSIF Lib.ParamCount() = 1 THEN
	    Lib.ParamStr(s, 1);
	ELSE
	    Err ("Only one file name may be specified");
	END(*IF*);
	WriteString (log, "File: ");
	WriteString (log, s);
	WriteLn (log);
	status := OpenFile (f, s, FALSE);
	IF status <> OK THEN
	    TranslateErrorCode (status, s);
	    Err (s);
	END (*IF*);
	status := ReadRecord (f, ADR(head), SIZE(head), rl);
	IF Str.Pos(head.name, 'Creative Voice File') = MAX(CARDINAL) THEN
	    Err ('Not VOC file');
	END(*IF*);
	status := SetPosition (f, LONGCARD(head.ofs));
	Lib.RANDOMIZE;
	RETURN f;
    END Init;

(************************************************************************)

VAR f: File;

BEGIN
    f := Init();
    Play (f);
    CloseFile (f);
END PlayVOC.
