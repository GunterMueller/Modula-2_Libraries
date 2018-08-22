IMPLEMENTATION MODULE Graphics;

	(********************************************************)
	(*							*)
	(*		Screen graphics output.			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*	VGA and VESA graphics support based on code	*)
	(*	developed by Luke Plaizier and Warren Reynolds.	*)
	(*  Last edited:	27 February 1995		*)
	(*  Status:		Working				*)
	(*							*)
	(*	ATI mode 85 has suddenly gone buggy: the	*)
	(*	vertical resolution is out by a factor of 2.	*)
	(*	I can't see why - it was working until quite	*)
	(*	recently.  (Noticed later: it works on one of	*)
	(*	my machines but not on the other.)		*)
	(*							*)
	(*	Next big job: graphics accelerator support.	*)
	(*							*)
	(*	Hercules, CGA, EGA, and VGA modes working.	*)
	(*	SVGA seems to be working now.  Still need to	*)
	(*	add graphics accelerator features.		*)
	(*							*)
	(********************************************************)

(************************************************************************)
(*									*)
(*  A particular problem in supporting screen operations on PC or AT	*)
(*  compatibles is that there are enormous differences between models	*)
(*  in what sort of graphics interface is provided.  Some of the	*)
(*  lowest-level part of this problem is handled by module Screen.	*)
(*									*)
(*  Remark: There is a considerable amount of code duplication in this	*)
(*  module.  This is deliberate.  Things need to be done slightly	*)
(*  differently in different modes, and we can gain some time by using	*)
(*  separate procedures for the different modes rather than putting	*)
(*  the decision logic inside a single general-purpose procedure.	*)
(*  Since graphics is expensive in computer time, this space/time	*)
(*  tradeoff can be worthwhile.  We haven't gone all the way in this	*)
(*  direction - that would make the module enormous - but the		*)
(*  separation has been done for those cases where the decision logic	*)
(*  was becoming unreasonably long.					*)
(*									*)
(*  For special applications, especially where it is known a priori	*)
(*  that certain modes will never be used, there is still some scope	*)
(*  for tuning this code.  If you decide to support only a subset of	*)
(*  modes (e.g. only the planar 16-colour modes) then certain variables	*)
(*  become constants, which then allows some unreachable code to be	*)
(*  stripped out.  This may allow some IF decisions to be removed,	*)
(*  thereby speeding up the code.					*)
(*									*)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE;

FROM Types IMPORT
    (* type *)	FarPointerPointer, FarBytePointer, FarWordPointer;

FROM LowLevel IMPORT
    (* proc *)	MakePointer, Virtual, FarAddOffset, OutByte,
		IANDB, IORB, INOTB, RS, RSB, LS, LSB, Mul, BlockFill,
		BlockFillWord, FarCopy, OFFSET, INCV, DECV;

FROM Screen IMPORT
    (* type *)	VideoAdaptorType, ModeInfoType,
    (* proc *)	VideoKind, SetVideoMode, GetAddresses, GetModeInfo,
		SelectReadBank, SelectWriteBank;

(************************************************************************)

TYPE

    (* A ScreenLocation consists of a bank number and a pointer to a	*)
    (* byte within the bank.  The first field of this record type does	*)
    (* double duty: it's a "long offset" during the initial calculation	*)
    (* and a pointer thereafter.					*)

    SLvariant = [0..3];
    ScreenLocation = RECORD
			CASE :SLvariant OF
			  | 0:	L: LONGCARD;
			  | 1:	pb: FarBytePointer;
			  | 2:	pw: FarWordPointer;
			  | 3:	offset, high: CARDINAL;
			END (*IF*);
			bank: CARDINAL;
		    END (*RECORD*);

VAR

    (* The type of video adaptor which is installed.	*)

    AdaptorKind: VideoAdaptorType;

    (* Our opinion of the "best" modes this adaptor supports.	*)

    DefaultTextMode, DefaultGraphicsMode: CARDINAL;

    (* Segment for the memory-mapped screen, and the starting port	*)
    (* number for the video I/O ports.  Defined as variables because	*)
    (* they depend on the adaptor type and current video mode.		*)
    (* NOTE: IObase not used so far in this module.  It's here solely	*)
    (* because it is obtained as a side-effect of getting ScreenSeg.	*)

    ScreenSeg, IObase: CARDINAL;

    (* Pointers to the currently active font table, for drawing text.	*)
    (* Because of a quirky design flaw, we need two tables: one for	*)
    (* each of two sets of first 128 characters.			*)

    FontAddress0, FontAddress1: FarBytePointer;

    (* Information about the current graphics mode.	*)

    ModeData: ModeInfoType;

    (* Information about the "current position" on the screen.  This	*)
    (* record is here for the sake of some operations where we want to	*)
    (* set the position and location once just before a repetitious	*)
    (* sequence.  The data are not expected to be up-to-date at all	*)
    (* times; only when in use by a few special procedures.		*)

    CurrentPos: RECORD
		    loc: ScreenLocation;
		    DotsPerByte: [1..8];
		    fill: BYTE;
		END (*RECORD*);

    (* Table of screen addresses for the beginning of each screen line.	*)
    (* We precompute this information to speed up address calculations.	*)

    LineStart: ARRAY [0..1023] OF
			    RECORD
				CASE :BOOLEAN OF
				  | FALSE:	L: LONGCARD;
				  | TRUE:	offset, bank: CARDINAL;
				END (*IF*);
			    END (*RECORD*);

TYPE
    FillProcType = PROCEDURE (CARDINAL,CARDINAL,CARDINAL,CARDINAL,ColourType);
    LineProcType = PROCEDURE (CARDINAL, CARDINAL, INTEGER,
			CARDINAL, CARDINAL, CARDINAL, CARDINAL,
			BOOLEAN, ColourType);
    CopyRectangleProcType = PROCEDURE (CARDINAL, CARDINAL, CARDINAL, CARDINAL,
						INTEGER, INTEGER);
    CopyPartByteProcType = PROCEDURE (VAR ScreenLocation,
					VAR ScreenLocation, BYTE);
    CopyStringProcType = PROCEDURE (VAR ScreenLocation,
					VAR ScreenLocation, CARDINAL);

VAR
    (* Procedure to perform the "Fill" operation. *)

    FillProc: FillProcType;

    (* Procedure to plot a straight line. *)

    VisibleLine: LineProcType;

    (* Procedures to copy screen data. *)

    CopyRectangle: CopyRectangleProcType;
    CopyPartByte: CopyPartByteProcType;
    CopyString: CopyStringProcType;

(************************************************************************)
(*			  SPECIAL COLOUR MODES				*)
(************************************************************************)

TYPE
    BlackOrWhite = ColourType [0..1];
    CGAColour = ColourType [0..3];
    EGAColour = ColourType [0..15];

(************************************************************************)
(*		 	MASKS FOR BIT OPERATIONS			*)
(************************************************************************)

    (* A Mask value is used for stripping out a pixel from a byte, and	*)
    (* a Fill value is for filling a byte with one colour.		*)
    (* A Mask array is indexed by the pixel position (left to right)	*)
    (* within the byte, and a Fill array is indexed by colour.		*)
    (* A Keep value is similar to a Mask, but selects all pixels from	*)
    (* a given position onwards: KeepL selects the given pixel and all	*)
    (* pixels to the left, and KeepR selects the given pixel and all	*)
    (* pixels to its right.						*)

TYPE
    B2M = ARRAY [0..1] OF BYTE;
    B4M = ARRAY [0..3] OF BYTE;
    B8M = ARRAY [0..7] OF BYTE;

    B4C = ARRAY CGAColour OF BYTE;
    B2C = ARRAY BlackOrWhite OF BYTE;
    B16 = ARRAY EGAColour OF BYTE;

CONST
    (* Monochrome	*)

    Mask2 = B8M (80H, 40H, 20H, 10H, 8, 4, 2, 1);
    KeepL2 = B8M (80H, 0C0H, 0E0H, 0F0H, 0F8H, 0FCH, 0FEH, 0FFH);
    KeepR2 = B8M (0FFH, 7FH, 3FH, 1FH, 0FH, 7, 3, 1);
    Fill2 = B2C (0, 0FFH);

    (* Four-colour palette: CGA colour	*)

    Mask4 = B4M (0C0H,30H,0CH,03H);
    KeepL4 = B4M (0C0H,0F0H,0FCH,0FFH);
    KeepR4 = B4M (0FFH,3FH,0FH,03H);
    Fill4 = B4C (0, 55H, 0AAH, 0FFH);

    (* Two pixels per byte: used only by one special ATI mode.	*)

    Mask16 = B2M (0F0H, 0FH);
    KeepL16 = B2M (0F0H, 0FFH);
    KeepR16 = B2M (0FFH, 0FH);
    Fill16 = B16 (0, 11H, 22H, 33H, 44H, 55H, 66H, 77H, 88H, 99H,
			0AAH, 0BBH, 0CCH, 0DDH, 0EEH, 0FFH);

VAR
    Mask, KeepL, KeepR: B8M;
    FillArray: B16;

(************************************************************************)
(*			MANIPULATING A SCREEN POINTER			*)
(************************************************************************)

PROCEDURE StepForwardSrc (VAR (*INOUT*) loc: ScreenLocation;  K: CARDINAL);

    (* Steps a source screen location K bytes forward. *)

    BEGIN
	WITH loc DO
	    IF INCV (offset, K) THEN
		INC (bank);
		SelectReadBank (bank);
	    END (*IF*);
	END (*WITH*);
    END StepForwardSrc;

(************************************************************************)

PROCEDURE StepForwardDst (VAR (*INOUT*) loc: ScreenLocation;  K: CARDINAL);

    (* Like StepForwardSrc, but for a destination location. *)

    BEGIN
	WITH loc DO
	    IF INCV (offset, K) THEN
		INC (bank);
		SelectWriteBank (bank);
	    END (*IF*);
	END (*WITH*);
    END StepForwardDst;

(************************************************************************)

PROCEDURE StepForwardTrans (VAR (*INOUT*) loc: ScreenLocation;  K: CARDINAL);

    (* Steps a screen location loc to K bytes beyond its current point.	*)
    (* This is the appropriate "step forward" procedure to use for	*)
    (* transparent write operations (plotting dots, etc.).		*)

    BEGIN
	WITH loc DO
	    IF INCV (offset, K) THEN
		INC (bank);
		SelectWriteBank (bank);
		IF ModeData.BitsPerPixel < 8 THEN
		    SelectReadBank (bank);
		END (*IF*);
	    END (*IF*);
	END (*WITH*);
    END StepForwardTrans;

(************************************************************************)
(*		THE BASIC GRAPHICS OPERATION - PLOTTING A DOT		*)
(************************************************************************)

PROCEDURE PlotDot64K (x, y: CARDINAL;  colour: ColourType);

    (* Writes a dot at screen position (x, y).  This procedure is for	*)
    (* the direct colour modes, i.e. those which use a full word per	*)
    (* pixel.								*)

    VAR PixelLocation: ScreenLocation;

    BEGIN
	WITH ModeData DO
	    IF colour > MaxColour THEN colour := MaxColour END(*IF*);
	    IF BLorigin THEN y := MaxY - y; END (*IF*);
	END (*WITH*);

	WITH PixelLocation DO
	    L := LineStart[y].L + Mul(2,x);
	    bank := high;
	    high := ScreenSeg;
	    SelectWriteBank (bank);
	    pw^ := colour;
	END (*WITH*);

    END PlotDot64K;

(************************************************************************)

PROCEDURE PlotDot (x, y: CARDINAL;  colour: ColourType);

    (* Writes a dot at screen position (x, y).  Handles all modes.	*)

    VAR mask, fill: BYTE;
	DotsPerByte: CARDINAL;
	PixelLocation: ScreenLocation;

    BEGIN
	WITH ModeData DO
	    IF BitsPerPixel > 8 THEN
		PlotDot64K (x, y, colour);
		RETURN;
	    END (*IF*);
	    DotsPerByte := 8 DIV BitsPerPixel;
	    IF colour > MaxColour THEN colour := MaxColour END(*IF*);
	    mask := Mask [x MOD DotsPerByte];
	    IF BLorigin THEN y := MaxY - y END (*IF*);
	END (*WITH*);
	fill := FillArray [colour];

	(* Turn the (x,y) coordinates into a video memory address.	*)

	WITH PixelLocation DO
	    IF ModeData.MultiBank THEN
		L := LineStart[y].L + LONGCARD(x DIV DotsPerByte);
		bank := high;
		SelectWriteBank (bank);
		IF DotsPerByte > 1 THEN
		    SelectReadBank (bank);
		END (*IF*);
	    ELSE
		offset := LineStart[y].offset + x DIV DotsPerByte;
		bank := 0;
	    END (*IF*);
	    high := ScreenSeg;
	END (*WITH*);

	(* Clear out the old pixel value and set a new value.  For the	*)
	(* planar modes, we do this by loading the colour code into	*)
	(* the set/reset register, the mask into the bit mask register,	*)
	(* and reading then writing the video memory location.  (The	*)
	(* actual data read and written are irrelevant, since the	*)
	(* actual data are taken from the graphics controller's		*)
	(* internal 32-bit latch.)  For the 256-colour modes, an 8-bit	*)
	(* colour code is written directly and no masking is needed.	*)
	(* For other modes, we have to do the masking ourselves.	*)

	IF ModeData.Planar THEN
	    OutByte (3CEH, 0);  OutByte (3CFH, BYTE(colour));
	    OutByte (3CEH, 8);  OutByte (3CFH, mask);
	    PixelLocation.pb^ := PixelLocation.pb^;
	ELSIF ModeData.MaxColour = 255 THEN
	    PixelLocation.pb^ := VAL(BYTE,colour);
	ELSE
	    PixelLocation.pb^ := IANDB (PixelLocation.pb^, INOTB(mask))
						+ IANDB (mask, fill);
	END (*IF*);

    END PlotDot;

(************************************************************************)
(*			PLOTTING A SHORT STRING OF DOTS			*)
(************************************************************************)

PROCEDURE SetScreenLocation (x, y: CARDINAL);

    (* Turns the (x,y) coordinates into a video memory address, storing	*)
    (* the answer in global variable CurrentPos.  The processing	*)
    (* includes setting the memory bank for the multi-bank modes.	*)
    (* This procedure is designed for situations where we are about to	*)
    (* do some repetitive operations starting at location (x,y), i.e.	*)
    (* situations where we don't want to have to re-do this operation	*)
    (* for each pixel.							*)

    (* At present this procedure handles only those modes with at most	*)
    (* 8 bits per pixel.  For the other modes it is never called.	*)

    BEGIN
	IF BLorigin THEN y := ModeData.MaxY - y END (*IF*);
	WITH CurrentPos DO
	    DotsPerByte := 8 DIV ModeData.BitsPerPixel;
	    WITH loc DO
		IF ModeData.MultiBank THEN
		    L := LineStart[y].L + LONGCARD(x DIV DotsPerByte);
		    bank := high;
		    SelectWriteBank (bank);
		    IF DotsPerByte > 1 THEN
			SelectReadBank (bank);
		    END (*IF*);
		ELSE
		    bank := 0;
		    offset := LineStart[y].offset + x DIV DotsPerByte;
		END (*IF*);
		high := ScreenSeg;
	    END (*WITH*);
	END (*WITH*);
    END SetScreenLocation;

(************************************************************************)

PROCEDURE SetColour (colour: ColourType);

    (* Processes the colour, storing the answer in global variable	*)
    (* CurrentPos.  The colour processing involves calculating a "fill"	*)
    (* variable appropriate to the current mode, or loading the		*)
    (* set/reset register in the case of a planar mode.  This procedure	*)
    (* is designed for situations where we are about to do some		*)
    (* repetitive operations in a fixed colour, i.e. situations where	*)
    (* we don't want to have to re-do this operation for each pixel.	*)

    (* At present this procedure handles only those modes with up to	*)
    (* 16 colours.  For the other modes it is never called.		*)

    BEGIN
	WITH ModeData DO
	    IF colour > MaxColour THEN colour := MaxColour END(*IF*);
	    IF Planar THEN
		OutByte (3CEH, 0);  OutByte (3CFH, BYTE(colour));
	    ELSIF MaxColour < 16 THEN
		CurrentPos.fill := FillArray[colour];
	    END (*IF*);
	END (*WITH*);
    END SetColour;

(************************************************************************)

PROCEDURE AlignedPattern (pattern: BYTE);

    (* Plots up to eight pixels starting at location (x,y) and		*)
    (* continuing horizontally.  This is a specialised version where	*)
    (* the caller must guarantee that x is aligned so that the pattern	*)
    (* in graphics memory starts at a byte boundary.  We don't use the	*)
    (* whole of "pattern", only the leftmost bits: as many bits as	*)
    (* would fill up one byte of graphics memory.			*)

    (* This procedure does NOT handle all modes; only those for which	*)
    (* ModeData.BitsPerPixel < 8.					*)

    (* The caller is expected to have set up CurrentPos.  On return,	*)
    (* CurrentPos.loc has been incremented.				*)

    VAR j: [0..3];
	DotsPerByte: [1..8];
	mask: BYTE;

    BEGIN
	DotsPerByte := CurrentPos.DotsPerByte;

	WITH ModeData DO

	    (* Note that the caller was required to guarantee that	*)
	    (* BitsPerPixel < 8, hence DotsPerByte > 1; and that	*)
	    (* x MOD DotsPerByte = 0.					*)

	    IF DotsPerByte = 8 THEN
		mask := pattern;
	    ELSE
		mask := 0;
		FOR j := 0 TO DotsPerByte-1 DO
		    IF IANDB(pattern,80H) <> BYTE(0) THEN
			INC (mask, Mask[j]);
		    END (*IF*);
		    pattern := LSB(pattern,1);
		END (*FOR*);
	    END (*IF*);

	END (*WITH*);

	(* Clear out the old pixel values and set new values.  For the	*)
	(* planar modes, the set/reset register has already been loaded	*)
	(* with the colour code; next we load the mask into the bit	*)
	(* mask register, and read and then write the video memory	*)
	(* location.  (The actual data read and written are irrelevant,	*)
	(* since the actual data are taken from the graphics		*)
	(* controller's internal 32-bit latch.)  For other modes, we	*)
	(* have to do the masking ourselves.				*)

	IF ModeData.Planar THEN
	    OutByte (3CEH, 8);  OutByte (3CFH, mask);
	    CurrentPos.loc.pb^ := CurrentPos.loc.pb^;
	ELSE
	    CurrentPos.loc.pb^ := IANDB (CurrentPos.loc.pb^, INOTB(mask))
					+ IANDB (mask, CurrentPos.fill);
	END (*IF*);
	StepForwardTrans (CurrentPos.loc, 1);

    END AlignedPattern;

(************************************************************************)

PROCEDURE PlotPattern (pattern: BYTE;  x, y: CARDINAL;  colour: ColourType);

    (* Plots up to eight pixels starting at location (x,y) and		*)
    (* continuing horizontally.						*)

    VAR j, PixelsPerByte: [0..8];

    BEGIN
	IF ModeData.BitsPerPixel >= 8 THEN

	    (* The best we can do is to plot the dots one by one. *)

	    INC (x,7);
	    FOR j := 0 TO 7 DO
		IF ODD(pattern) THEN
		    PlotDot (x, y, colour);
		END (*IF*);
		DEC (x);
		pattern := SHORTCARD(pattern) DIV 2;
	    END (*FOR*);

	ELSE

	    SetScreenLocation (x, y);
	    SetColour (colour);
	    PixelsPerByte := 8 DIV ModeData.BitsPerPixel;
	    j := x MOD PixelsPerByte;

	    IF j <> 0 THEN
		AlignedPattern (RSB(pattern, j));
		pattern := LSB (pattern, PixelsPerByte - j);
	    END (*IF*);
	    WHILE pattern <> BYTE(0) DO
		AlignedPattern (pattern);
		pattern := LSB (pattern, PixelsPerByte);
	    END (*WHILE*);

	END (*IF*);
    END PlotPattern;

(************************************************************************)
(*				BLOCK MOVES				*)
(*									*)
(*  Restriction: all of these procedures are restricted to the case	*)
(*  where distance to move the data is an integral number of bytes;	*)
(*  and in the case where the source and destination rectangles overlap	*)
(*  then the move has to be upwards on the screen.  Thus we do not	*)
(*  have a completely general "block copy" operation, but we do have	*)
(*  something sufficient to support "scroll up" and similar operations.	*)
(*									*)
(*  For all procedures in this group except ACopy, the y values are in	*)
(*  hardware-dependent form, i.e. y=0 is at the top of the screen.	*)
(*									*)
(************************************************************************)

PROCEDURE CopyPartByte0 (VAR (*INOUT*) src, dst: ScreenLocation;  mask: BYTE);

    (* Copies a partial byte, namely the bits selected by mask, from	*)
    (* src to dst.  This procedure is for packed modes with no		*)
    (* bank switching.  On return src and dst have been updated.	*)

    BEGIN
	dst.pb^ := IORB(IANDB(dst.pb^, INOTB(mask)), IANDB (src.pb^, mask));
	INC (src.offset);  INC (dst.offset);
    END CopyPartByte0;

(************************************************************************)

PROCEDURE CopyPartByteMultibank (VAR (*INOUT*) src, dst: ScreenLocation;
							mask: BYTE);

    (* Copies a partial byte, namely the bits selected by mask, from	*)
    (* src to dst.  On return src and dst have been updated.		*)
    (* This version is for packed modes which require bank switching.	*)

    VAR srcval: BYTE;

    BEGIN
	WITH src DO
	    SelectReadBank (bank);
	    srcval := IANDB (pb^, mask);
	    IF INCV (offset, 1) THEN INC(bank) END(*IF*);
	END (*WITH*);
	WITH dst DO
	    SelectReadBank (bank);
	    SelectWriteBank (bank);
	    pb^ := IORB(IANDB(pb^, INOTB(mask)), srcval);
	    IF INCV (offset, 1) THEN INC(bank) END(*IF*);
	END (*WITH*);
    END CopyPartByteMultibank;

(************************************************************************)

PROCEDURE CopyPartBytePlanar (VAR (*INOUT*) src, dst: ScreenLocation;
							mask: BYTE);

    (* Copies a partial byte, namely the bits selected by mask, from	*)
    (* src to dst.  This procedure is for all planar modes.  On return	*)
    (* src and dst have been updated.					*)

    (* The gross inefficiency is regretted, but I couldn't find a	*)
    (* better way of doing the job within the limitations of the	*)
    (* EGA/VGA hardware.  The basic problem is that, through some	*)
    (* inexplicable hardware design oversight, the Bit Mask Register	*)
    (* is ignored in Write Mode 1.  If anyone knows of a better way to	*)
    (* do this operation, I'd love to hear about it.			*)

    VAR value0, value1, value2, value3, dummy: BYTE;

    BEGIN
	(* Read four source bytes, one from each bit plane.  After	*)
	(* that we must prime the internal 32-bit buffer with the	*)
	(* original destination value.					*)

	OutByte (3CEH, 4);	(* select "read map select" register *)
	WITH src DO
	    IF ModeData.MultiBank THEN SelectReadBank (bank) END (*IF*);
	    OutByte (3CFH, 0);  value0 := pb^;
	    OutByte (3CFH, 1);  value1 := pb^;
	    OutByte (3CFH, 2);  value2 := pb^;
	    OutByte (3CFH, 3);  value3 := pb^;
	    IF INCV (offset, 1) THEN INC(bank) END(*IF*);
	END (*WITH*);

	(* Set the bit mask, and disable set/reset mode, before writing	*)
	(* the new data.						*)

	OutByte (3CEH, 1);  OutByte (3CFH, 0);
	OutByte (3CEH, 8);  OutByte (3CFH, mask);

	(* Write the source values for the four planes. *)

	OutByte (3C4H, 2);	(* select map mask register *)
	WITH dst DO
	    IF ModeData.MultiBank THEN
		SelectReadBank (bank);
		SelectWriteBank (bank);
	    END (*IF*);
	    dummy := pb^;
	    OutByte (3C5H, 1);  pb^ := value0;
	    OutByte (3C5H, 2);  pb^ := value1;
	    OutByte (3C5H, 4);  pb^ := value2;
	    OutByte (3C5H, 8);  pb^ := value3;
	    IF INCV (offset, 1) THEN INC(bank) END(*IF*);
	END (*WITH*);

	(* Re-enable all planes, and set/reset mode, before returning. *)

	OutByte (3C5H, 0FH);  OutByte (3CEH, 1);  OutByte (3CFH, 0FH);

    END CopyPartBytePlanar;

(************************************************************************)

PROCEDURE StringCopy0 (VAR (*INOUT*) src, dst: ScreenLocation;
						count: CARDINAL);

    (* Copies a string of bytes from src to dst.  On return src and dst	*)
    (* have been updated.						*)
    (* This is the version for all modes without bank switching.	*)

    BEGIN
	IF ModeData.Planar THEN

	    (* Do the copy using Write Mode 1.  The Mode Register	*)
	    (* contains several subfields, but fortunately for us the	*)
	    (* other fields are all 0 for the planar modes.		*)

	    OutByte (3CEH, 5);  OutByte (3CFH, 1);
	END (*IF*);

	FarCopy (src.pb, dst.pb, count);
	INC (src.offset, count);  INC (dst.offset, count);

	IF ModeData.Planar THEN

	    (* Switch back to the default Write Mode 0.	*)

	    OutByte (3CEH, 5);  OutByte (3CFH, 0);

	END (*IF*);

    END StringCopy0;

(************************************************************************)

PROCEDURE StringCopyMultibank (VAR (*INOUT*) src, dst: ScreenLocation;
						count: CARDINAL);

    (* Copies a string of bytes from src to dst.  On return src and dst	*)
    (* have been updated.  This version is for all bank-switching modes.*)

    VAR amount: CARDINAL;

    BEGIN
	IF ModeData.Planar THEN

	    (* Do the copy using Write Mode 1.  The Mode Register	*)
	    (* contains several subfields, but fortunately for us the	*)
	    (* other fields are all 0 for the planar modes.		*)

	    OutByte (3CEH, 5);  OutByte (3CFH, 1);
	END (*IF*);

	WHILE count > 0 DO
	    amount := count;
	    IF src.offset > MAX(CARDINAL) - amount + 1 THEN
		amount := MAX(CARDINAL) - src.offset + 1;
	    END (*IF*);
	    IF dst.offset > MAX(CARDINAL) - amount + 1 THEN
		amount := MAX(CARDINAL) - dst.offset + 1;
	    END (*IF*);
	    SelectReadBank (src.bank);
	    SelectWriteBank (dst.bank);
	    FarCopy (src.pb, dst.pb, amount);
	    DEC (count, amount);
	    WITH src DO
		IF INCV (offset, amount) THEN INC(bank) END(*IF*);
	    END (*WITH*);
	    WITH dst DO
		IF INCV (offset, amount) THEN INC(bank) END(*IF*);
	    END (*WITH*);
	END (*WHILE*);

	IF ModeData.Planar THEN

	    (* Switch back to the default Write Mode 0.	*)

	    OutByte (3CEH, 5);  OutByte (3CFH, 0);

	END (*IF*);

    END StringCopyMultibank;

(************************************************************************)

PROCEDURE RowCopy (VAR (*INOUT*) src, dst: ScreenLocation;
				Lmask, Rmask: BYTE;  middlecount: CARDINAL);

    (* Copies a row of data from srcloc to dstloc.  Lmask and Rmask	*)
    (* are masks for the first and last partial bytes, and middlecount	*)
    (* is the number of complete bytes in the middle.  If everything	*)
    (* is neatly byte-aligned then Lmask = 0FFH, Rmask = 0FFH.		*)
    (* On return src and dst have been updated.				*)

    (* This version works for all modes, although in fact it is never	*)
    (* called for modes with an integral number of bytes per pixel.	*)

    BEGIN

	(* Perform a masked copy in the first partial byte, if any. *)

	IF Lmask <> BYTE(0FFH) THEN
	    CopyPartByte (src, dst, Lmask);
	END (*IF*);

	(* Copy the middle section.	*)

	IF middlecount > 0 THEN
	    CopyString (src, dst, middlecount);
	END (*IF*);

	(* Masked copy in the last partial byte, if any.	*)

	IF Rmask <> BYTE(0FFH) THEN
	    CopyPartByte (src, dst, Rmask);
	END (*IF*);

    END RowCopy;

(************************************************************************)

PROCEDURE ACopy0 (x0, y0, x1, y1: CARDINAL;  dx, dy: INTEGER);

    (* This is the version of the Copy operation used for modes		*)
    (* with no bank switching.  We assume that dx is an integral	*)
    (* multiple of the number of pixels per byte for the current mode.	*)

    VAR Lmask, Rmask: BYTE;
	firstcol, dstcol, middlecount, y: CARDINAL;
	DotsPerByte: [1..8];
	src, dst: ScreenLocation;

    BEGIN
	DotsPerByte := 8 DIV ModeData.BitsPerPixel;

	(* Work out the horizontal column range.  For our present	*)
	(* purposes, a "column" is a group of pixels which fit into one	*)
	(* byte.							*)

	firstcol := x0 DIV DotsPerByte;
	middlecount := x1 DIV DotsPerByte - firstcol;

	(* Work out the mask values for the left and right edges.	*)

	Lmask := KeepR[x0 MOD DotsPerByte];
	Rmask := KeepL[x1 MOD DotsPerByte];

	IF middlecount = 0 THEN
	    Lmask := IANDB(Lmask,Rmask);  Rmask := 0FFH;
	END (*IF*);

	IF Lmask = BYTE(0FFH) THEN
	    INC (middlecount);
	END (*IF*);

	IF Rmask <> BYTE(0FFH) THEN
	    DEC (middlecount);
	END (*IF*);

	dstcol := INTEGER(firstcol) + dx DIV INTEGER(DotsPerByte);

	WITH src DO
	    bank := 0;
	    high := ScreenSeg;
	END (*WITH*);
	dst := src;
	FOR y := y0 TO y1 DO
	    src.offset := LineStart[y].offset+firstcol;
	    dst.offset := LineStart[INTEGER(y) + dy].offset + dstcol;
	    RowCopy (src, dst, Lmask, Rmask, middlecount);
	END (*FOR*);

    END ACopy0;

(************************************************************************)

PROCEDURE ACopyMultibank (x0, y0, x1, y1: CARDINAL;  dx, dy: INTEGER);

    (* This is the version of the Copy operation used for multibank	*)
    (* modes with up to one byte per pixel.  We assume that dx is an	*)
    (* integral multiple of the number of pixels per byte.		*)

    VAR Lmask, Rmask: BYTE;
	firstcol, middlecount, y, linestep: CARDINAL;
	DotsPerByte: [1..8];
	src, dst: ScreenLocation;

    BEGIN
	WITH ModeData DO
	    DotsPerByte := 8 DIV ModeData.BitsPerPixel;
	    linestep := BytesPerRow - 1;
	END (*WITH*);

	(* Work out the horizontal column range.  For our present	*)
	(* purposes, a "column" is a group of pixels which fit into one	*)
	(* byte.							*)

	firstcol := x0 DIV DotsPerByte;
	middlecount := x1 DIV DotsPerByte - firstcol;
	DEC (linestep, middlecount);

	(* Work out the mask values for the left and right edges.	*)

	Lmask := KeepR[x0 MOD DotsPerByte];
	Rmask := KeepL[x1 MOD DotsPerByte];

	IF middlecount = 0 THEN
	    Lmask := IANDB(Lmask,Rmask);  Rmask := 0FFH;
	END (*IF*);

	IF Lmask = BYTE(0FFH) THEN
	    INC (middlecount);
	END (*IF*);

	IF Rmask <> BYTE(0FFH) THEN
	    DEC (middlecount);
	END (*IF*);

	WITH src DO
	    L := LineStart[y0].L + LONGCARD(firstcol);
	    bank := high;  high := ScreenSeg;
	END (*WITH*);
	WITH dst DO
	    L := LineStart[INTEGER(y0) + dy].L
		+ LONGCARD(INTEGER(firstcol) + dx DIV INTEGER(DotsPerByte));
	    bank := high;  high := ScreenSeg;
	END (*WITH*);

	FOR y := y0 TO y1 DO
	    RowCopy (src, dst, Lmask, Rmask, middlecount);
	    WITH src DO
		IF INCV(offset, linestep) THEN INC (bank) END(*IF*);
	    END (*WITH*);
	    WITH dst DO
		IF INCV(offset, linestep) THEN INC (bank) END(*IF*);
	    END (*WITH*);
	END (*FOR*);

    END ACopyMultibank;

(************************************************************************)

PROCEDURE ACopy1 (x0, y0, x1, y1: CARDINAL;  dx, dy: INTEGER);

    (* This is the version of the Copy operation used for modes with an	*)
    (* integral number of bytes per pixel.				*)

    VAR BytesPerDot, count, y, linestep: CARDINAL;
	src, dst: ScreenLocation;

    BEGIN
	WITH ModeData DO
	    BytesPerDot := BitsPerPixel DIV 8;
	    count := BytesPerDot * (x1 - x0 + 1);
	    linestep := BytesPerRow - count;
	END (*WITH*);

	WITH src DO
	    L := LineStart[y0].L + Mul(BytesPerDot, x0);
	    bank := high;  high := ScreenSeg;
	END (*WITH*);
	WITH dst DO
	    L := LineStart[INTEGER(y0) + dy].L
			+ Mul(BytesPerDot, INTEGER(x0)+dx);
	    bank := high;  high := ScreenSeg;
	END (*WITH*);

	FOR y := y0 TO y1 DO
	    CopyString (src, dst, count);
	    WITH src DO
		IF INCV(offset, linestep) THEN INC(bank) END(*IF*);
	    END (*WITH*);
	    WITH dst DO
		IF INCV(offset, linestep) THEN INC(bank) END(*IF*);
	    END (*WITH*);
	END (*FOR*);

    END ACopy1;

(************************************************************************)

PROCEDURE ACopy (xs, ys, width, height: CARDINAL;  dx, dy: INTEGER);

    (* Copies a rectangular region by an offset (dx, dy).  The pair	*)
    (* (xs,ys) gives the coordinates of the top left of the source	*)
    (* rectangle.  For packed or planar modes, we assume that dx is an	*)
    (* integral multiple of the number of pixels per byte.		*)

    BEGIN
	IF BLorigin THEN
	    ys := ModeData.MaxY - ys;
	    dy := -dy;
	END (*IF*);
	CopyRectangle (xs, ys, xs+width-1, ys+height-1, dx, dy);
    END ACopy;

(************************************************************************)
(*			FILLING A RECTANGULAR REGION			*)
(*									*)
(*  The following "fill" procedures fill a rectangular region with the	*)
(*  indicated colour.  The rectangle is specified by two opposite	*)
(*  corners (x0,y0) and (x1,y1), where x0<=x1 and y0<=y1.  Unlike most	*)
(*  of the other procedures in this module, the y values are in		*)
(*  hardware-dependent form, i.e. y=0 is at the top of the screen.	*)
(*  The master procedure "Fill" performs the conversion of y values if	*)
(*  necessary and ensures that the values are in the correct order.	*)
(*									*)
(************************************************************************)

PROCEDURE Fill256 (x0, y0, x1, y1: CARDINAL;  colour: ColourType);

    (* This is the "fill" procedure for the modes supporting 256 or	*)
    (* more colours.							*)

    CONST MaxOffset = 0FFFFH;

    VAR loc: ScreenLocation;
	count, y, M1, BytesPerPixel, bytesleft: CARDINAL;

    BEGIN
	WITH ModeData DO
	    M1 := BytesPerRow;
	    BytesPerPixel := BitsPerPixel DIV 8;
	END (*WITH*);

	count := x1 - x0 + 1;
	WITH loc DO
	    L := LineStart[y0].L + Mul(BytesPerPixel,x0);
	    bank := high;  high := ScreenSeg;
	    SelectWriteBank (bank);
	END (*WITH*);

	FOR y := y0 TO y1 DO

	    (* Slightly faked calculation below, as a temporary(?)	*)
	    (* expedient.						*)

	    IF loc.offset = 0 THEN bytesleft := MaxOffset
	    ELSE bytesleft := MaxOffset - loc.offset + 1;
	    END (*IF*);

	    IF BytesPerPixel*count <= bytesleft THEN
		IF BytesPerPixel = 1 THEN
		    BlockFill (loc.pb, count, VAL(BYTE,colour));
		ELSE
		    BlockFillWord (loc.pw, count, colour);
		END (*IF*);
		StepForwardDst (loc, M1);
	    ELSE
		IF BytesPerPixel = 1 THEN
		    BlockFill (loc.pb, bytesleft, VAL(BYTE,colour));
		ELSE
		    BlockFillWord (loc.pw, bytesleft DIV 2, colour);
		END (*IF*);
		StepForwardDst (loc, bytesleft);
		IF BytesPerPixel = 1 THEN
		    BlockFill (loc.pb, count-bytesleft, VAL(BYTE,colour));
		ELSE
		    BlockFillWord (loc.pw, count - bytesleft DIV 2, colour);
		END (*IF*);
		StepForwardDst (loc, M1-bytesleft);
	    END (*IF*);
	END (*FOR*);

    END Fill256;

(************************************************************************)

PROCEDURE MultibankPlanarFill (ytop, ybottom: CARDINAL;  Lmask, Rmask: BYTE;
					firstcol, middlecount: CARDINAL);

    (* This procedure does part of the work for PlanarFill (see below)	*)
    (* in cases where memory bank switching is required.		*)

    CONST DotsPerByte = 8;  MaxOffset = 0FFFFH;

    VAR loc, BaseLocation: ScreenLocation;
	y, M1, bytesleft: CARDINAL;

    BEGIN
	(* Calculate the starting address in the video memory.  In what	*)
	(* follows, the actual data read and written are irrelevant;	*)
	(* the actual data are taken from the set/reset register (which	*)
	(* has already been loaded by the caller), and the pixels	*)
	(* affected are controlled by the bit mask register.		*)

	M1 := ModeData.BytesPerRow;
	WITH BaseLocation DO
	    L := LineStart[ytop].L + LONGCARD(firstcol);
	    bank := high;  high := ScreenSeg;
	    SelectWriteBank (bank);
	END (*WITH*);
	loc := BaseLocation;

	(* Redefine M1 to be the byte step between the end of a row	*)
	(* and the start of the next row.				*)

	DEC (M1, middlecount);
	IF Lmask <> BYTE(0FFH) THEN DEC(M1) END(*IF*);

	(* We go around the following loop once for each row. *)

	FOR y := ytop TO ybottom DO

	    (* Draw the dots in the first partial column, if any.	*)

	    IF Lmask <> BYTE(0FFH) THEN
		SelectReadBank (loc.bank);
		OutByte (3CFH, Lmask);
		loc.pb^ := loc.pb^;
		StepForwardDst (loc, 1);
	    END (*IF*);

	    (* Fill up the middle columns.	*)

	    IF middlecount > 0 THEN
		OutByte (3CFH, 0FFH);

		(* Slightly faked calculation below, as a temporary(?)	*)
		(* expedient.  It works because the case offset=0 will	*)
		(* never require a bank switch in the middle of a line.	*)

		IF loc.offset = 0 THEN bytesleft := MaxOffset
		ELSE bytesleft := MaxOffset - loc.offset + 1;
		END (*IF*);

		IF middlecount <= bytesleft THEN
		    BlockFill (loc.pb, middlecount, 0);
		    StepForwardDst (loc, middlecount);
		ELSE
		    BlockFill (loc.pb, bytesleft, 0);
		    StepForwardDst (loc, bytesleft);
		    BlockFill (loc.pb, middlecount-bytesleft, 0);
		    StepForwardDst (loc, middlecount-bytesleft);
		END (*IF*);
	    END (*IF*);

	    (* Draw the dots in the last partial column, if any.	*)

	    IF Rmask <> BYTE(0FFH) THEN
		SelectReadBank (loc.bank);
		OutByte (3CFH, Rmask);
		loc.pb^ := loc.pb^;
	    END (*IF*);

	    (* Move to the start of the next row. *)

	    StepForwardDst (loc, M1);

	END (*FOR*);

    END MultibankPlanarFill;

(************************************************************************)

PROCEDURE PlanarFill (x0, y0, x1, y1: CARDINAL;  colour: ColourType);

    (* This procedure is used only for the modes for which the video	*)
    (* memory is arranged as 4 planes.  (Most of the 16-colour modes	*)
    (* work this way.)  We treat these cases separately since the	*)
    (* method required is somewhat different than for the other modes.	*)

    CONST DotsPerByte = 8;

    VAR Lmask, Rmask: BYTE;
	loc: ScreenLocation;
	baseoffset, firstcol, middlecount, y, M1: CARDINAL;

    BEGIN
	M1 := ModeData.BytesPerRow;

	(* Work out the horizontal column range.  For our present	*)
	(* purposes, a "column" is a group of pixels which fit into one	*)
	(* byte.							*)

	firstcol := x0 DIV DotsPerByte;
	middlecount := x1 DIV DotsPerByte - firstcol;

	(* Work out the bit mask values for the left and right edges.	*)

	Lmask := KeepR2[x0 MOD DotsPerByte];
	Rmask := KeepL2[x1 MOD DotsPerByte];
	IF middlecount = 0 THEN
	    Lmask := IANDB (Lmask, Rmask);
	    Rmask := 0FFH;
	END (*IF*);

	IF Lmask = BYTE(0FFH) THEN
	    INC (middlecount);
	END (*IF*);

	IF Rmask <> BYTE(0FFH) THEN
	    DEC (middlecount);
	END (*IF*);

	(* Load the colour code into the set/reset register, then	*)
	(* select the bit mask register for all future operations.	*)

	OutByte (3CEH, 0);  OutByte (3CFH, VAL(BYTE,colour));
	OutByte (3CEH, 8);

	(* For modes where bank switching is required, the rest of this	*)
	(* job is done by a separate procedure.				*)

	IF ModeData.MultiBank THEN
	    MultibankPlanarFill (y0, y1, Lmask, Rmask, firstcol, middlecount);
	    RETURN;
	END (*IF*);

	(* Turn the (x0,y0) coordinates into an address in the video	*)
	(* memory.  In what follows, the actual data read and written	*)
	(* are irrelevant; the actual data are taken from the set/reset	*)
	(* register, and the pixels affected are controlled by the	*)
	(* bit mask register.						*)

	WITH loc DO
	    offset := LineStart[y0].offset + firstcol;
	    bank := 0;  high := ScreenSeg;
	END (*WITH*);
	baseoffset := loc.offset;

	(* Draw the strip in the first partial column, if any.	*)

	IF Lmask <> BYTE(0FFH) THEN
	    OutByte (3CFH, Lmask);
	    FOR y := y0 TO y1 DO
		loc.pb^ := loc.pb^;
		INC (loc.offset, M1);
	    END (*FOR*);
	    INC (baseoffset);
	    loc.offset := baseoffset;
	END (*IF*);

	(* Fill up the middle columns.	*)

	IF middlecount > 0 THEN
	    OutByte (3CFH, 0FFH);
	    FOR y := y0 TO y1 DO
		BlockFill (loc.pb, middlecount, 0);
		INC (loc.offset, M1);
	    END (*IF*);
	    INC (baseoffset, middlecount);
	    loc.offset := baseoffset;
	END (*IF*);

	(* Draw the strip in the last partial column, if any.	*)

	IF Rmask <> BYTE(0FFH) THEN
	    OutByte (3CFH, Rmask);
	    FOR y := y0 TO y1 DO
		loc.pb^ := loc.pb^;
		INC (loc.offset, M1);
	    END (*FOR*);
	END (*IF*);

    END PlanarFill;

(************************************************************************)

PROCEDURE Fill0 (x0, y0, x1, y1: CARDINAL;  colour: ColourType);

    (* This is the version of the Fill operation used for packed modes	*)
    (* with no bank switching.						*)

    VAR Lmask, Rmask, fillvalue: BYTE;
	Lfill, Rfill: SHORTCARD;
	loc: ScreenLocation;
	firstcol, middlecount, y: CARDINAL;
	DotsPerByte: [1..8];

    BEGIN
	DotsPerByte := 8 DIV ModeData.BitsPerPixel;
	fillvalue := FillArray[colour];

	(* Work out the horizontal column range.  For our present	*)
	(* purposes, a "column" is a group of pixels which fit into one	*)
	(* byte.							*)

	firstcol := x0 DIV DotsPerByte;
	middlecount := x1 DIV DotsPerByte - firstcol;

	(* Work out the mask values for the left and right edges.	*)
	(* Note: in the current version, each mask is the complement	*)
	(* of the corresponding mask in PlanarFill.  I have considered	*)
	(* changing my conventions, but it looks as if the hardware	*)
	(* differences justify keeping this inconsistency.		*)

	Lmask := INOTB (KeepR[x0 MOD DotsPerByte]);
	Rmask := INOTB (KeepL[x1 MOD DotsPerByte]);
	IF middlecount = 0 THEN
	    INC (Lmask, Rmask);  Rmask := 0;
	END (*IF*);

	IF Lmask = BYTE(0) THEN
	    INC (middlecount);
	ELSE
	    Lfill := IANDB (INOTB(Lmask), fillvalue);
	END (*IF*);

	IF Rmask <> BYTE(0) THEN
	    DEC (middlecount);
	    Rfill := IANDB (INOTB(Rmask), fillvalue);
	END (*IF*);

	loc.high := ScreenSeg;

	FOR y := y0 TO y1 DO

	    (* Turn the (x0,y) coordinates into an address in	*)
	    (* the video memory.				*)

	    loc.offset := LineStart[y].offset+firstcol;

	    (* Fix up the bits in the first partial column, if any.	*)

	    IF Lmask <> BYTE(0) THEN
		loc.pb^ := IORB(IANDB(loc.pb^, Lmask),Lfill);
		INC (loc.offset);
	    END (*IF*);

	    (* Fill up the middle columns.	*)

	    IF middlecount > 0 THEN
		BlockFill (loc.pb, middlecount, fillvalue);
		INC (loc.offset, middlecount);
	    END (*IF*);

	    (* Fix up the bits in the last partial column, if any.	*)

	    IF Rmask <> BYTE(0) THEN
		loc.pb^ := IORB(IANDB(loc.pb^, Rmask),Rfill);
	    END (*IF*);

	END (*FOR*);

    END Fill0;

(************************************************************************)

PROCEDURE Fill (x0, y0, x1, y1: CARDINAL;  colour: ColourType);

    (* Fills a rectangle with the indicated colour.  The rectangle is	*)
    (* specified by giving two opposite corners (x0,y0) and (x1,y1).	*)

    VAR temp: CARDINAL;

    BEGIN
	IF BLorigin THEN
	    WITH ModeData DO
		y0 := MaxY - y0;
		y1 := MaxY - y1;
	    END (*WITH*);
	END (*IF*);
	IF x0 > x1 THEN
	    temp := x0;  x0 := x1;  x1 := temp;
	END (*IF*);
	IF y0 > y1 THEN
	    temp := y0;  y0 := y1;  y1 := temp;
	END (*IF*);
	FillProc (x0, y0, x1, y1, colour);
    END Fill;

(************************************************************************)
(*			SETTING THE 256 COLOUR PALETTE			*)
(************************************************************************)

PROCEDURE SetPaletteColour (Palette_Index, Red, Green, Blue: SHORTCARD);

    (* Sets the colour for one palette register.  Applicable only to	*)
    (* VGA or better.							*)

    (* This procedure has apparently been observed to fail on a		*)
    (* Trident 9000C, though I haven't observed this myself.  Possibly	*)
    (* a problem of accessing the registers too quickly.  Apparently	*)
    (* a delay of 240 ns is needed between accesses to the PEL data	*)
    (* register.							*)

    VAR old_clock_value: SHORTCARD;

    BEGIN
	(* First disable the screen before the palette change. *)
(*
        OutByte(03C4H,1);
        old_clock_value := InByte(03C5H);
        OutByte(03C5H,IANDB(old_clock_value,254));
*)

	(* Now load the desired colour combination. *)

	OutByte(03C8H, Palette_Index);
	OutByte(03C9H, IANDB(Red,63));
	OutByte(03C9H, IANDB(Green,63));
	OutByte(03C9H, IANDB(Blue,63));

	(* Re-enable the screen. *)

	(* OutByte(03C5H,old_clock_value); *)

    END SetPaletteColour;

(************************************************************************)
(*									*)
(*			PLOTTING STRAIGHT LINES				*)
(*									*)
(************************************************************************)
(*									*)
(*  In the following group of procedures, a line is specified by giving	*)
(*  one point on it, a Boolean goingdown, and the slope deltay/deltax,	*)
(*  where deltax and deltay are nonnegative integers.  We always draw	*)
(*  lines from left to right, and the vertical direction is defined by	*)
(*  goingdown.  The "current point" is represented by a triple		*)
(*  (x,y,ScaledError).  The integer ScaledError is a measure of how far	*)
(*  the discretized point is from the true straight line; it is		*)
(*  implicitly calculated as						*)
(*		(y-y0)*deltax - (x-x0)*deltay				*)
(*  where (x0,y0) is a point which lies precisely on the line.  When	*)
(*  drawing a line which is partly hidden, we have to pass ScaledError	*)
(*  from one procedure to another, and for this to work we must use	*)
(*  the same deltax and deltay for each segment of the line.  This is	*)
(*  the main reason for passing deltax, deltay, and goingdown as	*)
(*  procedure parameters rather than calculating them internally from	*)
(*  the endpoints.							*)
(*									*)
(*  For efficiency, these procedures do not call PlotDot; instead, they	*)
(*  do the equivalent operations internally.  Although this produces	*)
(*  some code redundancy, the consequent gain in speed is worthwhile.	*)
(*									*)
(************************************************************************)

PROCEDURE VisibleLine64K (xcurrent, ycurrent: CARDINAL;  ScaledError: INTEGER;
			deltax, deltay, xlimit, ylimit: CARDINAL;
			goingdown: BOOLEAN;  colour: ColourType);

    (* Plots a straight line of slope deltay/deltax starting from	*)
    (* (xcurrent,ycurrent,ScaledError).  This procedure is used only	*)
    (* for the direct colour modes using two bytes per pixel.  For	*)
    (* details about the parameters, etc., see procedure VisibleLine0.	*)

    VAR xthreshold, ythreshold, OldError: INTEGER;
	M1, temp: CARDINAL;
	PixelLocation: ScreenLocation;

    BEGIN

	WITH ModeData DO
	    M1 := BytesPerRow;
	    IF BLorigin THEN temp := MaxY - ycurrent
	    ELSE temp := ycurrent
	    END (*IF*);
	END (*WITH*);

	(* Plot the initial point. *)

	WITH PixelLocation DO
	    L := LineStart[temp].L + Mul(2,xcurrent);
	    bank := high;  high := ScreenSeg;
	    SelectWriteBank (bank);
	    pw^ := colour;
	END (*WITH*);

	IF (deltax = 0) AND (deltay = 0) THEN RETURN END(*IF*);

	xthreshold := INTEGER(deltay DIV 2) - INTEGER(deltax);
	ythreshold := INTEGER(deltay) - INTEGER(deltax DIV 2);

	(* Now, here is the main part of the line algorithm.  Each time	*)
	(* around the loop we decide in which direction to move for the	*)
	(* next point. and then plot that point.			*)

	LOOP
	    OldError := ScaledError;

	    (* Increment x, if appropriate. *)

	    IF OldError > xthreshold THEN
		IF xcurrent = xlimit THEN EXIT(*LOOP*) END(*IF*);
		DEC (ScaledError, deltay);
		INC (xcurrent);
		WITH PixelLocation DO
		    IF INCV (offset, 2) THEN
			INC (bank);
			SelectWriteBank (bank);
		    END (*IF*);
		END (*WITH*);
	    END (*IF*);

	    (* Increment or decrement y, if appropriate. *)

	    IF OldError < ythreshold THEN
		IF ycurrent = ylimit THEN EXIT(*LOOP*) END(*IF*);
		INC (ScaledError, deltax);
		IF goingdown THEN
		    IF BLorigin THEN DEC(ycurrent) ELSE INC(ycurrent) END(*IF*);
		    WITH PixelLocation DO
			IF INCV (offset, M1) THEN
			    INC (bank);
			    SelectWriteBank (bank);
			END (*IF*);
		    END (*WITH*);
		ELSE			(* NOT goingdown*)
		    IF BLorigin THEN INC(ycurrent) ELSE DEC(ycurrent) END(*IF*);
		    WITH PixelLocation DO
			IF DECV (offset, M1) THEN
			    DEC (bank);
			    SelectWriteBank (bank);
			END (*IF*);
		    END (*WITH*);
		END (*IF*);
	    END (*IF*);

	    (* Plot the new point *)

	    PixelLocation.pw^ := colour;

	END (*LOOP*);

    END VisibleLine64K;

(************************************************************************)

PROCEDURE VisibleLinePlanar (xcurrent, ycurrent: CARDINAL;
			ScaledError: INTEGER;
			deltax, deltay, xlimit, ylimit: CARDINAL;
			goingdown: BOOLEAN;  colour: ColourType);

    (* For details about parameters, etc., see VisibleLine0 below.	*)
    (* This is a specialised version for the planar modes.  We single	*)
    (* out the planar modes for special treatment because there are	*)
    (* several such modes, they are popular, and therefore it is	*)
    (* worthwhile speeding up this case.				*)

    CONST DotsPerByte = 8;

    VAR xthreshold, ythreshold, OldError: INTEGER;
	mask0, mask: BYTE;
	M1, temp: CARDINAL;
	PixelLocation: ScreenLocation;

    BEGIN
	OutByte (3CEH, 0);  OutByte (3CFH, VAL(BYTE,colour));
	OutByte (3CEH, 8);
	mask0 := Mask2[0];
	mask := Mask2[xcurrent MOD DotsPerByte];

	WITH ModeData DO
	    M1 := BytesPerRow;
	    IF BLorigin THEN
		temp := MaxY - ycurrent;
	    ELSE
		temp := ycurrent;
	    END (*IF*);
	END (*WITH*);

	(* Plot the initial point. *)

	WITH PixelLocation DO
	    IF ModeData.MultiBank THEN
		L := LineStart[temp].L + LONGCARD(xcurrent DIV DotsPerByte);
		bank := high;
		SelectWriteBank (bank);
		SelectReadBank (bank);
	    ELSE
		offset := LineStart[temp].offset + xcurrent DIV DotsPerByte;
		bank := 0;
	    END (*IF*);
	    high := ScreenSeg;
	    OutByte (3CFH, mask);
	    pb^ := pb^;
	END (*WITH*);

	IF (deltax = 0) AND (deltay = 0) THEN RETURN END(*IF*);

	xthreshold := INTEGER(deltay DIV 2) - INTEGER(deltax);
	ythreshold := INTEGER(deltay) - INTEGER(deltax DIV 2);

	(* Now, here is the main part of the line algorithm.  Each time	*)
	(* around the loop we decide in which direction to move for the	*)
	(* next point. and then plot that point.			*)

	LOOP
	    OldError := ScaledError;

	    (* Increment x, if appropriate. *)

	    IF OldError > xthreshold THEN
		IF xcurrent = xlimit THEN EXIT(*LOOP*) END(*IF*);
		DEC (ScaledError, deltay);
		INC (xcurrent);
		mask := RSB (mask, 1);
		IF mask = BYTE(0) THEN
		    mask := mask0;
		    WITH PixelLocation DO
			IF INCV (offset, 1) THEN
			    INC (bank);
			    SelectWriteBank (bank);
			    SelectReadBank (bank);
			END (*IF*);
		    END (*WITH*);
		END (*IF*);
	    END (*IF*);

	    (* Increment or decrement y, if appropriate. *)

	    IF OldError < ythreshold THEN
		IF ycurrent = ylimit THEN EXIT(*LOOP*) END(*IF*);
		INC (ScaledError, deltax);
		IF goingdown THEN
		    IF BLorigin THEN DEC(ycurrent) ELSE INC(ycurrent) END(*IF*);
		    WITH PixelLocation DO
			IF INCV (offset, M1) THEN
			    INC (bank);
			    SelectWriteBank (bank);
			    SelectReadBank (bank);
			END (*IF*);
		    END (*WITH*);
		ELSE			(* NOT goingdown*)
		    IF BLorigin THEN INC(ycurrent) ELSE DEC(ycurrent) END(*IF*);
		    WITH PixelLocation DO
			IF DECV (offset, M1) THEN
			    DEC (bank);
			    SelectWriteBank (bank);
			    SelectReadBank (bank);
			END (*IF*);
		    END (*WITH*);
		END (*IF*);
	    END (*IF*);

	    (* Plot the new point *)

	    OutByte (3CFH, mask);
	    PixelLocation.pb^ := PixelLocation.pb^;

	END (*LOOP*);

    END VisibleLinePlanar;

(************************************************************************)

PROCEDURE VisibleLine0 (xcurrent, ycurrent: CARDINAL;  ScaledError: INTEGER;
			deltax, deltay, xlimit, ylimit: CARDINAL;
			goingdown: BOOLEAN;  colour: ColourType);

    (* Plots a straight line of slope deltay/deltax starting from	*)
    (* (xcurrent,ycurrent,ScaledError).  The method used is a variant	*)
    (* of Bresenham's algorithm.  Stops plotting when xcurrent is about	*)
    (* to step beyond xlimit or when ycurrent is about to step beyond	*)
    (* ylimit.  It is assumed that the input data have been		*)
    (* pre-processed to ensure that the initial point does not violate	*)
    (* the bounds, and that there is no risk of plotting a point	*)
    (* outside the range of the screen.					*)

    (* This version of the procedure works for all non-planar modes	*)
    (* which use one byte or less per pixel.				*)

    VAR xthreshold, ythreshold, OldError: INTEGER;
	mask0, mask, fill: BYTE;
	M1, D1, temp, frame, framewrap, DotsPerByte: CARDINAL;
	PixelLocation: ScreenLocation;

    BEGIN

	WITH ModeData DO
	    M1 := BytesPerRow;  D1 := FramesPerScreen;
	    IF D1 <> 1 THEN
		framewrap := 8192*(D1-1) - M1;
	    END (*IF*);
	    DotsPerByte := 8 DIV BitsPerPixel;

	    IF MaxColour < 16 THEN
		fill := FillArray[colour];
	    END (*IF*);

	    mask0 := Mask[0];
	    mask := Mask[xcurrent MOD DotsPerByte];

	    IF BLorigin THEN
		temp := MaxY - ycurrent;
	    ELSE
		temp := ycurrent;
	    END (*IF*);

	END (*WITH*);

	(* Plot the initial point. *)

	WITH PixelLocation DO
	    IF ModeData.MultiBank THEN
		frame := 0;
		L := LineStart[temp].L + LONGCARD(xcurrent DIV DotsPerByte);
		bank := high;
		SelectWriteBank (bank);
		IF DotsPerByte > 1 THEN
		    SelectReadBank (bank);
		END (*IF*);
	    ELSE
		frame := temp MOD D1;
		offset := LineStart[temp].offset + xcurrent DIV DotsPerByte;
		bank := 0;
	    END (*IF*);
	    high := ScreenSeg;

	    IF DotsPerByte = 1 THEN
		pb^ := VAL(SHORTCARD,colour);
	    ELSE
		pb^ := IANDB (pb^, INOTB(mask)) + IANDB (mask, fill);
	    END (*IF*);
	END (*WITH*);

	IF (deltax = 0) AND (deltay = 0) THEN RETURN END(*IF*);

	xthreshold := INTEGER(deltay DIV 2) - INTEGER(deltax);
	ythreshold := INTEGER(deltay) - INTEGER(deltax DIV 2);

	(* Now, here is the main part of the line algorithm.  Each time	*)
	(* around the loop we decide in which direction to move for the	*)
	(* next point. and then plot that point.			*)

	LOOP
	    OldError := ScaledError;

	    (* Increment x, if appropriate. *)

	    IF OldError > xthreshold THEN
		IF xcurrent = xlimit THEN EXIT(*LOOP*) END(*IF*);
		DEC (ScaledError, deltay);
		INC (xcurrent);
		mask := RSB (mask, ModeData.BitsPerPixel);
		IF mask = BYTE(0) THEN
		    mask := mask0;
		    WITH PixelLocation DO
			IF INCV (offset, 1) THEN
			    INC (bank);
			    SelectWriteBank (bank);
			    IF DotsPerByte > 1 THEN
				SelectReadBank (bank);
			    END (*IF*);
			END (*IF*);
		    END (*WITH*);
		END (*IF*);
	    END (*IF*);

	    (* Increment or decrement y, if appropriate. *)

	    IF OldError < ythreshold THEN
		IF ycurrent = ylimit THEN EXIT(*LOOP*) END(*IF*);
		INC (ScaledError, deltax);
		IF goingdown THEN
		    IF BLorigin THEN DEC(ycurrent) ELSE INC(ycurrent) END(*IF*);
		    IF D1 = 1 THEN
			IF INCV (PixelLocation.offset, M1) THEN
			    INC (PixelLocation.bank);
			    SelectWriteBank (PixelLocation.bank);
			    IF DotsPerByte > 1 THEN
				SelectReadBank (PixelLocation.bank);
			    END (*IF*);
			END (*IF*);
		    ELSIF frame = D1-1 THEN
			frame := 0;
			DEC (PixelLocation.offset, framewrap);
		    ELSE
			INC (frame);
			INC (PixelLocation.offset, 8192);
		    END (*IF*);
		ELSE			(* NOT goingdown*)
		    IF BLorigin THEN INC(ycurrent) ELSE DEC(ycurrent) END(*IF*);
		    IF D1 = 1 THEN
			IF DECV (PixelLocation.offset, M1) THEN
			    DEC (PixelLocation.bank);
			    SelectWriteBank (PixelLocation.bank);
			    IF DotsPerByte > 1 THEN
				SelectReadBank (PixelLocation.bank);
			    END (*IF*);
			END (*IF*);
		    ELSIF frame = 0 THEN
			frame := D1-1;
			INC (PixelLocation.offset, framewrap);
		    ELSE
			DEC (frame);
			DEC (PixelLocation.offset, 8192);
		    END (*IF*);
		END (*IF*);
	    END (*IF*);

	    (* Plot the new point *)

	    IF DotsPerByte = 1 THEN
		PixelLocation.pb^ := VAL(SHORTCARD,colour);
	    ELSE
		PixelLocation.pb^ := IANDB (PixelLocation.pb^, INOTB(mask))
						+ IANDB (mask, fill);
	    END (*IF*);

	END (*LOOP*);

    END VisibleLine0;

(************************************************************************)
(*
PROCEDURE OldVisibleLine (xcurrent, ycurrent: CARDINAL;  ScaledError: INTEGER;
			deltax, deltay, xlimit, ylimit: CARDINAL;
			goingdown: BOOLEAN;  colour: CARDINAL);

    (* Plots a straight line of slope deltay/deltax starting from	*)
    (* (xcurrent,ycurrent,ScaledError).  The method used is a variant	*)
    (* of Bresenham's algorithm.  Stops plotting when xcurrent is about	*)
    (* to step beyond xlimit or when ycurrent is about to step beyond	*)
    (* ylimit.  It is assumed that the input data have been		*)
    (* pre-processed to ensure that the initial point does not violate	*)
    (* the bounds, and that there is no risk of plotting a point	*)
    (* outside the range of the screen.					*)

    (* This procedure is no longer used, but is left here as internal	*)
    (* documentation: it uses the same logic as the more efficient	*)
    (* versions above, and is easier to read.				*)

    VAR xthreshold, ythreshold, OldError: INTEGER;

    BEGIN

	(* Plot the initial point. *)

	PlotDot (xcurrent, ycurrent, colour);
	IF (deltax = 0) AND (deltay = 0) THEN RETURN END(*IF*);

	xthreshold := INTEGER(deltay DIV 2) - INTEGER(deltax);
	ythreshold := INTEGER(deltay) - INTEGER(deltax DIV 2);

	(* Now, here is the main part of the line algorithm.  Each time	*)
	(* around the loop we decide in which direction to move for the	*)
	(* next point. and then plot that point.			*)

	LOOP
	    OldError := ScaledError;
	    IF OldError > xthreshold THEN
		IF xcurrent = xlimit THEN EXIT(*LOOP*) END(*IF*);
		DEC (ScaledError, deltay);
		INC (xcurrent);
	    END (*IF*);
	    IF OldError < ythreshold THEN
		IF ycurrent = ylimit THEN EXIT(*LOOP*) END(*IF*);
		INC (ScaledError, deltax);
		IF goingdown = BLorigin THEN
		    DEC (ycurrent);
		ELSE
		    INC (ycurrent);
		END (*IF*);
	    END (*IF*);
	    PlotDot (xcurrent, ycurrent, colour);
	END (*LOOP*);

    END OldVisibleLine;
*)
(************************************************************************)

PROCEDURE MoveToX (x0, y0, deltax, deltay, X: CARDINAL;  goingdown: BOOLEAN;
			VAR (*OUT*) ScaledError: INTEGER): CARDINAL;

    (* For the line starting at (x0,y0) of slope deltay/deltax, returns	*)
    (* the first y for which the discrete approximation to the line	*)
    (* hits x=X.  Also calculates the ScaledError at that point.	*)

    VAR result: LONGINT;
	longX, longdx, longdy: LONGINT;

    BEGIN
	(* Shift the origin. *)

	DEC (X, x0);

	(* The calculations below produce CARDINAL or INTEGER results,	*)
	(* but a greater range is needed for the temporary values after	*)
	(* each multiplication.  This is one of those unfortunate cases	*)
	(* where something which can be expressed clearly and concisely	*)
	(* in assembly language becomes obscure when written in a	*)
	(* high-level language.  The Modula-2 rules on assignment	*)
	(* compatibility don't help, either.				*)

	longX := LONGCARD(X);
	longdx := LONGCARD(deltax);  longdy := LONGCARD(deltay);
	
	(* We have to use a different method depending on whether the	*)
	(* line slope is greater or less than 1.			*)

	IF deltay <= deltax THEN
	    result := (2*longX*longdy + longdx - 1) DIV (2*longdx)
	ELSE
	    result := (2*longX - 1)*longdy DIV (2*longdx) + 1
	END (*IF*);

	ScaledError := INTEGER(result*longdx - longX*longdy);

	(* Reverse the origin shift. *)

	IF goingdown = BLorigin THEN RETURN y0-CARDINAL(result)
	ELSE RETURN y0+CARDINAL(result);
	END (*IF*);

    END MoveToX;

(************************************************************************)

PROCEDURE MoveToY (x0, y0, deltax, deltay, Y: CARDINAL;  goingdown: BOOLEAN;
			VAR (*OUT*) ScaledError: INTEGER): CARDINAL;

    (* For the line starting at (x0,y0) of slope deltay/deltax, returns	*)
    (* the first x for which the discrete approximation to the line	*)
    (* hits y=Y.  Also calculates the ScaledError at that point.	*)

    VAR result: LONGINT;
	longY, longdx, longdy: LONGINT;

    BEGIN
	(* Shift the origin. *)

	IF goingdown=BLorigin THEN Y := y0 - Y ELSE DEC (Y, y0) END(*IF*);

	longY := LONGINT(Y);
	longdx := LONGINT(deltax);  longdy := LONGINT(deltay);

	(* We have to use a different method depending on whether the	*)
	(* line slope is greater or less than 1.			*)

	IF deltay <= deltax THEN
	    result := (2*longY - 1)*longdx DIV (2*longdy) + 1
	ELSE
	    result := (2*longY*longdx + longdy - 1) DIV (2*longdy)
	END (*IF*);
	ScaledError := INTEGER(longY*longdx - result*longdy);

	(* Reverse the origin shift. *)

	RETURN VAL(CARDINAL,result)+x0;

    END MoveToY;

(************************************************************************)

PROCEDURE PlotLine (x0, y0, x1, y1: CARDINAL;  colour: ColourType);

    (* Plots a straight line from (x0,y0) to (x1,y1).  It is the	*)
    (* caller's responsibility to ensure that the coordinates are in	*)
    (* range for the current video mode.				*)

    VAR temp, deltay: CARDINAL;
	goingdown: BOOLEAN;

    BEGIN

	(* First, ensure that we are working in the +X direction.	*)

	IF x1 < x0 THEN
	    temp := x1;  x1 := x0;  x0 := temp;
	    temp := y1;  y1 := y0;  y0 := temp;
	END (*IF*);

	(* Check the Y direction. *)

	IF y1 >= y0 THEN
	    goingdown := NOT BLorigin;  deltay := y1 - y0;
	ELSE
	    goingdown := BLorigin;  deltay := y0 - y1;
	END (*IF*);

	(* Draw the line.*)

	VisibleLine (x0, y0, 0, x1-x0, deltay, x1, y1, goingdown, colour);

    END PlotLine;

(************************************************************************)

PROCEDURE PlotRectangle (R: Rectangle;  colour: ColourType);

    (* Plots a rectangle, with clipping if necessary to keep the	*)
    (* points within the screen boundary.				*)

    VAR leftOK, rightOK, topOK, bottomOK: BOOLEAN;

    BEGIN
	WITH R DO
	    WITH ModeData DO
		IF (left > INTEGER(MaxX)) OR (right < 0)
			OR (bottom > INTEGER(MaxY)) OR (top < 0) THEN RETURN;
		END (*IF*);
		leftOK := TRUE;  rightOK := TRUE;
		topOK := TRUE;  bottomOK := TRUE;
		IF left < 0 THEN
		    left := 0;  leftOK := FALSE;
		END(*IF*);
		IF right > INTEGER(MaxX) THEN
		    right := INTEGER(MaxX);  rightOK := FALSE;
		END(*IF*);
		IF bottom < 0 THEN
		    bottom := 0;  bottomOK := FALSE;
		END(*IF*);
		IF top > INTEGER(MaxY) THEN
		    top := INTEGER(MaxY);  topOK := FALSE;
		END(*IF*);
	    END (*WITH*);
	    IF leftOK THEN
		PlotLine (left, bottom, left, top, colour);
	    END(*IF*);
	    IF rightOK THEN
		PlotLine (right, bottom, right, top, colour);
	    END(*IF*);
	    IF bottomOK THEN
		PlotLine (left, bottom, right, bottom, colour);
	    END(*IF*);
	    IF topOK THEN
		PlotLine (left, top, right, top, colour);
	    END(*IF*);
	END (*WITH*);
    END PlotRectangle;

(************************************************************************)

PROCEDURE ClippedLine (x0, y0, x1, y1: CARDINAL;  colour: ColourType;
			left, right, ymin, ymax: CARDINAL);

    (* Like PlotLine, but plots only that part of the line which lies	*)
    (* in the rectangle (left <= x <= right), (ymin <= y <= ymax).	*)
    (* The caller is expected to ensure, by appropriate definition of	*)
    (* the rectangle, that all plotted points are in range for the	*)
    (* current video mode.						*)

    VAR temp, deltax, deltay, xlimit, ylimit: CARDINAL;
	goingdown: BOOLEAN;
	ScaledError: INTEGER;

    BEGIN

	(* First, ensure that we are working in the +X direction; and	*)
	(* check the Y direction.					*)

	IF x1 < x0 THEN
	    temp := x1;  x1 := x0;  x0 := temp;
	    temp := y1;  y1 := y0;  y0 := temp;
	END (*IF*);
	IF BLorigin THEN
	    goingdown := y1 < y0;
	ELSE
	    goingdown := y0 < y1;
	END (*IF*);

	(* Eliminate some (but not all) cases where we are going to	*)
	(* miss the rectangle entirely.  Also calculate the slope and	*)
	(* boundary parameters.						*)

	IF x1 < left THEN RETURN END(*IF*);
	ylimit := y1;
	IF goingdown = BLorigin THEN
	    IF (y0 < ymin) OR (y1 > ymax) THEN RETURN END(*IF*);
	    deltay := y0 - y1;
	    IF ylimit < ymin THEN ylimit := ymin END(*IF*);
	ELSE
	    IF (y0 > ymax) OR (y1 < ymin) THEN RETURN END(*IF*);
	    deltay := y1 - y0;
	    IF ylimit > ymax THEN ylimit := ymax END(*IF*);
	END (*IF*);

	deltax := x1 - x0;
	xlimit := x1;
	IF xlimit > right THEN xlimit := right END(*IF*);

	(* We've now extracted all we need to know about the target	*)
	(* point.  From here on, we use (x1,y1,ScaledError) to		*)
	(* represent the current point.					*)

	x1 := x0;  y1 := y0;  ScaledError := 0;

	(* Step up to the left boundary, if we're at the left of it.	*)

	IF x0 < left THEN
	    x1 := left;
	    y1 := MoveToX (x0, y0, deltax, deltay, left,
						goingdown, ScaledError);
	END (*IF*);

	(* We might not yet have hit the rectangle.	*)

	IF goingdown = BLorigin THEN
	    IF y1 < ymin THEN RETURN END(*IF*);
	    IF y1 > ymax THEN
		x1 := MoveToY (x0, y0, deltax, deltay, ymax,
					goingdown, ScaledError);
		y1 := ymax;
	    END (*IF*);
	ELSE
	    IF y1 > ymax THEN RETURN END(*IF*);
	    IF y1 < ymin THEN
		x1 := MoveToY (x0, y0, deltax, deltay, ymin,
					goingdown, ScaledError);
		y1 := ymin;
	    END (*IF*);
	END (*IF*);

	(* Check whether we missed the rectangle entirely.	*)

	IF x1 > right THEN RETURN END (*IF*);

	(* At last, we have something to plot.	*)

	VisibleLine (x1, y1, ScaledError, deltax, deltay,
					xlimit, ylimit, goingdown, colour);

    END ClippedLine;

(************************************************************************)
(*			PUTTING A MARK AT A POINT			*)
(************************************************************************)

PROCEDURE PlotMark (x, y: CARDINAL;
			colour: ColourType;  pointtype: SHORTCARD);

    (* Writes a symbol at screen position (x, y).	*)

    BEGIN
	CASE pointtype OF
	    1:	PlotLine (x-1,y-1,x+1,y+1, colour);	(*  X	*)
		PlotLine (x+1,y-1,x-1,y+1, colour);
	  |
	    2:	PlotLine (x-2,y-1,x+2,y-1, colour);	(* box	*)
		PlotLine (x+2,y-1,x+2,y+1, colour);
		PlotLine (x+2,y+1,x-2,y+1, colour);
		PlotLine (x-2,y+1,x-2,y-1, colour);
	  |
	    ELSE
		PlotDot (x, y, colour);			(* point *)
	END (*CASE*);
    END PlotMark;

(************************************************************************)
(*			    DRAWING CHARACTERS				*)
(************************************************************************)

PROCEDURE FontAddress (ch: CHAR): FarBytePointer;

    (* Returns the address of the font table entry for ch.	*)

    BEGIN
	IF ORD(ch) < 128 THEN
	    RETURN FarAddOffset (FontAddress0,
				(ModeData.LastCharRow+1)*ORD(ch));
	ELSE
	    RETURN FarAddOffset (FontAddress1,
				(ModeData.LastCharRow+1)*(ORD(ch)-128));
	END (*IF*);
    END FontAddress;

(************************************************************************)

PROCEDURE MakeCode (ch: CHAR;  row: CARDINAL): BYTE;

    (* Returns a bit pattern for one scan row of ch. *)

    VAR fontptr: FarBytePointer;

    BEGIN
	IF ORD(ch) < 128 THEN
	    fontptr := FarAddOffset (FontAddress0,
			(ModeData.LastCharRow+1)*ORD(ch) + row);
	ELSE
	    fontptr := FarAddOffset (FontAddress1,
			(ModeData.LastCharRow+1)*(ORD(ch)-128) + row);
	END (*IF*);
	RETURN fontptr^;
    END MakeCode;

(************************************************************************)

PROCEDURE DrawChar (ch: CHAR;  x, y: CARDINAL;  colour: ColourType);

    (* Draws the single character ch.  The coordinates (x,y) are the	*)
    (* location of the bottom left of the character.			*)

    VAR fontptr: FarBytePointer;  i: CARDINAL;

    BEGIN
	IF BLorigin THEN INC (y, ModeData.LastCharRow)
	ELSE DEC (y, ModeData.LastCharRow)
	END (*IF*);
	fontptr := FontAddress (ch);
	FOR i := 0 TO ModeData.LastCharRow DO
	    PlotPattern (fontptr^, x, y, colour);
	    IF BLorigin THEN DEC(y) ELSE INC(y) END(*IF*);
	    fontptr := FarAddOffset (fontptr, 1);
	END (*FOR*);
    END DrawChar;

(************************************************************************)

PROCEDURE ClippedChar (ch: CHAR;  x, y: CARDINAL;  colour: ColourType;
			left, right, ymin, ymax: CARDINAL);

    (* Like DrawChar, but excludes those parts of the character which	*)
    (* fall outside the given clip rectangle.				*)

    VAR fontptr: FarBytePointer;  i: CARDINAL;
	mask, Rmask: BYTE;

    BEGIN
	(* Eliminate the trivial cases.	*)

	IF BLorigin THEN
	    IF (x > right) OR (x+7 < left)
			OR (y > ymax) OR (y+ModeData.LastCharRow < ymin) THEN
		RETURN;
	    END (*IF*);
	ELSE
	    IF (x > right) OR (x+7 < left)
			OR (y > ymax) OR (y-ModeData.LastCharRow < ymin) THEN
		RETURN;
	    END (*IF*);
	END (*IF*);

	(* Check for going beyond the right boundary. *)

	Rmask := 0FFH;
	IF x+7 > right THEN
	    Rmask := LSB (0FFH, x+7-right);
	END(*IF*);

	(* Check for the case of a character clipped at the left side.	*)

	mask := 0FFH;
	IF x < left THEN
	    mask := RSB (mask, left - x);
	END (*IF*);
	mask := IANDB (mask, Rmask);

	(* Establish the desired vertical range.	*)

	IF BLorigin THEN
	    IF y > ymin THEN ymin := y END(*IF*);
	ELSE
	    IF y < ymax THEN ymax := y END(*IF*);
	END (*IF*);
	fontptr := FontAddress (ch);
	IF BLorigin THEN INC (y, ModeData.LastCharRow)
	ELSE DEC (y, ModeData.LastCharRow)
	END (*IF*);

	(* Check for a character clipped at the top.	*)

	IF BLorigin THEN
	    IF y <= ymax THEN
		ymax := y;
	    ELSE
		fontptr := FarAddOffset (fontptr, y-ymax);
	    END (*IF*);
	ELSE
	    IF y >= ymin THEN
		ymin := y;
	    ELSE
		fontptr := FarAddOffset (fontptr, ymin-y);
	    END (*IF*);
	END (*IF*);

	(* The following loop steps through bytes in the font table,	*)
	(* while stepping downwards on the screen.			*)

	IF BLorigin THEN
	    FOR i := ymax TO ymin BY -1 DO
		PlotPattern (IANDB(fontptr^,mask), x, i, colour);
		fontptr := FarAddOffset (fontptr, 1);
	    END (*FOR*);
	ELSE
	    FOR i := ymin TO ymax DO
		PlotPattern (IANDB(fontptr^,mask), x, i, colour);
		fontptr := FarAddOffset (fontptr, 1);
	    END (*FOR*);
	END (*IF*);

    END ClippedChar;

(************************************************************************)

PROCEDURE DrawCharUp (ch: CHAR;  x, y: CARDINAL;  colour: ColourType);

    (* Draws the single character ch sideways.  The coordinates (x,y)	*)
    (* are the location of the bottom left of the unrotated character,	*)
    (* or equivalently the bottom right of the character as plotted.	*)

    VAR fontptr: FarBytePointer;  pattern: SHORTCARD;
	i, j: CARDINAL;

    BEGIN
	fontptr := FontAddress (ch);
	FOR i := 0 TO ModeData.LastCharRow DO
	    pattern := fontptr^;
	    FOR j := 0 TO 7 DO
		IF ODD(pattern) THEN
		    IF BLorigin THEN
			PlotDot (x+i-7, y+7-j, colour);
		    ELSE
			PlotDot (x+i-7, y-7+j, colour);
		    END (*IF*);
		END (*IF*);
		pattern := pattern DIV 2;
	    END (*FOR*);
	    fontptr := FarAddOffset (fontptr, 1);
	END (*FOR*);
    END DrawCharUp;

(************************************************************************)

PROCEDURE ClippedCharUp (ch: CHAR;  x, y: CARDINAL;  colour: ColourType;
			left, right, ymin, ymax: CARDINAL);

    (* Like DrawCharUp, but excludes those parts of the character which	*)
    (* fall outside the given clip rectangle.				*)

    VAR fontptr: FarBytePointer;  pattern, mask: BYTE;
	ystart: CARDINAL;

    BEGIN
	(* Eliminate the trivial cases.	*)

	IF BLorigin THEN
	    IF (x > right+ModeData.LastCharRow) OR (x < left)
			OR (y > ymax) OR (y+7 < ymin) THEN
		RETURN;
	    END (*IF*);
	ELSE
	    IF (x > right+ModeData.LastCharRow) OR (x < left)
			OR (y < ymin) OR (y-7 > ymax) THEN
		RETURN;
	    END (*IF*);
	END (*IF*);

	ystart := y;  mask := 128;

	(* Check for the case of a character clipped at the side.	*)

	IF BLorigin THEN
	    IF ystart+7 < ymax THEN ymax := ystart+7 END(*IF*);
	    IF ystart < ymin THEN
		mask := RSB (mask, ymin - ystart);
		ystart := ymin;
	    END (*IF*);
	ELSE
	    IF ystart-7 > ymin THEN ymin := ystart-7 END(*IF*);
	    IF ystart > ymax THEN
		mask := RSB (mask, ystart - ymax);
		ystart := ymax;
	    END (*IF*);
	END (*IF*);

	(* Establish the desired horizontal range.	*)

	IF x < right THEN right := x END(*IF*);
	fontptr := FontAddress (ch);
	DEC (x, ModeData.LastCharRow);

	(* Check for a character clipped at the top.	*)

	IF x < left THEN
	    fontptr := FarAddOffset (fontptr, left-x);  x := left;
	END (*IF*);

	(* The outer loop steps through bytes in the font table,	*)
	(* while stepping in the +X direction.				*)

	LOOP
	    pattern := fontptr^;  y := ystart;

	    (* The inner loop steps towards the top of the screen while	*)
	    (* stepping through bits in the font table entry.		*)

	    LOOP
		IF pattern = BYTE(0) THEN EXIT(*LOOP*) END(*IF*);
		IF IANDB (pattern, mask) <> BYTE(0) THEN
		    PlotDot (x, y, colour);
		END (*IF*);
		IF BLorigin THEN
		    INC (y);
		    IF y > ymax THEN EXIT(*LOOP*) END(*IF*);
		ELSE
		    DEC (y);
		    IF y < ymin THEN EXIT(*LOOP*) END(*IF*);
		END (*IF*);
		pattern := LSB (pattern, 1);
	    END (*LOOP*);

	    INC (x);
	    IF x > right THEN EXIT(*LOOP*) END(*IF*);
	    fontptr := FarAddOffset (fontptr, 1);

	END (*LOOP*);

    END ClippedCharUp;

(************************************************************************)

PROCEDURE RasterPlotString (VAR (*IN*) text: ARRAY OF CHAR;
			x, y, length: CARDINAL;  colour: ColourType);

    (* Draws a string of "length" characters starting at location (x,y)	*)
    (* We assume length > 0.  This procedure is for packed modes which	*)
    (* have fewer than 8 bits per pixel.				*)
    (* The approach being used is a "raster" approach where we write	*)
    (* text a scan row at a time, rather than a character at a time.	*)

    TYPE TwoByte = RECORD
			CASE :BOOLEAN OF
			    | FALSE:	w: CARDINAL;
			    | TRUE:	l, h: BYTE;
			END (*CASE*);
		   END (*RECORD*);

    VAR row, CharCount, PixelsPerByte, s0, DotsToGo, LowDots: CARDINAL;
	code: TwoByte;

    BEGIN
	IF BLorigin THEN INC (y, ModeData.LastCharRow);
	ELSE DEC (y, ModeData.LastCharRow);
	END (*IF*);
	PixelsPerByte := 8 DIV ModeData.BitsPerPixel;
	s0 := x MOD PixelsPerByte;
	SetColour (colour);

	FOR row := 0 TO ModeData.LastCharRow DO

	    CharCount := 0;
	    SetScreenLocation (x, y);

	    (* Set up initial character with correct alignment. *)

	    DotsToGo := 8 + s0;
	    code.h := MakeCode (text[0], row);
	    code.l := 0;
	    IF s0 > 0 THEN
		code.w := RS (code.w, s0);
	    END (*IF*);

	    LOOP
		AlignedPattern (code.h);
		IF DotsToGo < PixelsPerByte THEN
		    EXIT (*LOOP*);
		END (*IF*);
		DEC (DotsToGo, PixelsPerByte);
		IF DotsToGo >= 8 THEN
		    code.w := LS (code.w, PixelsPerByte);
		ELSE

		    (* Load in next character. *)

		    INC (CharCount);
		    IF CharCount >= length THEN
			code.w := LS (code.w, PixelsPerByte);
		    ELSE
			LowDots := DotsToGo + PixelsPerByte - 8;
			IF LowDots > 0 THEN
			    code.w := LS (code.w, LowDots);
			END (*IF*);
			code.l := MakeCode (text[CharCount], row);
			code.w := LS (code.w, PixelsPerByte - LowDots);
			INC (DotsToGo, 8);
		    END (*IF*);
		END (*IF*);
	    END (*LOOP*);

	    IF BLorigin THEN DEC(y) ELSE INC(y) END(*IF*);

	END (*FOR*);

   END RasterPlotString;

(************************************************************************)

PROCEDURE PlotStringSimple (VAR (*IN*) text: ARRAY OF CHAR;
			x, y, length: CARDINAL;  colour: ColourType);

    (* Draws a string of "length" characters starting at location (x,y)	*)
    (* This is the version to use for unpacked video modes.		*)

    VAR j: CARDINAL;

    BEGIN
	FOR j := 0 TO length-1 DO
	    DrawChar (text[j], x, y, colour);
	    INC (x, 8);
	END (*FOR*);
    END PlotStringSimple;

(************************************************************************)

PROCEDURE PlotString (VAR (*IN*) text: ARRAY OF CHAR;
			x, y, length: CARDINAL;  colour: ColourType);

    (* Draws a string of "length" characters starting at location (x,y)	*)

    BEGIN
	IF length = 0 THEN RETURN
	ELSIF ModeData.BitsPerPixel < 8 THEN
	    RasterPlotString (text, x, y, length, colour);
	ELSE
	    PlotStringSimple (text, x, y, length, colour);
	END (*IF*);
    END PlotString;

(************************************************************************)

PROCEDURE PlotStringUp (VAR (*IN*) text: ARRAY OF CHAR;
			x, y, length: CARDINAL;  colour: ColourType);

    (* Like PlotString, but with text written in the +Y direction.	*)

    VAR j: CARDINAL;

    BEGIN
	FOR j := 0 TO length-1 DO
	    DrawCharUp (text[j], x, y, colour);
	    IF BLorigin THEN INC (y, 8) ELSE DEC(y,8) END(*IF*);
	END (*FOR*);
    END PlotStringUp;

(************************************************************************)

PROCEDURE RasterClippedString (VAR (*IN*) text: ARRAY OF CHAR;
			x, y, length: CARDINAL;  colour: ColourType;
			left, right, ymin, ymax: CARDINAL);

    (* Draws a string of "length" characters starting at location (x,y)	*)
    (* We assume length > 0.  This procedure is for packed modes which	*)
    (* have fewer than 8 bits per pixel.				*)
    (* The approach being used is a "raster" approach where we write	*)
    (* text a scan row at a time, rather than a character at a time.	*)

    TYPE TwoByte = RECORD
			CASE :BOOLEAN OF
			    | FALSE:	w: CARDINAL;
			    | TRUE:	l, h: BYTE;
			END (*CASE*);
		   END (*RECORD*);

    VAR row, firstrow, lastrow: CARDINAL;
	CharNum, FirstCharNum, LastCharNum, gap: CARDINAL;
	PixelsPerByte, s0, DotsToGo, LowDots: CARDINAL;
	code: TwoByte;
	Lmask, Rmask: BYTE;

    BEGIN
	(* Set the vertical bounds. *)

	firstrow := 0;
	lastrow := ModeData.LastCharRow;
	IF BLorigin THEN
	    IF y < ymin THEN
		IF ymin-y > lastrow THEN RETURN END(*IF*);
		DEC (lastrow, ymin-y);
	    END (*IF*);
	    INC (y, ModeData.LastCharRow);
	    IF y > ymax THEN
		INC (firstrow, y-ymax);
		y := ymax;
	    END (*IF*);
	ELSE
	    IF y > ymax THEN
		IF y-ymax > lastrow THEN RETURN END(*IF*);
		DEC (lastrow, y-ymax);
	    END (*IF*);
	    DEC (y, ModeData.LastCharRow);
	    IF y < ymin THEN
		INC (firstrow, ymin-y);
		y := ymin;
	    END (*IF*);
	END (*IF*);
	IF firstrow > lastrow THEN RETURN END(*IF*);

	(* Set the horizontal bounds and masks. *)

	FirstCharNum := 0;  Lmask := 0FFH;
	IF x < left THEN
	    gap := left - x;
	    FirstCharNum := gap DIV 8;
	    gap := gap MOD 8;
	    IF gap > 0 THEN
		Lmask := RSB (Lmask, gap);
	    END (*IF*);
	END (*IF*);
	LastCharNum := length-1;  Rmask := 0FFH;
	IF x + 8*length - 1 > right THEN
	    gap := x + 8*length - 1 - right;
	    IF 8*LastCharNum < gap THEN RETURN END(*IF*);
	    DEC (LastCharNum, gap DIV 8);
	    gap := gap MOD 8;
	    IF gap > 0 THEN
		Rmask := LSB (Rmask, gap);
	    END (*IF*);
	END (*IF*);
	IF FirstCharNum = LastCharNum THEN
	    Lmask := IANDB (Lmask, Rmask);
	ELSIF FirstCharNum > LastCharNum THEN
	    RETURN;
	END (*IF*);
	INC (x, 8*FirstCharNum);

	(* At last, we are ready to plot. *)

	PixelsPerByte := 8 DIV ModeData.BitsPerPixel;
	s0 := x MOD PixelsPerByte;
	SetColour (colour);

	FOR row := firstrow TO lastrow DO

	    CharNum := FirstCharNum;
	    SetScreenLocation (x, y);

	    (* Set up initial character with correct alignment. *)

	    DotsToGo := 8 + s0;
	    code.h := IANDB (MakeCode (text[CharNum], row), Lmask);
	    code.l := 0;
	    IF s0 > 0 THEN
		code.w := RS (code.w, s0);
	    END (*IF*);

	    LOOP
		AlignedPattern (code.h);
		IF DotsToGo < PixelsPerByte THEN
		    EXIT (*LOOP*);
		END (*IF*);
		DEC (DotsToGo, PixelsPerByte);
		IF DotsToGo >= 8 THEN
		    code.w := LS (code.w, PixelsPerByte);
		ELSE

		    (* Load in next character. *)

		    INC (CharNum);
		    IF CharNum > LastCharNum THEN
			code.w := LS (code.w, PixelsPerByte);
		    ELSE
			LowDots := DotsToGo + PixelsPerByte - 8;
			IF LowDots > 0 THEN
			    code.w := LS (code.w, LowDots);
			END (*IF*);
			code.l := MakeCode (text[CharNum], row);
			IF CharNum = LastCharNum THEN
			    code.l := IANDB (code.l, Rmask);
			END (*IF*);
			code.w := LS (code.w, PixelsPerByte - LowDots);
			INC (DotsToGo, 8);
		    END (*IF*);
		END (*IF*);
	    END (*LOOP*);

	    IF BLorigin THEN DEC(y) ELSE INC(y) END(*IF*);

	END (*FOR*);

   END RasterClippedString;

(************************************************************************)

PROCEDURE ClippedStringSimple (VAR (*IN*) text: ARRAY OF CHAR;
			x, y, length: CARDINAL;  colour: ColourType;
			left, right, ymin, ymax: CARDINAL);

    (* Draws a string of "length" characters starting at location (x,y)	*)
    (* This is the version to use for unpacked video modes.		*)

    VAR j: CARDINAL;

    BEGIN
	FOR j := 0 TO length-1 DO
	    ClippedChar (text[j], x, y, colour, left, right, ymin, ymax);
	    INC (x, 8);
	END (*FOR*);
    END ClippedStringSimple;

(************************************************************************)

PROCEDURE ClippedString (VAR (*IN*) text: ARRAY OF CHAR;
			x, y, length: CARDINAL;  colour: ColourType;
			left, right, ymin, ymax: CARDINAL);

    (* Like PlotString, but excludes any points which fall outside the	*)
    (* clip rectangle defined by (left,right,ymin,ymax).		*)

    BEGIN
	IF length = 0 THEN RETURN
	ELSIF ModeData.BitsPerPixel < 8 THEN
	    RasterClippedString (text, x, y, length, colour,
					left, right, ymin, ymax);
	ELSE
	    ClippedStringSimple (text, x, y, length, colour,
					left, right, ymin, ymax);
	END (*IF*);
    END ClippedString;

(************************************************************************)

PROCEDURE ClippedUpString (VAR (*IN*) text: ARRAY OF CHAR;
			x, y, length: CARDINAL;  colour: ColourType;
			left, right, ymin, ymax: CARDINAL);

    (* Like ClippedString, but with text written in the +Y direction.	*)

    VAR j: CARDINAL;

    BEGIN
	FOR j := 0 TO length-1 DO
	    ClippedCharUp (text[j], x, y, colour, left, right, ymin, ymax);
	    IF BLorigin THEN INC (y, 8) ELSE DEC(y,8) END(*IF*);
	END (*FOR*);
    END ClippedUpString;

(************************************************************************)
(*		      OPERATIONS ON THE VIDEO MODE			*)
(************************************************************************)

PROCEDURE SetMode (newmode: CARDINAL;  ClearScreen: BOOLEAN);

    (* Sets the video mode. *)

    VAR y, j: CARDINAL;  p: FarPointerPointer;

    BEGIN
	IF NOT SetVideoMode (newmode, ClearScreen) THEN
	    RETURN;
	END (*IF*);
	GetAddresses (ScreenSeg, IObase);
	GetModeInfo (newmode, ModeData);

	(* Assign procedures appropriate to this mode. *)

	VisibleLine := VisibleLine0;  FillProc := Fill0;
	CopyRectangle := ACopy0;  CopyPartByte := CopyPartByte0;
	CopyString := StringCopy0;

	WITH ModeData DO

	    IF Planar THEN
		FillProc := PlanarFill;
		VisibleLine := VisibleLinePlanar;
		CopyPartByte := CopyPartBytePlanar;

		(* For the 4-plane modes supported in the 16-colour	*)
		(* model, we use set/reset mode as the default.		*)

		OutByte (3CEH, 1);  OutByte (3CFH, 0FH);

	    ELSIF BitsPerPixel >= 8 THEN
		FillProc := Fill256;
		IF BitsPerPixel > 8 THEN
		    VisibleLine := VisibleLine64K;
		END (*IF*);
	    END (*IF*);

	    IF BitsPerPixel >= 8 THEN
		CopyRectangle := ACopy1;
	    ELSIF MultiBank THEN
		CopyRectangle := ACopyMultibank;
	    END (*IF*);

	    IF MultiBank THEN
		CopyString := StringCopyMultibank;
	    END (*IF*);

	    (* Set up the character set pointers. *)

	    p := MakePointer (0, 4*43H);
	    FontAddress0 := p^;
	    IF newmode < 7 THEN
		p := MakePointer (0, 4*1FH);
		FontAddress1 := p^;
	    ELSE
		FontAddress1
			:= FarAddOffset (FontAddress0, (LastCharRow+1)*128);
	    END (*IF*);

	    (* Calculate the table of screen addresses. *)

	    FOR y := 0 TO MaxY DO
		LineStart[y].L := Mul (BytesPerRow, y DIV FramesPerScreen)
					+ Mul (8192, y MOD FramesPerScreen);
	    END (*FOR*);

	    (* Set up the mask and fill arrays. *)

	    IF BitsPerPixel <= 8 THEN
		FOR j := 0 TO (8 DIV BitsPerPixel)-1 DO
		    IF BitsPerPixel = 1 THEN
			Mask[j] := Mask2[j];
			KeepL[j] := KeepL2[j];
			KeepR[j] := KeepR2[j];
		    ELSIF BitsPerPixel = 2 THEN
			Mask[j] := Mask4[j];
			KeepL[j] := KeepL4[j];
			KeepR[j] := KeepR4[j];
		    ELSIF BitsPerPixel = 4 THEN
			Mask[j] := Mask16[j];
			KeepL[j] := KeepL16[j];
			KeepR[j] := KeepR16[j];
		    ELSE     (* 8 bits/pixel - do we need to cover this? *)
			Mask[j] := 0FFH;
			KeepL[j] := 0FFH;
			KeepR[j] := 0FFH;
		    END (*IF*);
		END (*FOR*);
	    END (*IF*);
	    IF MaxColour < 16 THEN

		(* Note that we allow for 16 entries in FillArray even	*)
		(* for modes which support fewer colours.  This maps	*)
		(* illegal colours down to legal ones.			*)

		FOR j := 0 TO 15 DO
		    IF MaxColour = 1 THEN FillArray[j] := Fill2[j MOD 2]
		    ELSIF MaxColour = 3 THEN FillArray[j] := Fill4[j MOD 4]
		    ELSE FillArray[j] := Fill16[j];
		    END (*IF*);
		END (*IF*);
	    END (*IF*);

	END (*WITH*);

    END SetMode;

(************************************************************************)

PROCEDURE SetDefaultMode;

    (* Sets the video mode to (our opinion of) the best mode supported	*)
    (* by the hardware.							*)

    BEGIN
	SetMode (DefaultGraphicsMode, TRUE);
    END SetDefaultMode;

(************************************************************************)

PROCEDURE GraphicsOff (ClearScreen: BOOLEAN);

    (* Sets the video mode to a default text mode. *)

    BEGIN
	SetMode (DefaultTextMode, ClearScreen);
    END GraphicsOff;

(************************************************************************)

PROCEDURE GetScreenShape (VAR (*OUT*) xmax, ymax: CARDINAL;
				VAR (*OUT*) maxcolour: ColourType;
				VAR (*OUT*) CharHeight: CARDINAL);

    (* Returns the maximum values permitted by the current mode for	*)
    (* x, y, and colour; and the number of rows in a character.		*)

    BEGIN
	WITH ModeData DO
	    xmax := MaxX;  ymax := MaxY;
	    maxcolour := MaxColour;
	    CharHeight := LastCharRow + 1;
	END (*WITH*);
    END GetScreenShape;

(************************************************************************)
(*			      INITIALISATION				*)
(************************************************************************)

BEGIN
    (* Find out the video adaptor type.	*)

    AdaptorKind := VideoKind();
    FontAddress0 := Virtual (0FFA6EH);
    FontAddress1 := FarAddOffset (FontAddress0, 8*128);

    (* Work out the "best" modes to use for the available adaptor type.	*)

    CASE AdaptorKind OF
	MDA:		DefaultGraphicsMode := 7;
     |	Hercules:	DefaultGraphicsMode := HercGraphics;
     |	CGA:		DefaultGraphicsMode := 4;
     |	EGA:		DefaultGraphicsMode := 16;
     |	VGA, SVGA:	DefaultGraphicsMode := 18;
     |	ATI:		DefaultGraphicsMode := 84;
     |	S3:		DefaultGraphicsMode := 257;
     |	Trident:	DefaultGraphicsMode := 91;
    END (*CASE*);

    IF (AdaptorKind = MDA) OR (AdaptorKind = Hercules) THEN
	DefaultTextMode := 7;
    ELSE
	DefaultTextMode := 3;
    END (*IF*);

    (* Set up default values for the ModeData record.  This is helpful	*)
    (* for debugging: if SetMode is never called, the program will run	*)
    (* in a text mode but the graphics routines will still be working	*)
    (* with plausible values.						*)

    WITH ModeData DO
	MaxX := 639;  MaxY := 349;
	MaxColour := 15;
	LastCharRow := 7;
	BitsPerPixel := 1;
	BytesPerRow := 80;  FramesPerScreen := 1;
	Planar := TRUE;  MultiBank := FALSE;  TextMode := FALSE;
    END (*WITH*);
    FillProc := PlanarFill;  VisibleLine := VisibleLinePlanar;
    GetAddresses (ScreenSeg, IObase);

END Graphics.
