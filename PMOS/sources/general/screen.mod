IMPLEMENTATION MODULE Screen;

	(********************************************************)
	(*							*)
	(*		Low-level screen functions		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	28 February 1995		*)
	(*  Status:						*)
	(*	Working for Hercules, CGA, EGA, VGA; and	*)
	(*	supports SVGA for adaptors with VESA drivers.	*)
	(*							*)
	(*	Status of support for specific chips:		*)
	(*	    ATI		working				*)
	(*	    S3		partly working, but the VESA	*)
	(*			BIOS seems to be failing for	*)
	(*			many modes			*)
	(*	    Trident	untested			*)
	(*							*)
	(*	Portability warning: this module contains	*)
	(*	preprocessor directives, and might need to be	*)
	(*	processed by the PP preprocessor before being	*)
	(*	compiled.					*)
	(*							*)
	(********************************************************)

(************************************************************************)
(*									*)
(*  A particular problem in supporting screen operations on PC or AT	*)
(*  compatibles is that there are enormous differences between models	*)
(*  in what sort of graphics interface is provided.  Because it is hard	*)
(*  to predict what hardware will be present, this module uses ROM BIOS	*)
(*  routines to do some of the low-level things.  Note however that the	*)
(*  Hercules graphics cards are not supported by the BIOS, so we have	*)
(*  to treat them as a special case.					*)
(*									*)
(*  Note that the modes available will depend on whether a VESA BIOS	*)
(*  is present.								*)
(*									*)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE,
    (* proc *)	ADR;

FROM Types IMPORT
    (* type *)	FarPointer, FarCharPointer, FarCardPointer;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)	FarSEGMENT, SEGMENT, OFFSET, Virtual, MakePointer,
		AddOffset, FarAddOffset,
		InByte, OutByte, IAND, IANDB, IORB, IXORB, LSB, Div;

FROM MiscPMOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)	BIOS,
    (* proc *)	EnterCriticalSection, LeaveCriticalSection;

FROM TerminationControl IMPORT
    (* proc *)  SetTerminationProcedure;

(************************************************************************)

CONST
    VideoInt = 16;		(* Interrupt number for BIOS calls	*)

TYPE
    ModeSet = SET OF [0..511];

    (* Portability warning: the most direct form of VESA bank switching	*)
    (* requires calling a procedure which expects its parameters to be	*)
    (* in the BX and DX registers.  When porting to a compiler which	*)
    (* doesn't let you specify which registers to use for parameter	*)
    (* passing, we can't use this feature.  In this version, direct	*)
    (* VESA bank switching is implemented only when the compiler is	*)
    (* TopSpeed version 3; for other compilers, we use a procedure	*)
    (* VESABankSwitch which works by making a BIOS call.  For non-VESA	*)
    (* modes, we use yet another version of the bank switch procedure.	*)

    (*<TopSpeed3*) (*# save, call(reg_param => (bx, dx)) *) (*>*)
    BankSwitchProc = PROCEDURE (CARDINAL,CARDINAL);
    (*<TopSpeed3*) (*# restore *) (*>*)

    (* Format of the mode information record returned by a VESA driver.	*)

    VESAInformationRecord1 =
		    RECORD
			mode_attributes: WORD;
			window_a_attrib: BYTE;
			window_b_attrib: BYTE;
			window_granularity: WORD;
			window_size: WORD;
			window_a_segment_adr: WORD;
			window_b_segment_adr: WORD;
			bank_switch_function:
				(*<TopSpeed3*) BankSwitchProc; (*>*)
				(*<~TopSpeed3  ADDRESS; >*)
			bytes_per_row: CARDINAL;
	                x_res, y_res: CARDINAL;
			x_character_size,
			y_character_size: SHORTCARD;
			number_of_planes: BYTE;
			bits_per_pixel: BYTE;
			number_of_banks: BYTE;
			memory_model: SHORTCARD;
			bank_size: BYTE;
			number_of_image_pages: BYTE;
			BIOS_reserved: BYTE;
			red_mask_size: SHORTCARD;
			red_field_position: SHORTCARD;
			green_mask_size: SHORTCARD;
			green_field_position: SHORTCARD;
			blue_mask_size: SHORTCARD;
			blue_field_position: SHORTCARD;
			reserved_mask_size: SHORTCARD;
			reserved_field_position: SHORTCARD;
			direct_col_mode_info: BYTE;
			unused: ARRAY [40..255] OF BYTE;
		    END (*RECORD*);

CONST
    (* 'Monochrome' display modes supported by this module.  Note that	*)
    (* this means modes supported by a monochrome adaptor.  There are	*)
    (* some other single-colour modes which we don't classify as	*)
    (* monochrome, simply because they are not MDA modes.		*)

    MonochromeModes = ModeSet {0, 2, 7};

VAR
    (* Our opinion as to what type of adaptor is installed.	*)

    AdaptorType: VideoAdaptorType;

    (* BIOSsegment is a segment selector for the BIOS ROM at physical	*)
    (* address 0C00000H.  We make it a variable in order to simplify	*)
    (* the porting of code between real and protected modes.		*)

    BIOSsegment: CARDINAL;

    (* ScreenSeg is a segment selector for the hardware video buffer,	*)
    (* and CRTCport is the port number to use when addressing the CRT	*)
    (* controller chip.  They are variables, because the addresses	*)
    (* depend on whether a colour or monochrome mode is in effect.	*)

    ScreenSeg: CARDINAL;
    CRTCport: CARDINAL;

    (* OriginalMode and OriginalPage are the video mode and text page	*)
    (* number in effect when the program first runs.  Procedure		*)
    (* RestoreOriginalMode switches back to them.			*)

    OriginalMode, CurrentMode: CARDINAL;
    OriginalPage: SHORTCARD;

    (* For the modes which use bank-switching, the hardware may allow	*)
    (* two separate windows into the graphics memory.  The following	*)
    (* two variables each have value 0 or 1.  Where two separate	*)
    (* windows are not supported, we have ReadWindow = WriteWindow.	*)

    ReadWindow, WriteWindow: CARDINAL;

    (* For the modes which use bank-switching, CurrentReadBank and	*)
    (* CurrentWriteBank are the currently selected banks.		*)
    (* For all other modes, the two values are left at zero.		*)

    CurrentReadBank, CurrentWriteBank: CARDINAL;

    (* Set of all modes which we believe the hardware will support.	*)

    SupportedModes: ModeSet;

    (* Modes supported by the VESA driver, if present.  The first set	*)
    (* is the set officially supported, and the second is a set of	*)
    (* modes we've found by trial and error to be supported even though	*)
    (* the VESA driver doesn't have them in its list of supported modes.*)

    VesaModes, ExtraVesaModes: ModeSet;

    (* The procedure which does bank switching.  See the definition of	*)
    (* BankSwitchProc above for a comment about portability issues.	*)
    (* DefaultBankSwitch is the (hardware-dependent) procedure that	*)
    (* will normally be used, but a VESA driver might override this.	*)

    BankSwitch, DefaultBankSwitch: BankSwitchProc;

    (* EXTport is meaningful only for the ATI adaptor.  We work out its	*)
    (* value after detecting the presence of an ATI.			*)

    EXTport: CARDINAL;
    		
(************************************************************************)

TYPE
    (* Linked list of mode information records.  An unsorted list is	*)
    (* adequate for this job since we only need to access it for mode	*)
    (* changes; and mode setting is in any case a slow operation.	*)

    ModeInfoList = POINTER TO ListNode;
    ListNode = RECORD
		   next: ModeInfoList;
		   mode: CARDINAL;
		   data: ModeInfoType;
	       END (*RECORD*);

VAR
    (* Information about the non-VESA modes. *)

    ModeInfo: ModeInfoList;
		
(************************************************************************)
(*			    BANK SWITCHING				*)
(************************************************************************)

(*<~TopSpeed3
PROCEDURE VESABankSwitch (WindowNumber, NewBank: CARDINAL);

    (* This is the bank switching procedure that is used when a VESA	*)
    (* driver is present, but we can't do direct bank switching because	*)
    (* of compiler limitations.  It's not quite as fast as the direct	*)
    (* method, but is more portable.					*)

    VAR Registers: RegisterPacket;	

    BEGIN
	WITH Registers DO
	    AX := 4F05H;
	    BX := WindowNumber;
	    DX := NewBank;
	END (*WITH*);
	BIOS(VideoInt, Registers);
    END VESABankSwitch;
>*)

(************************************************************************)

(*<TopSpeed3*) (*# save, call(reg_param => (bx, dx)) *) (*>*)

PROCEDURE DummyBankSwitch (WindowNumber, NewBank: CARDINAL);

    (* This is the bank switching procedure that is used when we don't	*)
    (* expect to have to do any bank switching.  Its only function is	*)
    (* to avoid the problem of an uninitialised variable.		*)

    BEGIN
    END DummyBankSwitch;

(************************************************************************)

PROCEDURE ATIBankSwitch (WindowNumber, NewBank: CARDINAL);

    (* This is the bank switching procedure that is used when an ATI	*)
    (* adaptor has been detected.  We use it for all modes including	*)
    (* VESA modes, since the VESA driver doesn't seem to correctly	*)
    (* handle the case of separate read and write banks.		*)

    BEGIN
	IF WindowNumber = 0 THEN CurrentReadBank := NewBank
	ELSE CurrentWriteBank := NewBank
	END (*IF*);
	OutByte (EXTport, 0B2H);
	OutByte (EXTport+1,
		VAL (BYTE, 2*(16*CurrentReadBank + CurrentWriteBank)));
    END ATIBankSwitch;

(************************************************************************)

PROCEDURE TridentBankSwitch (WindowNumber, NewBank: CARDINAL);

    (* This is the bank switching procedure that is used, for non-VESA	*)
    (* modes only, when a lower-model Trident adaptor has been detected.*)
    (* A limitation of the current version is that there are no		*)
    (* separate read and write banks.					*)

    BEGIN
	OutByte (3C4H, 0EH);
	OutByte (3C5H, IXORB(VAL(BYTE,NewBank), 2));
    END TridentBankSwitch;

(************************************************************************)

PROCEDURE Trident9200BankSwitch (WindowNumber, NewBank: CARDINAL);

    (* This is the bank switching procedure that is used, for non-VESA	*)
    (* modes only, when a Trident 9200 or better has been detected.	*)

    BEGIN
	IF WindowNumber = 0 THEN
	    OutByte (3D9H, VAL(BYTE,NewBank));
	ELSE
	    OutByte (3D8H, VAL(BYTE,NewBank));
	END (*IF*);
    END Trident9200BankSwitch;

(*<TopSpeed3*) (*# restore *) (*>*)

(************************************************************************)
(*		      OPERATIONS ON THE VIDEO MODE			*)
(************************************************************************)

PROCEDURE Supported (mode: CARDINAL): BOOLEAN;

    (* Returns TRUE iff the specified mode is a mode supported	*)
    (* by the hardware and by this module.			*)

    BEGIN
	RETURN (mode IN SupportedModes);
    END Supported;

(************************************************************************)

PROCEDURE BugFixes (mode: CARDINAL;  VAR (*INOUT*) ModeData: ModeInfoType);

    (* Called when a new mode is set: we compensate, if possible, for	*)
    (* errors introduced by bugs in the BIOS.  (Obviously we're limited	*)
    (* to the small number of cases where we know the bug and have	*)
    (* worked out how to repair it - but every little bit helps.)	*)

    (* Curiously, I have yet to find a bug-free VESA BIOS.  I can only	*)
    (* conjecture that the equipment suppliers have a policy of giving	*)
    (* the software jobs to their less-competent people.		*)

    BEGIN
	IF AdaptorType = ATI THEN

	    (* Enable dual bank access mode. *)

	    OutByte (EXTport, 0BEH);  OutByte (EXTport+1, 8);

	    (* Fixes for known bugs in the ATI VESA BIOS.		*)
	    (* Main known bug is that on some models mode 259 is not	*)
	    (* set correctly; I haven't managed to find a fix for this.	*)

        ELSIF AdaptorType = S3 THEN

            (* The S3 VESA BIOS is a bit of a mystery to me.  There is	*)
            (* a definite bug in the "character size" information that	*)
            (* the BIOS returns, and the fix below corrects for this.	*)
            (* However there's also a problem in setting many modes,	*)
            (* and I don't know how to correct for that.		*)

            ModeData.LastCharRow := 15;

	ELSIF AdaptorType = Trident THEN

	    (* Fixes for known bugs in the Trident VESA BIOS.	*)

	    IF (mode = 272) OR (mode = 273) THEN
		ModeData.BytesPerRow := 1280;
	    END (*IF*);

	    (* There are also bugs in modes 106,258,260 - being		*)
	    (* problems with screen synch - but we don't yet know how	*)
	    (* to fix those.						*)

	END (*IF*);

    END BugFixes;

(************************************************************************)

PROCEDURE DecodeWindows (attra, attrb: BYTE);

    (* Works out which windows are to be used for reading and writing.	*)
    (* This is for VESA modes, where there is a provision with some	*)
    (* adaptors and some modes to allow different banks to be selected	*)
    (* for reading and writing.						*)

    BEGIN
	ReadWindow := 0;  WriteWindow := 0;
	IF IANDB (attra, 2) = BYTE(0) THEN
	    ReadWindow := 1;
	END (*IF*);
	IF IANDB (attra, 4) = BYTE(0) THEN
	    WriteWindow := 1;
	END (*IF*);
    END DecodeWindows;

(************************************************************************)

PROCEDURE SetHerculesMode (newmode: CARDINAL;  ClearScreen: BOOLEAN);

    (* A version of procedure SetVideoMode (see below) to be used for	*)
    (* Hercules adaptors only.						*)

    TYPE ControlTable = ARRAY [0..11] OF BYTE;

    CONST
	(* Tables of values to put in the 6845 video controller.  These	*)
	(* are details like character positions per row, sync		*)
	(* positions, etc., and can be fiddled with only at great risk.	*)
	(* HGtable gives values suitable for Hercules graphics, and	*)
	(* MDAtable gives values suitable for MDA or Hercules text mode.*)

	HGtable = ControlTable (53,45,46,07,91,02,87,87,02,03,00,00);
	MDAtable= ControlTable (97,80,82,15,25,06,25,25,02,13,11,12);

    VAR tableptr: POINTER TO ControlTable;
	controlcode: SHORTCARD;
	j, buffersize, fillcode, savedPS: CARDINAL;
	p: FarCardPointer;

    BEGIN
	IF newmode = HercGraphics THEN
	    tableptr := ADR(HGtable);  controlcode := 2;
	    buffersize := 32768;	(* words in video page 0 *)
	    fillcode := 0;

	    (* For maximal compatibility with any other hardware	*)
	    (* which might be present, we enable page 0 graphics only.	*)

	    OutByte (CRTCport+11, 1);

	ELSE
	    newmode := 7;
	    tableptr := ADR(MDAtable);  controlcode := 20H;
	    buffersize := 2000;	(* words for text mode *)
	    fillcode := 0720H;	(* space, attribute code 7 *)
	END (*IF*);

	(* Reprogram the 6845 video controller.  Warning: this part	*)
	(* is quite critical; wrong values could damage the		*)
	(* hardware.  We disable interrupts here to minimise the	*)
	(* time for which the 6845 register values are changing.	*)
	(* We also blank the screen during the operation, to avoid	*)
	(* or reduce the disconcerting effects while the monitor	*)
	(* regains synchronism.						*)
	
	savedPS := EnterCriticalSection();
	OutByte (CRTCport+4, controlcode);
	FOR j := 0 TO 11 DO
	    OutByte (CRTCport, BYTE(j));
	    OutByte (CRTCport+1, tableptr^[j]);
	END (*FOR*);
	LeaveCriticalSection (savedPS);

	(* Finished reprogramming, clear the screen buffer and	*)
	(* then enable video.					*)

	IF ClearScreen THEN
	    FOR j := 0 TO 2*(buffersize-1) BY 2 DO
		p := MakePointer (ScreenSeg, j);
		p^ := fillcode;
	    END (*FOR*);
	END (*IF*);

	OutByte (CRTCport+4, controlcode+8);
	CurrentReadBank := 0;  CurrentWriteBank := 0;
	CurrentMode := newmode;

    END SetHerculesMode;

(************************************************************************)

PROCEDURE SetVideoMode (newmode: CARDINAL;  ClearScreen: BOOLEAN): BOOLEAN;

    (* Sets the video mode.  The mode numbers are as defined in the	*)
    (* BIOS, plus HercGraphics to denote the Hercules graphics mode,	*)
    (* plus whatever the VESA BIOS (if present) will support.		*)
    (* Returns TRUE iff the mode change was successful.			*)

    VAR Registers: RegisterPacket;	

    BEGIN
	IF (newmode = CurrentMode) AND NOT ClearScreen THEN
	    RETURN TRUE;

	ELSIF AdaptorType = Hercules THEN
	    SetHerculesMode (newmode, ClearScreen);
	    RETURN TRUE;

	ELSIF newmode IN VesaModes+ExtraVesaModes THEN

	    WITH Registers DO

		(* Set a VESA mode. *);

		AX := 4F02H;
		BX := newmode;
		IF NOT ClearScreen THEN
		    INC (BX, 8000H);
		END (*IF*);
		BIOS(VideoInt, Registers);
		IF AX <> 004FH THEN RETURN FALSE END(*IF*);

	    END (*WITH*);

	ELSIF newmode IN SupportedModes THEN

	    (* For all the "standard" modes, let the BIOS do the job.	*)

	    Registers.AX := newmode;
	    IF NOT ClearScreen THEN
		INC (Registers.AX, 80H);
	    END (*IF*);
	    BIOS (VideoInt, Registers);

	ELSE
	    (* Not a supported mode. *)

	    RETURN FALSE;

	END (*IF*);

	CurrentMode := newmode;

	(* Initialise the bank switching. *)

	IF AdaptorType = ATI THEN
	    (* Enable dual bank access mode. *)
	    OutByte (EXTport, 0BEH);  OutByte (EXTport+1, 8);
	END (*IF*);
	BankSwitch := DefaultBankSwitch;
	ReadWindow := 0;  WriteWindow := 1;
	CurrentReadBank := 0;  CurrentWriteBank := 0;
	BankSwitch (0, 0);  BankSwitch (1, 0);

	(* ScreenSeg and CRTCport depend on whether we are using	*)
	(* a monochrome or colour mode.					*)

	IF newmode IN MonochromeModes THEN
	    ScreenSeg := FarSEGMENT(Virtual(0B0000H));
	    CRTCport := 03B4H;
	ELSE
	    ScreenSeg := FarSEGMENT(Virtual(0B8000H));
	    CRTCport := 03D4H;
	END (*IF*);

	(* For an EGA or better adaptor, ScreenSeg depends on	*)
	(* whether we are emulating a CGA mode.			*)

	IF newmode >= 13 THEN
	    ScreenSeg:= FarSEGMENT (Virtual(0A0000H));
	END (*IF*);

	RETURN TRUE;

    END SetVideoMode;

(************************************************************************)

PROCEDURE RestoreOriginalMode;

    (* Sets the video mode back to what it was before this program ran.	*)
    (* Also restores the original text page.				*)

    VAR Registers: RegisterPacket;  dummy: BOOLEAN;

    BEGIN
	dummy := SetVideoMode (OriginalMode, TRUE);
	Registers.AH := 5;  Registers.AL := OriginalPage;
	BIOS (VideoInt, Registers);
    END RestoreOriginalMode;

(************************************************************************)
(*			MISCELLANEOUS OPERATIONS			*)
(************************************************************************)

PROCEDURE SelectReadBank (bank: CARDINAL);

    (* Switches to a new bank of screen memory for reading.  Should be	*)
    (* used only with the adaptors which support the high-resolution	*)
    (* modes using bank switching.					*)

    BEGIN
	IF bank <> CurrentReadBank THEN
	    BankSwitch (ReadWindow, bank);
	    CurrentReadBank := bank;
	    IF ReadWindow = WriteWindow THEN
		CurrentWriteBank := bank;
	    END (*IF*);
	END (*IF*);
    END SelectReadBank;

(************************************************************************)

PROCEDURE SelectWriteBank (bank: CARDINAL);

    (* Switches to a new bank of screen memory for writing.  Should be	*)
    (* used only with the adaptors which support the high-resolution	*)
    (* modes using bank switching.					*)

    BEGIN
	IF bank <> CurrentWriteBank THEN
	    BankSwitch (WriteWindow, bank);
	    CurrentWriteBank := bank;
	    IF ReadWindow = WriteWindow THEN
		CurrentReadBank := bank;
	    END (*IF*);
	END (*IF*);
    END SelectWriteBank;

(************************************************************************)

PROCEDURE WaitForVerticalRetrace;

    (* Busy wait until we reach the vertical retrace period.		*)
    (* Warning: I wrote this quickly for one specific application, and	*)
    (* haven't gotten around to getting it right for the general case.	*)

    CONST CGAStatusReg = 3DAH;

    BEGIN
	WHILE IANDB(InByte(CGAStatusReg),8) = BYTE(0) DO
	    (* nothing *)
	END (*WHILE*);
    END WaitForVerticalRetrace;

(************************************************************************)
(*		    INFORMATION ABOUT CURRENT MODE			*)
(************************************************************************)

PROCEDURE GetAddresses (VAR (*OUT*) ScreenSegment, IObase: CARDINAL);

    (* Returns the segment of the screen memory and the port number	*)
    (* of the video controller.						*)

    BEGIN
	ScreenSegment := ScreenSeg;  IObase := CRTCport;
    END GetAddresses;

(************************************************************************)

PROCEDURE GetVESAModeInfo (mode: CARDINAL;  VAR (*OUT*) result: ModeInfoType);

    (* Returns information about a VESA mode.  If the information	*)
    (* is not available, the parameter values are left unchanged.	*)

    VAR VesaInfo: POINTER TO VESAInformationRecord1;
	Registers: RegisterPacket;

    BEGIN
	NEW (VesaInfo);
	WITH Registers DO
	    AX := 4F01H;
	    CX := mode;
	    DI := OFFSET (VesaInfo);
	    ES := SEGMENT (VesaInfo);
	    BIOS (VideoInt, Registers);
	    IF AX <> 004FH THEN
		DISPOSE (VesaInfo);  RETURN;
	    END(*IF*);
	END (*WITH*);

	WITH VesaInfo^ DO
	    IF (mode = CurrentMode) AND (AdaptorType <> ATI) THEN
		DecodeWindows (window_a_attrib, window_b_attrib);
		(*<TopSpeed3*) BankSwitch := bank_switch_function; (*>*)
		(*<~TopSpeed3 BankSwitch := VESABankSwitch; >*)
	    END (*IF*);
	
	    result.MaxX := x_res - 1;
	    result.MaxY := y_res - 1;
	    result.LastCharRow := VAL(CARDINAL,y_character_size) - 1;
	    result.BitsPerPixel :=
			ORD(bits_per_pixel) DIV ORD(number_of_planes);
	    result.BytesPerRow := bytes_per_row;
	    result.FramesPerScreen := 1;
	    result.Planar := FALSE;
	    CASE memory_model OF
		  0, 2:	result.MaxColour := 1;
		|
		  1:	result.MaxColour := 3;
		|
		  3:	result.MaxColour := 15;  result.Planar := TRUE;
		|
		  4, 5:	result.MaxColour := 255;
		|
		  6:	IF reserved_mask_size > 0 THEN
			    result.MaxColour := 32767;
			ELSE
			    result.MaxColour := 65535;
			END (*IF*);
		|
		  ELSE	result.MaxColour := 1;
	    END (*CASE*);
	    result.MultiBank := y_res > Div(10000H, bytes_per_row);
	    result.TextMode := (memory_model = 0);

	END (*WITH*);

	DISPOSE (VesaInfo);
	BugFixes (mode, result);

    END GetVESAModeInfo;

(************************************************************************)

PROCEDURE GetModeInfo (mode: CARDINAL;  VAR (*OUT*) result: ModeInfoType);

    (* Returns information about the given mode.  If the information	*)
    (* is not available, the parameter values are left unchanged.	*)

    VAR p: ModeInfoList;

    BEGIN
	IF mode IN (VesaModes+ExtraVesaModes) THEN

	    GetVESAModeInfo (mode, result);

	ELSIF mode IN SupportedModes THEN

	    (* Search the ModeInfo list. *)

	    p := ModeInfo;
	    LOOP
		IF p = NIL THEN EXIT (*LOOP*)
		ELSIF p^.mode = mode THEN
		    result := p^.data;
		    EXIT (*LOOP*);
		ELSE p := p^.next;
		END (*IF*);
	    END (*LOOP*);

	END (*IF*);

    END GetModeInfo;

(************************************************************************)
(*			    DEFINING A MODE				*)
(************************************************************************)

PROCEDURE DefineMode (newmode: CARDINAL;
			MaxX, MaxY, MaxColour, LastCharRow: CARDINAL;
			BitsPerPixel, BytesPerRow: CARDINAL;
			Planar, MultiBank, TextMode: BOOLEAN);

    (* Adds another mode to the list of supported modes. *)

    VAR p: ModeInfoList;

    BEGIN
	NEW (p);
	WITH p^ DO
	    next := ModeInfo;
	    mode := newmode;
	    data.BytesPerRow := BytesPerRow;
	    data.MaxX := MaxX;
	    data.MaxY := MaxY;
	    data.MaxColour := MaxColour;
	    data.LastCharRow := LastCharRow;
	    data.BitsPerPixel := BitsPerPixel;
	    data.BytesPerRow := BytesPerRow;
	    IF mode = HercGraphics THEN
		data.FramesPerScreen := 4;
	    ELSIF mode IN ModeSet{4..6} THEN
		data.FramesPerScreen := 2;
	    ELSE
		data.FramesPerScreen := 1;
	    END (*IF*);
	    data.Planar := Planar;
	    data.MultiBank := MultiBank;
	    data.TextMode := TextMode;
	END (*WITH*);
	ModeInfo := p;
	INCL (SupportedModes, newmode);
    END DefineMode;

(************************************************************************)
(*			EQUIPMENT KIND DETECTION			*)
(************************************************************************)

PROCEDURE VideoKind (): VideoAdaptorType;

    (* Returns the display adaptor type.  This is a best guess, and it	*)
    (* is possible that some adaptor types will be misclassified.	*)
    (* In the present version, most SVGA adaptors will be reported as	*)
    (* VGA or SVGA rather than something more specific; and no		*)
    (* distinction is drawn between the "ordinary" Hercules adaptor and	*)
    (* the Hercules Plus or Hercules InColor.				*)

    BEGIN
	RETURN AdaptorType;
    END VideoKind;

(************************************************************************)
(*		   DETECTING A HERCULES GRAPHICS ADAPTOR		*)
(************************************************************************)

PROCEDURE CheckForHercules;

    (* The Hercules graphics interface is a special case because the	*)
    (* BIOS equipment information does not distinguish between it and	*)
    (* the Monochrome Display Adaptor.  The HGA has the same text mode	*)
    (* as the MDA, but it has a graphics mode as well.  One way to work	*)
    (* out the difference is to look at the "vertical retrace" bit in	*)
    (* the display status port.  The MDA does not implement this bit,	*)
    (* so reading it will return a constant.				*)

    VAR i, j, changes: CARDINAL;
	oldvalue, newvalue: BYTE;

    BEGIN
	changes := 0;
	FOR i := 0 TO 24 DO
	    oldvalue := IANDB (InByte (CRTCport+6), 80H);
	    j := 32767;
	    LOOP
		newvalue := IANDB (InByte (CRTCport+6), 80H);
		IF newvalue <> oldvalue THEN
		    INC (changes);  EXIT(*LOOP*);
		END (*IF*);
		DEC (j);
		IF j = 0 THEN EXIT(*LOOP*) END(*IF*);
	    END (*LOOP*);
	END (*FOR*);
	IF changes > 20 THEN
	    AdaptorType := Hercules;
	    DefineMode (HercGraphics, 719, 347, 1, 7,
				1, 90, FALSE, FALSE, FALSE);
	END (*IF*);
    END CheckForHercules;

(************************************************************************)
(*		INITIALISATION FOR VESA SVGA ADAPTORS			*)
(************************************************************************)

PROCEDURE Match (StringPtr: FarPointer;  location: CARDINAL;
				OtherString: ARRAY OF CHAR): BOOLEAN;

    (* Compares two strings, where the first is a substring specified	*)
    (* in terms of an offset from a memory address.			*)

    VAR p: FarCharPointer;  k: CARDINAL;

    BEGIN
	p := FarAddOffset (StringPtr, location);
	k := 0;
	LOOP
	    IF k = HIGH(OtherString) THEN RETURN TRUE END(*IF*);
	    IF p^ <> OtherString[k] THEN RETURN FALSE END(*IF*);
	    INC (k);
	    p := FarAddOffset (p, 1);
	END (*LOOP*);
    END Match;

(************************************************************************)

PROCEDURE SpecificDriverChecks (OEMStringPtr: FarPointer);

    (* Tries to identify the hardware type from the VESA OEM string	*)
    (* pointer.								*)

    BEGIN
	IF Match (OEMStringPtr, 0, "761295520") THEN
	    AdaptorType := ATI;
	ELSIF Match (OEMStringPtr, 0, "S3") THEN
	    AdaptorType := S3;
	ELSIF Match (OEMStringPtr, 21, "TRIDENT") THEN
	    AdaptorType := Trident;
	    ExtraVesaModes := ModeSet {272, 273, 275, 276};
	END (*IF*);
    END SpecificDriverChecks;

(************************************************************************)

PROCEDURE CheckVESADriver;

    (* Compiles a list of supported VESA modes, if any.	*)

    TYPE PointerToCardinal = FarCardPointer;

    VAR Registers: RegisterPacket;
	ListPtr: PointerToCardinal;
	SysInfoPtr: POINTER TO
			    RECORD
				VesaSignature: ARRAY [0..3] OF CHAR;
				VesaVersion: WORD;
				OEM_OffsetString: FarPointer;
				capabilities: LONGWORD;
				ModeListPointer: PointerToCardinal;
				NumberOf64KBlocks: CARDINAL;
				reserved: ARRAY [20..255] OF BYTE;
			    END (*RECORD*);

    (********************************************************************)

    PROCEDURE VesaSignatureAbsent(): BOOLEAN;

	BEGIN
	    WITH SysInfoPtr^ DO
		RETURN (VesaSignature[0] <> "V")
			OR (VesaSignature[1] <> "E")
			OR (VesaSignature[2] <> "S")
			OR (VesaSignature[3] <> "A");
	    END (*WITH*);
	END VesaSignatureAbsent;

    (********************************************************************)

    BEGIN
	(* Call the VESA BIOS subfunction 0, which is supposed to	*)
	(* return system information.  If this call fails, we presume	*)
	(* that there is no VESA driver present.			*)

	NEW(SysInfoPtr);
	WITH Registers DO
	    AX := 4F00H;
	    DI := OFFSET(SysInfoPtr);
	    ES := SEGMENT(SysInfoPtr);
	    BIOS(VideoInt,Registers);
	    IF (Registers.AX <> 004FH) OR VesaSignatureAbsent() THEN
		DISPOSE (SysInfoPtr);
		RETURN;
	    END (*IF*);
	END (*WITH*);

	(* We've found a VESA driver, now work out what modes it	*)
	(* supports.							*)

	AdaptorType := SVGA;
	ListPtr := SysInfoPtr^.ModeListPointer;
	WHILE ListPtr^ <> 0FFFFH DO
	    VesaModes := VesaModes + ModeSet {ListPtr^};
	    ListPtr := FarAddOffset (ListPtr, 2);
	END (*WHILE*);
	ExtraVesaModes := ModeSet {};
	SpecificDriverChecks (SysInfoPtr^.OEM_OffsetString);
	SupportedModes := SupportedModes + VesaModes + ExtraVesaModes;
	DISPOSE (SysInfoPtr);

    END CheckVESADriver;

(************************************************************************)

PROCEDURE VESAdriverPresent (): BOOLEAN;

    (* Returns TRUE iff a VESA driver was detected. *)

    BEGIN
	RETURN VesaModes <> ModeSet {};
    END VESAdriverPresent;

(************************************************************************)
(*			TESTS FOR SPECIFIC SVGA TYPES			*)
(************************************************************************)

PROCEDURE AddTridentModes;

    (* Called only if we've identified a Trident adaptor.  Updates the	*)
    (* set of supported modes, also updates DefaultBankSwitch.		*)

    VAR model: CARDINAL;

    BEGIN
	(* Work out the hardware model from the hardware version	*)
	(* register.  Reading from this register has the side-effect of	*)
	(* toggling the definition of the mode control registers; to	*)
	(* get into a known state, we write before reading.		*)

	OutByte (3C4H, 0BH);  OutByte (3C5H, 0);
	CASE ORD(InByte(3C5H)) OF
	   | 3, 4:		model := 8900;
	   | 13H, 23H, 93H:	model := 9000;
	   | 53H, 83H:		model := 9200;
	   | 73H:		model := 9420;
	   | ELSE		model := 8800;
	END (*CASE*);

	(* Define (my best guess at) the available modes.  These are	*)
	(* all untested!  (I no longer have a Trident.)			*)

	DefaultBankSwitch := TridentBankSwitch;
	DefineMode (5BH, 799, 599, 15, 7, 1, 100, TRUE, TRUE, FALSE);
	DefineMode (5CH, 639, 399, 255, 15, 8, 640, FALSE, TRUE, FALSE);
	DefineMode (5DH, 639, 479, 255, 15, 8, 640, FALSE, TRUE, FALSE);
	DefineMode (5FH, 1023, 767, 15, 15, 1, 128, TRUE, TRUE, FALSE);
	DefineMode (61H, 767, 1023, 15, 15, 1, 96, TRUE, TRUE, FALSE);
	IF model > 8800 THEN
	    DefineMode (5EH, 799, 599, 255, 7, 8, 800, FALSE, TRUE, FALSE);
	    DefineMode (62H, 1023, 767, 255, 15, 8, 1024, FALSE, TRUE, FALSE);
	END (*IF*);
	IF model >= 9200 THEN
	    DefaultBankSwitch := Trident9200BankSwitch;
	    DefineMode (60H, 1023, 767, 3, 15, 2, 256, FALSE, TRUE, FALSE);
	    DefineMode (63H, 1279, 1023, 15, 15, 1, 160, TRUE, TRUE, FALSE);
	    DefineMode (64H, 1279, 1023, 255, 15, 8, 1280, FALSE, TRUE, FALSE);
	    DefineMode (6CH, 639, 479, 0, 7, 24, 1920, FALSE, TRUE, FALSE);
	    DefineMode (70H, 511, 479, 32767, 7, 16, 1024, FALSE, TRUE, FALSE);
	    DefineMode (71H, 511, 479, 65535, 7, 16, 1024, FALSE, TRUE, FALSE);
	    DefineMode (74H, 639, 479, 32767, 7, 16, 1280, FALSE, TRUE, FALSE);
	    DefineMode (75H, 639, 479, 65535, 7, 16, 1280, FALSE, TRUE, FALSE);
	    DefineMode (76H, 799, 599, 32767, 7, 16, 1600, FALSE, TRUE, FALSE);
	    DefineMode (77H, 799, 599, 65535, 7, 16, 1600, FALSE, TRUE, FALSE);
	END (*IF*);

    END AddTridentModes;

(************************************************************************)

PROCEDURE TridentPresent(): BOOLEAN;

    (* Returns TRUE iff a Trident adaptor is detected. *)

    VAR OldMiscValue, OldValue, NewValue: BYTE;

    BEGIN
	IF AdaptorType <> VGA THEN RETURN (AdaptorType=Trident) END(*IF*);

	(* The distinctive feature of a Trident is the presence of an	*)
	(* inverting bit in the Mode Control #1 Register.  The		*)
	(* following code purports to check for this.  I'm taking it on	*)
	(* trust - the code is from Ferraro's book and he doesn't tell	*)
	(* us the meaning of some of the ports.				*)

	(* First set memory map field in Miscellaneous Register to 01.	*)

	OutByte (3CEH, 6);
	OldMiscValue := InByte (3CFH);
	OutByte (3CFH, IORB(4, IANDB(OldMiscValue, 3)));

	(* Write zero to the mode control register, then read it back	*)
	(* and see whether bit 1 has been set.				*)

	OutByte (3C4H, 0EH);
	OldValue := InByte (03C5H);
	OutByte (03C5H, 0);
	NewValue := IANDB (InByte(03C5H), 0FH);

	(* I don't claim to understand this next part.  We're writing	*)
	(* back the old value; but if it's a Trident then we should	*)
	(* have inverted a bit before writing it back, and if it isn't	*)
	(* a Trident then it's hard to predict what side-effects will	*)
	(* arise from writing to a port whose function isn't known.	*)
	(* Nevertheless I'll just hope Ferraro knows what he's doing.	*)

	OutByte (03C5H, OldValue);

	(* Restore original contents of Miscellaneous Register. *)

	OutByte (3CEH, 6);  OutByte (3CFH, OldMiscValue);
	IF ORD(NewValue) >= 2 THEN RETURN TRUE END(*IF*);

	(* Further possibility: an older 8800 can be detected by	*)
	(* looking at the power up register.				*)

	OutByte (3C4H, 0FH);  OutByte (3C5H, 0);
	RETURN InByte (3C5H) <> BYTE(0);

    END TridentPresent;

(************************************************************************)

PROCEDURE AddATIModes;

    (* Called only if we've identified an ATI adaptor.  Updates the	*)
    (* set of supported modes, also works out the value of EXTport and	*)
    (* sets up the bank switching procedure.				*)

    (********************************************************************)

    PROCEDURE ModeAvailable (m: SHORTCARD): BOOLEAN;

	(* Returns TRUE if the ATI hardware supports mode m. *)

	VAR Registers: RegisterPacket;	

	BEGIN
	    WITH Registers DO
		AL := m;  AH := 12H;
		BX := 5506H;
		BP := 0FFFFH;
	    END (*WITH*);
	    BIOS(VideoInt, Registers);
	    RETURN Registers.BP <> 0FFFFH;
	END ModeAvailable;

    (********************************************************************)

    PROCEDURE TryMode (mode: CARDINAL;
			MaxX, MaxY, MaxColour, LastCharRow: CARDINAL;
			BitsPerPixel, BytesPerRow: CARDINAL;
			Planar, MultiBank, TextMode: BOOLEAN);

	(* Defines a new available mode, if the hardware can support it. *)

	BEGIN
	    IF ModeAvailable(SHORTCARD(mode)) THEN
		DefineMode (mode, MaxX, MaxY,
			MaxColour, LastCharRow, BitsPerPixel,
			BytesPerRow, Planar, MultiBank, TextMode);
	    END (*IF*);
	END TryMode;

    (********************************************************************)

    VAR p: FarCardPointer;

    BEGIN	(* Body of AddATIModes *)

	(* Find out the value of EXTport. *)

	p := MakePointer (BIOSsegment, 010H);
	EXTport := p^;

	(* Initialise the bank switching. *)

	DefaultBankSwitch := ATIBankSwitch;

	(* Define the modes which are specific to the ATI adaptor.	*)
	(* Mode 63H is buggy on some ATI models.			*)
	(* Modes 64H, 65H are untested.					*)
	(* Mode 67H uses a format that I don't support.			*)
	
	TryMode (53H, 799, 599, 15, 13, 1, 100, TRUE, TRUE, FALSE);
	TryMode (54H, 799, 599, 15, 13, 1, 100, TRUE, TRUE, FALSE);
	TryMode (55H, 1023, 767, 15, 15, 1, 128, TRUE, TRUE, FALSE);
	TryMode (61H, 639, 399, 255, 15, 8, 640, FALSE, TRUE, FALSE);
	TryMode (62H, 639, 479, 255, 15, 8, 640, FALSE, TRUE, FALSE);
	TryMode (63H, 799, 599, 255, 13, 8, 800, FALSE, TRUE, FALSE);
	TryMode (64H, 1023, 767, 255, 15, 8, 1024, FALSE, TRUE, FALSE);
	TryMode (65H, 1023, 767, 15, 15, 4, 512, FALSE, TRUE, FALSE);

    END AddATIModes;

(************************************************************************)

PROCEDURE ATIpresent(): BOOLEAN;

    (* Returns TRUE iff an ATI adaptor is detected. *)

    BEGIN
	RETURN (AdaptorType = ATI)
		OR Match (MakePointer(BIOSsegment,31H), 0, "761295520");
    END ATIpresent;

(************************************************************************)

PROCEDURE IdentifySVGAtype;

    (* On entry, AdaptorType >= VGA, i.e. we have detected at least a	*)
    (* VGA, and if a VESA driver was present then we might have already	*)
    (* made a more specific identification.  This procedure attempts	*)
    (* to pin down the hardware more precisely, and also augments the	*)
    (* list of supported modes where possible.				*)

    BEGIN
	IF TridentPresent() THEN
	    AdaptorType := Trident;
	    AddTridentModes;
	ELSIF ATIpresent() THEN
	    AdaptorType := ATI;
	    AddATIModes;
	END (*IF*);
    END IdentifySVGAtype;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

VAR Registers: RegisterPacket;
    p: FarCardPointer;  dummy: BOOLEAN;

BEGIN
    BIOSsegment := FarSEGMENT (Virtual(0C0000H));
    SupportedModes := ModeSet {};
    VesaModes := ModeSet {};  ExtraVesaModes := ModeSet {};
    ModeInfo := NIL;
    ReadWindow := 0;  WriteWindow := 1;
    CurrentReadBank := 0;  CurrentWriteBank := 0;
    DefaultBankSwitch := DummyBankSwitch;

    (* Read the low byte of the system equipment word at 0040:0010.	*)
    (* Bits 4 and 5 of this byte give the initial video mode: 00 is	*)
    (* unused, 01 or 10 implies a colour adaptor, 11 implies a		*)
    (* monochrome adaptor.						*)

    p := MakePointer (0040H, 0010H);
    IF ORD(IAND (p^, 30H)) = 30H THEN

	(* Must be an MDA or Hercules *)

	AdaptorType := MDA;  ScreenSeg:= FarSEGMENT (Virtual(0B0000H));
	CRTCport := 03B4H;
	DefineMode (0, 319, 199, 1, 7, 1, 40, FALSE, FALSE, TRUE);
	DefineMode (2, 639, 199, 1, 7, 1, 80, FALSE, FALSE, TRUE);
	DefineMode (7, 719, 349, 1, 13, 1, 80, FALSE, FALSE, TRUE);
	CheckForHercules;

    ELSE

	(* Colour display present.  If there is an expansion ROM at	*)
	(* C000:0000, we assume that it's an EGA or better.		*)

	CRTCport := 03D4H;
	DefineMode (1, 319, 199, 3, 7, 1, 40, FALSE, FALSE, TRUE);
	DefineMode (3, 639, 199, 3, 7, 1, 80, FALSE, FALSE, TRUE);
	DefineMode (4, 319, 199, 3, 7, 2, 80, FALSE, FALSE, FALSE);
	DefineMode (5, 319, 199, 1, 7, 2, 80, FALSE, FALSE, FALSE);
	DefineMode (6, 639, 199, 1, 7, 1, 80, FALSE, FALSE, FALSE);
	p := MakePointer (BIOSsegment, 0);
	IF p^ <> 0AA55H THEN
	    AdaptorType := CGA;
	    ScreenSeg:= FarSEGMENT (Virtual(0B8000H));
	ELSE
	    AdaptorType := EGA;
	    ScreenSeg:= FarSEGMENT (Virtual(0A0000H));
	    DefineMode (13, 319, 199, 15, 7, 1, 40, TRUE, FALSE, FALSE);
	    DefineMode (14, 639, 199, 15, 7, 1, 80, TRUE, FALSE, FALSE);
	    DefineMode (15, 639, 349, 1, 13, 1, 80, FALSE, FALSE, FALSE);
	    DefineMode (16, 639, 349, 15, 13, 1, 80, TRUE, FALSE, FALSE);

	    (* It's at least EGA, test now for VGA.  If it passes that	*)
	    (* test, check for the presence of a VESA driver and/or for	*)
	    (* specific SVGA types which we able to identify.		*)

	    Registers.AX := 1A00H;
	    BIOS (VideoInt, Registers);

	    IF Registers.AL = 26 THEN

		AdaptorType := VGA;
		DefineMode (17, 639, 479, 1, 15, 1, 80, FALSE, FALSE, FALSE);
		DefineMode (18, 639, 479, 15, 15, 1, 80, TRUE, FALSE, FALSE);
		DefineMode (19, 319, 199, 255, 7, 8,320, FALSE, FALSE, FALSE);
		
		(* The following two procedures attempt to identify	*)
		(* SVGA chips.  They might have the side-effect of	*)
		(* updating AdaptorType and SupportedModes.		*)

		CheckVESADriver;
		IdentifySVGAtype;

	    END (*IF*);

	END (*IF*);

    END (*IF*);

    (* Find out what video mode is currently in effect, so that it can	*)
    (* be restored when we are done.					*)

    Registers.AH := 15;
    BIOS (VideoInt, Registers);
    OriginalMode := CARDINAL(Registers.AL);
    OriginalPage := Registers.BH;
    SetTerminationProcedure (RestoreOriginalMode);

    (* By setting that mode, we ensure that our global variables are	*)
    (* set to meaningful initial values.				*)

    dummy := SetVideoMode (OriginalMode, FALSE);

END Screen.
