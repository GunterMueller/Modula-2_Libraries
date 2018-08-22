MODULE CursorOn;

	(****************************************************************)
	(*								*)
	(* Turns the screen cursor on; used for recovery when a program	*)
	(*		has accidentally left it off.			*)
	(*								*)
	(*	Programmer:	P. Moylan				*)
	(*	Last edited:	20 January 1993				*)
	(*								*)
	(****************************************************************)

FROM MiscPMOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)	BIOS;

(************************************************************************)

CONST VideoInt = 16;

VAR BIOSframe: RegisterPacket;
    BlackAndWhite: BOOLEAN;

(************************************************************************)

PROCEDURE TurnCursorOn;

    (* Turns on the physical screen cursor.  This has to be done with a	*)
    (* BIOS call, rather than by programming the CRTC registers		*)
    (* directly, to ensure correct treatment over the variety of	*)
    (* different video interfaces which could be present.		*)

    VAR startline: SHORTCARD;

    BEGIN
	IF BlackAndWhite THEN
	    startline := 11;
	ELSE
	    startline := 6;
	END (*IF*);
	WITH BIOSframe DO
	    AH := 1;  CH := startline;  CL := startline+1;
	END (*WITH*);
	BIOS (VideoInt, BIOSframe);
    END TurnCursorOn;

(************************************************************************)

BEGIN
    BlackAndWhite := TRUE;
    WITH BIOSframe DO
	AH := 0FH;  BIOS (VideoInt, BIOSframe);
	IF AL <> 7 THEN		(* must be colour *)

	    BlackAndWhite := FALSE;

	    (* Set the screen to 25*80 colour.	*)

	    AH := 0;  AL := 3;  BIOS (VideoInt, BIOSframe);

	END (*IF*);
    END (*WITH*);

    TurnCursorOn;

END CursorOn.
