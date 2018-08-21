IMPLEMENTATION MODULE GetCharacter;

(* GET a single character from the terminal ,
   without first setting the proper mode, ordinary non-GET mode is used.

   Use StartGet and StopGet to enter and exit single-character-mode.

   Set character modes on and off with the SET procedures,
   both PASSALL (all control characters passed to GET) and
      NOPASSALL (control characters not trapped) modes are available,

*)
(* bastardization of M. Mall's TTIO from ETH by Jaa, 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)
  
FROM SYSTEM                      IMPORT WORD, ADDRESS, ADR;
FROM VMS                         IMPORT SYS$ASSIGN, SYS$ALLOC, SYS$QIOW;
FROM ConditionHandlingProcedures IMPORT LIB$SIGNAL;
FROM IODefinitions               IMPORT IO$_TTYREADALL, IO$_READVBLK;

CONST
    IO$M_NOFILTR = 200H;
    IO$M_NOECHO  = 40H;
    IO$_WRITEVBLK= 30H;
    IO$_SENSEMODE= 27H;
    IO$M_TYPEAHDCNT=40H;
     
VAR 
    usegetln:    BOOLEAN;   (* read in complete lines        *)
    mustagain:   BOOLEAN;  (* procedure readagain was called*)
    bufferempty: BOOLEAN; 
    lfpending:   BOOLEAN;  (* a line feed must next be output*)
    oldch:       CHAR;	   (* last character read           *)
    buffer:      ARRAY [0..255] OF CHAR; (* holds one line of input *)
    size:        CARDINAL;      (* number of chars in buffer     *)
    pos:         CARDINAL;       (* next char to read from buffer *)
    inChannel,           (* input channel assigned by VMS *)
    outChannel:  CARDINAL;(* output channel assigned by VMS*)
    PhysDevNam:  ARRAY [0..29] OF CHAR; (* physical device name *)
    mode:        CARDINAL;      (* mode bits for input operations*)
    Result:      CARDINAL;    (* status code returned from VMS *)
    iosb:        RECORD c1, c2: CARDINAL END;
                           (* input/output status block     *)
                           (* filled by QIO system service  *)
                  

  (* --------------------------------------------------------------- *)
  PROCEDURE Get(VAR ch: CHAR);

  BEGIN (* Get *)

    IF mustagain THEN
      ch := oldch;
      mustagain := FALSE
    ELSIF bufferempty THEN

      IF usegetln THEN 
        (* note - MODE only as the qualifier on the read *)
        Result := SYS$QIOW(0,inChannel,mode,ADR(iosb),ADDRESS(0),
		           0,ADR(buffer),256,0,0,0,0);
        IF NOT ODD(Result) THEN LIB$SIGNAL(Result) END;
        pos := 1;
        bufferempty := FALSE;
        size := iosb.c1 DIV 10000H + iosb.c2 DIV 10000H - 1;
        ch := buffer[0];
        IF size < pos THEN
          bufferempty := TRUE
        END
      ELSE 
        Result := SYS$QIOW(0,inChannel,mode,ADR(iosb),ADDRESS(0),
		           0,ADR(ch),1,0,0,0,0);
        IF NOT ODD(Result) THEN LIB$SIGNAL(Result) END;
      END
    ELSE
      ch := buffer[pos]; INC(pos);
      IF size < pos THEN
        bufferempty := TRUE;
      END;
    END;

    oldch := ch;
    lfpending := ch = 15C;

  END Get;
 
  (* --------------------------------------------------------------- *)
  PROCEDURE GetNoWait(VAR ch: CHAR);

  VAR
     getln : BOOLEAN;

  BEGIN (* GetNoWait *)

    IF mustagain OR NOT bufferempty THEN
       Get(ch);
    ELSE 
        Result := SYS$QIOW(0,inChannel,
                           IO$M_TYPEAHDCNT+IO$_SENSEMODE,
                           ADDRESS(0),ADDRESS(0),
                           0,ADR(iosb),0,0,0,0,0);
        IF NOT ODD(Result) THEN LIB$SIGNAL(Result) END;
        IF iosb.c1 MOD 10000H > 0 THEN
           (* there is a character in the type ahead buffer *)
           getln := usegetln;
           usegetln := FALSE;
           Get(ch);
           usegetln := getln;
        ELSE
           ch := 0C;
        END;
    END;

  END GetNoWait;
 
  (* --------------------------------------------------------------- *)
  PROCEDURE StartGet;
  (* Turn on single character mode *)
  BEGIN
     SetNoPassallCharacterMode;
  END StartGet;

  (* --------------------------------------------------------------- *)
  PROCEDURE StopGet;
  (* Turn off single character mode *)
  BEGIN
     UnSetNoPassallCharacterMode;
  END StopGet;

  (* --------------------------------------------------------------- *)
  PROCEDURE SetMode(m: CARDINAL; getln: BOOLEAN);
    (* m= modifier bits from VMS *)

  BEGIN (* SetMode *)

    mode := m;
    usegetln := getln;

  END SetMode;

  (* ------------------------------------------------------------ *)
  PROCEDURE SetPassallCharacterMode;

  BEGIN (* SetCharacterMode *)
 
    SetMode(IO$M_NOECHO + IO$_TTYREADALL , FALSE );

  END SetPassallCharacterMode;

  (* ------------------------------------------------------------ *)
  PROCEDURE UnSetPassallCharacterMode;

  BEGIN (* UnSetPassallCharacterMode *)
 
    SetMode( 0 , TRUE );

  END UnSetPassallCharacterMode;

  (* ------------------------------------------------------------ *)
  PROCEDURE SetNoPassallCharacterMode;

  BEGIN (* SetNoPassallCharacterMode *)

    SetMode(IO$M_NOECHO + IO$_READVBLK , FALSE );

  END SetNoPassallCharacterMode;

  (* ------------------------------------------------------------ *)
  PROCEDURE UnSetNoPassallCharacterMode;

  BEGIN (* UnSetNopassallCharacterMode *)

    SetMode( 0 , TRUE );

  END UnSetNoPassallCharacterMode;


BEGIN (* GetCharacter *)

  Result := SYS$ASSIGN("TT",outChannel,0,ADDRESS(0));
  IF NOT ODD(Result) THEN LIB$SIGNAL(Result) END;
  inChannel := outChannel;
  mustagain := FALSE;
  oldch := 0C;
  usegetln := FALSE;
  bufferempty := TRUE;
  lfpending := TRUE;
  SetMode(0,TRUE);

END GetCharacter.
