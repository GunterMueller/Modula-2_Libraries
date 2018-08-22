MODULE M3D07;

	(********************************************************)
	(*							*)
	(*		  Test of module Music3			*)
	(*							*)
	(*		W.A. Mozart: Variations on		*)
	(*		"Ah! vous dirais-je, Maman"		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM Music3 IMPORT
    (* proc *)	Voice1, Voice2, Voice3, SetDuration, PlayTheMusic,
		SetInstrument, EnableVoice;

(************************************************************************)

PROCEDURE Theme;

    VAR repeat: [1..2];

    BEGIN

	Voice3 ("*2d");

	FOR repeat := 1 TO 2 DO
	    Voice1 ("CC uGG AA GG FF EE D*3/4D/3E *8C/2");
	    Voice2 ("CuC EC FC EC DB CA FG        *2dC/2");
	    Voice3 ("R   R  R  R  R  R  R           R   ");
	END (*FOR*);

	FOR repeat := 1 TO 2 DO
	    Voice1 ("uGG  FF   EE DD GG   FF   E*3/4E/3F*4 ED");
	    Voice2 ("uEdG uDdG CG BG uEdG uDdG C*3/4C/3D*4 CBd");
	    Voice3 ("R    R    R  R  R    R    R           G  ");

	    Voice1 ("CC uGG AA GG FF EE D*3/4D/3E *8C/2");
	    Voice2 ("CuC EC FC EC DB CA FG *2dC/2");
	    Voice3 ("R   R   R  R R  R  R  R   ");
	END (*FOR*);

	Voice3 ("/2");

    END Theme;

(************************************************************************)

PROCEDURE Variation1;

    VAR repeat: [1..2];

    BEGIN
	Voice3 ("*2");

	FOR repeat := 1 TO 2 DO
	    Voice1 ("/4DCBCBCBC uAGF#GF#GF#G G#ACBDCBA AGuEDCBAG GFuDCBAGF FEuCBAGFE *2DuAGdB *2CR");
	    Voice2 ("CuC EC FC E*3/4R/3C#*4 D*3/4R/3B*4 C*3/4R/3A*4 FG");
	    IF repeat = 1 THEN
		Voice2 ("/2CGEG*2d");
	    ELSE
		Voice2 ("CdC");
	    END (*IF*);
	    Voice3 ("R   R   R  /2CR*2 R  R  R  R   ");
	END (*FOR*);

	FOR repeat := 1 TO 2 DO
	    Voice1 ("/4uAGF#GF#GAG GFEFEFGF FED#ED#EFE");
	    Voice2 ("uEdG          uDdG     CG        ");
	    Voice3 (" R            R        R         ");

	    Voice1 ("EDC#DC#DED uAGF#GuECAG GFEFuDBGF FED#EuCGFE *3G/3E*4D/4 DCBCBCBC");
	    Voice2 ("uFdG *2uF D/2 C*3/4R/3C *3E/3C*4B dCuC");
	    Voice3 ("R    G    G/2 GR*2      G         R   ");

	    Voice1 ("uAGF#GF#GF#G G#ACBDCBA AGuEDCBAG    GFuDCBAGF   FEuCBAGFE   *2DuAGdB *2CR");
	    Voice2 ("EC           FC        E*3/4R/3C#*4 D*3/4R/3B*4 C*3/4R/3A*4 FG       CdC ");
	    Voice3 ("R            R        /2CR*2        R           R           R        R   ");
	END (*FOR*);
	Voice3 ("/2");

    END Variation1;

(************************************************************************)

PROCEDURE Variation2;

    VAR repeat: [1..2];

    BEGIN
	Voice2 ("*2u");

	FOR repeat := 1 TO 2 DO
	    Voice1 ("C/2RC*2 uG/2RG*2 A/2RA*2 G/2RG   RFRF   RERE RDRD *2CR");
	    Voice2 ("R       R        R    *3/4R/3E   RERD   RDRC RCRB *4R ");
	    Voice3 ("/2RC*2E /2RE*2C  /2RF*2C /2RE*2C /2DRBR CRAR FRGR RC*2dCu");
	END (*FOR*);

	FOR repeat := 1 TO 2 DO

	    Voice1 ("uG/2RG *2F#/2RF *2E/2REb *2D/2RdG *2uG/2R/2GF# *4F/2R/2FE *4Eb/2R/2F#G *4D/2R/2FD *4C/2R/2CE");
	    Voice2 ("R        R        R        R        R            R          R            R          R       ");
	    Voice3 ("/2RE*2dG /2RuD*2dG /2RC*2G /2RB*2G /2RuE*2dG   /2RuD*2dG  /2RC*2G      /2RB*2G   /2R/2dCE*4G");

	    Voice1 ("*4G/2R/2GE *4A/2R/2AC *4G/2R/2EG *2R/2GF#*2R/2DF *2R/2FE*2R/2GE *2R/2C#D*2R/2FD *2R/2BC*4R");
	    Voice2 ("R            R         R          R               R               R               R       ");
	    Voice3 ("/2R/2CE*4G /2R/2FA*4dC /2R/2EG*4dC /2DRBR         CRdER          FRGR            CR*2dCu  ");

	END (*FOR*);

	Voice2 ("/2d");

    END Variation2;

(************************************************************************)

PROCEDURE Variation3;

    VAR repeat: [1..2];

    BEGIN
	Voice1 ("/2");
	Voice2 ("/2");
	Voice3 ("d/4");

	FOR repeat := 1 TO 2 DO

	    Voice1 ("C R C R uG R G R  A R A R  G R G R ");
	    Voice2 ("G R G R uE R E R  G R F R  F R E R ");
	    Voice3 ("CBCBCBCB CBCBCDEC FEFEFGAB CBCBCDEC");

	    Voice1 ("F  R F R  E R E  R  D R B  R   *2CR/2     ");
	    Voice2 ("E  R D R  D R C  R  A R F  R   *2GR/2     ");
	    Voice3 ("DC#DCBABA CBCBAG#AG FEFDGF#GdG CGEG*4dCu/4");

	END (*FOR*);

	FOR repeat := 1 TO 2 DO

	    Voice1 ("/2dGF#GF#GF#GF# GF#GF#GABG CBCDEDEF# GF#GF#GF#GF#");
	    Voice2 ("*2E      E      F     F    E   E     D     D     ");
	    Voice3 ("*4G      G     uD     D    C   C     B     B     ");

	    Voice1 ("GF#GF#GF#GF# GF#GF#GABG CBCDEDEF# GAFGEFDE*2");
	    Voice2 ("G     G      F     F    E   E     D   R   /2");
	    Voice3 ("E     E      D     D    C   C     B   R   /4");

	    Voice1 ("C R C R uG R G R  A R A R  G R G R ");
	    Voice2 ("G R G R uE R E R  G R F R  F R E R ");
	    Voice3 ("CBCBCBCB CBCBCDEC FEFEFGAB CBCBCDEC");

	    Voice1 ("F  R F R  E R E  R  D R B  R   *2CR/2     ");
	    Voice2 ("E  R D R  D R C  R  A R F  R   *2GR/2     ");
	    Voice3 ("DC#DCBABA CBCBAG#AG FEFDGF#GdG CGEG*4dCu/4");

	END (*FOR*);

    END Variation3;

(************************************************************************)

PROCEDURE EncodeMusic;

    (* Plays a piece of music. *)

    VAR repeat: [1..2];

    BEGIN	(* EncodeMusic *)

	SetInstrument (2, 60000, 1200, 0);
	EnableVoice (1, TRUE);

	Voice2("d");  Voice3 ("d");

	Theme;
	Variation1;
	Variation2;
	Variation3;

    END EncodeMusic;

(************************************************************************)

BEGIN
    SetDuration (400);
    EncodeMusic;
    PlayTheMusic;
END M3D07.
