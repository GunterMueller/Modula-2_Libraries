MODULE M3D03;

	(********************************************************)
	(*							*)
	(*		Test of module Music3.			*)
	(*							*)
	(*	     Fernando Sor: Estudio VII from		*)
	(*	the Segovia edition of "Twenty Studies for	*)
	(*			the Guitar"			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

FROM Music3 IMPORT
    (* proc *)	Voice1, Voice2, Voice3, SetDuration, SetInstrument,
		PlayTheMusic;

(************************************************************************)

PROCEDURE EncodeMusic;

    (* Plays a piece of music. *)

    VAR repeat: [1..2];

    BEGIN	(* EncodeMusic *)

	SetInstrument (3, 40000, 1000, 500);

	(************************)
	(* F. Sor, Estudio VII	*)
	(************************)

	Voice2("d");  Voice3("d");

	FOR repeat := 1 TO 2 DO

	    Voice1 ("CCC  *3/2C/3CDE  FGAGFE *EDR");
	    Voice2 ("AABb *3/2A/3ABbC DEFEDC *CBbR");
	    Voice3 ("RFE  *3/2FR      *2R    R/3");

	    Voice1 ("DDD    D/3DCBCEF GEBbGECBbGC");
	    Voice2 ("BbBbBb BbRR      *3R");
	    Voice3 ("RGF    E RR      RRE");

	    Voice1 ("*3BbAR /3CDEFAG/2GF*2EF Gd*8/9C/8R*8C/8R*9*3CC");
	    Voice2 ("R      /9ABbGACBbAGA        *3Bb/3REFGABb");
	    Voice3 ("*2F/2R *3R                  /3R/3RCDEFG");

	    Voice1 ("/3CDEFAG/2GF*2EF Gd*8/9C/8R*8C/8R*9*3CC /3CDEFGABbCA");
	    Voice2 ("ABbGACBbAGA          *3Bb/3REFGABb          ABbCDEFGAF");
	    Voice3 ("*3/2FR*2RR           R/3RCDEFG              *3FRR");

	    Voice1 ("*3GFE           FED         CCR");
	    Voice2 ("EdGuEDdG#uDCAC *3DCB        /3CEdGCdEG*3R");
	    Voice3 ("*3R            /9RFARGCRGBb *3RRdC");

	END (*FOR*);

	(* Second section. *)

	FOR repeat := 1 TO 2 DO

	    Voice1 ("/3uGE*8/9C/8R*8C/8R*8C/8R*8C/8R*8C/8R*8C/8R*8C/8R*9"
		+ " CDEFAGFGA BbGE*8/9C/8R*8C/8R*8C/8R*8C/8R*8C/8R*8C/8R*9");
	    Voice2 ("RRBb ARR RRBb");
	    Voice3 ("RRE  FRR GGE");

	    Voice1 ("CBCDEFF#GG#" +
		" Ad*8/9D/8R*8D/8R*8D/8R*8D/8R*8D/8R*9D*8/9G/8R*8G/8R*9"
		+ " Gd*8/9C/8R*8C/8R*8C/8R*8C/8R*8C/8R*8C/8R*8F/8R*8F/8R*9");
	    Voice2 ("ARR RC/2BbR *4Bb/2A");
	    Voice3 ("FRR RF#/2GR *2REF");

	    Voice1 ("FDG*3FE      /3FCBCDEFGG# *3A/2BbR*2B");
	    Voice2 ("/3RRBbRACRGC *3ARR        /3RCEb*3/2DR*2/3RDF*3");
	    Voice3 ("dBb/3CRRCRR  *3FRR        F/2BbR*2G");

	    Voice1 ("/2CR*2C#/2DR      *4dF/2E         *2F/2Rd");
	    Voice2 ("/2ER*2/3REG*3/2FR *4dA/2R         *2A/2R");
	    Voice3 ("/2CR*2A/2BbR      *2/3dCFACAFCuGBb *3dFuFR");

	END (*FOR*);

    END EncodeMusic;

(************************************************************************)

BEGIN
    SetDuration (324);
    EncodeMusic;
    PlayTheMusic;
END M3D03.
