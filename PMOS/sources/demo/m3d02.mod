MODULE M3D02;

	(********************************************************)
	(*							*)
	(*		Test of module Music3.			*)
	(*							*)
	(*	     J.S. Bach: Minuet from the			*)
	(*	 "Notenbuch vor Anna Magdalena Bach"		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

FROM Music3 IMPORT
    (* proc *)	Voice1, Voice2, SetDuration, PlayTheMusic, SetInstrument,
		SetEnvelope, SetWaveform;

(************************************************************************)

PROCEDURE EncodeMusic;

    (* Plays a piece of music. *)

    BEGIN	(* EncodeMusic *)

	(************************)
	(* J.S. Bach, Minuet	*)
	(************************)

	SetWaveform (1, 2);
	SetEnvelope (1, 20000, 2000, 0);
	SetWaveform (2, 1);
	SetEnvelope (2, 10000, 40000, 50000);

	Voice1 ("u");
	Voice1 ("D/dGABC*DdGG uE/CDEF#*GdGG");
	Voice1 ("C/DCBA*B/CBAG *F#/GABG*6A/3");
	Voice1 ("D/dGABC*DdGG uE/CDEF#*GdGG");
	Voice1 ("C/DCBA*B/CBAG *A/BAGF#*6G/3u");

	Voice2 ("d");
	Voice2 ("*G/A*3B CB");
	Voice2 ("AG /3uDd*2G/2uD/DCBA");
	Voice2 ("*4B/2AGBG *3C/3B/CBAG");
	Voice2 ("*4A/2F#*2G/2B C*2D/2dGuDdG");

	(* Repeat of the first part. *)

	Voice1 ("D/dGABC*DdGG uE/CDEF#*GdGG");
	Voice1 ("C/DCBA*B/CBAG *F#/GABG*6A/3");
	Voice1 ("D/dGABC*DdGG uE/CDEF#*GdGG");
	Voice1 ("C/DCBA*B/CBAG *A/BAGF#*6G/3u");

	Voice2 ("*G/A*3B CB");
	Voice2 ("AG /3uDd*2G/2uD/DCBA");
	Voice2 ("*4B/2AGBG *3C/3B/CBAG");
	Voice2 ("*4A/2F#*2G/2B C*2D/2dGuDdG");

	(* Page 2. *)

	Voice1 ("B/GABG*A/dDEF#D *G/EF#GD*C#/BC#*A");
	Voice1 ("/ABC#DEF#*GF#E F#dAC#*3D/3");
	Voice1 ("D/dGF#*GuE/dGF#*G uDCB/AGF#G*A");
	Voice1 ("/dDEF#GAB*CBA /BD*dGF#*3G/3u");

	Voice2 ("uGDGF#AdD EGEAdAuA");
	Voice2 ("dAuAdABDC# DdF#ADDC");
	Voice2 ("*2B/2B*2C/2C BAGuDRRd");
	Voice2 ("RRF#EGF# GBDdGuDdG");

	(* Repeat of the second part. *)

	Voice1 ("B/GABG*A/dDEF#D *G/EF#GD*C#/BC#*A");
	Voice1 ("/ABC#DEF#*GF#E F#dAC#*3D/3");
	Voice1 ("D/dGF#*GuE/dGF#*G uDCB/AGF#G*A");
	Voice1 ("/dDEF#GAB*CBA /BD*dGF#*3G/3u");

	Voice2 ("uGDGF#AdD EGEAdAuA");
	Voice2 ("dAuAdABDC# DdF#ADDC");
	Voice2 ("*2B/2B*2C/2C BAGuDRRd");
	Voice2 ("RRF#EGF# GBDdGuDdG");

    END EncodeMusic;

(************************************************************************)

BEGIN
    SetDuration (280);
    EncodeMusic;
    PlayTheMusic;
END M3D02.
