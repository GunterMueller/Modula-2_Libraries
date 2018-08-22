MODULE M3D04;

	(********************************************************)
	(*							*)
	(*		Test of module Music3.			*)
	(*							*)
	(*	 J.S. Bach: O Haupt voll Blut und Wunden,	*)
	(*	from the Passion according to Saint Matthew	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM Music3 IMPORT
    (* proc *)	Voice1, Voice2, Voice3, SetDuration, PlayTheMusic,
		SetInstrument;

(************************************************************************)

PROCEDURE EncodeMusic;

    (* Plays a piece of music. *)

    VAR repeat: [1..2];

    BEGIN	(* EncodeMusic *)

	(*******************************************)
	(* J.S. Bach, O Haupt voll Blut und Wunden *)
	(*******************************************)

	SetInstrument (0, 8000, 50, 20000);

	Voice3("d");

	FOR repeat := 1 TO 2 DO

	    Voice1 ("B EDCB        *2A/2BuF#");
	    Voice2 ("G CGGG        RRGR");
	    Voice3 ("G CB/2dEF#*2G CDdGF#");

	    Voice1 ("GG/2F#E*2F#  *3E/3");
	    Voice2 ("BBB/2BA      *6B/3");
	    Voice3 ("/2EF#GA*2BD# *3dE/3");

	END (*FOR*);

	Voice1 ("G /2F#E*2DEF#");
	Voice2 ("B CGCC");
	Voice3 ("E AB/2CB*2A");

	Voice1 ("*2G/2GD   EDCC *3B/3uG /2F#G*2AGF#");
	Voice2 ("C/2BA*2BR CBBA *3G#/3A DR/2BC#*2D");
	Voice3 ("*2G/2GG   CG#/2ABCD    *6E/3C# DC#BA");

	Voice1 ("*2E/2F#dB CBAD      *3B");
	Voice2 ("BC#DB     AGAF#     *3G");
	Voice3 ("G#ADdG    A/2BC*2DD *3dG");

    END EncodeMusic;

(************************************************************************)

BEGIN
    SetDuration (400);
    EncodeMusic;
    PlayTheMusic;
END M3D04.
