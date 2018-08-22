MODULE M3D01;

	(********************************************************)
	(*							*)
	(*		Test of module Music3.			*)
	(*							*)
	(*	J.S. Bach: Prelude from Cello Suite No. 1	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		OK				*)
	(*							*)
	(*	Note that this music has only a single voice.	*)
	(*							*)
	(********************************************************)

FROM Music3 IMPORT
    (* proc *)	Voice1, SetDuration, PlayTheMusic, SetInstrument;

(************************************************************************)

PROCEDURE EncodeMusic;

    (* Plays a piece of music. *)

    BEGIN	(* EncodeMusic *)

	(********************************************************)
	(* Opening of J.S. Bach, Cello Suite No. 1, Prelude.	*)
	(********************************************************)

	SetDuration (130);
	SetInstrument (7, 6000, 500, 30000);

	Voice1 ("dDuAuF#EF#dAuF#dAddDuuAuF#EF#dAuF#dA");
	Voice1 ("dDuBuGF#GdBuGdBddDuuBuGF#GdBuGdB");
	Voice1 ("DC#GF#GC#GC#ddDuuC#GF#GC#GC#");
	Voice1 ("dDuDF#EF#DF#DddDuuDF#EF#DF#C#");
	Voice1 ("dDuBuF#EF#DC#DBDC#DdF#AG#F#");
	Voice1 ("G#uDEDEDEDdG#uDEDEDED");
	Voice1 ("C#EAG#AEDEC#EDEdAC#BA");
	Voice1 ("dBuF#uDC#DdF#uDdF#dBuF#uDC#DdF#uDdF#");
	Voice1 ("dBuG#ABAG#F#EuDC#BuAG#F#ED");
	Voice1 ("C#BAuAEAdC#EdABC#EDC#BA");
	Voice1 ("uD#dACBCAuD#dAuF#dACBCAuD#dA");
	Voice1 ("GBEF#GEBAGBEF#GEC#B");
	Voice1 ("A#C#A#C#EC#EC#A#C#A#C#EC#EC#");
	Voice1 ("DC#BDC#DEC#DC#BAGF#ED");

	(* Page 2. *)

	Voice1 ("C#GAGAGAGC#GAGAGAG");
	Voice1 ("DF#uCBCdF#uCdF#DF#uCBCdF#uCdF#");
	Voice1 ("DGBABGBGDGBABGBG");
	Voice1 ("DuC#GF#GC#GC#dDuC#GF#GC#GC#d");
	Voice1 ("DuAuF#EF#DC#BAGF#EDC#BA");
	Voice1 ("G#uEuBC#DBC#DddG#uEuBC#DBC#D");
	Voice1 ("ddGuEABC#ABC#ddGuEABC#ABC#");
	Voice1 ("ddGuEAC#EG#*A/AdEF#GABC#D");
	Voice1 ("EC#ABC#DEF#GEC#DEF#GA");
	Voice1 ("BbAG#AAGF#GGEC#BAEF#G");
	Voice1 ("dAuEAC#EF#GEF#DAGF#DEF#");
	Voice1 ("dADF#ADEF#DG#FEFFED#E");
	Voice1 ("EDC#DDBG#F#EG#BDEG#AG#");
	Voice1 ("AEC#BC#EdAC#dEAG#F#EDC#B");

	(* Page 3. *)

	Voice1 ("*A/uuGF#EDC#BAuGF#EDC#BA");
	Voice1 ("GuF#EDC#BAGF#uEDC#BAGF#");
	Voice1 ("EuDC#BC#EdAuEBEC#EDEBE");
	Voice1 ("C#EdAuEDEBEC#EdAuEDEBE");
	Voice1 ("C#EdAuEBEC#EDdEuEdEuF#EddAuEu");
	Voice1 ("EdEuF#EGEddAuuEF#EGEAEF#E");
	Voice1 ("GEF#EGEdEuEF#EdEuEF#EDE");
	Voice1 ("EdEuDEdEuEC#EDEC#EDEBE");
	Voice1 ("C#EdABCdAuC#dAuDdAuuD#ddAuuEddAuuFddA");
	Voice1 ("uuF#ddAuuGddAuuG#ddAuuAddAuuBbddAuuBddAuuCddAuuC#ddA");
	Voice1 ("uuDdF#dAuF#uDdF#uDdF#uDdF#dAuF#uDdF#uDdF#");
	Voice1 ("uDdEdAuEuDdEuDdEuDdEdAuEuDdEuDdE");
	Voice1 ("uC#dGdAuGuC#dGuC#dGuC#dGdAuGuC#dGuC#dG");
	Voice1 ("u***D");

    END EncodeMusic;

(************************************************************************)

BEGIN
    EncodeMusic;
    PlayTheMusic;
END M3D01.
