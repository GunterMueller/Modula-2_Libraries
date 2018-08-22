IMPLEMENTATION MODULE MusicDemonstration;

	(********************************************************)
	(*							*)
	(*		Test of module Music.			*)
	(*							*)
	(*	J.S. Bach: Prelude from Cello Suite No. 1	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	16 August 1993			*)
	(*  Status:						*)
	(*	Working.  Musically, it could be polished up a	*)
	(*	little, but from the software development	*)
	(*	viewpoint it is complete - module Music does	*)
	(*	indeed work as desired.				*)
	(*							*)
	(*	Some notes possibly still wrong.		*)
	(*							*)
	(********************************************************)

FROM TaskControl IMPORT
    (* proc *)	CreateTask;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM Music IMPORT
    (* proc *)	SetNoteDuration, PlayMusic, WaitForMusicFinished;

(************************************************************************)

VAR

    (* The MusicFinished semaphore is used to signal the end of the	*)
    (* demonstration.							*)

    MusicFinished: Semaphore;

(************************************************************************)

PROCEDURE PlayATune;

    (* Plays a piece of music.  This procedure runs as a separate task,	*)
    (* which does not exit until the playing is over.			*)

    BEGIN	(* PlayATune *)

	(********************************************************)
	(* Opening of J.S. Bach, Cello Suite No. 1, Prelude.	*)
	(********************************************************)

	SetNoteDuration (200);

	PlayMusic ("dDuAuF#EF#dAuF#dAddDuuAuF#EF#dAuF#dA");
	PlayMusic ("dDuBuGF#GdBuGdBddDuuBuGF#GdBuGdB");
	PlayMusic ("DC#GF#GC#GC#ddDuuC#GF#GC#GC#");
	PlayMusic ("dDuDF#EF#DF#DddDuuDF#EF#DF#C#");
	PlayMusic ("dDuBuF#EF#DC#DBDC#DdF#AG#F#");
	PlayMusic ("G#uDEDEDEDdG#uDEDEDED");
	PlayMusic ("C#EAG#AEDEC#EDEdAC#BA");
	PlayMusic ("dBuF#uDC#DdF#uDdF#dBuF#uDC#DdF#uDdF#");
	PlayMusic ("dBuG#ABAG#F#EuDC#BuAG#F#ED");
	PlayMusic ("C#BAuAEAdC#EdABC#EDC#BA");
	PlayMusic ("uD#dACBCAuD#dAuF#dACBCAuD#dA");
	PlayMusic ("GBEF#GEBAGBEF#GEC#B");
	PlayMusic ("A#C#A#C#EC#EC#A#C#A#C#EC#EC#");
	PlayMusic ("DC#BDC#DEC#DC#BAGF#ED");

	(* Page 2. *)

	PlayMusic ("C#GAGAGAGC#GAGAGAG");
	PlayMusic ("DF#uCBCdF#uCdF#DF#uCBCdF#uCdF#");
	PlayMusic ("DGBABGBGDGBABGBG");
	PlayMusic ("DuC#GF#GC#GC#dDuC#GF#GC#GC#d");
	PlayMusic ("DuAuF#EF#DC#BAGF#EDC#BA");
	PlayMusic ("G#uEuBC#DBC#DddG#uEuBC#DBC#D");
	PlayMusic ("ddGuEABC#ABC#ddGuEABC#ABC#");
	PlayMusic ("ddGuEAC#EG#*A/AdEF#GABC#D");
	PlayMusic ("EC#ABC#DEF#GEC#DEF#GA");
	PlayMusic ("BbAG#AAGF#GGEC#BAEF#G");
	PlayMusic ("dAuEAC#EF#GEF#DAGF#DEF#");
	PlayMusic ("dADF#ADEF#DG#FEFFED#E");
	PlayMusic ("EDC#DDBG#F#EG#BDEG#AG#");
	PlayMusic ("AEC#BC#EdAC#dEAG#F#EDC#B");

	(* Page 3. *)

	PlayMusic ("*A/uuGF#EDC#BAuGF#EDC#BA");
	PlayMusic ("GuF#EDC#BAGF#uEDC#BAGF#");
	PlayMusic ("EuDC#BC#EdAuEBEC#EDEBE");
	PlayMusic ("C#EdAuEDEBEC#EdAuEDEBE");
	PlayMusic ("C#EdAuEBEC#EDdEuEdEuF#EddAuEu");
	PlayMusic ("EdEuF#EGEddAuuEF#EGEAEF#E");
	PlayMusic ("GEF#EGEdEuEF#EdEuEF#EDE");
	PlayMusic ("EdEuDEdEuEC#EDEC#EDEBE");
	PlayMusic ("C#EdABCdAuC#dAuDdAuuD#ddAuuEddAuuFddA");
	PlayMusic ("uuF#ddAuuGddAuuG#ddAuuAddAuuBbddAuuBddAuuCddAuuC#ddA");
	PlayMusic ("uuDdF#dAuF#uDdF#uDdF#uDdF#dAuF#uDdF#uDdF#");
	PlayMusic ("uDdEdAuEuDdEuDdEuDdEdAuEuDdEuDdE");
	PlayMusic ("uC#dGdAuGuC#dGuC#dGuC#dGdAuGuC#dGuC#dG");
	PlayMusic ("u***D");

	WaitForMusicFinished;
	Signal (MusicFinished);
    END PlayATune;

(************************************************************************)

PROCEDURE WaitForEndOfMusic;

    (* Synchronization procedure: does not return until the music	*)
    (* demonstration is over.						*)

    BEGIN
	Wait (MusicFinished);
    END WaitForEndOfMusic;

(************************************************************************)

PROCEDURE StartTheMusic;

    (* Creates the player task, after which it will run autonomously.	*)

    CONST PlayerPriority = 12;

    BEGIN
	CreateSemaphore (MusicFinished, 0);
	CreateTask (PlayATune, PlayerPriority, "Music demo");
    END StartTheMusic;

(************************************************************************)

BEGIN
    StartTheMusic;
END MusicDemonstration.
