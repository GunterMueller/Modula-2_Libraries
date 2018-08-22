MODULE M3D06;

	(********************************************************)
	(*							*)
	(*		  Test of module Music3B.		*)
	(*							*)
	(*		Fernando Sor, Estudio XVII		*)
	(*		  of the Segovia edition		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		Incomplete			*)
	(*							*)
	(*	I'm not yet sure I'll have the patience		*)
	(*		  to finish this.			*)
	(*							*)
	(********************************************************)

FROM Music3 IMPORT
    (* proc *)	Voice1, Voice2, Voice3, SetDuration, SetInstrument,
		SetWaveform, SetEnvelope, EnableVoice, PlayTheMusic;

(************************************************************************)

PROCEDURE EncodeMusic;

    (* Plays a piece of music. *)

    CONST BaseDuration = 130;

    BEGIN	(* EncodeMusic *)

	SetDuration (BaseDuration);
	SetInstrument (1, 65535, 2000, 0);
	SetEnvelope (2, 65535, 3000, 0);
	SetWaveform (1, 1);
	SetWaveform (2, 1);
	SetWaveform (3, 1);

	EnableVoice (1, TRUE);
	EnableVoice (2, TRUE);
	EnableVoice (3, TRUE);

	Voice1 ("d*3");  Voice2("d");  Voice3("dd*4*3");

	Voice1 ("R  B  E  E    E  B  B  B  ");
	Voice2 ("REGREGREGREG  REGREGREGREG");
	Voice3 ("E             E           ");

	Voice1 ("R  B  E  G    *2G    F#   /2 R  F# F# F# ");
	Voice2 ("REGREGREGREG  REACdEAREACdEA REAREAREAREA");
	Voice3 ("E             E              E           ");

	Voice1 ("F# dB   B  dC    uR  F#  G   F# *2F#     E    /2");
	Voice2 ("RD#ARD#ARD#ARD#A RD#ARD#ARD#ARD#A REABdEAREGBdEG");
	Voice3 ("E                uB               dE            ");

	Voice1 ("R   G    G    G      G  F# *2F#/2 R  A  A  A  ");
	Voice2 ("REuBRdEuBRdEuBRdEuC# RADRADRADRAD RCDRCDRCDRCD");
	Voice3 ("E                    *3/4uD /3D   *3F#   /3F# ");

	Voice1 ("A  G*2G/2    R  B  B  B   B  A*2A   /2");
	Voice2 ("RBDRBDRBDRBD RBDRBDRBDRBD RCERCERCERCE");
	Voice3 ("*3G    /3G   *4dG         *3/4C  /3C  ");

	Voice1 ("R     G     G     G      G   F#*2F# /2 R  F# F# F# ");
	Voice2 ("RdBbuERdBbuERdBbuERdBbuE RdADRADRADRAD RADRADRADRAD");
	Voice3 ("*4C#                     *3/4D   /3D   *2C   R     ");

	Voice1 ("F#   G   *2G/2       R    A    A    A    A#   B  *2B/2     ");
	Voice2 ("RdGuDRdGuDRdGuDRdGuD RdDuCRdDuCRdDuCRdDuCRdDuBRdDuBRdDuBRdDG");
	Voice3 ("*3/2B        /3B    *4dF#            /4*3G            /3G   ");

	(* Some faults still exist in the next two lines. *)

	Voice1 ("R  G  G  G   R  A  R  C    R dF# F# F# ");
	Voice2 ("RGBRGBRGBRGB RCERCERCERCE dRACRACRACRAC");
	Voice3 ("*3E   /3uE  *2C    A       D   /2R  D# ");

	Voice1 ("F# G *2G  /2 R     F     F     F     F  E  R  A  ");
	Voice2 ("RACRGBRGBRGB RG#uDdRG#uDdRG#uDdRG#uD RADRACRCERCE");
	Voice3 ("*3E    /3E  *4B                      /2C   A     ");

	(* Page 2. *)

    END EncodeMusic;

(************************************************************************)

BEGIN
    EncodeMusic;
    PlayTheMusic;
END M3D06.
