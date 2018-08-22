MODULE M3D05;

	(********************************************************)
	(*							*)
	(*		  Test of module Music3B.		*)
	(*							*)
	(*		     Francisco Tarrega			*)
	(*		 Recuerdos de la Alhambra		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(*	I'm still not completely happy with the		*)
	(*		closing chords.				*)
	(*							*)
	(********************************************************)

FROM Music3 IMPORT
    (* proc *)	Voice1, Voice2, Voice3, SetDuration,
		SetWaveform, SetEnvelope, EnableVoice, PlayTheMusic;

(************************************************************************)

PROCEDURE Section1;

    (* First section (first page and a half of the music). *)

    BEGIN
	Voice1 ("REEEEEEEEEEEEEEEDDDDDDDD CCCCCCCCCCCCCCCCDDDDDDDD");
	Voice2 ("R   E  uC  dE  uB  dE    R   E   A   E  uB  dE   ");
	Voice3 ("A                        A                       ");

	Voice1 ("EEEEEEEEEEEEEEEEEEEEEEEE EEEEEEEEEEEEEEEEFFFFFFFF");
	Voice2 ("R   E  uC  dE  uC  dE    R   E  uC  dE  uD  dG  ");
	Voice3 ("A                        A                       ");

	Voice1 ("GGGGGGGGGGGGGGGGFFFFFFFF EEEEEEEEEEEEEEEEFFFFFFFF");
	Voice2 ("R   G  uE  dG  uD  dG    R   G   C   G  uD  dG   ");
	Voice3 ("C                        C                       ");

	Voice1 ("GGGGGGGGGGGGGGGGGGGGGGGG GGGGGGGGGGGGGGGGGGGGGGGG");
	Voice2 ("R   G  uE  dG  uE  dG    R   G  uE  dG  uE  dG   ");
	Voice3 ("C                        C                       ");

	Voice1 ("CCCCCCCCCCCCCCCCBBBBBBBB AAAAAAAAAAAAAAAABBBBBBBB");
	Voice2 ("R   C  uA  dC  uG  dC    R   C   F   C  dD  uF   ");
	Voice3 ("F                        F                       ");

	Voice1 ("AAAAAA*2/3ABA*3/2G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#");
	Voice2 ("R  dB            E       B       E       B       ");
	Voice3 ("dE                                               ");
	Voice1 ("G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#");
	Voice2 ("R       D       E       D       E       D       ");
	Voice3 ("E                                               ");

	(* Page 2. *)

	Voice1 ("BbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbAAAAAAAA");
	Voice2 ("R       C#      E       C#      E   C#  ");
	Voice3 ("A                                       ");
	Voice1 ("GGGGGGGGGGGGGGGGAAAAAAAA");
	Voice2 ("R   A  uE  dA  uF  dA   ");
	Voice3 ("C#                      ");

	Voice1 ("GGGGGG*2/3GAG*3/2FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFFFFFFFFFF");
	Voice2 ("R  uE            D   A   D   A    R   A   D   A   D   A   ");
	Voice3 ("D                                 D                       ");

	Voice1 ("EEEEEEEEEEEEEEEEDDDDDDDD CCCCCCCCCCCCCCCCDDDDDDDD");
	Voice2 ("R   A   B   A   B   A    R   Eb  A   Eb  A   Eb  ");
	Voice3 ("D                        dF                      ");

	Voice1 ("CCCCCC*2/3CDC*3/2BBBBBBBBBBBBBBBB BBBBBBBBBBBBBBBBBBBBBBBB");
	Voice2 ("R   E            G#  E   G#  E    R   E   G#  E   G#  E   ");
	Voice3 ("E                                 E                       ");

    END Section1;

(************************************************************************)

PROCEDURE Section2;

    (* Last three lines of page 2, plus first four and a half lines of	*)
    (* page 3.								*)

    BEGIN

	(* Page 2, line 5 (where key changes to A major). *)

	Voice1 ("EEEEEEEEEEEEEEEEDDDDDDDD");
	Voice2 ("R   E  uC# dE  uB  dE   ");
	Voice3 ("A                       ");
	Voice1 ("C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#DDDDDDDD");
	Voice2 ("R       E       A       E      uB  dE   ");
	Voice3 ("A                                       ");

	Voice1 ("EEEEEEEEEEEEEEEEEEEEEEEE EEEEEEEEEEEEEEEEEEEEEEEE");
	Voice2 ("R   E  uC# dE  uC# dE    R   E  uC# dE  uC# dE   ");
	Voice3 ("A                        A                       ");

	Voice1 ("F#F#F#F#F#F#F#F#F#F#F#F#F#F#F#F#F#F#F#F#F#F#F#F#");
	Voice2 ("R       F#     uD      dF#     uD      dF#      ");
	Voice3 ("A                                               ");
	Voice1 ("uDDDDDDDDDDDDDDDDdF#F#F#F#F#F#F#F#");
	Voice2 (" R  uD  uB  dD    D      dF#      ");
	Voice3 (" A                                ");

	(* Page 3. *)

	Voice1 ("F#F#F#F#F#F#*2/3F#G#F#*3/2EEEEEEEEEEEEEEEE");
	Voice2 ("R      uD                 C# dE  uC# dE   ");
	Voice3 ("A                                         ");
	Voice1 ("EEEEEEEEEEEEEEEEEEEEEEEE");
	Voice2 ("R   E  uC# dE  uC# dE   ");
	Voice3 ("A                       ");

	Voice1 ("AAAAAAAAAAAAAAAAAAAAAAAA");
	Voice2 ("R   F#  A   C#  A   F#  ");
	Voice3 ("F#                      ");
	Voice1 ("G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#DDDDDDDD");
	Voice2 ("R       G#      C       D#      C   G#  ");
	Voice3 ("G#                                      ");

	Voice1 ("F#F#F#F#F#F#F#F#EEEEEEEEEEEEEEEE EEEEEEEEEEEEEEEEEEEEEEEE");
	Voice2 ("R       G#      C#  G#  C#  G#   R   G#  C#  G#  C#  G#  ");
	Voice3 ("C#                               C#                      ");

	Voice1 ("DDDDDDDDDDDDDDDDDDDDDDDD");
	Voice2 ("R   F   F   D   E   F   ");
	Voice3 ("B                       ");
	Voice1 ("C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#BBBBBBBB");
	Voice2 ("R       E       E       E      dE  uD   ");
	Voice3 ("dE                                      ");

	Voice1 ("BBBBBB*2/3BC#B*3/2AAAAAAAAAAAAAAAA");
	Voice2 ("R   E             E   E   F#  G#  ");
	Voice3 ("A                                 ");

    END Section2;

(************************************************************************)

PROCEDURE EncodeMusic;

    (* Plays a piece of music. *)

    CONST BaseDuration = 48;

    BEGIN	(* EncodeMusic *)

	SetDuration (BaseDuration);
	SetWaveform (1, 1);
	SetWaveform (2, 1);
	SetWaveform (3, 6);
	SetEnvelope (1, 50000, 10000, 10000);
	SetEnvelope (2, 50000, 3000, 0);
	SetEnvelope (3, 50000, 2000, 0);

	EnableVoice (1, TRUE);
	EnableVoice (2, TRUE);
	EnableVoice (3, TRUE);

	(****************************************)
	(* F. Tarrega, Recuerdos de la Alhambra	*)
	(****************************************)

	Voice2("d*4");  Voice3("dd*4*6");

	(* Two repeats of the A minor section. *)

	Section1;
	Section1;

	(* Two repeats of the A major section, with slightly different	*)
	(* endings.							*)

	Section2;
	Voice1 ("AAAAAAAAAAAABBBBC#C#C#C#DDDD");
	Voice2 ("R   E   C#  E   A       B  d");
	Voice3 ("A                           ");

	Section2;
	Voice1 ("AAAAAAAAAAAABBBBCCCCDDDD");
	Voice2 ("R   E   E   G#  A   B  d");
	Voice3 ("A                       ");

	(* D.C. al S/S. *)

	Section1;
	Section2;
	Voice1 ("AAAAAAAAAAAABBBBCCCCDDDD");
	Voice2 ("R   E   E   G#  A   B  d");
	Voice3 ("A                       ");

	(* Continue with the remainder of the A major section. *)

	Voice1 ("AAAAAAAAAAAAAAAAAAAAAAAA");
	Voice2 ("R   E   E   E   E   E   ");
	Voice3 ("A                       ");

	Voice1 ("AAAAAAAAAAAAAAAAAAAAAAAA BBBBBBBBCCCCCCCCDDDDDDDD");
	Voice2 ("R   F   F   F   F   F    R   F   A   F   A   F   ");
	Voice3 ("A                        A                       ");

	(* Page 4. *)

	Voice1 ("EEEEEEEEEEEEEEEEEEEEEEEE EEEEEEEEEEEEEEEEEEEEEEEE");
	Voice2 ("R   E  uC# dE  uC# dE    R   E  uC# dE  uC# dE   ");
	Voice3 ("A                        A                       ");

	Voice1 ("EEEEEEEEEEEEEEEEEEEEEEEE DDDDDDDDC#C#C#C#C#C#C#C#BBBBBBBB");
	Voice2 ("R   G#  G#  G#  G#  G#   R   F   E       E      dE  uD   ");
	Voice3 ("E                        E                               ");

	Voice1 ("AAAAAAAAAAAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAA");
	Voice2 ("R   E   E   E#  F#  E#   R   E   E   E   E   E   ");
	Voice3 ("A                        A                       ");

	Voice1 ("AAAAAAAAAAAAAAAAAAAAAAAA BBBBBBBBCCCCCCCCDDDDDDDD");
	Voice2 ("R   F   F   F   F   F    R   F   A   F   A   F   ");
	Voice3 ("A                        A                       ");

	Voice1 ("EEEEEEEEEEEEEEEEEEEEEEEE EEEEEEEEEEEEEEEEEEEEEEEE");
	Voice2 ("R   E  uC# dE  uC# dE    R   E  uC# dE  uC# dE   ");
	Voice3 ("A                        A                       ");

	Voice1 ("G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#G#");
	Voice2 ("R       E      uE      dE      uE      dE       ");
	Voice3 ("E                                               ");
	Voice1 ("F#F#F#F#F#F#F#F#EEEEEEEEDDDDDDDD");
	Voice2 ("R       E      uC# dE  uB  dE   ");
	Voice3 ("E                               ");

	Voice1 ("C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#");
	Voice2 ("R       E       A       E       F#      A       ");
	Voice3 ("A                                               ");
	SetDuration (9*BaseDuration DIV 8);
	Voice2("*4");  Voice3("*4*6");
	Voice1 ("C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#");
	Voice2 ("R       E       A      dE      uC       C#      ");
	Voice3 ("A                                               ");

	SetDuration (6*BaseDuration DIV 5);
	Voice2("*4");  Voice3("*4*6");

	Voice1 ("C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#");
	Voice2 ("R       E       A      dE      uC       C#      ");
	Voice3 ("A                                               ");
	Voice1 ("*4*6R                   ");
	Voice2 ("R   E   A   C#  E   A   ");
	Voice3 ("A                       ");

	SetEnvelope (1, 50000, 20000, 20000);
	SetEnvelope (2, 50000, 20000, 20000);
	SetEnvelope (3, 50000, 20000, 20000);
	SetWaveform (3, 1);

	SetDuration (36*BaseDuration);

	Voice1 ("uE *2/3 ddA");
	Voice2 (" A *2/3  dE");
	Voice3 (" A *2/3   A");

    END EncodeMusic;

(************************************************************************)

BEGIN
    EncodeMusic;
    PlayTheMusic;
END M3D05.
