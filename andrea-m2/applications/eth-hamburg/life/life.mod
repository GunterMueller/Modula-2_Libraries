MODULE Life;

(*
  The program gets input from a file that contains
  a first line of a file description ( any characters )
  and a list of coordinates ( row , column ) for the first generation
  of LIVE cells.
*)
(*
  see: Computer Recreations Dept. in Oct.83 and March.84 Scientific American
       and the original paper by Martin Gardner in
            Mathematical Games, Scientific American, Oct. 1970
*)
(* The Game of Life, John Andrea, Oct.25/ 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)


FROM InOut      IMPORT WriteString, WriteLn, OpenInput, CloseInput;
FROM FileSystem IMPORT Done;
FROM LifeModule IMPORT Genesis, PlayGod;


BEGIN (* Life *)

  WriteLn;
  WriteString('What is the name of the coordinate file ? ');
  OpenInput('.DAT');

  IF Done() THEN

     Genesis;

     CloseInput;

     PlayGod;

  ELSE

    WriteLn;
    WriteString('No such file available !');
    WriteLn;

  END; (* if *)

END Life.
