MODULE TestBase3;

(* Test the Base3 module *)
(* J. Andrea, 1992 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn;
FROM Base3 IMPORT ToBase3, HowMany3;

VAR
  string3 :ARRAY [0..30] OF CHAR;
  i :CARDINAL;
  ok :BOOLEAN;

BEGIN

  FOR i := 0 TO 20 DO
    WriteCard( i, 2 );
    ToBase3( i, string3, ok );
    WriteString( '   base 3 = ' ); WriteString( string3 );
    WriteString( '   #digits= ' ); WriteCard( HowMany3(i), 0 );
    WriteLn;
  END;

END TestBase3.
