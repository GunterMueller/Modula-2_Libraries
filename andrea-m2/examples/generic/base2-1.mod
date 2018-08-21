MODULE TestBase2;

(* Test the Base2 module *)
(* John Andrea, Nov.18/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn;
FROM Base2 IMPORT ToBase2, HowMany2;

VAR
  string2 :ARRAY [0..30] OF CHAR;
  i :CARDINAL;
  ok :BOOLEAN;

BEGIN

  FOR i := 0 TO 20 DO
    WriteCard( i, 2 );
    ToBase2( i, string2, ok );
    WriteString( '   base 2 = ' ); WriteString( string2 );
    WriteString( '   #digits= ' ); WriteCard( HowMany2(i), 0 );
    WriteLn;
  END;

END TestBase2.
