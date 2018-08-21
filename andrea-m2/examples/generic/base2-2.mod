MODULE TestBase2;

(* Test the Base2 module *)
(* John Andrea, Nov.18/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn;
FROM Base2 IMPORT ToBase2, FromBase2;

VAR
  string2 :ARRAY [0..30] OF CHAR;
  i, j :CARDINAL;
  ok, errors :BOOLEAN;

BEGIN

  errors := FALSE;

  FOR i := 0 TO 4000 DO
    ToBase2( i, string2, ok );
    FromBase2( string2, j, ok );
    IF i # j THEN
      errors := TRUE;
      WriteCard( i, 2 );
      WriteString( '   base 2 = ' ); WriteString( string2 );
      WriteString( ' convert error ' ); WriteCard( j, 0 );
      WriteLn;
    END;
  END;

  IF NOT errors THEN
    WriteString( 'all ok' ); WriteLn;
  END;

END TestBase2.
