MODULE TestBase3;

(* Test the Base3 module *
(* J. Andrea, 1992 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn;
FROM Base3 IMPORT ToBase3, FromBase3;

VAR
  string3 :ARRAY [0..30] OF CHAR;
  i, j :CARDINAL;
  ok, errors :BOOLEAN;

BEGIN

  errors := FALSE;

  FOR i := 0 TO 4000 DO
    ToBase3( i, string3, ok );
    FromBase3( string3, j, ok );
    IF i # j THEN
      errors := TRUE;
      WriteCard( i, 2 );
      WriteString( '   base 3 = ' ); WriteString( string3 );
      WriteString( ' convert error ' ); WriteCard( j, 0 );
      WriteLn;
    END;
  END;

  IF NOT errors THEN
    WriteString( 'all ok' ); WriteLn;
  END;
  
END TestBase3.
