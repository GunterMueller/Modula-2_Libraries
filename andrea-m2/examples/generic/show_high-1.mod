MODULE ShowHigh;

(* show some examples of using objects passed as ARRAY OF BYTE and HIGH *)
(* Jaa, Oct.3/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn;
FROM SYSTEM IMPORT BYTE;

VAR
  s :ARRAY [0..20] OF CHAR;
  x :REAL;
  b :BOOLEAN;
  c :CARDINAL;

  (* ----------------------------------------- *)
  PROCEDURE Show( data :ARRAY OF BYTE );
  BEGIN
     WriteString( 'HIGH+1 gives ' ); WriteCard( HIGH(data)+1, 0 );
  END Show;

BEGIN

  WriteString( 'using string   ' ); Show( s ); WriteLn;
  WriteString( 'using real     ' ); Show( x ); WriteLn;
  WriteString( 'using boolean  ' ); Show( b ); WriteLn;
  WriteString( 'using cardinal ' ); Show( c ); WriteLn;
END ShowHigh.
