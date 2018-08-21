MODULE ShowHigh;

(* show some examples of using objects passed as ARRAY OF BYTE and HIGH *)
(* Jaa, Oct.3/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteInt, WriteLn;
FROM SYSTEM IMPORT BYTE;
FROM Conversions IMPORT ByteToInt;

VAR
  s :ARRAY [0..9] OF CHAR;
  x :REAL;
  b :BOOLEAN;
  c :CARDINAL;

  (* ----------------------------------------- *)
  PROCEDURE Show( data :ARRAY OF BYTE );

  VAR
    i :CARDINAL;

  BEGIN
     FOR i := 0 TO HIGH(data) DO
       WriteInt( ByteToInt(data[i]), 4 );
     END;
  END Show;

BEGIN

  s := 'abcdefghij';
  x := 1.0;
  b := TRUE;
  c := 1;

  WriteString( 'using string   ' ); WriteLn; Show( s ); WriteLn;
  WriteString( 'using real     ' ); WriteLn; Show( x ); WriteLn;
  WriteString( 'using boolean  ' ); WriteLn; Show( b ); WriteLn;
  WriteString( 'using cardinal ' ); WriteLn; Show( c ); WriteLn;

END ShowHigh.
