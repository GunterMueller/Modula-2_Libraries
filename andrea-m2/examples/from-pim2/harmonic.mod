MODULE Harmonic;

(* Example program from Programming In Modula-2, N. Wirth, pg.26 *)

FROM InOut IMPORT Done, ReadCard, WriteString, WriteLn, WriteReal;

VAR
   i, n          :CARDINAL;
   x, d, s1, s2  :REAL; 

BEGIN (* Harmonic *)

WriteString( " n = " );
ReadCard( n );

WHILE Done DO
  s1 := 0.0; d := 0.0; i := 0;

  REPEAT
    d := d + 1.0;
    i := i + 1;
    s1 := s1 + 1.0 / d
  UNTIL i >= n;

  WriteReal( s1, 16 );
  s2 := 0.0;

  REPEAT
     s2 := s2 + 1.0 / d;
     d := d - 1.0;
     i := i - 1;
  UNTIL i = 0;

  WriteReal( s2, 16 );
  WriteLn;
  WriteString( " n = " );
  ReadCard( n );

END; (* while *)

WriteLn;

END Harmonic.
