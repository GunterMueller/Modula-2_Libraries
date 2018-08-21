MODULE Fractions;

(* Example program from  Programming In Modula-2, N. Wirth, pg.40 *)

FROM InOut IMPORT Write, WriteLn, WriteString, WriteCard;

CONST
     Base = 10;
     N = 32;

VAR
     rem, i, j, m :CARDINAL;
     d            :ARRAY [1..N] OF CARDINAL;
     x            :ARRAY [0..N] OF CARDINAL;

BEGIN (* Fractions *)

FOR i := 2 TO N DO

  FOR j := 0 TO i - 1 DO
     x[j] := 0;
  END; (* for *)

  m := 0;
  rem := 1;

  REPEAT
    m := m + 1;
    x[rem] := m;
    rem := Base * rem;
    d[m] := rem DIV i;
    rem := rem MOD i;
  UNTIL x[rem] # 0;

  WriteCard( i, 6 );
  WriteString( ' 0.' );
  FOR j := 1 TO x[rem] - 1  DO
     Write( CHR( d[j] + ORD("0")))
  END; (* for *)

  Write( "'" );
  FOR j := x[rem] TO m DO
    Write( CHR( d[j] + ORD("0")))
  END; (* for *)

  WriteLn;

END; (* for *)

END Fractions.
