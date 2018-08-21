MODULE AllQueens;
(* From "Algorithms and Data Structures", N. Wirth, Prentice Hall, 1986 *)
(* find all the solutions to the Eight Queens problem *)

FROM InOut IMPORT WriteInt, WriteLn;

VAR
  i :INTEGER;
  a :ARRAY [1..8] OF BOOLEAN;
  b :ARRAY [2..16] OF BOOLEAN;
  c :ARRAY [-7..7] OF BOOLEAN;
  x :ARRAY [1..8] OF INTEGER;

  PROCEDURE Print;
  BEGIN
    FOR i := 1 TO 8 DO
       WriteInt( x[i], 4 );
    END;
    WriteLn;
  END Print;

  PROCEDURE Try( i :INTEGER );
  VAR
    j :INTEGER;
  BEGIN

    FOR j := 1 TO 8 DO

       IF a[j] & b[i+j] & c[i-j] THEN
         x[i] := j;
         a[j] := FALSE; b[i+j] := FALSE; c[i-j] := FALSE;

         IF i < 8 THEN
           Try( i+1 );
         ELSE
           Print;
         END;

         a[j] := TRUE; b[i+j] := TRUE; c[i-j] := TRUE;
       END;

    END;

  END Try;

BEGIN

  FOR i := 1 TO 8 DO
     a[i] := TRUE;
  END;

  FOR i := 2 TO 16 DO
     b[i] := TRUE;
  END;

  FOR i := -7 TO 7 DO
     c[i] := TRUE;
  END;

  Try( 1 );

END AllQueens.
