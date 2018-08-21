MODULE Graphics;

(* J. Andrea, 1991 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM VeryScreen IMPORT Line, Home, EraseScreen;
FROM InOut IMPORT WriteString, WriteLn;
FROM GetCharacter IMPORT StartGet, StopGet, Get, GetNoWait;
FROM Randomly IMPORT Choose_1_To_N;

CONST
  nul = 0C;

VAR
  r1, r2, c1, c2 :CARDINAL;
  c :CHAR;

BEGIN


  WriteString( 'Hit any key to start program' ); WriteLn;
  WriteString( 'Hit SPACE to clear the screen, any other key to end program');
  WriteLn;

  StartGet;

  Get( c );  c := ' ';

  WHILE c = ' ' DO
    c := nul;

    Home; EraseScreen;

    WHILE c = nul DO

      r1 := Choose_1_To_N( 23 );
      c1 := Choose_1_To_N( 79 );

      IF Choose_1_To_N( 100 ) < 51 THEN
        r2 := r1;
        c2 := Choose_1_To_N( 79 - c1 );
      ELSE
        r2 := Choose_1_To_N( 23 - r1 );
        c2 := c1;
      END;

      Line( r1, c1, r2, c2 );

      GetNoWait( c );
    END;

  END;

  StopGet;

END Graphics.
