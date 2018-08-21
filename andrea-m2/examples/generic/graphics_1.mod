MODULE Graphics;

(* J. Andrea, 1991 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM VeryScreen IMPORT Box, Home, EraseScreen;
FROM InOut IMPORT WriteString, WriteLn;
FROM GetCharacter IMPORT StartGet, StopGet, Get, GetNoWait;

CONST
  nul = 0C;

VAR
  r1, r2, c1, c2 :CARDINAL;
  c :CHAR;

BEGIN


  WriteString( 'Hit any key to start program' ); WriteLn;
  WriteString( 'Hit any key to end program' ); WriteLn;

  StartGet;

  Get( c );  c := nul;
  WHILE c = nul DO
    Home; EraseScreen;

    r1 := 8;        c1 := 28;
    r2 := r1 + 11;  c2 := c1 + 20;

    WHILE ( c = nul ) & ( r1 < r2 ) DO
      Box( r1, c1, r2, c2 );
      GetNoWait( c );
      r1 := r1 + 1; c1 := c1 + 1;
      r2 := r2 - 1; c2 := c2 - 1;
    END;

  END;

  StopGet;

END Graphics.
