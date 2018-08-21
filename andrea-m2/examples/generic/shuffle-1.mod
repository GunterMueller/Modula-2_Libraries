MODULE TestShuffle;

(* This program show how to use the module Shuffle *)
(* John Andrea, Apr.2/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT ReadCard, WriteCard, WriteLn, WriteString;
FROM Shuffle IMPORT Deck, Create, Next, Reset;

VAR
  d :Deck;
  n, m, i, j :CARDINAL;

BEGIN

  WriteString( 'what size list ? ' ); ReadCard( n );
  WriteString( 'how many runs  ? ' ); ReadCard( m );

  (* set some maximums *)
  IF n > 30 THEN
    n := 30;
    WriteString( 'list size truncated to ' ); WriteCard( n, 0 ); WriteLn;
  END;
  IF m > 20 THEN
    m := 20;
    WriteString( 'number of runs truncated to ' ); WriteCard( m, 0 ); WriteLn;
  END;

  Create( d, 1, n );

  FOR i := 1 TO m DO
     Reset( d );

     FOR j := 1 TO n DO
        WriteCard( Next( d ), 0 ); WriteString( ' ' );
     END;
     WriteLn;

  END;

END TestShuffle.
