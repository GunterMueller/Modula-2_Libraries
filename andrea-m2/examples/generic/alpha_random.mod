MODULE AlphaRandom;

(* Randomize the alphabet, and show how to use the module Shuffle *)
(* John Andrea, Apr.2/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteLn, WriteString;
FROM Shuffle IMPORT Deck, Create, Next, Reset;

VAR
  d                 :Deck;
  i, j, min, max, n :CARDINAL;

BEGIN

  min := ORD( 'a' );
  max := ORD( 'z' );

  n   := max - min + 1;

  Create( d, min, max );

  FOR i := 1 TO 10 DO

     WriteString( 'random alphabet = ' );
     FOR j := 1 TO n DO
        WriteString( CHR( Next( d ) ) );
     END;
     WriteLn;

     Reset( d );
  END;

END AlphaRandom.
