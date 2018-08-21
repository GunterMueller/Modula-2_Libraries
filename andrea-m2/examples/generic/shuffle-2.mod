MODULE TestShuffle;

(* This program show test the module Shuffle *)
(* John Andrea, Apr.2/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteCard, WriteLn, WriteString;
FROM Shuffle IMPORT Deck, Create, Next, Reset;

CONST
  size  = 100;
  tries = 300;

VAR
  d      :Deck;
  i, j   :CARDINAL;
  test   :ARRAY [1..size] OF CARDINAL;
  errors :CARDINAL;

BEGIN

  Create( d, 1, size );

  errors := 0;

  WriteString( 'size of deck = ' ); WriteCard( size, 0 );
  WriteString( ' being tested ' ); WriteCard( tries, 0 );
  WriteString( ' times' ); WriteLn;

  FOR i := 1 TO tries DO
     Reset( d );

     FOR j := 1 TO size DO
        test[j] := 0;
     END;

     FOR j := 1 TO size DO
        INC( test[ Next( d ) ] );
     END;

     (* each number should be used once, and only once *)
     FOR j := 1 TO size DO
        IF test[j] # 1 THEN errors := errors + 1 END;
     END;

  END;

  IF errors = 0 THEN
    WriteString( 'no errors found' );
  ELSE
    WriteCard( errors, 0 ); WriteString( ' errors found' );
  END;
  WriteLn;

END TestShuffle.
