MODULE TestShuffle;

(* This program show test the module Shuffle *)
(* John Andrea, Apr.6/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteCard, WriteLn, WriteString;
FROM Shuffle IMPORT Deck, Create, Next, Reset;

CONST
  min = 102;
  max = 151;
  try =  50;

VAR
  d       :Deck;
  i, j, k :CARDINAL;
  test    :ARRAY [min..max] OF CARDINAL;
  errors  :CARDINAL;

BEGIN

  Create( d, min, max );

  errors := 0;

  WriteString( 'size of deck = ' ); WriteCard( max - min + 1, 0 );
  WriteString( ' being tested ' ); WriteCard( try, 0 );
  WriteString( ' times' ); WriteLn;

  FOR i := 1 TO try DO
     Reset( d );

     FOR j := min TO max DO
        test[j] := 0;
     END;

     FOR j := min TO max DO
        k := Next( d );
        IF k < min THEN
          errors := errors + 1;
        ELSE
          IF k > max THEN
            errors := errors + 1;
          ELSE
            INC( test[k] );
          END;
        END;
     END;

     (* each number should be used once, and only once *)
     FOR j := min TO max DO
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
