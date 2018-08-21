MODULE TestSay;

(* Make a cardinal number into a phrase of words of numbers *)
(* V1.0, J. Andrea, Oct.11/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteString, WriteLn, WriteCard;
FROM SayNumbers IMPORT SayCard;
FROM StringOperations IMPORT Length;

CONST
  n = 100;

VAR
  i, x, max, len :CARDINAL;
  word           :ARRAY [1..200] OF CHAR;

BEGIN

  max := 0;
  len := 0;

  FOR i := 0 TO n DO

     SayCard( i, word );
     x := Length( word );
     IF x > len THEN
       max := i; len := x;
     END;
  END;

  WriteString( 'up to ' ); WriteCard( n, 0 );
  WriteString( ' max length is ' ); WriteCard( len, 0 );
  WriteString( ' at value ' ); WriteCard( max, 0 );

  SayCard( max, word );
  WriteString( ' "' ); WriteString( word ); WriteString( '"' );
  WriteLn;

END TestSay.
