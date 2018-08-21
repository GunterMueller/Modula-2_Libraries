MODULE Test;

(* Test the module PackArray *)
(* J. Andrea, Jun.15/93 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn;
FROM PackArray IMPORT Array, Build, Get, Put, Size;

CONST
  min =  0;
  max = 17;
  range = max - min;
  n   = range + 1;

VAR
  a :Array;
  bits, bytes, elem :CARDINAL;

  i, x :CARDINAL;
  list :ARRAY [1..n] OF CARDINAL;
  
BEGIN

  WriteString( 'creating array with' ); WriteLn;
  WriteString( '   min/max = ' ); WriteCard( min, 0 ); 
  WriteString( '/' ); WriteCard( max, 0 ); WriteLn;
  WriteString( '   elements = ' ); WriteCard( n, 0 ); WriteLn;

  Build( a, min, max, n );

  Size( a, bits, bytes, i );
  WriteString( 'takes ' ); WriteCard( bits, 0 ); WriteString( ' bits for data' );
  WriteLn;
  WriteString( 'whole array is ' ); WriteCard( bytes, 0 ); WriteString( ' bytes' );
  WriteLn;

  (* fill the array *)
  FOR i := 1 TO n DO
     list[i] := min + i - 1;
     Put( a, i, list[i] );
  END;

  WriteString( 'put     got' ); WriteLn;
  FOR i := 1 TO n DO
     x := Get( a, i );
     WriteCard( list[i], 0 ); WriteString( '      ' );
     WriteCard( x, 0 );
     IF x # list[i] THEN WriteString( ' <---- error' ) END;
     WriteLn;
  END;

END Test.
