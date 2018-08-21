MODULE TestRandomly;

(* Test the random number functions and show how they're used *)
(* J. Andrea, Aug.12/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM Randomly IMPORT Choose_0_To_N, Choose_1_To_N, 
                     Choose_0_To_N_Minus_1, Choose_1_To_N_Minus_1;
FROM InOut IMPORT ReadCard, WriteCard, WriteString, WriteLn;

VAR
  n, m, i, x, min, max :CARDINAL;

BEGIN

WriteString( 'input a number to test (555 to exit) ? ' );
ReadCard( n );
WHILE n # 555 DO

   WriteString( 'how many times for the test ? ' );
   ReadCard( m );

   WriteString( 'testing Choose_0_To_N for N=' ); WriteCard( n, 0 );
   WriteString( ', ' ); WriteCard( m, 0 ); WriteString( ' times ');
   WriteLn;

   min := n + 1;
   max := 0;
   FOR i := 1 TO m DO
      x := Choose_0_To_N( n );
      IF x < min THEN min := x; END;
      IF x > max THEN max := x; END;
      (* WriteCard( x, 0 ); WriteLn; *)
   END;

   WriteString( 'found min = ' ); WriteCard( min, 0 ); WriteLn;
   WriteString( 'found max = ' ); WriteCard( max, 0 ); WriteLn;

   WriteString( 'testing Choose_1_To_N for N=' ); WriteCard( n, 0 );
   WriteString( ', ' ); WriteCard( m, 0 ); WriteString( ' times ');
   WriteLn;

   min := n + 1;
   max := 0;
   FOR i := 1 TO m DO
      x := Choose_1_To_N( n );
      IF x < min THEN min := x; END;
      IF x > max THEN max := x; END;
   END;

   WriteString( 'found min = ' ); WriteCard( min, 0 ); WriteLn;
   WriteString( 'found max = ' ); WriteCard( max, 0 ); WriteLn;

   WriteString( 'testing Choose_0_To_N-1 for N=' ); WriteCard( n, 0 );
   WriteString( ', ' ); WriteCard( m, 0 ); WriteString( ' times ');
   WriteLn;

   min := n + 1;
   max := 0;
   FOR i := 1 TO m DO
      x := Choose_0_To_N_Minus_1( n );
      IF x < min THEN min := x; END;
      IF x > max THEN max := x; END;
   END;

   WriteString( 'found min = ' ); WriteCard( min, 0 ); WriteLn;
   WriteString( 'found max = ' ); WriteCard( max, 0 ); WriteLn;

   WriteString( 'input a number to test (555 to exit) ? ' );
   ReadCard( n );
END;

END TestRandomly.
