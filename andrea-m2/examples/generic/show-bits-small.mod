MODULE ShowBitsSmall;

(* This is a small program to show how to use the BITSET conversion
   to test for bits inside a number. *)
(* J. Andrea, Jun.14/93 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteString, WriteLn, WriteCard;

CONST
  max_value = 5;
  max_bit   = 3;

VAR
  i, j :CARDINAL;
  x    :BITSET;
BEGIN

   FOR i := 0 TO max_value DO
      WriteString( 'value = ' ); WriteCard( i, 0 ); WriteLn;

      x := BITSET( i );

      FOR j := 0 TO max_bit DO
         WriteString( '     bit ' ); WriteCard( j, 0 );
         WriteString( ': ' );

         IF j IN x THEN
           WriteString( 'on' );
         ELSE
           WriteString( 'off' );
         END;

         WriteLn;
      END;

   END;

END ShowBitsSmall.
