MODULE TestRoman;

(* Test the RomanNumerals procedures *)
(* J. Andrea, 1992 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn, Read, ReadCard;
FROM RomanNumerals IMPORT FromRoman, ToRoman;

VAR
   a, b, i, x :CARDINAL;
   string     :ARRAY [0..132] OF CHAR;
   display    :BOOLEAN;
   errors     :CARDINAL;

BEGIN

   WriteString( 'what starting decimal value ? ' ); ReadCard( a );
   WriteString( 'what ending   decimal value ? ' ); ReadCard( b );

   WriteString( 'do you want display on (y/n) ? ' ); Read( string[0] );
   display := CAP( string[0] ) = 'Y';

   errors := 0;

   FOR i := a TO b DO
      IF display THEN WriteCard( i, 1 ); WriteString( ' roman>' ); END;

      ToRoman( i, string );

      IF display THEN WriteString( string ); WriteString( '<' ); END;

      FromRoman( string, x );

      IF i = x THEN
        IF display THEN WriteString( ' ok' ); END;
      ELSE
        errors := errors + 1;
        IF display THEN WriteString( ' ----not same '); WriteCard( x, 1 ); END;
      END;

      IF display THEN WriteLn; END;
   END;

   WriteLn;
   IF errors = 0 THEN
     WriteString( 'no conversion problems found' );
   ELSE
     WriteCard( errors, 0 ); WriteString( ' conversion errors !' );
   END;
   WriteLn;

END TestRoman.
