IMPLEMENTATION MODULE MoreMath;

(* misc. extra math functions *)
(* new functions - J. Andrea, Aug.13/91 *)
(* J. Andrea, 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM MathematicsProcedures IMPORT OTS$POWJJ, OTS$POWLULU, OTS$POWRLU,
                                  MTH$AMIN1, MTH$AMAX1,
                                  MTH$JMIN0, MTH$JMAX0,
                                  MTH$UMIN,  MTH$UMAX;
FROM MathLib0 IMPORT sqrt, ln;


    (* -------------------------------------------- *)
    PROCEDURE IModulus( a, b :INTEGER ) :INTEGER;
    (* return the integer modulus of two numbers
       handles a negative 'a' value (unlike the languages built in MOD) *)
    BEGIN

       IF a < 0 THEN
         RETURN b - (ABS(a) MOD b);
       ELSE
         RETURN a MOD b;
       END; (* if *)

    END IModulus;


    (* -------------------------------------------------- *)
    PROCEDURE CardPower( base, exponent :CARDINAL ) :CARDINAL;
    (* raise a cardinal to a cardinal power *)
    BEGIN

       RETURN OTS$POWLULU( base, exponent );

    END CardPower;


    (* -------------------------------------------------- *)
    PROCEDURE RealPower( base :REAL; exponent :CARDINAL ) :REAL;
    (* raise a real value to a cardinal power *)
    BEGIN

       RETURN OTS$POWRLU( base, exponent );

    END RealPower;


    (* -------------------------------------------------- *)
    PROCEDURE IntPower( base, exponent :INTEGER ) :INTEGER;
    (* raise an integer to an integer power *)
    BEGIN

      RETURN OTS$POWJJ( base, exponent );

    END IntPower;


    (* -------------------------------------------------- *)
    PROCEDURE entier( a :REAL ) :INTEGER;
    (* return the nearest integer *)
    BEGIN

      IF ( a < -2147483648.0 ) OR ( a > 2147483647.0 ) THEN
	RETURN 0
      ELSE
        RETURN INTEGER( TRUNC( a ) )
      END (* if *);

    END entier;


    (* -------------------------------------------------- *)
    PROCEDURE MinReal( a, b :REAL ) :REAL;
    BEGIN

       RETURN MTH$AMIN1( a, b );

    END MinReal;

    (* -------------------------------------------------- *)
    PROCEDURE MaxReal( a, b :REAL ) :REAL;
    BEGIN

       RETURN MTH$AMAX1( a, b );

    END MaxReal;


    (* -------------------------------------------------- *)
    PROCEDURE MinInt( a, b :INTEGER ) :INTEGER;
    BEGIN

       RETURN MTH$JMIN0( a, b );

    END MinInt;

    (* -------------------------------------------------- *)
    PROCEDURE MaxInt( a, b :INTEGER ) :INTEGER;
    BEGIN

       RETURN MTH$JMAX0( a, b );

    END MaxInt;


    (* -------------------------------------------------- *)
    PROCEDURE MinCard( a, b :CARDINAL ) :CARDINAL;
    BEGIN

       RETURN MTH$UMIN( a, b );

    END MinCard;

    (* -------------------------------------------------- *)
    PROCEDURE MaxCard( a, b :CARDINAL ) :CARDINAL;
    BEGIN

       RETURN MTH$UMAX( a, b );

    END MaxCard;


    (* -------------------------------------------------- *)
    PROCEDURE SizeCard( a :CARDINAL ) :CARDINAL;
    BEGIN

       IF a < 10 THEN
         RETURN 1;
       ELSE
         RETURN 1 + SizeCard( a DIV 10 );
       END;

    END SizeCard;

    (* -------------------------------------------------- *)
    PROCEDURE SizeInt( a :INTEGER ) :CARDINAL;
    BEGIN

       IF ABS( a ) < 10 THEN
         IF a < 0 THEN
           RETURN 2;
         ELSE
           RETURN 1;
         END;
       ELSE
         RETURN 1 + SizeInt( a DIV 10 );
       END;

    END SizeInt;


    (* -------------------------------------------------- *)
    PROCEDURE CalcPi() :REAL;

    CONST
      one  = 1.0;
      two  = 2.0;
      four = 4.0;
      six  = 6.0;
      
    VAR
      root_two, last, out, y, a, d, e :REAL;
      i                               :CARDINAL;

    BEGIN

      root_two := sqrt( two );

      last := 0.0;

      y := root_two - one;
      a := six - four * root_two;

      out := one / a;

      i := 0;
      WHILE out # last DO
        last := out;

        d := one - RealPower( y, 4 );
        d := sqrt( sqrt( d ) );

        y := ( one - d ) / ( one + d );

        i := i + 1;

        e := RealPower(two, 2*i+1) * y * ( one + y + y * y );
        a := RealPower(one+y, 4) * a - e;

        out := one / a;

      END;
      
      RETURN out;

    END CalcPi;


    (* -------------------------------------------- *)
    PROCEDURE Log10( x :REAL ) :REAL;
    BEGIN
      IF x <= 0.0 THEN
        RETURN 0.0;
      ELSE
        RETURN ln( x ) / ln( 10.0 );
      END;
    END Log10;

    (* -------------------------------------------- *)
    PROCEDURE Log2( x :REAL ) :REAL;
    BEGIN
      IF x <= 0.0 THEN
        RETURN 0.0;
      ELSE
        RETURN ln( x ) / ln( 2.0 );
      END;
    END Log2;

BEGIN
END MoreMath.
