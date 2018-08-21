IMPLEMENTATION MODULE MoreMath;

(* misc. extra math functions *)
(* new functions - J. Andrea, Aug.13/91 *)
(* J. Andrea, May.18/92 -dos version *)
(* J. Andrea, 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM MathLib0 IMPORT sqrt, pow, ln;


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
    
    VAR
       x, i :CARDINAL;
       
    BEGIN

       IF exponent = 0 THEN
         x := 1;
       ELSE
       
         x := base;
         
         FOR i := 2 TO exponent DO
            x := x * base;
         END;
         
       END;

       RETURN x;
    END CardPower;


    (* -------------------------------------------------- *)
    PROCEDURE RealPower( base :REAL; exponent :CARDINAL ) :REAL;
    (* raise a real value to a cardinal power *)
    BEGIN

       RETURN pow( base, FLOAT( exponent ) );

    END RealPower;


    (* -------------------------------------------------- *)
    PROCEDURE IntPower( base, exponent :INTEGER ) :INTEGER;
    (* raise an integer to an integer power *)

    VAR
       x, i :INTEGER;
       
    BEGIN

       IF exponent = 0 THEN
         x := 1;
       ELSE
       
         IF exponent < 0 THEN
           x := 0;
         ELSE
         
           x := base;
         
           FOR i := 2 TO exponent DO
              x := x * base;
           END;
         END;
       END;

       RETURN x;
    END IntPower;


    (* -------------------------------------------------- *)
    PROCEDURE entier( a :REAL ) :INTEGER;
    (* return the nearest integer *)
    BEGIN

      IF ( a < -32768.0 ) OR ( a > 32767.0 ) THEN
	RETURN 0
      ELSE
        RETURN VAL( INTEGER, TRUNC( a ) )
      END;

    END entier;


    (* -------------------------------------------------- *)
    PROCEDURE Float( a :INTEGER ) :REAL;
    BEGIN
      RETURN real( a );
    END Float;


    (* -------------------------------------------------- *)
    PROCEDURE MinReal( a, b :REAL ) :REAL;
    BEGIN

       IF a < b THEN
         RETURN a;
       ELSE
         RETURN b;
       END;
       
    END MinReal;

    (* -------------------------------------------------- *)
    PROCEDURE MaxReal( a, b :REAL ) :REAL;
    BEGIN

       IF a > b THEN
         RETURN a;
       ELSE
         RETURN b;
       END;

    END MaxReal;


    (* -------------------------------------------------- *)
    PROCEDURE MinInt( a, b :INTEGER ) :INTEGER;
    BEGIN

       IF a < b THEN
         RETURN a;
       ELSE
         RETURN b;
       END;

    END MinInt;

    (* -------------------------------------------------- *)
    PROCEDURE MaxInt( a, b :INTEGER ) :INTEGER;
    BEGIN

       IF a > b THEN
         RETURN a;
       ELSE
         RETURN b;
       END;

    END MaxInt;


    (* -------------------------------------------------- *)
    PROCEDURE MinCard( a, b :CARDINAL ) :CARDINAL;
    BEGIN
    
       IF a < b THEN
         RETURN a;
       ELSE
         RETURN b;
       END;

    END MinCard;

    (* -------------------------------------------------- *)
    PROCEDURE MaxCard( a, b :CARDINAL ) :CARDINAL;
    BEGIN

       IF a > b THEN
         RETURN a;
       ELSE
         RETURN b;
       END;

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
