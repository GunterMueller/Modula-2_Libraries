IMPLEMENTATION MODULE MoreMath;

(* John Andrea, Mar.27/92 *)
(* This code may NOT be sold, it may be freely used and distributed *)

    PROCEDURE MinInt( a, b :INTEGER ) :INTEGER;
    BEGIN

       IF a <= b THEN
         RETURN a;
       ELSE
         RETURN b;
       END;

    END MinInt;

    PROCEDURE MaxInt( a, b :INTEGER ) :INTEGER;
    BEGIN

       IF a >= b THEN
         RETURN a;
       ELSE
         RETURN b;
       END;

    END MaxInt;

    PROCEDURE MinCard( a, b :CARDINAL ) :CARDINAL;
    BEGIN

       IF a <= b THEN
         RETURN a;
       ELSE
         RETURN b;
       END;

    END MinCard;

    PROCEDURE MaxCard( a, b :CARDINAL ) :CARDINAL;
    BEGIN

       IF a >= b THEN
         RETURN a;
       ELSE
         RETURN b;
       END;

    END MaxCard;

    PROCEDURE SizeCard( a :CARDINAL ) :CARDINAL;
    BEGIN

       IF a < 10 THEN
         RETURN 1;
       ELSE
         RETURN 1 + SizeCard( a DIV 10 );
       END;

    END SizeCard;

    PROCEDURE SizeInt( a :INTEGER ) :CARDINAL;
    BEGIN

       IF ABS( a ) < 10 THEN
         IF a < 0 THEN
           RETURN 2;   (* account for the minus sign *)
         ELSE
           RETURN 1;
         END;
       ELSE
         RETURN 1 + SizeCard( a DIV 10 );
       END;

    END SizeInt;

BEGIN (* MoreMath *)
END MoreMath.
