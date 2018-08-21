IMPLEMENTATION MODULE Randomly;

(* procedures for getting random numbers *)

(* J. Andrea, Jun.2/92 - add NormalRandom *)
(* J. Andrea, Oct.4/91, add min and max to RandomReal *)
(* reorganized, J. Andrea, Aug.12/91 *)
(* J. Andrea, 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM DateTime IMPORT TimeType, Time, ConvertTime;
FROM MathLib0 IMPORT random, sqrt;

VAR
  seed :INTEGER;


    (* --------------------------------------------- *)
    PROCEDURE RandomReal( min, max :REAL ) :REAL;
    (* return a real number X, such that min >= X < max *)

    BEGIN

       RETURN min + ( max - min ) * random( seed );

    END RandomReal;

    (* --------------------------------------------- *)
    PROCEDURE NormalRandom( mean, sigma :REAL ) :REAL;
    (* from CACM Aug.63, algorithm #200 *)
    CONST
       n = 13;
    VAR
       i   :CARDINAL;
       sum :REAL;
    BEGIN

       sum := 0.0;
       FOR i := 1 TO n DO
         sum := sum + RandomReal( -1.0, +1.0 );
       END;
       RETURN mean + sigma * sum * sqrt( 3.0 / FLOAT( n ) );

    END NormalRandom;

    (* --------------------------------------------- *)
    PROCEDURE Choose_0_To_N( n :CARDINAL) :CARDINAL;
    (* return a value in the range  [0..n] *)

    BEGIN

       RETURN TRUNC( random(seed) * FLOAT(n+1) );

    END Choose_0_To_N;

    (* --------------------------------------------- *)
    PROCEDURE Choose_0_To_N_Minus_1( n :CARDINAL) :CARDINAL;
    (* return a value in the range  [0..n-1] *)

    BEGIN

       IF n < 2 THEN
         RETURN 0;
       ELSE

         RETURN TRUNC( random(seed) * FLOAT(n) );

       END;

    END Choose_0_To_N_Minus_1;


    (* --------------------------------------------- *)
    PROCEDURE Choose_1_To_N( n :CARDINAL) :CARDINAL;
    (* return a value in the range  [1..n] *)

    BEGIN

       IF n < 2 THEN
         RETURN 1;
       ELSE

         RETURN TRUNC( random(seed) * FLOAT(n) ) + 1;

       END;

    END Choose_1_To_N;


    (* --------------------------------------------- *)
    PROCEDURE Choose_1_To_N_Minus_1( n :CARDINAL) :CARDINAL;
    (* return a value in the range  [1..n-1] *)

    BEGIN

       IF n < 2 THEN
         RETURN 1;
       ELSE

         RETURN TRUNC( random(seed) * FLOAT(n-1) ) + 1;

       END;

    END Choose_1_To_N_Minus_1;


    (* --------------------------------------------- *)
    PROCEDURE InputSeed( a_seed :INTEGER );
    (* build a seed from a value from the input value *)

    VAR
      x :REAL;
    BEGIN

      (* make sure the seed is a positive large odd integer *)
      seed := ABS(a_seed);
      IF seed = 0 THEN
         seed := 654321;
      END; (* if *)

      WHILE seed < 1000000 DO
         seed := seed * 11;
      END; (* while *)
      IF NOT ODD(seed) THEN
         seed := seed + 1;
      END; (* if *)

      (* call random a couple of times to get the seed going *)
      x := random( seed );
      x := random( seed );
      x := random( seed );
      
    END InputSeed;

    (* --------------------------------------------- *)
    PROCEDURE MakeSeed;
    (* build a seed from time of day *)

    VAR
      time_string          :ARRAY [0..80] OF CHAR;
      now                  :TimeType;
      seconds , i , factor :CARDINAL;
      seed                 :INTEGER;

    BEGIN

      Time( now );     ConvertTime( now, time_string );

      seconds := 0;
      factor  := 3600;

      FOR i := 12 TO 18 BY 3 DO
         seconds := seconds + factor * 10 * ( ORD( time_string[i]   ) - 48 );
         seconds := seconds + factor *      ( ORD( time_string[i+1] ) - 48 );
         factor  := factor DIV 60;
      END; (* for *)

      (* give it the number of seconds since midnight *)
      seed := INTEGER( seconds);

      InputSeed( seed ); 

    END MakeSeed;

BEGIN

   MakeSeed;

END Randomly.
