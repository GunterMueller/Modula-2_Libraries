IMPLEMENTATION MODULE Randomly;

(* procedures for getting random numbers *)

(* J. Andrea, May.18/92 -dos version *)
(* J. Andrea, Oct.4/91, add min and max to RandomReal *)
(* reorganized, J. Andrea, Aug.12/91 *)
(* J. Andrea, 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)

IMPORT Random;
FROM MathLib0 IMPORT sqrt;

    (* --------------------------------------------- *)
    PROCEDURE RandomReal( min, max :REAL ) :REAL;
    (* return a real number X, such that min >= X < max *)
    BEGIN

       RETURN min + ( max - min ) * Random.RandomReal();

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

       RETURN Random.RandomCard( n + 1 );

    END Choose_0_To_N;

    (* --------------------------------------------- *)
    PROCEDURE Choose_0_To_N_Minus_1( n :CARDINAL) :CARDINAL;
    (* return a value in the range  [0..n-1] *)
    BEGIN

       IF n < 2 THEN
         RETURN 0;
       ELSE

         RETURN Random.RandomCard( n );

       END;

    END Choose_0_To_N_Minus_1;


    (* --------------------------------------------- *)
    PROCEDURE Choose_1_To_N( n :CARDINAL) :CARDINAL;
    (* return a value in the range  [1..n] *)
    BEGIN

       IF n < 2 THEN
         RETURN 1;
       ELSE

         RETURN 1 + Random.RandomCard( n );

       END;

    END Choose_1_To_N;


    (* --------------------------------------------- *)
    PROCEDURE Choose_1_To_N_Minus_1( n :CARDINAL) :CARDINAL;
    (* return a value in the range  [1..n-1] *)
    BEGIN

       IF n < 2 THEN
         RETURN 1;
       ELSE

         RETURN 1 + Random.RandomCard( n - 1 );

       END;

    END Choose_1_To_N_Minus_1;


    (* --------------------------------------------- *)
    PROCEDURE InputSeed( a_seed :INTEGER );
    (* build a seed from a value from the input value *)
    
    VAR x :REAL;
    
    BEGIN

      Random.RandomInit( CARDINAL( a_seed ) );
      
      (* call random a couple of times to get the seed going *)
      x := Random.RandomReal( );
      x := Random.RandomReal();
      x := Random.RandomReal();
      
    END InputSeed;

    (* --------------------------------------------- *)
    PROCEDURE MakeSeed;
    BEGIN
        Random.Randomize;
    END MakeSeed;

BEGIN
END Randomly.
