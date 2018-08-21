IMPLEMENTATION MODULE Randomly;
(* for 16 bit computers *)
(* $S-, $R-, $T- *)

(* John Andrea, Mar.27/92 *)
(* This code may NOT be sold, it may be freely used and distributed *)

FROM TimeDate IMPORT Time, GetTime;

CONST
   modulus    = 65535;
   multiplier = 25173;
   increment  = 13849;

VAR
  seed :CARDINAL;
  now  :Time;

(* -------------------------------------------------------- *)
PROCEDURE Choose_1_To_N( n :CARDINAL ) :CARDINAL;
BEGIN
  seed := ( ( seed * multiplier ) + increment ) MOD modulus;

  IF seed >= n THEN
    RETURN 1 + ( seed MOD n );
  ELSE
    RETURN 1 + seed;
  END;
END Choose_1_To_N;

(* -------------------------------------------------------- *)
PROCEDURE Choose_0_To_N( n :CARDINAL ) :CARDINAL;
BEGIN
   RETURN Choose_1_To_N( n + 1 ) - 1;
END Choose_0_To_N;

BEGIN
  GetTime( now );   seed := now.millisec;
  IF NOT ODD( seed ) THEN seed := seed + 1; END;
END Randomly.
