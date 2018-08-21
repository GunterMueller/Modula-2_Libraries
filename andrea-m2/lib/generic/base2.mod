IMPLEMENTATION MODULE Base2;

(* for 16 bit machines use max_power = 15 *)
(* for 32 bit machines use max_power = 31 *)
(* John Andrea, Nov.18/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM StringOperations IMPORT Length;

CONST
  max_power = 31;

VAR
  power2 :ARRAY [0..max_power] OF CARDINAL;
  max_value, i :CARDINAL;

  (* --------------------------------------------------------- *)
  PROCEDURE Max10() :CARDINAL;
  BEGIN
     RETURN max_value;
  END Max10;

  (* --------------------------------------------------------- *)
  PROCEDURE HowMany2( base10 :CARDINAL ) :CARDINAL;
  (* return the number of base 2 digits needed to represent 'base10' *)
  VAR n :CARDINAL;
  BEGIN

     IF base10 < 2 THEN
       n := 1;
     ELSE

       (* find the largest position in the power list *)
       n := 0;
       WHILE base10 >= power2[n] DO
         n := n + 1;
       END;

     END;

     RETURN n;

  END HowMany2;

  (* --------------------------------------------------------- *)
  PROCEDURE ToBase2( base10 :CARDINAL; VAR base2 :ARRAY OF CHAR; VAR ok :BOOLEAN );
  VAR
    n, i, j :CARDINAL;
    high    :CARDINAL;

  BEGIN

    high := HIGH( base2 );

    IF base10 = 0 THEN
      ok := TRUE;

      base2[0] := '0';
      IF high > 0 THEN base2[1] := 0C; END;

    ELSE

      IF base10 > max_value THEN
        ok := FALSE;
        base2[0] := 0C;
      ELSE

        n := HowMany2( base10 ) - 1;
        IF n > high THEN
          ok := FALSE;
          base2[0] := 0C;
        ELSE

          (* for each of the powers of 2, count their numbers *)
          FOR i := 0 TO n DO
             j := n - i;
             IF base10 >= power2[j] THEN
               base10   := base10 - power2[j];
               base2[i] := '1';
             ELSE
               base2[i] := '0';
             END;
          END;

          IF high > n THEN base2[n+1] := 0C; END;
       END;
      END;
    END;

  END ToBase2;

  (* --------------------------------------------------------- *)
  PROCEDURE FromBase2( base2 :ARRAY OF CHAR; VAR base10 :CARDINAL; VAR ok :BOOLEAN );
  VAR
    i, n, d :CARDINAL;
    c       :CHAR;
  BEGIN
     ok     := TRUE;
     base10 := 0;

     n := Length( base2 );

     i := 0;
     WHILE ( i < n ) & ok DO

       c := base2[i];
       IF c = '0' THEN
         d := 0;
       ELSIF c = '1' THEN
         d := 1;
       ELSE
         ok := FALSE;  d := 0;
       END;

       base10 := base10 * 2 + d;
       i := i + 1;
     END;

  END FromBase2;

BEGIN

  (* precompute the powers of 2 *)
  power2[0] := 1;
  FOR i := 1 TO max_power DO
     power2[i] := 2 * power2[i-1];
  END;

  max_value := power2[max_power];

END Base2.
