IMPLEMENTATION MODULE Base3;

(* John Andrea, Nov.18/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

(* for 16 bit machines use max_power of 11 *)
(* for 32 bit machines use max_power of 20 *)

FROM StringOperations IMPORT Length;

CONST
  max_power = 20;

VAR
  power3 :ARRAY [0..max_power] OF CARDINAL;
  max_value, i :CARDINAL;

  (* --------------------------------------------------------- *)
  PROCEDURE Max10() :CARDINAL;
  BEGIN
     RETURN max_value;
  END Max10;

  (* --------------------------------------------------------- *)
  PROCEDURE HowMany3( base10 :CARDINAL ) :CARDINAL;
  (* return the number of base 3 digits needed to represent 'x' *)
  VAR n :CARDINAL;
  BEGIN

     IF base10 < 3 THEN
       n := 1;
     ELSE

       (* find the largest position in the power list *)
       n := 0;
       WHILE base10 >= power3[n] DO
         n := n + 1;
       END;

     END;

     RETURN n;

  END HowMany3;

  (* --------------------------------------------------------- *)
  PROCEDURE ToBase3( base10 :CARDINAL; VAR base3 :ARRAY OF CHAR; VAR ok :BOOLEAN );
  VAR
    n, i, j, k, high :CARDINAL;

  BEGIN

    high := HIGH( base3 );

    IF base10 = 0 THEN
      ok := TRUE;
      base3[0] := '0';
      IF high > 0 THEN base3[1] := 0C; END;
    ELSE

      IF base10 > max_value THEN
        ok := FALSE;
        base3[0] := 0C;
      ELSE

        n := HowMany3( base10 ) - 1;

        IF n > HIGH( base3 ) THEN
          ok := FALSE;
          base3[0] := 0C;
        ELSE

          (* for each of the powers of 3, count their numbers *)
          FOR i := 0 TO n DO
             j := n - i;
             k := 0;
             WHILE base10 >= power3[j] DO
               k      := k + 1;
               base10 := base10 - power3[j];
             END;
             IF k = 0 THEN
               base3[i] := '0';
             ELSIF k = 1 THEN
               base3[i] := '1';
             ELSE
               base3[i] := '2';
             END;
          END;

          IF high > n THEN base3[n+1] := 0C; END;
       END;
      END;
    END;

  END ToBase3;

  (* --------------------------------------------------------- *)
  PROCEDURE FromBase3( base3 :ARRAY OF CHAR; VAR base10 :CARDINAL; VAR ok :BOOLEAN );
  VAR
    i, n, d :CARDINAL;
    c       :CHAR;
  BEGIN
     ok     := TRUE;
     base10 := 0;

     n := Length( base3 );

     i := 0;
     WHILE ( i < n ) & ok DO

       c := base3[i];
       IF c = '0' THEN
         d := 0;
       ELSIF c = '1' THEN
         d := 1;
       ELSIF c = '2' THEN
         d := 2;
       ELSE
         ok := FALSE;  d := 0;
       END;

       base10 := base10 * 3 + d;
       i := i + 1;
     END;

  END FromBase3;

BEGIN

  (* precompute the powers of 3 *)
  power3[0] := 1;
  FOR i := 1 TO max_power DO
     power3[i] := 3 * power3[i-1];
  END;

  max_value := power3[max_power];

END Base3.
