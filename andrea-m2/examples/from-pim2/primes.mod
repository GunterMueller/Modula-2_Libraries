MODULE Primes;

(* Example program from Programming In Modula-2, N. Wirth, pg 41 *)

FROM InOut IMPORT WriteLn, WriteCard;

CONST
    N = 500; M = 23; (*  M ~ sqrt(N)  *)
    LL = 10;  (* No. of primes placed on a line *)

VAR
    i, k, x              :CARDINAL;
    inc, lim, square, L  :CARDINAL;
    prime                :BOOLEAN;
    P, V                 :ARRAY [0..M] OF CARDINAL;

BEGIN (* Primes *)

L := 0;
x := 1;
inc := 4;
lim := 1;
square := 9;

FOR i := 3 TO N DO  (*  find next prime no. p[i] *)

   REPEAT
     x := x + inc;
     inc := 6 - inc;

     IF square <= x THEN 
        lim := lim + 1;
        V[lim] := square;
        square := P[lim + 1] * P[lim + 1]
     END; (* if *)

     k := 2;
     prime := TRUE;

     WHILE prime & ( k < lim ) DO
       k := k + 1;
       IF V[k] < x THEN 
          V[k] := V[k] + 2 * P[k]
       END; (* if *)
       prime := x # V[k]
     END; (* while *)

   UNTIL prime;

   IF i <= M THEN
      P[i] := x
   END; (* if *)

   WriteCard( x, 6 );
   L := L + 1;

   IF L = LL THEN
      WriteLn;
      L := 0
   END; (* if *)

END; (* for i *)

END Primes.
