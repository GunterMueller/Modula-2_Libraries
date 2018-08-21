MODULE SomeBits;

(* test out some bitset operations *)
(* J. Andrea, 1984 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn;

VAR
  a_card, i  :CARDINAL;
  some_bits  :BITSET;

BEGIN

WriteLn; WriteString('Show off some BITSET properties '); WriteLn; WriteLn;

(* subtract 8 the long way *)

a_card := 25;
some_bits := BITSET(a_card);       (* get the bit representation of 25 *)
EXCL(some_bits,3);                 (* remove 2^3 from the bits         *)
a_card := CARDINAL(some_bits);     (* make it a number again           *)
WriteString('25-8='); WriteCard(a_card,5); WriteLn;


WriteLn;

(* now show the largest integer *)

some_bits := {};

(* set all the bits to ON *)
FOR i := 0 TO 31 DO
   INCL(some_bits,i);
END; (* for *)

a_card := CARDINAL(some_bits);
WriteString('MaxCard='); WriteCard(a_card,20); WriteLn;

END SomeBits.
