IMPLEMENTATION MODULE BitsetFunctions;

(* logical operations on BITSETS *)

(* V2.0, J. Andrea, Jun.17/93 - determine word size at runtime *)
(* V1.0, J. Andrea, 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SYSTEM IMPORT TSIZE;

VAR
  empty, full    :BITSET;
  i, bits_in_set :CARDINAL;

   (* --------------------------------------------------------- *)
   PROCEDURE Or( a, b :BITSET ) :BITSET;
   BEGIN
     RETURN ( a + b );                (* set union *)
   END Or;

   (* --------------------------------------------------------- *)
   PROCEDURE Nor( a, b :BITSET ) :BITSET;
   BEGIN
     RETURN Not(Or(a,b));
   END Nor;

   (* --------------------------------------------------------- *)
   PROCEDURE Xor( a, b :BITSET ) :BITSET;
   BEGIN
     RETURN ( a / b );          (* symmetric set difference *)
   END Xor;

   (* --------------------------------------------------------- *)
   PROCEDURE And( a, b :BITSET ) :BITSET;
   BEGIN
     RETURN ( a * b );        (* set intersection *)
   END And;

   (* --------------------------------------------------------- *)
   PROCEDURE Nand( a, b :BITSET ) :BITSET;
   BEGIN
     RETURN Not(And(a,b));
   END Nand;

   (* --------------------------------------------------------- *)
   PROCEDURE Xand( a, b :BITSET ) :BITSET;
   BEGIN
     RETURN (a - b);          (* set difference *)
   END Xand;

   (* --------------------------------------------------------- *)
   PROCEDURE Not( a :BITSET ) :BITSET;
   BEGIN
     RETURN Xor( a, full );           (* a Xor with <all bits> *)
   END Not;

   (* --------------------------------------------------------- *)
   PROCEDURE ShiftLeft( a :BITSET; n :CARDINAL ) :BITSET;
   (* perform a circular shift left, n times *)

   VAR
     i, last :CARDINAL;
     save    :BOOLEAN;

   BEGIN

      last := bits_in_set - 1;

      FOR i := 1 TO n DO

         save := last IN a;             (* is the 31st bit set ? *)
         EXCL( a, last );               (* remove it in any case *)
         a := BITSET( CARDINAL(a) * 2 );(* shift = multiply by 2 *)
         EXCL( a, 0 );                  (* remove the 0th bit    *)
         IF save THEN INCL( a, 0 ) END; (* shift the last bit around *)

      END;
      RETURN a;
   END ShiftLeft;

   (* --------------------------------------------------------- *)
   PROCEDURE ShiftRight( a :BITSET; n :CARDINAL ) :BITSET;
   (* perform a circular shift right, n times *)

   VAR
     i, last :CARDINAL;
     save    :BOOLEAN;

   BEGIN
      last := bits_in_set - 1;

      FOR i := 1 TO n DO

         save :=  0 IN a;                  (* is the  0th bit set ? *)
         EXCL( a, 0 );                     (* remove it in any case *)
         a := BITSET( CARDINAL(a) DIV 2 ); (* shift = divide by 2   *)
         EXCL( a, last );                  (* remove the last bit   *)
         IF save THEN INCL( a, last ) END; (* shift the  0th around *)

      END;
      RETURN a;
   END ShiftRight;

BEGIN

  bits_in_set := TSIZE( BITSET ) * 8;

  empty := {};

  full := empty;
  FOR i := 1 TO bits_in_set DO
     INCL( full, i-1 );
  END;

END BitsetFunctions.
