IMPLEMENTATION MODULE LargeSets;

(* V2.1, John Andrea, Jun.22/93 -add Copy and Duplicate *)
(* V2.0, John Andrea, Jun.17/93 -determine bitset size ar runtime *)
(* V1.0, John Andrea, Apr.5/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM SYSTEM IMPORT TSIZE, ADDRESS;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

TYPE
   LargeSet = POINTER TO RECORD
                  start    :ADDRESS;    (* memory address of the first item *)
                  min, max :CARDINAL;   (* size of the set *)
                  sets     :CARDINAL;   (* number of bitsets used *)
                  in_last  :CARDINAL;   (* number of bits in the last set *)
               END;
   DataType  = BITSET;
   DataPoint = POINTER TO DataType;

VAR
  bset_bits, bset_bytes :CARDINAL;
  empty, full           :BITSET;
  i                     :CARDINAL;

(* -------------------------------------------------------- *)
PROCEDURE Offset( a :LargeSet; x :CARDINAL; VAR set, bit :CARDINAL );
(* calculate the memory offset to the item from the first item *)
VAR
  count_bit :CARDINAL;
BEGIN
  count_bit := x - a^.min;
  set       := count_bit DIV bset_bits;
  (* and how far into this set is the dot that we need *)
  bit       := count_bit - set * bset_bits;
END Offset;

(* -------------------------------------------------------- *)
PROCEDURE Build( VAR a :LargeSet; min, max :CARDINAL );
VAR
  i, n :CARDINAL;
BEGIN
  NEW( a );

  IF min > max THEN
   i   := min;
   min := max;
   max := i;
  END;

  a^.min := min;   a^.max := max;

  n := max - min + 1;

  a^.in_last := n MOD bset_bits;

  IF a^.in_last = 0 THEN
    a^.in_last := bset_bits;   (* already a multiple of bset_bits *)
  ELSE
    n := n + bset_bits - a^.in_last;    (* bump up to a multiple of bset_bits *)
  END;

  a^.sets := n DIV bset_bits;

  ALLOCATE( a^.start, a^.sets * bset_bytes );       (* grab bytes from memory *)
  Empty( a );
END Build;

(* -------------------------------------------------------- *)
PROCEDURE Destroy( VAR a :LargeSet );
BEGIN
  DEALLOCATE( a^.start, a^.sets * bset_bytes );
  DISPOSE( a );
END Destroy;

(* -------------------------------------------------------- *)
PROCEDURE Empty( a :LargeSet );
VAR
  adra :DataPoint;
  i, k :CARDINAL;
BEGIN
  k := 0;
  FOR i := 1 TO a^.sets DO
     adra  := a^.start + k;
     adra^ := empty;
     k     := k + bset_bytes;
  END;
END Empty;

(* -------------------------------------------------------- *)
PROCEDURE Fill( a :LargeSet );
VAR
  adra :DataPoint;
  i, k :CARDINAL;
BEGIN
  k := 0;
  FOR i := 1 TO a^.sets DO
     adra  := a^.start + k;
     adra^ := full;
     k     := k + bset_bytes;
  END;
END Fill;

(* -------------------------------------------------------- *)
PROCEDURE Incl( a :LargeSet; x :CARDINAL );
VAR
  adra                 :DataPoint;
  which_set, which_bit :CARDINAL;
BEGIN
  IF ( a^.min <= x ) & ( x <= a^.max ) THEN

    Offset( a, x, which_set, which_bit );
    adra := a^.start + which_set * bset_bytes;
    INCL( adra^, which_bit );

  END;
END Incl;

(* -------------------------------------------------------- *)
PROCEDURE Excl( a :LargeSet; x :CARDINAL );
VAR
  adra                 :DataPoint;
  which_set, which_bit :CARDINAL;
BEGIN
  IF ( a^.min <= x ) & ( x <= a^.max ) THEN

    Offset( a, x, which_set, which_bit );
    adra := a^.start + which_set * bset_bytes;
    EXCL( adra^, which_bit );

  END;
END Excl;

(* -------------------------------------------------------- *)
PROCEDURE In( a :LargeSet; x :CARDINAL ) :BOOLEAN;
VAR
  adra                 :DataPoint;
  which_set, which_bit :CARDINAL;
  result               :BOOLEAN;
BEGIN
  IF ( a^.min <= x ) & ( x <= a^.max ) THEN

    Offset( a, x, which_set, which_bit );
    adra   := a^.start + which_set * bset_bytes;
    result := which_bit IN adra^;

  ELSE
    result := FALSE;
  END;
  RETURN result;
END In;

(* -------------------------------------------------------- *)
PROCEDURE Not( a :LargeSet );
VAR
  adra :DataPoint;
  i, k :CARDINAL;
BEGIN
  k := 0;
  FOR i := 1 TO a^.sets DO
     adra  := a^.start + k;
     adra^ := adra^ / full;    (* Xor with <all bits> *)
     k     := k + bset_bytes;
  END;
END Not;

(* -------------------------------------------------------- *)
PROCEDURE Equal( a, b :LargeSet ) :BOOLEAN;
VAR
  adra, adrb :DataPoint;
  i, k       :CARDINAL;
  ok         :BOOLEAN;
BEGIN

   IF ( a^.min = b^.min ) & ( a^.max = b^.max ) THEN
     ok := TRUE;

     k  := 0;
     i  := 1;
     WHILE ( i <= a^.sets - 1 ) & ok DO
       adra := a^.start + k;
       adrb := b^.start + k;
       ok   := adra^ = adrb^;
       i    := i + 1;
       k    := k + bset_bytes;
     END;

     (* only the used portion of the last set should be compared *)
     IF ok THEN
       adra := a^.start + k;
       adrb := b^.start + k;

       i := 0;
       WHILE ( i < a^.in_last ) & ok DO
         ok := ( i IN adra^ ) = ( i IN adrb^ );
         i  := i + 1;
       END;

     END;

   ELSE
     ok := FALSE;
   END;

   RETURN ok;
END Equal;

(* -------------------------------------------------------- *)
PROCEDURE Copy( a, b :LargeSet );
VAR
  adra, adrb :DataPoint;
  i, k :CARDINAL;
BEGIN
  IF ( a^.min = b^.min ) & ( a^.max = b^.max ) THEN

    k := 0;
    FOR i := 1 TO a^.sets DO
       adra  := a^.start + k;
       adrb  := b^.start + k;
       adrb^ := adra^;
       k     := k + bset_bytes;
    END;

  END;
END Copy;

(* -------------------------------------------------------- *)
PROCEDURE Duplicate( a :LargeSet; VAR b :LargeSet );
BEGIN
   Build( b, a^.min, a^.max );
   Copy( a, b );
END Duplicate;

BEGIN
   bset_bytes := TSIZE( DataType );
   bset_bits  := bset_bytes * 8;

   empty := {};

   full  := {};
   FOR i := 0 TO bset_bits - 1 DO
     INCL( full, i );
   END;

END LargeSets.
