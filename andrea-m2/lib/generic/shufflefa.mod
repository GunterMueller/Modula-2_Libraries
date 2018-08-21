IMPLEMENTATION MODULE ShuffleFast;

(* See the definition module for details *)
(* V3.0, John Andrea, Jun.1793 - determine bitmap size at runtime *)
(* V2.0, John Andrea, Apr.3/92 - use bitmaps to further reduce memory size *)
(* This code may be freely used and distributed, it may not be sold *)

FROM SYSTEM IMPORT TSIZE, ADDRESS;
FROM Storage IMPORT ALLOCATE;
FROM Randomly IMPORT Choose_1_To_N;

TYPE
    Deck = POINTER TO RECORD
             length  :CARDINAL;   (* number of numbers *)
             n_left  :CARDINAL;   (* number of numbers not yet picked *)
             start   :ADDRESS;    (* first element in memory *)
             size    :CARDINAL;   (* actual size of the bitmap in bytes *)
             sets    :CARDINAL;   (* number of bitsets *)
           END;

VAR
   adr         :POINTER TO BITSET;
   i, k        :CARDINAL;
   empty, full :BITSET;

   bset_bits, bset_bytes :CARDINAL;

(* ------------------------------------------- *)
PROCEDURE Create( VAR d :Deck; n :CARDINAL );
VAR n_bits, in_last :CARDINAL;
BEGIN

  (* protect users from themselves *)
  IF n = 0 THEN n := 1 END;

  NEW( d );

  d^.length := n;

  n_bits    := n;

  in_last := n_bits MOD bset_bits;

  IF in_last = 0 THEN
    in_last := bset_bits;       (* last one is filed *)
  ELSE
    (* must be a multiple of bits in a bitset *)
    n_bits := n_bits + bset_bits - in_last;
  END;

  d^.sets := n_bits DIV bset_bits;   (* convert to bitsets *)
  d^.size := d^.sets * bset_bytes;   (* convert to bytes *)

  ALLOCATE( d^.start, d^.size );

  Reset( d );
END Create;

(* ------------------------------------------- *)
PROCEDURE Reset( d :Deck );
BEGIN
  d^.n_left := d^.length;
  k := 0;
  FOR i := 1 TO d^.length DO
     adr  := d^.start + k;
     adr^ := full;
     k    := k + bset_bytes;
  END;
END Reset;

(* ------------------------------------------- *)
PROCEDURE Next( d :Deck ) :CARDINAL;
VAR n, count, which_bit :CARDINAL;
BEGIN
 IF d^.n_left = 0 THEN Reset( d ); END;

 n := Choose_1_To_N( d^.n_left );

 (* find the n'th available item in the list                *)
 (* the index of that item will the random number to return *)

 which_bit := bset_bits;   (* force a new value the very first time *)

 i     := 0;
 k     := 0;
 count := 0;
 REPEAT

   which_bit := which_bit + 1;

   IF which_bit >= bset_bits THEN
     which_bit := 0;

     adr := d^.start + k;        (* get a whole new bitset *)
     k   := k + bset_bytes;

     (* but skip any bitset which has already been completeley chosen *)
     WHILE adr^ = empty DO
       i   := i + bset_bits;   (* jump the index for all of them *)
       adr := d^.start + k;
       k   := k + bset_bytes;
     END;

   END;

   i := i + 1;

   IF which_bit IN adr^ THEN count := count + 1; END;

 UNTIL count = n;

 EXCL( adr^, which_bit );      (* make this number un-available *)

 d^.n_left := d^.n_left - 1;   (* now the list is smaller by one *)

 RETURN i;
END Next;

BEGIN
  bset_bytes := TSIZE( BITSET );
  bset_bits  := bset_bytes * 8;

  empty := {};

  full := {};
  FOR i := 0 TO bset_bits - 1 DO
     INCL( full, i );
  END;

END ShuffleFast.
