IMPLEMENTATION MODULE Shuffle;

(* See the definition module for details *)

(* V1.1, John Andrea, Jun.22/93 -add Copy and Duplicate *)
(* V1.0, John Andrea, Apr.5/92 *)
(* This code may be freely used and distributed, it may not be sold *)

IMPORT LargeSets;
FROM Storage IMPORT ALLOCATE;
FROM Randomly IMPORT Choose_1_To_N;

TYPE
    Deck = POINTER TO RECORD
             min, max :CARDINAL;
             size     :CARDINAL;
             n_left   :CARDINAL;
             set      :LargeSets.LargeSet;
           END;

(* ------------------------------------------- *)
PROCEDURE Create( VAR d :Deck; min, max :CARDINAL );
VAR
  i :CARDINAL;
BEGIN
  NEW( d );

  IF min > max THEN
    i   := min;
    min := max;
    max := i;
  END;

  d^.min  := min;
  d^.max  := max;
  d^.size := max - min + 1;

  LargeSets.Build( d^.set, min, max );

  Reset( d );
END Create;

(* ------------------------------------------- *)
PROCEDURE Reset( d :Deck );
BEGIN
  d^.n_left := d^.size;
  LargeSets.Fill( d^.set );
END Reset;

(* ------------------------------------------- *)
PROCEDURE Next( d :Deck ) :CARDINAL;
VAR
  i, n, count :CARDINAL;
BEGIN
 IF d^.n_left = 0 THEN Reset( d ); END;

 n := Choose_1_To_N( d^.n_left );

 (* find the n'th non-zero number in the list *)
 i     := d^.min;
 count := 0;
 WHILE count # n DO
   IF LargeSets.In( d^.set, i ) THEN count := count + 1 END;
   i := i + 1;
 END;
 i := i - 1;

 LargeSets.Excl( d^.set, i );   (* make this number un-available *)
 d^.n_left := d^.n_left - 1;    (* now the list is smaller by one *)

 RETURN i;
END Next;

(* ------------------------------------------- *)
PROCEDURE Copy( d, e :Deck );
BEGIN
  IF ( d^.min = e^.min ) & ( d^.max = e^.max ) THEN
    LargeSets.Copy( d^.set, e^.set );
    e^.n_left := d^.n_left;
  END;
END Copy;

(* ------------------------------------------- *)
PROCEDURE Duplicate( d :Deck; VAR e :Deck );
BEGIN
  Create( e, d^.min, d^.max );
  Copy( d, e );
END Duplicate;

BEGIN
END Shuffle.
