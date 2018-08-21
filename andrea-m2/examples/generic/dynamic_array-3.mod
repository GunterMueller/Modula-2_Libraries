MODULE DynamicArray;

(* This program shows how to build and use dynamic memory as if its a matrix *)
(* This specific example shows the use of memory as an
   ARRAY OF ARRAY OF CARDINAL *)
(* This example also shows how to use this technique to perform global operations *)
(* J. Andrea, Mar.31/92 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SYSTEM  IMPORT ADDRESS, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM InOut IMPORT WriteLn, WriteCard;

TYPE
  Dyn = RECORD                   (*this type contains the two essential items *)
          rows, cols :CARDINAL;  (* the size of the matrix *)
          start      :ADDRESS;   (* the memory address of the first element *)
        END;

VAR
  dyn       :Dyn;                      (* simple example here, use a variable *)
  type_size :CARDINAL;                 (* this will be size of items *)
  adr       :POINTER TO CARDINAL;      (* pointer to any single item *)


  (* ------------------------------------------------- *)
  PROCEDURE Offset( r, c :CARDINAL ) :CARDINAL;
  BEGIN
     RETURN type_size * ( ( r - 1 ) * dyn.cols  + c - 1 );
  END Offset;

  (* ------------------------------------------------- *)
  PROCEDURE Make( r, c :CARDINAL );
  (* create a new dynamic matrix *)
  BEGIN
     dyn.rows := r;
     dyn.cols := c;

     ALLOCATE( dyn.start, type_size * r * c );    (* grab the memory *)
                                                  (* specify number of bytes *)
  END Make;

  (* ------------------------------------------------- *)
  PROCEDURE Put( r, c :CARDINAL; value :CARDINAL );
  (* put 'value' into the array at element 'r,c' *)
  BEGIN

    (* make sure the specified element is available *)
    IF ( r >= 1 ) & ( r <= dyn.rows ) & ( c >= 1 ) & ( c <= dyn.cols ) THEN

      adr  := dyn.start + Offset( r, c );            (* calculate the address *)
                                                     (* by offset from #1     *)

      adr^ := value;                                 (* point to that element *)

    END;
         
  END Put;

  (* ------------------------------------------------- *)
  PROCEDURE Get( r, c :CARDINAL ) :CARDINAL;
  (* return the element at position 'r,c' *)

  VAR
    value :CARDINAL;
  BEGIN

    (* make sure the specified element is available *)
    IF ( r >= 1 ) & ( r <= dyn.rows ) & ( c >= 1 ) & ( c <= dyn.cols ) THEN

      adr   := dyn.start + Offset( r, c );            (* calculate to address *)
                                                      (* by offset from #1 *)

      value := adr^;                                  (* point to it *)

    ELSE
      value := 0;
    END;
         
    RETURN value;
  END Get;

  (* ------------------------------------------------- *)
  PROCEDURE Fill;
  VAR
    r, c, x :CARDINAL;
  BEGIN
    x := 1;
    FOR r := 1 TO dyn.rows DO
      FOR c := 1 TO dyn.cols DO
         Put( r, c, x );
         x := x + 1;
      END;
    END;
  END Fill;

  (* ------------------------------------------------- *)
  PROCEDURE Show;
  VAR
    r, c :CARDINAL;
  BEGIN
    WriteLn; WriteLn;
    FOR r := 1 TO dyn.rows DO
      FOR c := 1 TO dyn.cols DO
         WriteCard( Get( r, c ), 5 );
      END;
      WriteLn;
    END;
  END Show;

  (* ------------------------------------------------- *)
  PROCEDURE Scale( value :CARDINAL );
  (* this procedure performs the exact same operation on every element *)
  (* so run through memory sequentially, and quickly *)
  VAR
    i, n :CARDINAL;
  BEGIN
    n := 0;
    FOR i := 1 TO dyn.rows * dyn.cols DO
      adr  := dyn.start + n;
      adr^ := value * adr^;
      n    := n + type_size;
    END;
  END Scale;

BEGIN

  type_size := TSIZE( CARDINAL );     (*get the number of bytes in a CARDINAL *)

  Make( 6, 4 );                       (* create the dynamic matrix *)

  Fill;                               (* load with sequential values *)

  Show;

  Scale( 2 );

  Show;

END DynamicArray.
