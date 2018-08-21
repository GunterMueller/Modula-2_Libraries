MODULE DynamicArray;

(* This program shows how to build and use dynamic memory as if its an array *)
(* This specific example shows the use of memory as an ARRAY OF CARDINAL *)
(* J. Andrea, Mar.16/92 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SYSTEM  IMPORT ADDRESS, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM InOut IMPORT WriteLn, WriteCard;

TYPE
  Dyn = RECORD                   (*this type contains the two essential items *)
          size  :CARDINAL;       (* the size of the array, and *)
          start :ADDRESS;        (* the memory address of the first element *)
        END;

VAR
  dyn       :Dyn;                      (* simple example here, use a variable *)
  type_size :CARDINAL;                 (* this will be size of items *)
  adr       :POINTER TO CARDINAL;      (* pointer to any single item *)
  i, n      :CARDINAL;                 (* typical counters *)


  (* ------------------------------------------------- *)
  PROCEDURE Make( length :CARDINAL );
  (* create a new dynamic array with the number of elements in 'length' *)
  BEGIN
     dyn.size := length;

     ALLOCATE( dyn.start, type_size * length );    (* grab the memory *)
                                                   (* specify number of bytes *)
  END Make;

  (* ------------------------------------------------- *)
  PROCEDURE Put( pos :CARDINAL; value :CARDINAL );
  (* put 'value' into the array at element 'pos' *)
  BEGIN

    (* make sure the specified element is available *)
    IF ( pos >= 1 ) & ( pos <= dyn.size ) THEN

      adr  := dyn.start + ( pos - 1 ) * type_size;   (* calculate the address *)
                                                     (* by offset from #1     *)

      adr^ := value;                                 (* point to that element *)

    END;
         
  END Put;

  (* ------------------------------------------------- *)
  PROCEDURE Get( pos :CARDINAL ) :CARDINAL;
  (* return the element at position 'pos' *)

  VAR
    value :CARDINAL;
  BEGIN

    (* make sure the specified element is available *)
    IF ( pos >= 1 ) & ( pos <= dyn.size ) THEN

      adr   := dyn.start + ( pos - 1 ) * type_size;   (* calculate to address *)
                                                      (* by offset from #1 *)

      value := adr^;                                  (* point to it *)

    ELSE
      value := 0;
    END;
         
    RETURN value;
  END Get;

BEGIN

  type_size := TSIZE( CARDINAL );     (*get the number of bytes in a CARDINAL *)

  n := 10;                            (* choose 10 elements *)

  Make( n );                          (* create the dynamic array *)

  FOR i := 1 TO n DO                  (* load the values 10,9,8,7... *)
    Put( i, n+1-i );
  END;

  FOR i := 1 TO n DO                     (* read the items and print them *)
    WriteCard( Get( i ), 0 ); WriteLn;
  END;

END DynamicArray.
