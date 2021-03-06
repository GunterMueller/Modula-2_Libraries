IMPLEMENTATION MODULE RealArrays;

(* Operations on a dynamic array of reals *)

(* V1.1, J. Andrea, Jun.22/93 -add Duplicate *)
(* V1.0, J. Andrea, May.18/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM SYSTEM  IMPORT ADDRESS, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

TYPE
  RealArray = POINTER TO RECORD
             min, max :CARDINAL;      (* dimemsions of the matrix *)
             start    :ADDRESS;       (* memory address of the first item *)
             size     :CARDINAL;      (* actual size in bytes *)
           END;


  ArrayType = REAL;

VAR
  type_size  :CARDINAL;             (* #bytes in a single item *)

(* -------------------------------------------------------------- *)
PROCEDURE Build( VAR a :RealArray; min_index, max_index :CARDINAL );
VAR
  i :CARDINAL;
BEGIN
  NEW( a );

  IF min_index > max_index THEN
    i         := min_index;
    min_index := max_index;
    max_index := i;
  END;

  a^.min := min_index;
  a^.max := max_index;

  a^.size := ( max_index - min_index + 1 ) * type_size;

  ALLOCATE( a^.start, a^.size );
END Build;

(* -------------------------------------------------------- *)
PROCEDURE Destroy( VAR a :RealArray );
BEGIN
  DEALLOCATE( a^.start, a^.size );
  DISPOSE( a );
END Destroy;

(* -------------------------------------------------------- *)
PROCEDURE Put( a :RealArray; index :CARDINAL; value :ArrayType );
VAR
  adra :POINTER TO ArrayType;
BEGIN
  IF ( a^.min <= index ) & ( index <= a^.max ) THEN
    adra  := a^.start + ( index - a^.min ) * type_size;
    adra^ := value;
  END;
END Put;

(* -------------------------------------------------------- *)
PROCEDURE Get( a :RealArray; index :CARDINAL ) :ArrayType;
VAR
  adra :POINTER TO ArrayType;
BEGIN
  IF ( a^.min <= index ) & ( index <= a^.max ) THEN
    adra := a^.start + ( index - a^.min ) * type_size;
    RETURN adra^;
  ELSE
    RETURN 0.0;
  END;
END Get;

(* -------------------------------------------------------- *)
PROCEDURE Size( a :RealArray; VAR min_index, max_index :CARDINAL );
BEGIN
  min_index := a^.min;
  max_index := a^.max;
END Size;

(* -------------------------------------------------------- *)
PROCEDURE Min( a :RealArray ) :ArrayType;
VAR
  adra   :POINTER TO ArrayType;
  i, k   :CARDINAL;
  result :ArrayType;
BEGIN

  k := 0;

  adra   := a^.start;
  result := adra^;

  FOR i := a^.min+1 TO a^.max DO
     k     := k + type_size;
     adra  := a^.start + k;
     IF adra^ < result THEN result := adra^ END;
  END;

  RETURN result;
END Min;

(* -------------------------------------------------------- *)
PROCEDURE Max( a :RealArray ) :ArrayType;
VAR
  adra   :POINTER TO ArrayType;
  i, k   :CARDINAL;
  result :ArrayType;
BEGIN

  k := 0;

  adra   := a^.start;
  result := adra^;

  FOR i := a^.min+1 TO a^.max DO
     k     := k + type_size;
     adra  := a^.start + k;
     IF adra^ > result THEN result := adra^ END;
  END;

  RETURN result;
END Max;

(* -------------------------------------------------------- *)
PROCEDURE Compare( a, b :RealArray ) :BOOLEAN;
VAR
  adra, adrb :POINTER TO ArrayType;
  i, k :CARDINAL;
  ok   :BOOLEAN;
BEGIN
  IF ( a^.min = b^.min ) & ( a^.max = b^.max ) THEN

    ok := TRUE;

    k := 0;
    i := a^.min;
    WHILE ok & ( i <= a^.max ) DO
       adra  := a^.start + k;
       adrb  := b^.start + k;
       ok    := adra^ = adrb^;
       k     := k + type_size;
       i     := i + 1;
    END;

  ELSE
    ok := FALSE;
  END;

  RETURN ok;
END Compare;

(* -------------------------------------------------------- *)
PROCEDURE Assign( a :RealArray; x :ArrayType );
VAR
  adra :POINTER TO ArrayType;
  i, k :CARDINAL;
BEGIN
  k := 0;
  FOR i := a^.min TO a^.max DO
     adra  := a^.start + k;
     adra^ := x;
     k     := k + type_size;
  END;
END Assign;

(* -------------------------------------------------------- *)
PROCEDURE Copy( a, b :RealArray );
VAR
  adra, adrb :POINTER TO ArrayType;
  i, k :CARDINAL;
BEGIN
  IF ( a^.min = b^.min ) & ( a^.max = b^.max ) THEN

    k := 0;
    FOR i := a^.min TO a^.max DO
       adra  := a^.start + k;
       adrb  := b^.start + k;
       adrb^ := adra^;
       k     := k + type_size;
    END;

  END;
END Copy;

(* -------------------------------------------------------- *)
PROCEDURE Duplicate( a :RealArray; VAR b :RealArray );
VAR
  adra :POINTER TO ArrayType;
BEGIN
  Build( b, a^.min, a^.max );
  Copy( a, b );
END Duplicate;

(* -------------------------------------------------------- *)
PROCEDURE Add( a :RealArray; x :ArrayType );
VAR
  adra :POINTER TO ArrayType;
  i, k :CARDINAL;
BEGIN
  k := 0;
  FOR i := a^.min TO a^.max DO
     adra  := a^.start + k;
     adra^ := adra^ + x;
     k     := k + type_size;
  END;
END Add;

(* -------------------------------------------------------- *)
PROCEDURE Multiply(  a :RealArray; x :ArrayType );
VAR
  adra :POINTER TO ArrayType;
  i, k :CARDINAL;
BEGIN
  k := 0;
  FOR i := a^.min TO a^.max DO
     adra  := a^.start + k;
     adra^ := adra^ * x;
     k     := k + type_size;
  END;
END Multiply;

BEGIN
  type_size := TSIZE( ArrayType );
END RealArrays.
