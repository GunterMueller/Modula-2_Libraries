IMPLEMENTATION MODULE PackMatrix;

(* Matrix of small cardinal values packed into bit items,
   see the corresponding definition *)
(* V1.1, J. Andrea, Jun.22/93 -add Duplictate, fix bug in Offset *)
(* V1.0, J. Andrea, Jun.15/93 *)
(* This code may be freely used and distributed, it may not be sold. *)

IMPORT PackArray;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

TYPE
  DataType = CARDINAL;

  Matrix = POINTER TO RECORD
             rows, cols :CARDINAL;
             min,  max  :DataType;
             data       :PackArray.Array;
           END;

(* -------------------------------------------------------- *)
PROCEDURE InRange( a :Matrix; row, col :CARDINAL ) :BOOLEAN;
(* Is the specified row/col item in this matrix ? *)
BEGIN
  RETURN ( row >= 1 ) & ( row <= a^.rows ) & ( col >= 1 ) & ( col <= a^.cols );
END InRange;

(* -------------------------------------------------------- *)
PROCEDURE Offset( a :Matrix; row, col :CARDINAL ) :CARDINAL;
(* Calculate the memory offset to the row/col item from the first item *)
(* This is not zero based, so no "-1" *)
BEGIN
  RETURN ( row - 1 ) * a^.cols + col;
END Offset;

(* -------------------------------------------------------- *)
PROCEDURE Build( VAR a :Matrix; minimum, maximum, rows, cols :CARDINAL );
VAR
  temp :DataType;
BEGIN
  NEW( a );

  IF rows = 0 THEN rows := 1 END;
  IF cols = 0 THEN cols := 1 END;

  a^.rows := rows;
  a^.cols := cols;

  IF minimum > maximum THEN
    temp    := maximum;
    maximum := minimum;
    minimum := temp;
  END;

  a^.min := minimum;
  a^.max := maximum;

  PackArray.Build( a^.data, minimum, maximum, rows * cols );

END Build;

(* -------------------------------------------------------- *)
PROCEDURE Destroy( VAR a :Matrix );
BEGIN
  PackArray.Destroy( a^.data );
  DISPOSE( a );
END Destroy;

(* -------------------------------------------------------- *)
PROCEDURE Put( a :Matrix; row, col :CARDINAL; x :DataType );
BEGIN
  IF InRange( a, row, col ) THEN
    PackArray.Put( a^.data, Offset( a, row, col ), x );
  END;
END Put;

(* -------------------------------------------------------- *)
PROCEDURE Get( a :Matrix; row, col :CARDINAL ) :DataType;
BEGIN
  IF NOT InRange( a, row, col ) THEN
    RETURN 0;
  ELSE
    RETURN PackArray.Get( a^.data, Offset( a, row, col ) );
  END;
END Get;

(* -------------------------------------------------------- *)
PROCEDURE Size( a :Matrix; VAR bits, bytes, rows, cols :CARDINAL );
VAR
  length :CARDINAL;
BEGIN
  rows := a^.rows;
  cols := a^.cols;
  PackArray.Size( a^.data, bits, bytes, length );
END Size;

(* -------------------------------------------------------- *)
PROCEDURE Min( a :Matrix ) :DataType;
BEGIN
  RETURN PackArray.Min( a^.data );
END Min;

(* -------------------------------------------------------- *)
PROCEDURE Max( a :Matrix ) :DataType;
BEGIN
  RETURN PackArray.Max( a^.data );
END Max;

(* -------------------------------------------------------- *)
PROCEDURE Compare( a, b :Matrix ) :BOOLEAN;
BEGIN
  IF ( a^.rows # b^.rows ) OR ( a^.cols # b^.cols ) THEN
    RETURN FALSE;
  ELSE
    RETURN PackArray.Compare( a^.data, b^.data );
  END;
END Compare;

(* -------------------------------------------------------- *)
PROCEDURE Assign( a :Matrix; x :DataType );
BEGIN
  PackArray.Assign( a^.data, x );
END Assign;

(* -------------------------------------------------------- *)
PROCEDURE Copy( a, b :Matrix );
BEGIN
  IF ( a^.min  = b^.min )  & ( a^.max  = b^.max  ) &
     ( a^.rows = b^.rows ) & ( a^.cols = b^.cols ) THEN
    PackArray.Copy( a^.data, b^.data );
  END;
END Copy;

(* -------------------------------------------------------- *)
PROCEDURE Duplicate( a :Matrix; VAR b :Matrix );
BEGIN
  Build( b, a^.min, a^.max, a^.rows, a^.cols );
  Copy( a, b );
END Duplicate;

BEGIN
END PackMatrix.
