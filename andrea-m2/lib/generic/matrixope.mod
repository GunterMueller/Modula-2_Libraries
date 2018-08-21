IMPLEMENTATION MODULE MatrixOperations;

(* matrix operations using dynamic memory, see the corresponding definition *)

(* V1.2, J. Andrea, Jun.22/93 -add Duplicate *)
(* V1.1, J. Andrea, Jun.11/92 - recursive determinant for any size matrix *)
(* V1.0, J. Andrea, Mar.16/92 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SYSTEM  IMPORT ADDRESS, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

TYPE
  Matrix = POINTER TO RECORD
             rows, cols :CARDINAL;   (* dimemsions of the matrix *)
             start      :ADDRESS;    (* memory address of the first item *)
             size       :CARDINAL;   (* actual size in bytes *)
           END;

  DataType  = REAL;
  DataPoint = POINTER TO DataType;

VAR
  type_size :CARDINAL;             (* #bytes in a single item *)
  delta     :DataType;             (* small number for some operations *)

(* -------------------------------------------------------- *)
PROCEDURE InRange( a :Matrix; row, col :CARDINAL ) :BOOLEAN;
(* is the specified row/col item in this matrix ? *)
BEGIN
  RETURN ( row >= 1 ) & ( row <= a^.rows ) & ( col >= 1 ) & ( col <= a^.cols );
END InRange;

(* -------------------------------------------------------- *)
PROCEDURE Offset( a :Matrix; row, col :CARDINAL ) :CARDINAL;
(* calculate the memory offset to the row/col item from the first item *)
BEGIN
  RETURN type_size * ( ( row - 1 ) * a^.cols + ( col - 1 ) );
END Offset;

(* -------------------------------------------------------- *)
PROCEDURE Build( VAR a :Matrix; n_rows, n_columns :CARDINAL );
BEGIN
  NEW( a );

  (* get something *)
  IF n_rows    = 0 THEN n_rows    := 1 END;
  IF n_columns = 0 THEN n_columns := 1 END;

  a^.rows := n_rows;
  a^.cols := n_columns;

  a^.size := n_rows * n_columns * type_size;
  ALLOCATE( a^.start, a^.size );
END Build;

(* -------------------------------------------------------- *)
PROCEDURE Destroy( VAR a :Matrix );
BEGIN
  DEALLOCATE( a^.start, a^.size );
  DISPOSE( a );
END Destroy;

(* -------------------------------------------------------- *)
PROCEDURE Put( a :Matrix; row, col :CARDINAL; x :DataType );
VAR
  adra :DataPoint;
BEGIN
  IF InRange( a, row, col ) THEN
    adra  := a^.start + Offset( a, row, col );
    adra^ := x;
  END;
END Put;

(* -------------------------------------------------------- *)
PROCEDURE Get( a :Matrix; row, col :CARDINAL ) :DataType;
VAR
  adra   :DataPoint;
  result :DataType;
BEGIN
  IF InRange( a, row, col ) THEN
    adra   := a^.start + Offset( a, row, col );
    result := adra^;
  ELSE
    result := 0.0;
  END;
  RETURN result;
END Get;

(* -------------------------------------------------------- *)
PROCEDURE Size( a :Matrix; VAR n_rows, n_cols :CARDINAL );
BEGIN
  n_rows := a^.rows;
  n_cols := a^.cols;
END Size;

(* -------------------------------------------------------- *)
PROCEDURE Min( a :Matrix ) :DataType;
VAR
  result  :DataType;
  adra    :DataPoint;
  i, k, n :CARDINAL;
BEGIN

  n := a^.rows * a^.cols;

  adra   := a^.start;
  result := adra^;

  k := type_size;
  FOR i := 2 TO n DO
     adra  := a^.start + k;
     k     := k + type_size;
     IF adra^ < result THEN result := adra^ END;
  END;

  RETURN result;
END Min;

(* -------------------------------------------------------- *)
PROCEDURE Max( a :Matrix ) :DataType;
VAR
  result  :DataType;
  adra    :DataPoint;
  i, k, n :CARDINAL;
BEGIN

  n := a^.rows * a^.cols;

  adra   := a^.start;
  result := adra^;

  k := type_size;
  FOR i := 2 TO n DO
     adra  := a^.start + k;
     k     := k + type_size;
     IF adra^ > result THEN result := adra^ END;
  END;

  RETURN result;
END Max;

(* -------------------------------------------------------- *)
PROCEDURE Compare( a, b :Matrix ) :BOOLEAN;
VAR
  ok         :BOOLEAN;
  adra, adrb :DataPoint;
  i, k, n    :CARDINAL;
BEGIN

  IF ( a^.rows = b^.rows ) & ( a^.cols = b^.cols ) THEN

    ok := TRUE;

    n := a^.rows * a^.cols;

    i := 1;
    k := 0;
    WHILE ok & ( i <= n ) DO
       adra := a^.start + k;
       adrb := b^.start + k;
       k    := k + type_size;
       ok   := ABS( adra^ - adrb^ ) <= delta;
       i    := i + 1;
    END;

  ELSE
    ok := FALSE;
  END;

  RETURN ok;

END Compare;

(* -------------------------------------------------------- *)
PROCEDURE Assign( a :Matrix; x :DataType );
VAR
  adra    :DataPoint;
  i, k, n :CARDINAL;
BEGIN
  n := a^.rows * a^.cols;
  k := 0;
  FOR i := 1 TO n DO
     adra  := a^.start + k;
     adra^ := x;
     k     := k + type_size;
  END;
END Assign;

(* -------------------------------------------------------- *)
PROCEDURE Ident( a :Matrix );
VAR
  adra       :DataPoint;
  i, j, k, n :CARDINAL;
BEGIN

  n   := a^.rows * a^.cols;
  j   := 1;                  (* item number of first '1' *)
  k   := 0;
  FOR i := 1 TO n DO
     adra := a^.start + k;

     IF i = j THEN
       adra^ := 1.0;
       j     := j + a^.cols + 1;   (* next item number which will be a '1' *)
     ELSE
       adra^ := 0.0;
     END;

     k := k + type_size;
  END;

END Ident;

(* -------------------------------------------------------- *)
PROCEDURE Copy( a, b :Matrix );
VAR
  adra, adrb :DataPoint;
  i, k, n    :CARDINAL;
BEGIN
  IF ( a^.rows = b^.rows ) & ( a^.cols = b^.cols ) THEN

    n := a^.rows * a^.cols;

    k := 0;
    FOR i := 1 TO n DO
       adra  := a^.start + k;
       adrb  := b^.start + k;
       adrb^ := adra^;
       k     := k + type_size;
    END;

  END;
END Copy;

(* -------------------------------------------------------- *)
PROCEDURE Duplicate( a :Matrix; VAR b :Matrix );
BEGIN
  Build( b, a^.rows, a^.cols );
  Copy( a, b );
END Duplicate;

(* -------------------------------------------------------- *)
PROCEDURE Scale( a :Matrix; x :DataType );
VAR
  adra    :DataPoint;
  i, k, n :CARDINAL;
BEGIN

  n := a^.rows * a^.cols;

  k := 0;
  FOR i := 1 TO n DO
     adra  := a^.start + k;
     adra^ := adra^ * x;
     k     := k + type_size;
  END;

END Scale;

(* -------------------------------------------------------- *)
PROCEDURE Add( a, b :Matrix; VAR c :Matrix; VAR ok :BOOLEAN );
VAR
  adra, adrb, adrc :DataPoint;
  i, k, n          :CARDINAL;
BEGIN

  IF ( a^.rows # b^.rows ) OR ( a^.cols # b^.cols ) THEN
    ok := FALSE;
  ELSE

    ok := TRUE;
    Build( c, a^.rows, a^.cols );

    n := a^.rows * a^.cols;

    k := 0;
    FOR i := 1 TO n DO
       adra  := a^.start + k;
       adrb  := b^.start + k;
       adrc  := c^.start + k;
       adrc^ := adra^ + adrb^;
       k     := k + type_size;
    END;

  END;

END Add;

(* -------------------------------------------------------- *)
PROCEDURE Multiply( a, b :Matrix; VAR c :Matrix; VAR ok :BOOLEAN );
VAR
  sum              :DataType;
  adra, adrb, adrc :DataPoint;
  i, j, k          :CARDINAL;
BEGIN

  IF a^.cols # b^.rows THEN
    ok := FALSE;
  ELSE
    ok := TRUE;

    Build( c, a^.rows, b^.cols );

    FOR i := 1 TO a^.rows DO
      FOR j := 1 TO b^.cols DO

        sum := 0.0;

        FOR k := 1 TO a^.cols DO
           adra := a^.start + Offset( a, i, k );
           adrb := b^.start + Offset( b, k, j );
           sum  := sum + adra^ * adrb^;
        END;

        adrc  := c^.start + Offset( c, i, j );
        adrc^ := sum;

      END;
    END;

  END;

END Multiply;

(* -------------------------------------------------------- *)
PROCEDURE Transpose( a :Matrix; VAR b :Matrix );
VAR
  adra, adrb :DataPoint;
  i, j       :CARDINAL;
BEGIN

  Build( b, a^.cols, a^.rows );  (* flip dimensions *)

  FOR i := 1 TO a^.rows DO
    FOR j := 1 TO a^.cols DO
       adra  := a^.start + Offset( a, i, j );
       adrb  := b^.start + Offset( b, j, i );
       adrb^ := adra^;
    END;
  END;

END Transpose;

(* -------------------------------------------------------- *)
PROCEDURE Invert( b :Matrix; VAR a :Matrix; VAR ok :BOOLEAN ); (* note a <-> b *)
VAR
  r, p             :DataType;
  adra, adrb, adrc :DataPoint;
  i, j, k, n       :CARDINAL;
BEGIN

  ok := TRUE;

  IF ( b^.rows # b^.cols ) OR ( b^.rows < 2 ) THEN
    ok := FALSE;       (* not square, forget it *)
  ELSE

    Copy( b, a );

    n := a^.rows;

    k := 1;
    WHILE ok & ( k <= n ) DO

      adra := a^.start + Offset( a, k, 1 );         (* pivot := mat(k,1) *)
      p    := adra^;

      IF p <= 1.E-10 THEN
        ok := FALSE;  (* zero pivot point *)
      ELSE

        FOR j := 1 TO n - 1 DO
           adra  := a^.start + Offset( a, k, j+1 );
           adrb  := a^.start + Offset( a, k, j );
           adrb^ := adra^ / p;                    (* mat(k,j) := mat(k,j+1)/p *)
        END;

        adra  := a^.start + Offset( a, k, n );
        adra^ := 1.0 / p;                          (* mat(k,n) := 1/p *)

        FOR i := 1 TO n DO
           IF i # k THEN

             adra := a^.start + Offset( a, i, 1 );
             r    := adra^;                         (* r := mat(i,1) *)

             FOR j := 1 TO n - 1 DO
                adra  := a^.start + Offset( a, k, j );
                adrb  := a^.start + Offset( a, i, j+1 );
                adrc  := a^.start + Offset( a, i, j );
                adrc^ := adrb^ - r * adra^; 
                                     (* mat(i,j) := mat(i,j+1) - r * mat(k,j) *)
             END;

             adra  := a^.start + Offset( a, k, n );
             adrb  := a^.start + Offset( a, i, n );
             adrb^ := - r * adra^;              (* mat(i,n) := - r * mat(k,n) *)
           END;
        END;

      END;

      k := k + 1;
    END;

    IF NOT ok THEN
      Destroy( b );
    END;

  END;

END Invert;


(* -------------------------------------------------------- *)
PROCEDURE Determinant( a :Matrix; VAR d :DataType; VAR ok :BOOLEAN ); 
VAR
  n, i, j, p, q, k :CARDINAL;
  b             :Matrix;
  f, x, y, z    :DataType;
  adra, adrb    :DataPoint;
BEGIN
 
   ok := TRUE;
   n  := a^.rows;

   IF n # a^.cols THEN
     ok := FALSE;
     d  := 0.0;
   ELSE
   
     IF n = 1 THEN

       adra := a^.start;
       d    := adra^;

     ELSIF n = 2 THEN

       adra := a^.start;
       adrb := a^.start + Offset( a, 2, 2 );
       d    := adra^ * adrb^;
       adra := a^.start + Offset( a, 2, 1 );
       adrb := a^.start + Offset( a, 1, 2 );
       d    := d - adra^ * adrb^;

     ELSE
     
       z := 0.0;
       f := 1.0;
              
       (* create sub matricies, and found their determinants *)
       
       Build( b, n-1, n-1 );

       (* run across all the columns in the input matrix *)
       FOR k := 1 TO n DO
       
          adra := a^.start + Offset( a, 1, k );
          x    := adra^;
       
          (* skip any zero element *)
          IF x # 0.0 THEN
          
            q := 1;
            FOR j := 1 TO n DO
            
               (* skip the column that matches the top selected column *)
               IF k # j THEN
               
                 (* pick up all the rows in this whole column *)
                 FOR i := 2 TO n DO
                    p := i - 1;
                     
                    adra  := a^.start + Offset( a, i, j );
                    adrb  := b^.start + Offset( b, p, q );
                    adrb^ := adra^;

                 END;
                 q := q + 1;
                 
             END;
           END;
           
           Determinant( b, y, ok );
           
           z := z + f * x * y;  (* add in the result from that submatrix *)
           
         END;
         f := - f; (* +-+-+-+-... is how the summing goes *)
       END;
       
       Destroy( b );
       
       d := z; (* return the final result *)
     END;

   END;
END Determinant;


BEGIN

  delta     := 1.E-6;
  type_size := TSIZE( DataType );

END MatrixOperations.
