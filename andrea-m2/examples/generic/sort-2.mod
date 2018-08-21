MODULE TestSort;

(* Test the sorting module *)
(* J. Andrea, 1992 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut    IMPORT WriteReal, WriteCard, WriteString, WriteLn;
FROM Sorting  IMPORT CardBSort, RealBSort,
                     CardQSort, CardQSortIndex, RealQSortIndex;
FROM Randomly IMPORT RandomReal, Choose_0_To_N, Choose_1_To_N_Minus_1;

CONST
  max_n = 20;

VAR
  n     :CARDINAL;
  a     :ARRAY [1..max_n] OF CARDINAL;
  x     :ARRAY [1..max_n] OF REAL;
  index :ARRAY [1..max_n] OF CARDINAL;
  i     :CARDINAL;
  ok    :BOOLEAN;

     (* ------------------------------------------- *)
     PROCEDURE RandomCard( VAR n :CARDINAL );
     BEGIN
        n := Choose_1_To_N_Minus_1( max_n ) + 1; (* minimum of 2 *)
        FOR i := 1 TO n DO
           a[i] := Choose_0_To_N( 99 );
        END;
     END RandomCard;

     (* ------------------------------------------- *)
     PROCEDURE RandomR( VAR n :CARDINAL );
     BEGIN
        n := Choose_1_To_N_Minus_1( max_n ) + 1; (* minimum of 2 *)
        FOR i := 1 TO n DO
           x[i] := RandomReal( 1., 100. );
        END;
     END RandomR;

     (* ------------------------------------------- *)
     PROCEDURE ShowCardArray;
     BEGIN
        FOR i := 1 TO n DO
          WriteCard( a[i], 3 );
        END;
        WriteLn;
     END ShowCardArray;

     (* ------------------------------------------- *)
     PROCEDURE ShowCardIndex;
     BEGIN
        FOR i := 1 TO n DO
          WriteCard( a[index[i]+1], 3 );
        END;
        WriteLn;
     END ShowCardIndex;

     (* ------------------------------------------- *)
     PROCEDURE CheckCardArray;
     BEGIN
        ok := TRUE;
        FOR i := 2 TO n DO
          IF a[i] < a[i-1] THEN
            ok := FALSE;
            WriteString( 'bad pair '); WriteCard( a[i-1], 2 );
            WriteString( ' ' );        WriteCard( a[i], 2 ); WriteLn;
          END;
        END;

        IF ok THEN
          WriteString( 'all ok' ); WriteLn;
        END;

     END CheckCardArray;

     (* ------------------------------------------- *)
     PROCEDURE CheckCardIndex;
     BEGIN
        ok := TRUE;
        FOR i := 2 TO n DO
          IF a[index[i]+1] < a[index[i-1]+1] THEN
            ok := FALSE;
            WriteString( 'bad pair '); WriteCard( a[index[i-1]+1], 2 );
            WriteString( ' ' );        WriteCard( a[index[i]+1], 2 ); WriteLn;
          END;
        END;

        IF ok THEN
          WriteString( 'all ok' ); WriteLn;
        END;

     END CheckCardIndex;

     (* ------------------------------------------- *)
     PROCEDURE CheckRealArray;
     BEGIN
        ok := TRUE;
        FOR i := 2 TO n DO
          IF x[i] < x[i-1] THEN
            ok := FALSE;
            WriteString( 'bad pair '); WriteReal( x[i-1], 7 );
            WriteString( ' ' );        WriteReal( x[i], 7 ); WriteLn;
          END;
        END;

        IF ok THEN
          WriteString( 'all ok' ); WriteLn;
        END;

     END CheckRealArray;

     (* ------------------------------------------- *)
     PROCEDURE CheckRealIndex;
     BEGIN
        ok := TRUE;
        FOR i := 2 TO n DO
          IF x[index[i]+1] < x[index[i-1]+1] THEN
            ok := FALSE;
            WriteString( 'bad pair '); WriteReal( x[index[i-1]+1], 7 );
            WriteString( ' ' );        WriteReal( x[index[i]+1], 7 ); WriteLn;
          END;
        END;

        IF ok THEN
          WriteString( 'all ok' ); WriteLn;
        END;

     END CheckRealIndex;

BEGIN

  WriteLn; WriteString( 'bubblesort random cardinal test ' );
  RandomCard( n );
  WriteCard( n, 0 ); WriteString( ' elements' ); WriteLn;
  WriteString('before'); ShowCardArray;
  CardBSort( a, n );
  WriteString('after '); ShowCardArray; CheckCardArray;

  WriteLn; WriteString( 'bubble random reals test ' );
  RandomR( n );
  WriteCard( n, 0 ); WriteString( ' elements' ); WriteLn;
  RealBSort( x, n );
  CheckRealArray;



  WriteLn; WriteString( 'quicksort random cardinal test ' );
  RandomCard( n );
  WriteCard( n, 0 ); WriteString( ' elements' ); WriteLn;
  WriteString('before'); ShowCardArray;
  CardQSort( a, n );
  WriteString('after '); ShowCardArray; CheckCardArray;


  WriteLn; WriteString( 'quicksort random cardinal index test ' );
  RandomCard( n );
  WriteCard( n, 0 ); WriteString( ' elements' ); WriteLn;
  WriteString('before'); ShowCardArray;
  CardQSortIndex( a, n, index );
  WriteString('after '); ShowCardIndex; CheckCardIndex;

  WriteLn; WriteString( 'quicksort random reals index test ' );
  RandomR( n );
  WriteCard( n, 0 ); WriteString( ' elements' ); WriteLn;
  RealQSortIndex( x, n, index );
  CheckRealIndex;

END TestSort.
