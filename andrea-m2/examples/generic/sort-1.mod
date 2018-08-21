MODULE TestSort;

(* Test the sorting module *)
(* J. Andrea, 1992 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut    IMPORT WriteReal, WriteCard, WriteString, WriteLn;
FROM Sorting  IMPORT CardBSort, RealBSort, 
                     CardQSort, CardQSortIndex, RealQSortIndex;

CONST
  n = 20;

VAR
  a     :ARRAY [1..n] OF CARDINAL;
  x     :ARRAY [1..n] OF REAL;
  index :ARRAY [1..n] OF CARDINAL;
  i     :CARDINAL;
  ok    :BOOLEAN;

     (* ------------------------------------------- *)
     PROCEDURE Forward;
     BEGIN
        FOR i := 1 TO n DO
           a[i] := i;
        END;
     END Forward;

     (* ------------------------------------------- *)
     PROCEDURE Reverse;
     BEGIN
        FOR i := 1 TO n DO
           a[i] := n + 1 - i;
        END;
     END Reverse;

     (* ------------------------------------------- *)
     PROCEDURE ReverseReal;
     BEGIN
        FOR i := 1 TO n DO
           x[i] := FLOAT( n - i ) + 1.0;;
        END;
     END ReverseReal;

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
     PROCEDURE CheckSameArray;
     BEGIN
        ok := TRUE;
        FOR i := 1 TO n DO
          IF a[i] # i THEN
            ok := FALSE;
            WriteString( 'bad match '); WriteCard( a[i], 2 ); WriteLn;
          END;
        END;

        IF ok THEN
          WriteString( 'all ok' ); WriteLn;
        END;

     END CheckSameArray;

     (* ------------------------------------------- *)
     PROCEDURE CheckSameIndex;
     BEGIN
        ok := TRUE;
        FOR i := 1 TO n DO
          IF a[index[i]+1] # i THEN
            ok := FALSE;
            WriteString( 'bad match ' ); WriteCard( a[index[i]+1], 2 ); WriteLn;
          END;
        END;

        IF ok THEN
          WriteString( 'all ok' ); WriteLn;
        END;

     END CheckSameIndex;

     (* ------------------------------------------- *)
     PROCEDURE CheckSameRealArray;
     BEGIN
        ok := TRUE;
        FOR i := 1 TO n DO
          IF x[i] # FLOAT(i) THEN
            ok := FALSE;
            WriteString( 'bad match '); WriteReal( x[i], 7 ); WriteLn;
          END;
        END;

        IF ok THEN
          WriteString( 'all ok' ); WriteLn;
        END;

     END CheckSameRealArray;

     (* ------------------------------------------- *)
     PROCEDURE CheckSameRealIndex;
     BEGIN
        ok := TRUE;
        FOR i := 1 TO n DO
          IF x[index[i]+1] # FLOAT(i) THEN
            ok := FALSE;
            WriteString( 'bad match ' ); WriteReal( x[index[i]+1], 7 ); WriteLn;
          END;
        END;

        IF ok THEN
          WriteString( 'all ok' ); WriteLn;
        END;

     END CheckSameRealIndex;

BEGIN

  WriteLn; WriteString( 'bubblesort forward cardinal test' ); WriteLn;
  Forward;
  WriteString('before'); ShowCardArray;
  CardBSort( a, n );
  WriteString('after '); ShowCardArray; CheckSameArray;

  WriteLn; WriteString( 'bubble reverse cardinals test' ); WriteLn;
  Reverse;
  WriteString('before'); ShowCardArray;
  CardBSort( a, n );
  WriteString('after '); ShowCardArray; CheckSameArray;

  WriteLn; WriteString( 'bubble reverse reals test' ); WriteLn;
  ReverseReal;
  RealBSort( x, n );
  CheckSameRealArray;




  WriteLn; WriteString( 'quicksort forward cardinal test' ); WriteLn;
  Forward;
  WriteString('before'); ShowCardArray;
  CardQSort( a, n );
  WriteString('after '); ShowCardArray; CheckSameArray;

  WriteLn; WriteString( 'quicksort reverse cardinals test' ); WriteLn;
  Reverse;
  WriteString('before'); ShowCardArray;
  CardQSort( a, n );
  WriteString('after '); ShowCardArray; CheckSameArray;


  WriteLn; WriteString( 'quicksort forward cardinal index test' ); WriteLn;
  Forward;
  WriteString('before'); ShowCardArray;
  CardQSortIndex( a, n, index );
  WriteString('after '); ShowCardIndex; CheckSameIndex;

  WriteLn; WriteString( 'quicksort reverse cardinals index test' ); WriteLn;
  Reverse;
  WriteString('before'); ShowCardArray;
  CardQSortIndex( a, n, index );
  WriteString('after '); ShowCardIndex; CheckSameIndex;

  WriteLn; WriteString( 'quicksort reverse reals index test' ); WriteLn;
  ReverseReal;
  RealQSortIndex( x, n, index );
  CheckSameRealIndex;

END TestSort.
