MODULE TestMatrix;

(* Test the Determinant routine of module MatrixOperations *)
(* J. Andrea, Mar.1992 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM RealInOut IMPORT WriteReal;
FROM InOut IMPORT WriteString, WriteLn, Read;
FROM MatrixOperations IMPORT Matrix, Build, Destroy,
                             Get, Put, Determinant,
                             Size;
VAR
  a :Matrix;
  ok      :BOOLEAN;
  ch      :CHAR;
  x       :REAL;

  (* ----------------------------------------------------- *)
  PROCEDURE Show( m :Matrix );
  VAR i, j, r, c :CARDINAL;
  BEGIN
    Size( m, r, c );
    FOR i := 1 TO r DO
       FOR j := 1 TO c DO
          WriteReal( Get( m, i, j ), 1 );
       END;
       WriteLn;
     END;
  END Show;

  (* ----------------------------------------------------- *)
  PROCEDURE NumFill( m :Matrix );
  VAR i, j, r, c, n :CARDINAL;
  BEGIN
    Size( m, r, c );
    n := 1;
    FOR i := 1 TO r DO
       FOR j := 1 TO c DO
          Put( m, i, j, FLOAT(n) );
          n := n + 1;
       END;
       WriteLn;
     END;
  END NumFill;

  (* ----------------------------------------------------- *)
  PROCEDURE ExpectX( exact, found :REAL );
  BEGIN
    IF found = exact THEN
      WriteString( 'as expected, ' ); WriteReal( found, 11 );
    ELSE
      WriteString( 'in error !' ); WriteLn;
      WriteString( 'produced is' ); WriteReal( found, 11 );  WriteLn;
      WriteString( 'expected is' ); WriteReal( exact, 11 ); WriteLn;
    END;
    WriteLn;
  END ExpectX;

BEGIN

        (* ----------------new test ------------------*)

  Build( a, 2, 2 ); NumFill( a );

  WriteString( 'given' ); WriteLn;
  Show( a );
  Determinant( a, x, ok );

  WriteLn; WriteString( 'determinant ' );
  IF ok THEN
    ExpectX( -2.0, x );
  ELSE
    WriteString( 'failed !' );
  END;
  WriteLn;

  WriteLn; WriteString( 'hit CR' ); Read(ch);
  Destroy( a );

        (* ----------------new test ------------------*)
  
  Build( a, 3, 3 );
  Put( a, 1, 1, 2.0 );   Put( a, 1, 2, 2.0 );  Put( a, 1, 3, -2.0 );
  Put( a, 2, 1, 1.0 );   Put( a, 2, 2, 2.0 );  Put( a, 2, 3,  3.0 );
  Put( a, 3, 1, 2.0 );   Put( a, 3, 2, 3.0 );  Put( a, 3, 3,  4.0 );

  WriteLn;
  WriteString( 'given' ); WriteLn;
  Show( a );
  Determinant( a, x, ok );

  WriteLn; WriteString( 'determinant ' );
  IF ok THEN
    ExpectX( 4.0, x );
  ELSE
    WriteString( 'failed !' );
  END;
  WriteLn;

  WriteLn; WriteString( 'hit CR' ); Read(ch);
  Destroy( a );

        (* ----------------new test ------------------*)
   
  Build( a, 4, 4 );
  Put( a, 1, 1,  2.0 );   Put( a, 1, 2,  3.0 );  Put( a, 1, 3, -2.0 );
  Put( a, 1, 4,  4.0 );
  Put( a, 2, 1,  3.0 );   Put( a, 2, 2, -2.0 );  Put( a, 2, 3,  1.0 );
  Put( a, 2, 4,  2.0 );
  Put( a, 3, 1,  3.0 );   Put( a, 3, 2,  2.0 );  Put( a, 3, 3,  3.0 );
  Put( a, 3, 4,  4.0 );
  Put( a, 4, 1, -2.0 );   Put( a, 4, 2,  4.0 );  Put( a, 4, 3,  0.0 );
  Put( a, 4, 4,  5.0 );

  WriteLn;
  WriteString( 'given' ); WriteLn;
  Show( a );
  Determinant( a, x, ok );

  WriteLn; WriteString( 'determinant ' );
  IF ok THEN
    ExpectX( -286.0, x );
  ELSE
    WriteString( 'failed !' );
  END;
  WriteLn;

  WriteLn; WriteString( 'hit CR' ); Read(ch);
  Destroy( a );

        (* ----------------new test ------------------*)
  
  Build( a, 5, 5 );
  Put( a, 1, 1,  1.0 );   Put( a, 1, 2, -2.0 );  Put( a, 1, 3,  3.0 );
  Put( a, 1, 4, -2.0 );   Put( a, 1, 5, -2.0 );
  Put( a, 2, 1,  2.0 );   Put( a, 2, 2, -1.0 );  Put( a, 2, 3,  1.0 );
  Put( a, 2, 4,  3.0 );   Put( a, 2, 5,  2.0 );
  Put( a, 3, 1,  1.0 );   Put( a, 3, 2,  1.0 );  Put( a, 3, 3,  2.0 );
  Put( a, 3, 4,  1.0 );   Put( a, 3, 5,  1.0 );
  Put( a, 4, 1,  1.0 );   Put( a, 4, 2, -4.0 );  Put( a, 4, 3, -3.0 );
  Put( a, 4, 4, -2.0 );   Put( a, 4, 5, -5.0 );
  Put( a, 5, 1,  3.0 );   Put( a, 5, 2, -2.0 );  Put( a, 5, 3,  2.0 );
  Put( a, 5, 4,  2.0 );   Put( a, 5, 5, -2.0 );

  WriteLn;
  WriteString( 'given' ); WriteLn;
  Show( a );
  Determinant( a, x, ok );

  WriteLn; WriteString( 'determinant ' );
  IF ok THEN
    ExpectX( 118.0, x );
  ELSE
    WriteString( 'failed !' );
  END;
  WriteLn;

  WriteLn; WriteString( 'hit CR' ); Read(ch);
  Destroy( a );

END TestMatrix.
