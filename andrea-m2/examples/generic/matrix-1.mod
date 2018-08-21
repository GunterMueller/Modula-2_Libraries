MODULE TestMatrix;

(* Test the module MatrixOperations *)
(* J. Andrea, Mar.1992 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteReal, WriteString, WriteLn, Read;
FROM MatrixOperations IMPORT Matrix, Build, Destroy,
                             Copy, Ident, Get, Put, Compare,
                             Size, Transpose, Invert, Multiply, Scale, Add,
                             Min, Max;

VAR
  a, b, c :Matrix;
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
          WriteReal( Get( m, i, j ), 11 );
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

  (* ----------------------------------------------------- *)
  PROCEDURE Expect( a, b :Matrix );
  BEGIN
    IF Compare( a, b ) THEN
      WriteString( 'as expected' );
    ELSE
      WriteString( 'in error !' ); WriteLn;
      WriteString( 'produced matrix is' ); WriteLn; Show( b );
      WriteString( 'expected matrix is' ); WriteLn; Show( a );
    END;
    WriteLn;
  END Expect;

BEGIN

  Build( a, 3, 4 ); Ident( a );

  WriteLn; WriteString( 'identity' ); WriteLn;
  Show( a );

  WriteLn; WriteString( 'hit CR' ); Read(ch);

        (* ----------------new test ------------------*)

  Copy( a, b );

  WriteLn; WriteString( 'compared to a copy of itself ' ); WriteLn;
  IF Compare( a, b ) THEN
    WriteString( 'same, as expected' );
  ELSE
    WriteString( 'not same, error !' );
  END;
  WriteLn;

  WriteLn; WriteString( 'hit CR' ); Read(ch);

        (* ----------------new test ------------------*)

  WriteLn; WriteString( 'modify copy' ); WriteLn;
  Put( b, 1, 3, 42.0 );

  Show( b );

  WriteLn; WriteString( 'compare to modified ' ); WriteLn;
  IF Compare( a, b ) THEN
    WriteString( 'same, error !' );
  ELSE
    WriteString( 'not same, as expected' );
  END;
  WriteLn;

  WriteLn; WriteString( 'hit CR' ); Read(ch);

        (* ----------------new test ------------------*)

  NumFill( b );

  WriteLn; WriteString( 'before transpose' ); WriteLn;
  Show( b );

  Transpose( b, c );

  WriteLn; WriteString( 'after transpose' ); WriteLn;
  Show( c );

  Destroy( a );

  Build( a, 4, 3 );
  Put( a, 1, 1, 1.0 );   Put( a, 1, 2, 5.0 );   Put( a, 1, 3,  9.0 );
  Put( a, 2, 1, 2.0 );   Put( a, 2, 2, 6.0 );   Put( a, 2, 3, 10.0 );
  Put( a, 3, 1, 3.0 );   Put( a, 3, 2, 7.0 );   Put( a, 3, 3, 11.0 );
  Put( a, 4, 1, 4.0 );   Put( a, 4, 2, 8.0 );   Put( a, 4, 3, 12.0 );

  WriteLn; WriteString( 'transpose ' );   Expect( c, a );

  WriteLn; WriteString( 'hit CR' ); Read(ch);
  Destroy( b ); Destroy( c );

        (* ----------------new test ------------------*)

  Build( a, 3, 3 );
  Put( a, 1, 1, 1.0 );   Put( a, 1, 2, 3.0 );    Put( a, 1, 3, -1.0 );
  Put( a, 2, 1, -2.0 );  Put( a, 2, 2, 0.0 );    Put( a, 2, 3,  1.0 );
  Put( a, 3, 1, 2.0 );   Put( a, 3, 2, -1.0 );   Put( a, 3, 3,  0.0 );

  WriteLn; WriteString( 'original' ); WriteLn;
  Show( a );

  Invert( a, b, ok );

  WriteLn;  WriteString( 'invert ' );
  IF ok THEN
    WriteLn; Show( b );

    Build( c, 3, 3 );

    Put( c, 1, 1, 0.2 );   Put( c, 1, 2, 0.2 );    Put( c, 1, 3, 0.6 );
    Put( c, 2, 1, 0.4 );   Put( c, 2, 2, 0.4 );    Put( c, 2, 3, 0.2 );
    Put( c, 3, 1, 0.4 );   Put( c, 3, 2, 1.4 );    Put( c, 3, 3, 1.2 );

    WriteString( 'inverted matrix ' );    Expect( b, c );

    Destroy( c );
  ELSE
    WriteString( 'failed !' );
  END;
  WriteLn;

  Destroy( a ); Destroy( b );
  WriteLn; WriteString( 'hit CR' ); Read(ch);

        (* ----------------new test ------------------*)

  Build( a, 4, 4 );  NumFill( a );   Copy( a, b );
  Add( a, b, c, ok );

  WriteLn; WriteString( 'addition ' );
  IF ok THEN
    Scale( a, 2.0 );
    Expect( a, c );
    Destroy( c );
  ELSE
    WriteString( 'failed !' );
  END;
  WriteLn;
    
  Destroy( a ); Destroy( b );
  WriteLn; WriteString( 'hit CR' ); Read(ch);

        (* ----------------new test ------------------*)

  Build( a, 3, 2 );
  Put( a, 1, 1, 2.0 );   Put( a, 1, 2, -1.0 );
  Put( a, 2, 1, 1.0 );   Put( a, 2, 2,  4.0 );
  Put( a, 3, 1, -3.0 );  Put( a, 3, 2,  0.0 );

  Build( b, 2, 2 );
  Put( b, 1, 1, 2.0 );   Put( b, 1, 2, 3.0 );
  Put( b, 2, 1, -5.0 );  Put( b, 2, 2, 1.0 );

  Multiply( a, b, c, ok );

  WriteLn; WriteString( 'multiply ' );
  IF ok THEN
    Destroy(a);

    Build( a, 3, 2 );
    Put( a, 1, 1, 9.0 );    Put( a, 1, 2,  5.0 );
    Put( a, 2, 1, -18.0 );  Put( a, 2, 2,  7.0 );
    Put( a, 3, 1, -6.0 );   Put( a, 3, 2, -9.0 );

    Expect( a, c );
  ELSE
    WriteString( 'failed !' );
  END;
  WriteLn;

  Destroy( a ); Destroy( b );
  WriteLn; WriteString( 'hit CR' ); Read(ch);

        (* ----------------new test ------------------*)

  x := 2.5;
  Build( a, 6, 4 ); NumFill( a ); Scale( a, 2.5 );
  WriteString( 'given' ); WriteLn; Show( a );

  WriteLn; WriteString( 'minimum ' );  ExpectX( x, Min(a) );

  x := x * 6.0 * 4.0;
  WriteLn; WriteString( 'maximum ' );  ExpectX( x, Max(a) );

END TestMatrix.
