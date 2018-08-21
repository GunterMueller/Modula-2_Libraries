MODULE TestLarge;

FROM InOut IMPORT WriteString, WriteLn;
FROM StringOperations IMPORT Compare;
FROM LargeNumbers IMPORT NumType, NumToString, StringToNum, SetDigits,
                         Add, Multiply, Divide, Subtract;

CONST
  max = 100;
  
VAR
  string :ARRAY [1..max] OF CHAR;
  a, b, c :NumType;
  
  PROCEDURE Perform( in_a, in_b :ARRAY OF CHAR; operation :CHAR;
                     expected_result :ARRAY OF CHAR );
  BEGIN
    WriteString( 'result should be ' );
    WriteString( in_a );
    WriteString( ' ' ); WriteString( operation ) ; WriteString( ' ' );
    WriteString( in_b );
    WriteString( ' = ' ); WriteString( expected_result ); WriteLn;

    StringToNum( in_a, a );
    NumToString( a, string ); WriteString( string );
    WriteString( ' ' ); WriteString( operation ) ; WriteString( ' ' );

    StringToNum( in_b, b );
    NumToString( b, string ); WriteString( string );
    WriteString( ' = ' );

    CASE operation OF
  '+' :Add( a, b, c );
| '-' :Subtract( a, b, c );
| '*' :Multiply( a, b, c );
| '/' :WriteLn;
       Divide( a, b, c );
    ELSE
      WriteString( 'unknown operation' ); WriteLn;
    END;

    NumToString( c, string ); WriteString( string ); WriteLn;

    IF Compare( string, '=', expected_result ) THEN
      WriteString( 'tests ok' );
    ELSE
      WriteString( 'not as expected' );
    END;
    WriteLn; WriteLn;
 
  END Perform;

BEGIN
 
  SetDigits( 4 );   Perform( '11.2', '23.5', '+', '3.47E1' );
 
  SetDigits( 5 );   Perform( '23.14', '5.0', '*', '1.157E2' );

  SetDigits( 20 );   Perform( '125.', '5.', '/', '2.5E1' );
 
END TestLarge.
