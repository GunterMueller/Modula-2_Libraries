MODULE TestLarge;

FROM InOut IMPORT WriteString, WriteLn, WriteCard;
FROM LargeNumbers IMPORT NumType, NumToString, SetDigits,
                         Add, Multiply, Divide, zero, one;

CONST
  max = 100;
  max_factorial = 40;
  
VAR
  string :ARRAY [1..max] OF CHAR;
  Fact :ARRAY [0..max_factorial] OF NumType;
  e :NumType;
  i :CARDINAL;
  
  (* -------------------------- *)
  PROCEDURE FactTable;
  VAR
    i :CARDINAL;
    N :NumType;
  BEGIN
    Fact[0] := one;
    N       := one;
    FOR i := 1 TO max_factorial DO
      WriteCard( i, 2 ); WriteString( ' ' );
      Multiply( N, Fact[i-1], Fact[i] );
      NumToString( Fact[i], string ); WriteString( string ); WriteLn;
      Add( N, one, N );
    END;
  END FactTable;
   
  (* -------------------------- *)
  PROCEDURE Exp( VAR exp :NumType; power :NumType );
  VAR
    i :CARDINAL;
    numerator, quotient :NumType;
  BEGIN
    exp := zero;
    numerator := one;
    FOR i := 0 TO max_factorial DO
      Divide( numerator, Fact[i], quotient );
      Add( exp, quotient, exp );
      Multiply( power, numerator, numerator );
    END;
  END Exp;
  
BEGIN

 SetDigits( 30 );
 
 FactTable;
 
 (* calculate and print e *)
 Exp( e, one );
 NumToString( e, string );
 WriteString( 'e = ' ); WriteString( string ); WriteLn;
 
END TestLarge.
