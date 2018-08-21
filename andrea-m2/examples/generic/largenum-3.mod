MODULE TestLarge;

FROM InOut IMPORT WriteString, WriteLn, WriteCard;
FROM LargeNumbers IMPORT NumType, NumToString, SetDigits,
                         Add, Multiply, zero, one;

CONST
  max = 70;
  max_factorial = 200;
  
VAR
  string :ARRAY [1..max+5] OF CHAR;
  
  (* -------------------------- *)
  PROCEDURE FactTable;
  VAR
    i :CARDINAL;
    N, f0, f1 :NumType;
  BEGIN
    f0 := one;
    N  := one;
    FOR i := 1 TO max_factorial DO
      WriteCard( i, 2 ); WriteString( ' ' );
      Multiply( N, f0, f1 );
      NumToString( f1, string ); WriteString( string ); WriteLn;
      Add( N, one, N );
      f0 := f1;
    END;
  END FactTable;
   
BEGIN

 SetDigits( max );
 
 FactTable;
 
END TestLarge.
