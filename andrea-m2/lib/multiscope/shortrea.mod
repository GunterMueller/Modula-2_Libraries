(*$T-*)
(*$R-*)
IMPLEMENTATION MODULE ShortRealInOut;

IMPORT RealInOut;

VAR
  x :LONGREAL;
  
PROCEDURE ReadReal( VAR r :REAL );
BEGIN
  RealInOut.ReadReal( x );   r := SHORT( x );
  Done := RealInOut.Done;
END ReadReal;

PROCEDURE WriteReal( r      :REAL; 
                     length :CARDINAL );
BEGIN
  RealInOut.WriteReal( LONG( r ), length );
END WriteReal;

END ShortRealInOut.
