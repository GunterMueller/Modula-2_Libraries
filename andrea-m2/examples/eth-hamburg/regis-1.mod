MODULE TestRegis;

(* do some simple tests of the RegisGraphics module, and show how its used *)
(* J. Andrea, June 1991 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM REGISGraphics IMPORT BeginRegis, EndRegis,
                          EraseScreen, GetDimensions,
                          Dot, Move, Line, Circle, Rectangle, VectorLine;

VAR
  max_row, max_col :CARDINAL;
  mid_row, mid_col :CARDINAL;

BEGIN

BeginRegis( 'SYS$OUTPUT' );

EraseScreen;

Move( 200, 200 ); Circle( 50, TRUE );
Move( 200, 200 ); Line( 300, 300 );

Move( 50, 400 ); Dot;

Move( 50, 50 ); Rectangle( 50, 100, TRUE ); Rectangle( 55, 105, FALSE );

GetDimensions( max_col, max_row );
mid_row := max_row DIV 2; mid_col := max_col DIV 2;

Move( mid_col, mid_row );
VectorLine( 100, 0. ); VectorLine( 100, 45. );

EndRegis;

END TestRegis.
