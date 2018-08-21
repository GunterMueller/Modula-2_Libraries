MODULE TestRegis;

(* test the RegisGraphics module and show how its used *)
(* J. Andrea, June 1991 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM REGISGraphics IMPORT BeginRegis, EndRegis, GetDimensions,
                          EraseScreen, Move, VectorLine;

VAR
  max_row, max_col :CARDINAL;
  mid_row, mid_col :CARDINAL;

BEGIN

BeginRegis( 'SYS$OUTPUT' );

EraseScreen;

GetDimensions( max_col, max_row );
mid_row := max_row DIV 2; mid_col := max_col DIV 2;

Move( mid_col, mid_row ); VectorLine( 100, 0. );
Move( mid_col, mid_row ); VectorLine( 100, 20. );
Move( mid_col, mid_row ); VectorLine( 100, 45. );
Move( mid_col, mid_row ); VectorLine( 100, 80. );
Move( mid_col, mid_row ); VectorLine( 100, 90. );
Move( mid_col, mid_row ); VectorLine( 100, 100. );
Move( mid_col, mid_row ); VectorLine( 100, 135. );
Move( mid_col, mid_row ); VectorLine( 100, 180. );
Move( mid_col, mid_row ); VectorLine( 100, 225. );
Move( mid_col, mid_row ); VectorLine( 100, 270. );
Move( mid_col, mid_row ); VectorLine( 100, 315. );
Move( mid_col, mid_row ); VectorLine( 100, 360. );

EndRegis;

END TestRegis.
