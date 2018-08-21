MODULE Hilbert;

(* Draw a Hilbert curve,
   taken from Algorithms and Data Structures, N. Wirth *)
(* implemented using REGIS graphics by J. Andrea, May.7/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM Terminal      IMPORT Read, WriteString, WriteLn;
FROM REGISGraphics IMPORT BeginRegis, EndRegis,
                          EraseScreen, GetDimensions,
                          Move, VectorLine;

VAR
  max_row, max_col :CARDINAL;
  square_size      :CARDINAL;
  i, h, x0, y0     :CARDINAL;
  ch               :CHAR;

  (* ----------------------------------------------------------- *)
  PROCEDURE line( direction, length :CARDINAL );
  BEGIN
    VectorLine( length, FLOAT( direction ) * 45. );
  END line;
  
  (* ----------------------------------------------------------- *)
  PROCEDURE A( k :CARDINAL );
  BEGIN
    IF k > 0 THEN
      D(k-1); line(4,h); A(k-1); line(6,h); A(k-1); line(0,h); B(k-1);
    END;
  END A;

  (* ----------------------------------------------------------- *)
  PROCEDURE B( k :CARDINAL );
  BEGIN
    IF k > 0 THEN
      C(k-1); line(2,h); B(k-1); line(0,h); B(k-1); line(6,h); A(k-1);
    END;
  END B;

  (* ----------------------------------------------------------- *)
  PROCEDURE C( k :CARDINAL );
  BEGIN
    IF k > 0 THEN
      B(k-1); line(0,h); C(k-1); line(2,h); C(k-1); line(4,h); D(k-1);
    END;
  END C;

  (* ----------------------------------------------------------- *)
  PROCEDURE D( k :CARDINAL );
  BEGIN
    IF k > 0 THEN
      A(k-1); line(6,h); D(k-1); line(4,h); D(k-1); line(2,h); C(k-1);
    END;
  END D;

BEGIN

GetDimensions( max_col, max_row );
IF max_row < max_col THEN
  square_size := max_row;
ELSE
  square_size := max_col;
END;

WriteLn;
WriteString( 'Hit RETURN for the next iteration (max=6)' ); WriteLn;
WriteString( 'or SPACE RETURN to stop the display' ); WriteLn;
WriteLn;
Read( ch );

i := 0;  h := square_size;
x0 := max_col DIV 4;  y0 := max_row DIV 2;

REPEAT
  i := i + 1;    h := h DIV 2;
  x0 := x0 + h;  y0 := y0 + h;

  BeginRegis( 'SYS$OUTPUT' );   EraseScreen;

  Move( x0, y0 );  A(i);

  EndRegis;

  Read( ch );
UNTIL ( i = 7 ) OR ( ch = ' ' );

END Hilbert.
