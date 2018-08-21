MODULE Sierpinski;

(* Draw a Sierpinksi curve, taken from Programming In Modula-2, pp.111 *)
(* implemented using REGIS graphics by J. Andrea, May.1/91 *)
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
      A(k-1); line(7,h); B(k-1); line(0,2*h); D(k-1); line(1,h); A(k-1);
    END;
  END A;

  (* ----------------------------------------------------------- *)
  PROCEDURE B( k :CARDINAL );
  BEGIN
    IF k > 0 THEN
      B(k-1); line(5,h); C(k-1); line(6,2*h); A(k-1); line(7,h); B(k-1);
    END;
  END B;

  (* ----------------------------------------------------------- *)
  PROCEDURE C( k :CARDINAL );
  BEGIN
    IF k > 0 THEN
      C(k-1); line(3,h); D(k-1); line(4,2*h); B(k-1); line(5,h); C(k-1);
    END;
  END C;

  (* ----------------------------------------------------------- *)
  PROCEDURE D( k :CARDINAL );
  BEGIN
    IF k > 0 THEN
      D(k-1); line(1,h); A(k-1); line(2,2*h); C(k-1); line(3,h); D(k-1);
    END;
  END D;

BEGIN

GetDimensions( max_col, max_row );
IF max_row < max_col THEN
  square_size := max_row;
ELSE
  square_size := max_col;
END;

i := 0;  h := square_size DIV 4;

x0 := max_col DIV 2; y0 := max_row DIV 2;

WriteLn;
WriteString( 'Hit RETURN for the next iteration (max=6)' ); WriteLn;
WriteString( 'or SPACE RETURN to stop the display' ); WriteLn;
WriteLn;
Read( ch );

REPEAT
  x0 := x0 - h;  y0 := y0 + h;

  BeginRegis( 'SYS$OUTPUT' );   EraseScreen;

  Move( x0, y0 );
  A(i); line(7,h); B(i); line(5,h); C(i); line(3,h); D(i); line(1,h);

  EndRegis;

  h := h DIV 2;

  i := i + 1;
  Read( ch );
UNTIL ( i = 7 ) OR ( ch = ' ' );

END Sierpinski.
