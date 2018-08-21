IMPLEMENTATION MODULE VGAGraphics;
(*/NOWARN*)
(* simple VGA Graphics routines *)
(* first try at making pc graphics like regis module on the vax *)
(* v1.0, J. Andrea, Apr.12/92 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM Strings IMPORT Length;
IMPORT Graphics;

CONST
   absolute_rows = 480;
   absolute_cols = 640;

   row_offset    =  4;
   col_offset    =  4; 

   max_rows      = absolute_rows - 2 * row_offset;
   max_cols      = absolute_cols - 2 * col_offset;

VAR
  initial_mode             :INTEGER;
  current_row, current_col :INTEGER;

  vga_on :BOOLEAN;

  biggest :CARDINAL;

(* ----------------------------------------------------------------- *)
PROCEDURE BeginVGA;
BEGIN
   IF NOT vga_on THEN
     Graphics.GetScreenMode( initial_mode );
   END;
   
   vga_on := TRUE;
   Graphics.ScreenMode( Graphics.gph16CHiRes );
   
END BeginVGA;

(* ----------------------------------------------------------------- *)
PROCEDURE EndVGA;
BEGIN
  IF vga_on THEN
    Graphics.ScreenMode( initial_mode );    
    vga_on := FALSE;
    current_row := 0; current_col := 0;
  END;
END EndVGA;

(* ----------------------------------------------------------------- *)
PROCEDURE EraseScreen;
BEGIN
  IF vga_on THEN
    Graphics.ScreenMode( -1 );
  END;
END EraseScreen;

(* ----------------------------------------------------------------- *)
PROCEDURE GetDimensions( VAR columns, rows :CARDINAL );
BEGIN
  columns := CARDINAL( max_cols );
  rows    := CARDINAL( max_rows );
END GetDimensions;

(* ----------------------------------------------------------------- *)
PROCEDURE Move( col, row :CARDINAL );
BEGIN
  IF vga_on THEN
    IF col >= max_cols THEN col := max_cols END;
    IF row >= max_rows THEN row := max_rows END;
    
    current_row := INTEGER( row );
    current_col := INTEGER( col );

  END;
END Move;

(* ----------------------------------------------------------------- *)
PROCEDURE Dot;
BEGIN
  DotColor( 14 );
END Dot;

(* ----------------------------------------------------------------- *)
PROCEDURE DotColor( color :CARDINAL );
BEGIN
  IF vga_on THEN
    Graphics.Dot(            col_offset + current_col,
                  max_rows + row_offset - current_row,
                  color );
  END;
END DotColor;

(* ----------------------------------------------------------------- *)
PROCEDURE Line( col, row :CARDINAL );
BEGIN
  LineColor( col, row, 14 );
END Line;

(* ----------------------------------------------------------------- *)
PROCEDURE LineColor( col, row :CARDINAL; color :CARDINAL );
VAR save_row, save_col :INTEGER;
BEGIN
  IF vga_on THEN
    save_row := current_row;
    save_col := current_col;
    
    IF col >= max_cols THEN col := max_cols END;
    IF row >= max_rows THEN row := max_rows END;
    
    current_row := INTEGER( row );
    current_col := INTEGER( col );
    
    Graphics.Line(            col_offset + save_col,
                   max_rows + row_offset - save_row,
                              col_offset + current_col,
                   max_rows + col_offset - current_row,
                   color );
  END;
END LineColor;

(* ----------------------------------------------------------------- *)
PROCEDURE Circle( col, row :CARDINAL; radius :CARDINAL; fill :BOOLEAN );
BEGIN
   CircleColor( col, row, radius, 14, fill );
END Circle;

 (* ----------------------------------------------------------------- *)
PROCEDURE CircleColor( col, row :CARDINAL; radius :CARDINAL; color :CARDINAL;
                       fill :BOOLEAN );
BEGIN
  IF vga_on THEN
    IF col >= max_cols THEN col := max_cols END;
    IF row >= max_rows THEN row := max_rows END;
    
    current_row := INTEGER( row );
    current_col := INTEGER( col );
    
    Graphics.Circle(            col_offset + current_col,
                     max_rows + row_offset - current_row,
                                col_offset + current_col,
                     max_rows + row_offset - current_col - INTEGER( radius ),
                     color, fill );
    
  END;
END CircleColor;

(* ----------------------------------------------------------------- *)
PROCEDURE Text( string :ARRAY OF CHAR );
BEGIN
  TextColor( string, 14 );
END Text;

(* ----------------------------------------------------------------- *)
PROCEDURE TextColor( string :ARRAY OF CHAR; color :CARDINAL );
BEGIN
  IF vga_on THEN
    Graphics.WriteString( string,
                                     col_offset + current_col,
                          max_rows + row_offset - current_row,
                          color, 0, Length( string ) );
  END;
END TextColor;

BEGIN

   current_row := 0; current_col := 0;

   vga_on := FALSE;

   IF absolute_rows > absolute_cols THEN
     biggest := absolute_rows;
   ELSE
     biggest := absolute_cols;
   END;

END VGAGraphics.
