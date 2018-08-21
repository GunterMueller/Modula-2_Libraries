IMPLEMENTATION MODULE PSGraphics;

(* simple PostScript Graphics routines *)
(* J. Andrea, May.16/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM FileSystem IMPORT Done, Create, Close, WriteChar, File, EOL;
FROM MathLib0   IMPORT sin, cos;

CONST
   absolute_rows = 776;
   absolute_cols = 575;

   row_offset    = 20;
   col_offset    = 20;

   max_rows      = absolute_rows - row_offset;
   max_cols      = absolute_cols - col_offset;

VAR
  current_row, current_col :CARDINAL;
  true_row,    true_col    :CARDINAL;

  output_open              :BOOLEAN;

  biggest :CARDINAL;

  digits  :ARRAY [0..9] OF CHAR;
  i       :CARDINAL;

  outf    :File;

  font_chosen  :CARDINAL;

  unstroked_lines :CARDINAL;

(* ----------------------------------------------------------------- *)
PROCEDURE CheckLocation( row, col :CARDINAL; VAR new_row, new_col :CARDINAL );
(* don't allow new position to be out of bounds *)
BEGIN
   IF row > max_rows THEN row := max_rows; END;
   IF col > max_cols THEN col := max_cols; END;
   new_row := row;   new_col := col;
END CheckLocation;

(* ----------------------------------------------------------------- *)
PROCEDURE TrueLocation;
(* and invert positions so that y (rows) run bottom to top,
   and apply offsets as required *)
BEGIN
   true_row := current_row + row_offset;
   true_col := current_col + col_offset;
END TrueLocation;

(* ----------------------------------------------------------------- *)
PROCEDURE Output( string :ARRAY OF CHAR );
(* write the given string to the output file *)
VAR i, L :CARDINAL;
BEGIN

  L := LEN( string ) - 1;
  FOR i := 0 TO L DO
    WriteChar( outf, string[i] );
  END;
END Output;

(* ----------------------------------------------------------------- *)
PROCEDURE OutputNumber( x :CARDINAL );
(* assumption that x is less than 1000 *)

BEGIN

  IF x > 99 THEN

    (* 3 digits *)

    i := x DIV 100;
    WriteChar( outf, digits[i] );
    x := x - 100 * i;
    i := x DIV 10;
    WriteChar( outf, digits[i] );
    x := x - 10 * i;
    WriteChar( outf, digits[x] );

  ELSE
    IF x > 9 THEN

      (* 2 digits *)

      i := x DIV 10;
      WriteChar( outf, digits[i] );
      x := x - 10 * i;
      WriteChar( outf, digits[x] );

    ELSE

      (* 1 digit *)

      WriteChar( outf, digits[x] );

    END;
  END;

END OutputNumber;

(* ----------------------------------------------------------------- *)
PROCEDURE NewLine;
BEGIN
  WriteChar( outf, EOL );
END NewLine;

(* ----------------------------------------------------------------- *)
PROCEDURE DoingALine( yes :BOOLEAN );
(* define the output file to hold the graphics commands *)

  (* ----------------------------------------------- *)
  PROCEDURE DoStroke;
  BEGIN
     Output( 'stroke' ); NewLine;
     unstroked_lines := 0;

     (* but where did the current point go, it disappeared with
        to stroke, so get a new current point *)
     Move( current_col, current_row );
  END DoStroke;

BEGIN

  IF yes THEN

    IF unstroked_lines = 0 THEN

      (* if there arn't any lines already done, then there probably isn't
         a PostScript current point, make one *)
      Move( current_col, current_row );

    ELSE

      (* every 1000 lines do a stroke because of a possible PostScript limit *)
      IF unstroked_lines > 1000 THEN
        DoStroke;
      END;

    END;

    unstroked_lines := unstroked_lines + 1;

  ELSE

    (* this next operation won't be a line, so if there are lines
       left over, we better paint them *)

    IF unstroked_lines > 0 THEN
      DoStroke;
    END;

  END;

END DoingALine;

(* ----------------------------------------------------------------- *)
PROCEDURE BeginPS( filename :ARRAY OF CHAR );
(* define the output file to hold the graphics commands *)
BEGIN
  IF output_open THEN  EndPS;  END;

  Create( outf, filename, TRUE, TRUE );

  IF Done() THEN
    output_open := TRUE;

    Output( '%!' ); NewLine;
    Output( '/L { lineto } bind def' ); NewLine;
    Output( '/M { moveto } bind def' ); NewLine;

    current_row := 0; current_col := 0;
    font_chosen := 0;
    unstroked_lines := 0;

  END;
END BeginPS;

(* ----------------------------------------------------------------- *)
PROCEDURE EndPS;
(* close the current output file *)
BEGIN
  IF output_open THEN
    IF unstroked_lines > 0 THEN Output( 'stroke ' ); END;
    Output( 'showpage' ); NewLine;
    Close( outf );
    output_open := FALSE;
  END;
END EndPS;

(* ----------------------------------------------------------------- *)
PROCEDURE GetDimensions( VAR columns, rows :CARDINAL );
(* return the dimensions of the display *)
BEGIN
  columns := max_cols;
  rows    := max_rows;
END GetDimensions;

(* ----------------------------------------------------------------- *)
PROCEDURE GetPosition( VAR column, row :CARDINAL );
(* return the current location of the cursor *)
BEGIN
  column := current_col;
  row    := current_row;
END GetPosition;

(* ----------------------------------------------------------------- *)
PROCEDURE Border;
(* draw a border *)
BEGIN
  IF output_open THEN
    DoingALine( FALSE );

    (* move to the corner *)
    Move( col_offset, row_offset );

    (* output the lines to draw the border, using relative motion *)
    OutputNumber( max_cols );
    Output( ' ' );
    OutputNumber( 0 );
    Output( ' rlineto' );

    Output( ' ' );
    OutputNumber( max_rows );
    Output( ' ' );
    OutputNumber( 0 );
    Output( ' rlineto' );

    Output( ' ' );
    OutputNumber( max_cols );
    Output( ' neg ' );
    OutputNumber( 0 );
    Output( ' rlineto' );

    Output( ' closepath stroke' ); NewLine;

    (* return to the current location *)
    Move( current_col, current_row );

  END;
END Border;

(* ----------------------------------------------------------------- *)
PROCEDURE Move( column, row :CARDINAL );
(* move to the new location, make it the current point *)
BEGIN
  IF output_open THEN
    DoingALine( FALSE );

    CheckLocation( row, column, current_row, current_col );
    TrueLocation;

    OutputNumber( true_col );
    Output( ' ' );
    OutputNumber( true_row );
    Output( ' M' );
    NewLine;
  END;
END Move;

(* ----------------------------------------------------------------- *)
PROCEDURE Dot;
(* draw a dot at the current location *)
BEGIN
  IF output_open THEN
    TrueLocation;
    Line( true_col, true_row+1 );
  END;
END Dot;

(* ----------------------------------------------------------------- *)
PROCEDURE Line( column, row :CARDINAL );
(* draw a line from the current point to the new point *)
BEGIN
  IF output_open THEN

    DoingALine( TRUE );

    CheckLocation( row, column, current_row, current_col );
    TrueLocation;

    OutputNumber( true_col );
    Output( ' ' );
    OutputNumber( true_row );
    Output( ' L' );
    NewLine;
  END;
END Line;

(* ----------------------------------------------------------------- *)
PROCEDURE Circle( radius :CARDINAL; filled :BOOLEAN );
(* draw a circle of the given radius from the current point *)
BEGIN
  IF output_open THEN
    IF radius > biggest THEN radius := biggest; END;

    TrueLocation;

    DoingALine( FALSE );

    OutputNumber( true_col );
    Output( ' ' );
    OutputNumber( true_row );

    Output( ' ' );
    OutputNumber( radius );
    Output( ' 0 360 arc ' );

    Output( ' closepath' );

    IF filled THEN
      Output( ' fill' );
    ELSE
      Output( ' stroke' );
    END;
    NewLine;

    (* return to the current location *)
    Move( current_col, current_row );
  END;
END Circle;

(* ----------------------------------------------------------------- *)
PROCEDURE Rectangle( width, height :CARDINAL; filled :BOOLEAN );
(* draw a rectangle, one corner at current point,
   opposite corner at current+width,current+height *)
BEGIN
  IF output_open THEN
    IF width  + current_col > max_cols THEN
      width  := max_cols - current_col; END;
    IF height + current_row > max_rows THEN
      height := max_rows - current_row; END;

    DoingALine( FALSE );

    OutputNumber( width );
    Output( ' ' );
    OutputNumber( 0 );
    Output( ' rlineto' );

    Output( ' ' );
    OutputNumber( height );
    Output( ' ' );
    OutputNumber( 0 );
    Output( ' rlineto' );

    Output( ' ' );
    OutputNumber( width );
    Output( ' neg ' );
    OutputNumber( 0 );
    Output( ' rlineto' );

    Output( ' closepath' );

    IF filled THEN
      Output( ' fill' );
    ELSE
      Output( ' stroke' );
    END;
    NewLine;

    (* return to the current location *)
    Move( current_col, current_row );
  END;
END Rectangle;

(* ----------------------------------------------------------------- *)
PROCEDURE Vector( length :CARDINAL; angle :REAL;
                  VAR new_col, new_row :CARDINAL );

VAR
  a, z  :REAL;
  delta :CARDINAL;

  (* ------------------------ *)
  PROCEDURE Delta( length :REAL ) :CARDINAL;
  BEGIN
    length := ABS( length );
    IF length > FLOAT( biggest ) THEN
      RETURN biggest;
    ELSE
      RETURN CARDINAL( TRUNC( length ) );
    END;
  END Delta;

BEGIN

   IF length > biggest THEN
     length := biggest;
   END;

   a := ABS( angle );

   WHILE a >= 360. DO
     a := a - 360.
   END;

   IF angle >= 0. THEN
     angle := a;
   ELSE
     angle := 360. - a;
   END;

   IF ( angle <= 1. ) OR ( angle >= 359. ) THEN
     new_col := current_col + length;
     new_row := current_row;
   ELSE
     IF ( angle >= 89. ) AND ( angle <= 91. ) THEN
       new_row := current_row + length;
       new_col := current_col;
     ELSE
       IF ( angle >= 179. ) AND ( angle <= 181. ) THEN
         IF length >= current_col THEN
           new_col := 0;
         ELSE
           new_col := current_col - length;
         END;
         new_row := current_row;
       ELSE
         IF ( angle >= 269. ) AND ( angle <= 271. ) THEN
           IF length >= current_row THEN
             new_row := 0;
           ELSE
             new_row := current_row - length;
           END;
           new_col := current_col;
         ELSE

           a := angle * 3.141592653 / 180.;

           z := FLOAT( length ) * sin( a );

           IF z >= 0. THEN
             new_row := current_row + Delta(z);
           ELSE
             delta := Delta(z);
             IF delta >= current_row THEN
               new_row := 0;
             ELSE
               new_row := current_row - delta;
             END;
           END;

           z := FLOAT( length ) * cos( a );

           IF z >= 0. THEN
             new_col := current_col + Delta(z);
           ELSE
             delta := Delta(z);
             IF delta >= current_col THEN
               new_col := 0;
             ELSE
               new_col := current_col - delta;
             END;
           END;

         END;
       END;
     END;
   END;

END Vector;

(* ----------------------------------------------------------------- *)
PROCEDURE VectorLine( length :CARDINAL; angle :REAL );
(* draw a line from the current location given a length and angle *)
VAR
  new_row, new_col :CARDINAL;

BEGIN
  IF output_open THEN
    Vector( length, angle, new_col, new_row );
    Line( new_col, new_row );
  END;
END VectorLine;

(* ----------------------------------------------------------------- *)
PROCEDURE VectorMove( length :CARDINAL; angle :REAL );
(* move from the current location given a length and angle *)
VAR
  new_row, new_col :CARDINAL;

BEGIN
  IF output_open THEN
    Vector( length, angle, new_col, new_row );
    Move( new_col, new_row );
  END;
END VectorMove;

(* ----------------------------------------------------------------- *)
PROCEDURE Text( string :ARRAY OF CHAR; size :CARDINAL );
(* put up some text at the current location *)
BEGIN
  IF output_open THEN
    DoingALine( FALSE );

    IF size > biggest THEN size := biggest END;

    IF font_chosen # size THEN
      font_chosen := size;
      Output( '/Times-Roman findfont ' );
      OutputNumber( size );
      Output( ' scalefont setfont' );
      NewLine;
    END;

    Output( '(' ); Output( string ); Output( ')show' ); NewLine;

    (* return to the current location *)
    Move( current_col, current_row );
  END;
END Text;

(* ----------------------------------------------------------------- *)
PROCEDURE Command( string :ARRAY OF CHAR );
(* put some other PostScript command into the output stream *)
BEGIN
  IF output_open THEN
    DoingALine( FALSE );
    Output( string ); NewLine;
  END;
END Command;

BEGIN (* PSGraphics *)

   output_open := FALSE;

   IF absolute_rows > absolute_cols THEN
     biggest := absolute_rows;
   ELSE
     biggest := absolute_cols;
   END;

   FOR i := 0 TO 9 DO
      digits[i] := CHAR( i + 48 );
   END;

END PSGraphics.
