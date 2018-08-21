IMPLEMENTATION MODULE REGISGraphics;

(* simple REGIS Graphics routines *)
(* the offsets are used, but as far as the user is concerned there is
   no offset, just that smaller dimensions are used *)
(* J. Andrea, Apr.30/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM FileSystem IMPORT Done, Create, Close, WriteChar, File;
FROM MathLib0   IMPORT sin, cos;

CONST
   absolute_rows = 475;
   absolute_cols = 790;

   row_offset    = 20;
   col_offset    =  0;

   max_rows      = absolute_rows - row_offset;
   max_cols      = absolute_cols - col_offset;

   esc = 33C;

VAR
  current_row, current_col :CARDINAL;
  true_row,    true_col    :CARDINAL;

  output_open, regis_on :BOOLEAN;

  biggest :CARDINAL;

  digits  :ARRAY [0..9] OF CHAR;
  i       :CARDINAL;

  outf :File;

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
   true_row := absolute_rows - current_row;
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
PROCEDURE BeginRegis( filename :ARRAY OF CHAR );
(* define the output file to hold the graphics commands,
   if interactive graphics then define filename as SYS$OUTPUT *)
BEGIN
  IF output_open THEN  EndRegis;  END;

  Create( outf, filename, TRUE, FALSE );

  IF Done() THEN
    output_open := TRUE;
    RegisOn;
  END;
END BeginRegis;

(* ----------------------------------------------------------------- *)
PROCEDURE EndRegis;
(* close the current output file *)
BEGIN
  IF output_open THEN
    RegisOff;
    Close( outf );
    output_open := FALSE;
  END;
END EndRegis;

(* ----------------------------------------------------------------- *)
PROCEDURE RegisOn;
(* enter REGIS mode, this is required *)
BEGIN
  IF NOT regis_on THEN
    regis_on := TRUE;
    Output( esc ); Output( 'Pp' );
    EraseScreen;  (* this seems to be required *)
  END;
END RegisOn;

(* ----------------------------------------------------------------- *)
PROCEDURE RegisOff;
(* exit REGIS mode, this is required *)
BEGIN
  IF regis_on THEN
    Output( esc ); Output( '\' );   regis_on := FALSE;
  END;
END RegisOff;

(* ----------------------------------------------------------------- *)
PROCEDURE EraseScreen;
(* erase the whole display *)
BEGIN
  IF output_open THEN
    Output( 'S(E)' );
  END;
END EraseScreen;

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
    (* move to the corner *)
    Output( 'P[0,' ); OutputNumber( row_offset ); Output( ']' );

    (* output the lines to draw the border, using relative motion *)
    Output( 'V(B)' );
    Output( '[,+' ); OutputNumber( max_rows );
    Output( '[+,' ); OutputNumber( max_cols );
    Output( '[,-' ); OutputNumber( max_rows );
    Output( '(E)' );
  END;
END Border;

(* ----------------------------------------------------------------- *)
PROCEDURE Move( column, row :CARDINAL );
(* move to the new location, make it the current point *)
BEGIN
  IF output_open THEN
    CheckLocation( row, column, current_row, current_col );
    TrueLocation;

    Output( 'P[' );
    OutputNumber( true_col );
    Output( ',' );
    OutputNumber( true_row );
    Output( ']' );
  END;
END Move;

(* ----------------------------------------------------------------- *)
PROCEDURE Dot;
(* draw a dot at the current location *)
BEGIN
  IF output_open THEN
    TrueLocation;

    Output( 'P[' );
    OutputNumber( true_col );
    Output( ',' );
    OutputNumber( true_row );
    Output( ']V[]' );
  END;
END Dot;

(* ----------------------------------------------------------------- *)
PROCEDURE Line( column, row :CARDINAL );
(* draw a line from the current point to the new point *)
BEGIN
  IF output_open THEN
    CheckLocation( row, column, current_row, current_col );
    TrueLocation;

    Output( 'V(S)[' );
    OutputNumber( true_col );
    Output( ',' );
    OutputNumber( true_row );
    Output( '](E)' );
  END;
END Line;

(* ----------------------------------------------------------------- *)
PROCEDURE Circle( radius :CARDINAL; filled :BOOLEAN );
(* draw a circle of the given radius from the current point *)
BEGIN
  IF output_open THEN
    IF radius > biggest THEN radius := biggest; END;

    IF filled THEN Output( 'F(' ); END;

    Output( 'C[+' );
    OutputNumber( radius );
    Output( ']' );

    IF filled THEN Output( ')' ); END;
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

    IF filled THEN Output( 'F(' ); END;

    Output( 'V(B)' );
    Output( '[+' );
    OutputNumber( width );
    Output( ']' );
    Output( '[,-' );
    OutputNumber( height );
    Output( ']' );
    Output( '[-' );
    OutputNumber( width );
    Output( ']' );
    Output( '(E)' );

    IF filled THEN Output( ')' ); END;
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
    IF size > biggest THEN size := biggest END;

    Output( 'T(S' ); OutputNumber( size ); Output( ')' );
    Output( "'" ); Output( string ); Output( "'" );
  END;
END Text;

(* ----------------------------------------------------------------- *)
PROCEDURE Command( string :ARRAY OF CHAR );
(* put some other REGIS command into the output stream *)
BEGIN
  IF output_open THEN
    Output( string );
  END;
END Command;

BEGIN (* REGISGraphics *)

   current_row := 0; current_col := 0;

   output_open := FALSE;
   regis_on    := FALSE;

   IF absolute_rows > absolute_cols THEN
     biggest := absolute_rows;
   ELSE
     biggest := absolute_cols;
   END;

   FOR i := 0 TO 9 DO
      digits[i] := CHAR( i + 48 );
   END;

END REGISGraphics.
