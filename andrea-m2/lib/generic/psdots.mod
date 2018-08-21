IMPLEMENTATION MODULE PSDots;

(* J. Andrea, Nov.13/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM FileSystem IMPORT Done, Create, Close, WriteChar, File, EOL;

CONST
   absolute_rows = 776;
   absolute_cols = 575;

   margin        = 18;

   max_rows      = absolute_rows - margin - margin;
   max_cols      = absolute_cols - margin - margin;

   max_buff      = 80;

VAR
  page          :ARRAY [1..max_rows] OF ARRAY [1..max_cols] OF BOOLEAN;

  output_open   :BOOLEAN;

  digits  :ARRAY [0..9] OF CHAR;
  i       :CARDINAL;

  outf    :File;

  buffer  :ARRAY [1..max_buff] OF CHAR;
  n_buff  :CARDINAL;

(* ----------------------------------------------------------------- *)
PROCEDURE Flush;
VAR i :CARDINAL;
BEGIN

  IF n_buff > 0 THEN
    FOR i := 1 TO n_buff DO
      WriteChar( outf, buffer[i] );
    END;
    WriteChar( outf, EOL );
    n_buff := 0;
  END;

END Flush;

(* ----------------------------------------------------------------- *)
PROCEDURE Output( string :ARRAY OF CHAR );
(* write the given string to the output file *)
VAR i, L :CARDINAL;
BEGIN

  L := LEN( string );
  IF L + n_buff > max_buff THEN Flush; END;

  L := L - 1;
  FOR i := 0 TO L DO
    n_buff := n_buff + 1;
    buffer[n_buff] := string[i];
  END;

END Output;

(* ----------------------------------------------------------------- *)
PROCEDURE OutputNumber( x :CARDINAL );
(* assumption that x is less than 1000 *)
VAR num :ARRAY [0..2] OF CHAR;
BEGIN

  IF x > 99 THEN

    (* 3 digits *)

    i := x DIV 100;
    num[0] := digits[i];
    x := x - 100 * i;
    i := x DIV 10;
    num[1] := digits[i];
    x := x - 10 * i;
    num[2] := digits[x];

  ELSE
    IF x > 9 THEN

      (* 2 digits *)

      i := x DIV 10;
      num[0] := digits[i];
      x := x - 10 * i;
      num[1] := digits[x];
      num[2] := 0C;

    ELSE

      (* 1 digit *)
      num[0] := digits[x];
      num[1] := 0C;

    END;
  END;

  Output( num );

END OutputNumber;

(* ----------------------------------------------------------------- *)
PROCEDURE BeginPS( filename :ARRAY OF CHAR );
(* define the output file to hold the graphics commands *)
BEGIN
  IF output_open THEN  EndPS;  END;

  Create( outf, filename, TRUE, TRUE );

  IF Done() THEN
    output_open := TRUE;

    n_buff := 0;

    Output( '%!PS-Adobe' ); Flush;
    Output( '/D { moveto 0 1 rlineto stroke } bind def' ); Flush;
    Output( '18 18 translate 0 setlinewidth' ); Flush;

  END;
END BeginPS;

(* ----------------------------------------------------------------- *)
PROCEDURE EndPS;

VAR
  row, col :CARDINAL;

BEGIN
  IF output_open THEN

    FOR row := 1 TO max_rows DO
      FOR col := 1 TO max_cols DO
         IF page[row][col] THEN
           OutputNumber( col );
           Output( ' ' );
           OutputNumber( row );
           Output( ' D ' );
         END;
      END;
    END;

    Output( ' showpage' ); Flush;
    Output( 4C );
    Flush;
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
PROCEDURE Dot( col, row :CARDINAL );
(* move to the new location, make a dot *)
(* columns are in the x direction, rows are in the y direction *)
(* columns are in the width direction, rows are in the height direction *)

BEGIN
  IF output_open THEN

    IF col = 0 THEN
      col := 1;
    ELSE
      IF col > max_cols THEN
        col := max_cols;
      END;
    END;

    IF row = 0 THEN
      row := 1;
    ELSE
      IF row > max_rows THEN
        row := max_rows;
      END;
    END;

    page[row][col] := TRUE;

  END;
END Dot;

(* ----------------------------------------------------------------- *)
PROCEDURE Title( string :ARRAY OF CHAR );
BEGIN
   Output( '2 2 moveto ' );
   Output( '/Times-Roman findfont 10 scalefont setfont' );
   Flush;

   WriteChar( outf, '(' );
   FOR i := 0 TO LEN( string ) - 1 DO
     WriteChar( outf, string[i] );
   END;
   WriteChar( outf, ')' );
   WriteChar( outf, EOL );

   Output( 'show ' );
END Title;

BEGIN (* PSDots *)

   output_open := FALSE;

   FOR i := 0 TO 9 DO
      digits[i] := CHAR( i + 48 );
   END;

END PSDots.
