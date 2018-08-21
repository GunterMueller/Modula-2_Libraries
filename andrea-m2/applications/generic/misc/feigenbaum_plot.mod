MODULE FeigenbaumPlot;

(*
 This program will make a plot of a Feigenbaum Chaos diagram
 for output on a PostScript printer.
 The output file produced is always called CHAOS.PS

 Users input the dimensions of the plot, recommended values are displayed.

 Compile this program with no run time checks for best speed.
*)
(* J. Andrea, Nov.11/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT ReadLn, ReadReal, ReadCard, Read,
                  WriteLn, WriteString, WriteReal, WriteCard;
FROM PSDots IMPORT BeginPS, EndPS, Dot, GetDimensions, Title;
FROM Conversions IMPORT CardToString;
FROM RealInOut IMPORT ToFString;
FROM StringOperations IMPORT Append;

VAR
  range, i :CARDINAL;
  population, feedback :REAL;
  max_row, max_col :CARDINAL;
  real_max_row, real_max_col :REAL;
  right, left, top, bottom, xrange, yrange, xscale, yscale :REAL;
  visible, invisible :CARDINAL;
  
  title, string  :ARRAY [0..100] OF CHAR;

  (* ----------------------------------------------------------- *)
  PROCEDURE NewPop;
  VAR
    delta :REAL;
  BEGIN
      population := feedback * population * ( 1.0 - population );

      IF population > 1.0 THEN
        population := population - FLOAT( TRUNC( population ) );
      END;

  END NewPop;

BEGIN

WriteLn;
WriteString( 'recommended values are:' ); WriteLn;
WriteString( 'left=2.7 right=4 bottom=0 top=1 invisible=300 visible=150' ); WriteLn;

WriteLn;
WriteString( 'left ? ' ); ReadReal( left );
WriteString( 'right ? ' ); ReadReal( right );
WriteString( 'bottom ? ' ); ReadReal( bottom );
WriteString( 'top ? ' ); ReadReal( top );
WriteLn;

WriteString( 'invisible ? ' ); ReadCard( invisible );
WriteString( 'visible ? ' ); ReadCard( visible );

xrange := right - left;
yrange := top   - bottom;

GetDimensions( max_col, max_row );

xscale := FLOAT( max_col ) / xrange;
yscale := FLOAT( max_row ) / yrange;

WriteString( 'the output file will be CHAOS.PS' ); WriteLn;

BeginPS( 'chaos.ps' );

title := ' ';
Append( 'left=', title );
     ToFString( left, 3, 1, string );  Append( string, title );
Append( ' right=', title );
     ToFString( right, 3, 1, string ); Append( string, title );
Append( ' bottom=', title );
     ToFString( bottom, 3, 1, string ); Append( string, title );
Append( ' top=', title );
     ToFString( top, 3, 1, string );   Append( string, title );
Append( ' invisible=', title );
     CardToString( invisible, 0, string ); Append( string, title );
Append( ' visible=', title );
     CardToString( visible, 0, string ); Append( string, title );
Title( title );

FOR range := 0 TO max_col DO
   feedback   := left + FLOAT( range ) / xscale;
   population := 0.3;

   FOR i := 0 TO invisible DO
      NewPop;
   END;

   FOR i := 0 TO visible DO
      Dot( TRUNC( ( feedback   - left )   * xscale ),
           TRUNC( ( population - bottom ) * yscale ) );
      NewPop;
   END;

END;

EndPS;

END FeigenbaumPlot.
