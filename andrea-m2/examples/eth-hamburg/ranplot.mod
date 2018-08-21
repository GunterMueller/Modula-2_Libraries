MODULE TestRanPlot;

(* Test the plotter routines and the random number generation *)

(* J. Andrea, Oct.13/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn, WriteReal;
FROM Plotter IMPORT Plot;
FROM Randomly IMPORT NormalRandom;

CONST
  n_bins =  200;
  n      = 2000;
  
VAR
  i, j :CARDINAL;
  bottom, middle, top, z, delta :REAL;
  x, y :ARRAY [1..n_bins] OF REAL;
   
BEGIN

  bottom := 100.0;
  middle := 300.0;
  top    := 500.0;
  
  FOR i := 1 TO n_bins DO
    y[i] := 0.0;
  END;
  delta := ( top - bottom ) / FLOAT( n_bins );
  x[1] := bottom;
  FOR i := 2 TO n_bins DO
    x[i] := x[i-1] + delta;
  END;
  WriteString( 'smallest bin: ' ); WriteReal( x[1], 7 ); WriteLn;
  WriteString( 'biggest  bin: ' ); WriteReal( x[n_bins], 7 ); WriteLn;
  
  FOR i := 1 TO n DO
    z := NormalRandom( middle, 40.0 );
    IF z <= bottom THEN
      y[1] := y[1] + 1.0;
    ELSE
      IF z >= top THEN
        y[n_bins] := y[n_bins] + 1.0;
      ELSE
      
        j := 1;
        WHILE z > x[j] DO
           j := j + 1;
        END;
        y[j] := y[j] + 1.0;
        
      END;
    END;
  END;
   
  Plot( 'test.regis', 'test plot', x, y, n_bins );
  
END TestRanPlot.
