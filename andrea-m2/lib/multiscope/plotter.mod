IMPLEMENTATION MODULE Plotter;
(*/NOWARN*)
FROM LewertsScale IMPORT Scale;
FROM VGAGraphics IMPORT BeginVGA, EndVGA,
   			EraseScreen, GetDimensions,
   			Move, LineColor, Text;
FROM CardArrays IMPORT CardArray, Build, Destroy, Put, Get;
FROM Terminal IMPORT Read;
 
  (* ---------------------------------------- *)
  PROCEDURE Plot( title :ARRAY OF CHAR; x, y :ARRAY OF REAL; n :CARDINAL );

  VAR
    x_min, x_max, y_min, y_max,
    x_scale_min, x_scale_max,
    y_scale_min, y_scale_max,
    x_scale_inc, y_scale_inc :REAL;

    i, j  :CARDINAL;
    index :CardArray;

    (* ------------------------------------------ *)
    PROCEDURE SetScale( min, max :REAL;
                        VAR scale_min, scale_max, scale_inc :REAL );
    VAR
      tenth :REAL;
    BEGIN
      IF min = max THEN
        (* its flat, so fake it *)
        tenth := min / 10.0;
        min := min - tenth;
        max := max + tenth; 
      END;

      (* get nice scaling parameters *)
      Scale( min, max, 7, scale_min, scale_max, scale_inc );
    END SetScale;
    
  BEGIN
  
    Build( index, 0, n-1 ); (* create a temporary array for sorting index *)
    
    XYSort( x, y, n, index );
    
    x_min := x[Get(index,0)];  x_max := x[Get(index,n-1)];
    
    (* but we must run over the array to get y range *)
    y_min := y[Get(index,0)];  y_max := y[Get(index,0)];
    FOR i := 1 TO n-1 DO
      j := Get( index, i );
      IF y[j] < y_min THEN y_min := y[j] END;
      IF y[j] > y_max THEN y_max := y[j] END;
    END;
    
    SetScale( x_min, x_max, x_scale_min, x_scale_max, x_scale_inc );
    SetScale( y_min, y_max, y_scale_min, y_scale_max, y_scale_inc );
    
    VGAPlot( title, x, y, n, index,
             x_scale_min, x_scale_max, y_scale_min, y_scale_max,
             x_scale_inc, y_scale_inc );
            
    Destroy( index ); (* all done, get rid of the temporary index *)
    
  END Plot;

  (* ---------------------------------------------------- *)
  PROCEDURE VGAPlot( title :ARRAY OF CHAR;
                     x, y :ARRAY OF REAL; n :CARDINAL; index :CardArray;
                     x_min, x_max, y_min, y_max, x_inc, y_inc :REAL );

  CONST
     tic_len = 11;
       
  VAR
    x_size, y_size :REAL;
    i, j           :CARDINAL; 
    z              :REAL;
    c		   :CHAR;

    x_range, y_range, x_scale, y_scale :REAL;
    x_pos, y_pos		       :REAL;

  BEGIN

    EraseScreen;
        
    GetDimensions( i, j );
    x_size := FLOAT( i );
    y_size := FLOAT( j );

    x_range := x_max - x_min;
    x_scale := x_size / x_range;

    y_range := y_max - y_min;
    y_scale := y_size / y_range;

    BeginVGA;
      
    (* draw the axis *)
    Move( 0, 0 ); LineColor( TRUNC( x_size ), 0, 12 );
    Move( 0, 0 ); LineColor( 0, TRUNC( y_size ), 12 );
      
    (* put in the tick marks *)
      
    (* run along the x axis, putting in the numbers *)
    (* run along the y axis, putting in the numbers *)
    (* now draw the actual data points *)

    (* go to the first data point *)

    i := 0;
    j := Get( index, i );
    x_pos := ( x[j] - x_min ) * x_scale;
    y_pos := ( y[j] - y_min ) * y_scale;
    Move( TRUNC(x_pos), TRUNC(y_pos) );

    FOR i := 1 TO n-1 DO

       j := Get( index, i );
       x_pos := ( x[j] - x_min ) * x_scale;
       y_pos := ( y[j] - y_min ) * y_scale;
               
       LineColor( TRUNC(x_pos), TRUNC(y_pos), 14 );

    END;

    Move( 200, 200 ); Text( title );

    Read( c );
    EndVGA;
      
  END VGAPlot;
    
  (* ----------------------------------------------------- *)
  PROCEDURE XYSort( x, y :ARRAY OF REAL; array_len :CARDINAL;
		    index :CardArray );
      
  CONST
    max_stack = 20;
    n_small   = 6; (* use a simple sort for this size and smaller *)

  VAR
    L, R, i, j, k, n :INTEGER;

    tos            :CARDINAL;
    Lstack, Rstack :ARRAY [1..max_stack] OF INTEGER;

    median :REAL;

    (* -------------------------------- *)
    PROCEDURE Swap( i, j :CARDINAL );
    VAR  ctemp :CARDINAL;
    BEGIN
      ctemp := Get(index,i);
      Put( index, i, Get(index,j) );
      Put( index, j, ctemp );
    END Swap;
    
  BEGIN

    n := INTEGER( array_len ) - 1; (* back to zero offset *)

    (* initialize the index *)
    FOR i := 0 TO n DO
       Put( index, i, i );
    END;

    tos := 0;

    L := 0;  R := n;

    (* PUSH very first set *)
    tos := tos + 1;  Lstack[tos] := L;  Rstack[tos] := R;

    REPEAT

      (* POP *)
      L := Lstack[tos];  R := Rstack[tos];  tos := tos - 1;

      IF R - L + 1 > n_small THEN

        REPEAT
          i := L;  j := R;    median := x[Get(index,( L + R ) DIV 2)];

          REPEAT
            WHILE x[Get(index,i)] < median DO
              i := i + 1;
            END;
            WHILE median < x[Get(index,j)] DO
              j := j - 1;
            END;

            IF i <= j THEN
              Swap( i, j );
              i := i + 1;  j := j - 1;
            END;
          UNTIL i > j;

          IF j - L < R - i THEN
            IF i < R THEN (* PUSH *)
              tos := tos + 1;  Lstack[tos] := i;  Rstack[tos] := R;
            END;
            R := j;
          ELSE
            IF L < j THEN (* push *)
              tos := tos + 1;  Lstack[tos] := L;  Rstack[tos] := j;
            END;
            L := i;
          END;

        UNTIL L >= R;

     ELSE

       (* small sort for small number of values *)
       FOR i := L TO R - 1 DO
          FOR j := i TO R DO
             IF x[Get(index,i)] > x[Get(index,j)] THEN
               Swap( i, j );
             END;
          END;
       END;

     END; (* check for small *)

   UNTIL tos = 0;
 
   (* now look for duplicates of x, and sort those by y *)
        
   k := 1; (* start at second element *)
   WHILE k <= n DO

     IF x[Get(index,k)] = x[Get(index,k-1)] THEN

       L := k - 1;    (* first of the duplicated x's *)

       (* search for more duplictes *)
       j := k;
       WHILE ( j <= n ) & ( x[Get(index,L)] = x[Get(index,j)] ) DO
         j := j + 1;
       END;
            
       R := j - 1;   (* last of the duplicated x's *)

       (* now sort between first and last by Y *)
       FOR i := L TO R - 1 DO
          FOR j := i TO R DO
             IF y[Get(index,i)] > y[Get(index,j)] THEN
               Swap( i, j );
             END;
          END;
       END;

       (* and advance the current index to the last of the duplicates *)
       (* because all these values have been examined *)

       k := R;
            
     END;
          
     k := k + 1;
   END;

 END XYSort;


BEGIN
END Plotter.
