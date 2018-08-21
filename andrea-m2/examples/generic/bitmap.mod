MODULE TestBitmap;

(* Produce a dragon curve to show how to use the bitmaps module *)
(* J. Andrea, Apr.2/92 *)

FROM BitmapOperations IMPORT Bitmap, Build, Clear, Put, PrintSixel, Line;

CONST
  rows = 200;  cols = 400;

VAR
  b :Bitmap;
  ok :BOOLEAN;
  x, y, side, angle :CARDINAL;

  (* --------------------------------- *)
  PROCEDURE Right;
  BEGIN
     angle := angle + 90;
     IF angle >= 360 THEN
       angle := angle - 360;
     END;
  END Right;

  (* --------------------------------- *)
  PROCEDURE Forward;
  VAR
    oldx, oldy :CARDINAL;

    (* -------------------------------- *)
    PROCEDURE Next( direction :CHAR; old, min, max :CARDINAL ) :CARDINAL;
    (* don't grow off the ends *)
    VAR new :CARDINAL;
    BEGIN

      IF direction = '+' THEN

        IF max - old >= side THEN
          new := old + side;
        ELSE
          new := max;
        END;

      ELSE

        IF old - min >= side THEN
          new := old - side;
        ELSE
          new := min;
        END;

      END;

      RETURN new;
    END Next;

  BEGIN

    oldx  := x;   oldy  := y;

    IF    angle =   0 THEN
      x := Next( '+', oldx, 1, cols );
    ELSIF angle =  90 THEN
      y := Next( '+', oldy, 1, rows );
    ELSIF angle = 180 THEN
      x := Next( '-', oldx, 1, cols );
    ELSE (* 270 *)
      y := Next( '-', oldy, 1, rows );
    END;

    Line( b, oldy, oldx, y, x, 1 );

  END Forward;

  (* --------------------------------- *)
  PROCEDURE Dragon( depth :INTEGER );
  BEGIN

     IF depth = 0 THEN
       Forward;
     ELSE

       IF depth > 0 THEN

         Dragon( + (depth - 1) );
         Right;
         Dragon( - (depth - 1) );

       ELSE

         Dragon( - (depth + 1) );
         Right; Right; Right;
         Dragon( + (depth + 1) );

       END;

     END;

  END Dragon;

BEGIN

  x := cols DIV 2;             (* start in the middle *)
  y := rows DIV 2;

  angle := 90;                 (* initial angle *)

  side := 10;                  (* how many pixels to move every line *)


  Build( b, rows, cols ); Clear( b );     (* create the bitmap *)

  Dragon( 8 );                            (* draw the curve *)

  PrintSixel( b, "dragon.sixel", ok );    (* put it into a file *)

END TestBitmap.
