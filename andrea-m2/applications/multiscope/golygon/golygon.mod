MODULE Golygon;

(* 
 Find all the golygons of a given length,
 see Mathematical Recreations, by. A.K. Dewdney in
     Scientific American, July 1990
 and Serial Isogons of 90 Degrees
     by Lee Sallows and Martin Gardner, Richard Guy and Donald Knuth
     Mathematics Magazine (in press).

 A golygon is a polygon with n sides, each side being one unit longer
 then the previous side, and every two sides are attached at a right angle.

 This program uses a backtracking algorithm to compute all possible golygons
 of a given number of sides, running in 2^(n-1) time. To find all the 
 golygons of length 32 will take approximately 21 hours on a 2 mip computer,
 after trying more than 2 billion configurations.

 This program will generate solutions which are mirror images, i consider
 these to be unique, which is not considered in Dewdney's article.

 4 sides = 0 solutions, 8 sides = 2 solutions,
 16 sides = 16 solutions, 24 sides = 4216 solutions,
 64 sides = 2 * 127,674,038,970,623 solutions (from Knuth)

 Golygons only exist with the number of sides as a multiple of 8.

 This program display's the solutions as a list of directions North, South,
 East, West. The first side is if length 1.
*)
(* John Andrea, Nov.24/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM VGAGraphics IMPORT BeginVGA, EndVGA, Move, Line, EraseScreen,
			GetDimensions, TextColor;
FROM NumberConversion IMPORT CardToString;
FROM MoreMath IMPORT MinCard, MaxCard;
FROM Break IMPORT EnableBreak;
(* FROM Keyboard IMPORT KeyPressed; *)

CONST
  max = 32;

TYPE
  Directions = (north,south,east,west);

VAR
  i     :CARDINAL;
  list  :ARRAY [1..max] OF Directions;
  number_solutions, tries, start, stop :CARDINAL;
  initial_dir :Directions;
  max_row, max_col :CARDINAL;
  continue :BOOLEAN;
   
  (* ------------------------------------------------- *)
  PROCEDURE KeyPressed():BOOLEAN;
  BEGIN
    RETURN FALSE;
  END KeyPressed;

  (* ------------------------------------------------- *)
  PROCEDURE DrawIt( len :CARDINAL );

  VAR 
    i           :CARDINAL;
    x, y, min_x, max_x, min_y, max_y, s :INTEGER;
    range_x, range_y :CARDINAL;
    screen_x, screen_y :CARDINAL;
    factor           :CARDINAL;

        (* ------------------------------------------------- *)
        PROCEDURE Vector( length, angle :CARDINAL );
        BEGIN
           IF angle = 0 THEN
             screen_x := screen_x + length;
           ELSIF angle = 90 THEN
             screen_y := screen_y + length;
           ELSIF angle = 180 THEN
             screen_x := screen_x - length;
           ELSIF angle = 270 THEN
             screen_y := screen_y - length;
           END;
           Line( screen_x, screen_y );
        END Vector;

        (* ------------------------------------------------- *)
        PROCEDURE Title;
        VAR string :ARRAY [1..10] OF CHAR;
        BEGIN
           Move( 20, 500 );
           CardToString( len, string, 0 );
           TextColor( string, 3 );
           
           Move( 200, 500 );
           CardToString( number_solutions, string, 0 );
           TextColor( string, 3 );
        END Title;

        (* ------------------------------------------------- *)
        PROCEDURE FindOffset( min, max :INTEGER ) :INTEGER;
        VAR range, center :INTEGER;
        BEGIN
           range  := max - min;
           center := min + range DIV 2;
           RETURN - center;
        END FindOffset;

        (* ------------------------------------------------- *)
        PROCEDURE ScreenCenter( max :CARDINAL; offset :INTEGER ) :CARDINAL;
        VAR center, diff :CARDINAL;
        BEGIN

          center := max DIV 2;

          diff := factor * CARDINAL( ABS( offset ) );

          IF offset < 0 THEN
            center := center - diff;
          ELSE
            center := center + diff;
          END;

          RETURN center;
        END ScreenCenter;

  BEGIN

    (* compute the bounding box of the golygon *)
    x := 0; y := 0;    min_x := 0; max_x := 0;    min_y := 0; max_y := 0;
      
    FOR i := 1 TO len DO
       s := INTEGER( i );
      
       IF list[i] = north THEN
         y := y + s;
       ELSIF list[i] = south THEN
         y := y - s;
       ELSIF list[i] = east THEN
         x := x + s;
       ELSE
         x := x - s;
       END;
      
       IF x < min_x THEN min_x := x END;
       IF x > max_x THEN max_x := x END;
       IF y < min_y THEN min_y := y END;
       IF y > max_y THEN max_y := y END;
      
     END;
             
     range_x := CARDINAL( max_x - min_x + 1 );
     range_y := CARDINAL( max_y - min_y + 1 );
      
     (* the screen is much bigger than any golygon side *)
     (* so find a scale factor *)
     factor := MinCard( max_row, max_col ) DIV MaxCard( range_x, range_y );
      
     (* so the actual location of 0,0 is the same relative offset from *)
     (* the center of the screen *)
     screen_x := ScreenCenter( max_col, FindOffset( min_x, max_x ) );
     screen_y := ScreenCenter( max_row, FindOffset( min_y, max_y ) );
      
     EraseScreen;   Title;
      
     (* move to the center of the screen *)
      
     Move( screen_x, screen_y );
      
     FOR i := 1 TO len DO
        IF list[i] = north THEN
          Vector( i*factor, 90 );
        ELSIF list[i] = south THEN
          Vector( i*factor, 270 );
        ELSIF list[i] = east THEN
          Vector( i*factor, 0 );
        ELSE
          Vector( i*factor, 180 );
        END;
     END;

   END DrawIt;

    (* ---------------------------------------------------- *)
    PROCEDURE TrySolve( nsides :CARDINAL; prev_dir :Directions;
                        sides :CARDINAL; x, y :INTEGER );
    VAR
        s :INTEGER;
    BEGIN

       IF KeyPressed() THEN
         continue := FALSE;
       ELSE
       IF sides > nsides THEN
         tries := tries + 1;
         (* have we returned to the origin too *)
         IF ( x = 0 ) & ( y = 0 ) & ( prev_dir # initial_dir ) THEN
           number_solutions := number_solutions + 1;
           DrawIt( nsides );
         END;
       ELSE

         s := INTEGER( sides );

         (* make another side, trying each of the directions *)

         IF ( prev_dir = east ) OR ( prev_dir = west ) THEN
           list[sides] := north;  TrySolve( nsides, north, sides+1, x, y+s );
           list[sides] := south;  TrySolve( nsides, south, sides+1, x, y-s );
         ELSE
           list[sides] := east;  TrySolve( nsides, east, sides+1, x+s, y );
           list[sides] := west;  TrySolve( nsides, west, sides+1, x-s, y );
         END;

       END;
       END;
    END TrySolve;

BEGIN EnableBreak;
  start := 4;  stop := 32;
  
  GetDimensions( max_col, max_row );

  initial_dir := north;

  (* always head north, first *)
  list[1] := initial_dir;

  BeginVGA;

  continue := TRUE;
  i := start;
  WHILE continue & ( i <= stop ) DO  

    number_solutions := 0;
    TrySolve( i, initial_dir, 2, 0, 1 );

    i := i + 4;
  END;

  EndVGA;
  
END Golygon.