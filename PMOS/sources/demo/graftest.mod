MODULE GrafTest;

	(****************************************************************)
	(*								*)
	(*		Test of music and graphics.			*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	7 March 1994				*)
	(*  Status:		Working					*)
	(*								*)
	(****************************************************************)

FROM Timer IMPORT
    (* proc *)	Sleep;

FROM MusicDemonstration IMPORT
    (* proc *)	WaitForEndOfMusic;

FROM Graphics IMPORT
    (* proc *)	SetMode, PlotMark, PlotLine;

FROM MATHLIB IMPORT
    (* proc *)	Sin, Cos, Sqrt, ATan;

(************************************************************************)

CONST colour = 1;
      stretch = 2.0;
      PI = LONGREAL(3.14159265358979323);

(************************************************************************)

PROCEDURE PlotScaledLine (x0, y0, x1, y1: LONGREAL);

    (* Draws a line from (x0,y0) to (x1,y1), with coordinates scaled to	*)
    (* the standards expected by procedure Triangle, below.		*)

    CONST xorg = 159.5;  yorg = 99.5;  scale = 99.5;

    VAR intx0, inty0, intx1, inty1: CARDINAL;

    BEGIN
	intx0 := TRUNC (stretch*(xorg+scale*x0)+0.5);
	inty0 := TRUNC (yorg+scale*y0+0.5);
	intx1 := TRUNC (stretch*(xorg+scale*x1)+0.5);
	inty1 := TRUNC (yorg+scale*y1+0.5);
	PlotLine (intx0,inty0,intx1,inty1,colour);
    END PlotScaledLine;

(************************************************************************)

PROCEDURE Triangle (x0, y0, side, theta, alpha: LONGREAL);

    (* Plots an equilateral triangle, starting at (x0, y0), with each	*)
    (* side being "side" units long, and tilted at angle theta to the	*)
    (* the horizontal.  The units are scaled such that the centre of	*)
    (* the screen is point (0.0,0.0), and side=1.0 gives the biggest	*)
    (* triangle which will fit in the first quadrant.  Part of the	*)
    (* second side is deliberately omitted - only the first "alpha" of	*)
    (* it (0<alpha<1) is plotted.					*)

    VAR x1, y1, x2, y2: LONGREAL;

    BEGIN
	x1 := x0 + side*Cos(theta);  x2 := x0 + side*Cos(theta + PI/3.0);
	y1 := y0 + side*Sin(theta);  y2 := y0 + side*Sin(theta + PI/3.0);
	PlotScaledLine (x0,y0, x1,y1);
	PlotScaledLine (x1,y1,(1.0-alpha)*x1+alpha*x2,(1.0-alpha)*y1+alpha*y2);
	PlotScaledLine (x2,y2, x0,y0);
    END Triangle;

(************************************************************************)

PROCEDURE FilledTriangle (k: CARDINAL);

    (* Plots a sequence of nested triangles, with the outer one		*)
    (* oriented at 60k degrees from the horizontal.			*)

    CONST alpha = 0.1;  small = 0.01;

    VAR xorigin, yorigin, side: LONGREAL;
	theta: LONGREAL;

    BEGIN
	xorigin := 0.0;  yorigin := 0.0;  side := 1.0;
	theta := LONGREAL(k)*PI/3.0;
	REPEAT
	    Triangle (xorigin, yorigin, side, theta, alpha);
	    xorigin := xorigin + alpha*side*Cos(theta);
	    yorigin := yorigin + alpha*side*Sin(theta);
	    theta := theta + ATan(alpha*Sqrt(3.0)/(2.0-3.0*alpha));
	    side := side * Sqrt(1.0 - 3.0*alpha*(1.0-alpha));
	UNTIL side < small;
    END FilledTriangle;

(************************************************************************)

PROCEDURE Hexagon;

    (* Plots an interesting hexagon shape. *)

    VAR j: [0..5];

    BEGIN
	SetMode (16, TRUE);
	FOR j := 0 TO 5 DO
	    FilledTriangle(j);
	END (*FOR*);
	Sleep (3000);
    END Hexagon;

(************************************************************************)

PROCEDURE Doyley (vertices: CARDINAL);

    (* Plots a polygon, with lines between all pairs of vertices.	*)
    (* Assumption: "vertices" is a prime number.  If not, the procedure	*)
    (* still does something but the resulting picture is not as		*)
    (* interesting.							*)

    CONST colour = 1;
	  stretch = 2.0;

    TYPE Subscript = [0..30];

    VAR previous, next: Subscript;
	step, j: CARDINAL;
	X, Y: ARRAY Subscript OF CARDINAL;
	xcentre, ycentre, radius, theta, angle: LONGREAL;

    BEGIN
	SetMode (16, TRUE);

	(* Set up the vertex coordinates in arrays X and Y. *)

	xcentre := 160.0;  ycentre := 99.5;  radius := 99.5;
	theta := 2.0*PI/LONGREAL(FLOAT(vertices));
	FOR j := 0 TO vertices-1 DO
	    angle := LONGREAL(j)*theta;
	    X[j] := TRUNC (stretch*(xcentre + radius*Cos(angle) + 0.5));
	    Y[j] := TRUNC (ycentre + radius*Sin(angle) + 0.5);
	END (*FOR*);

	(* Now the actual plotting. *)

	FOR step := 1 TO vertices DIV 2 DO
	    previous := 0;
	    FOR j := 1 TO vertices DO
		next := (previous + step) MOD vertices;
		    PlotLine (X[previous], Y[previous], X[next], Y[next],
								colour);
		previous := next;
	    END (*FOR*)
	END (*FOR*);

	Sleep (3000);
    END Doyley;

(************************************************************************)

PROCEDURE RunGraphicsTest;

    (* Runs us through a sequence of graphics tests.	*)

    VAR N: [3..23];

    BEGIN
	FOR N := 3 TO 23 BY 2 DO
	    Doyley (N);
	END (*FOR*);
	Hexagon;
    END RunGraphicsTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    RunGraphicsTest;
    RunGraphicsTest;
    WaitForEndOfMusic;
END GrafTest.
