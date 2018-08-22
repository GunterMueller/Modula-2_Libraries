IMPLEMENTATION MODULE Bounce;

	(********************************************************)
	(*							*)
	(*		Bouncing ball demonstration.		*)
	(*							*)
	(*  This module is not intended to be of any practical	*)
	(*  use, but it tests the time delay functions of the	*)
	(*  kernel and acts as a demonstration that multiple	*)
	(*  tasks really can coexist.				*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	15 February 1994		*)
	(*  Status:		Working, behaviour not quite	*)
	(*			ideal.  For now, improvements	*)
	(*			probably not worth the effort.	*)
	(*							*)
	(********************************************************)

FROM TaskControl IMPORT
    (* proc *)	CreateTask;

FROM Timer IMPORT
    (* proc *)	Sleep;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteChar,
		SetCursor;

FROM UserInterface IMPORT
    (* type *)	UIWindow, Capability, CapabilitySet,
    (* proc *)	AllowMouseControl;

(************************************************************************)

VAR w: Window;
    debug: Window;

    (* The ball is confined inside window w.  The following four bounds	*)
    (* define where within the window it can go.			*)

    xmin, xmax, ymin, ymax: REAL;

    (* We must remember where the ball was, so we can delete it.	*)

    oldx, oldy: CARDINAL;

(************************************************************************)

PROCEDURE DisplayBall (x, y: REAL);

    (* Displays the ball at the given screen coordinates.	*)

    BEGIN
	SetCursor (w, oldy, oldx);  WriteChar (w, " ");
	oldx := CARDINAL(x+0.5);  oldy := CARDINAL(y+0.5);
	SetCursor (w, oldy, oldx);  WriteChar (w, CHR(9));
	WriteString (debug, " .. moved");
    END DisplayBall;

(************************************************************************)

PROCEDURE BouncingBall;

    (* This procedure runs as a separate task.	*)

    CONST delay = 40;
	  g = 0.1;  reflection = 0.79;

    VAR xposition, yposition, xspeed, yspeed: REAL;

    BEGIN
	oldx := 1;  oldy := 1;
	xposition := 2.0;  yposition := 2.0;
	xspeed := 1.0;  yspeed := 0.7;
	LOOP
	    (* Display current position *)
	    DisplayBall (xposition, yposition);
	    Sleep (delay);
	    (* Update position and speed *)
	    xposition := xposition + xspeed;
	    yposition := yposition + yspeed;
	    yspeed := yspeed + g;
	    (* Reverse direction if we hit a wall *)
	    IF xposition < xmin THEN
		xposition := 2.0*xmin - xposition;
		xspeed := -xspeed;
	    ELSIF xposition > xmax THEN
		xposition :=  2.0*xmax - xposition;
		xspeed := -xspeed;
	    END (*IF*);
	    IF yposition < ymin THEN
		yposition := 2.0*ymin - yposition;
		yspeed := -reflection*yspeed;
	    ELSIF yposition > ymax THEN
		yposition :=  2.0*ymax - yposition;
		yspeed := -reflection*yspeed;
	    END (*IF*);
	END (*LOOP*);
    END BouncingBall;

(************************************************************************)

PROCEDURE Bouncing (top, bottom: RowRange;  left, right: ColumnRange);

    (* Creates a screen window, and runs a bouncing ball demonstration	*)
    (* inside this window.						*)

    VAR UIW1, UIW2: UIWindow;

    BEGIN
	xmin := 0.5;  xmax := FLOAT(right - left) - 0.5;
	ymin := 0.5;  ymax := FLOAT(bottom - top) - 0.5;
	OpenWindow (w,yellow,red,top,bottom,left,right,simpleframe,nodivider);
	UIW1 := AllowMouseControl (w, "Bouncing ball",
				CapabilitySet {wmove, wshow, whide});
	OpenWindow (debug,yellow,red,20,24,0,39,simpleframe,nodivider);
	UIW2 := AllowMouseControl (debug, "Bounce/debug",
				CapabilitySet {wmove, wshow, whide});
	CreateTask (BouncingBall, 2, "Bounce");
    END Bouncing;

END Bounce.
