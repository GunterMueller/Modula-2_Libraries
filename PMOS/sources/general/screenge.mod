IMPLEMENTATION MODULE ScreenGeometry;

	(********************************************************)
	(*							*)
	(*	    Support module for screen graphics		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	10 October 1993			*)
	(*  Status:		Working				*)
	(*							*)
	(*	Might be worth looking for a better algorithm	*)
	(*	for SolveForX and SolveForY - see module	*)
	(*	RawGraphics, where I think I've done it better.	*)
	(*							*)
	(********************************************************)

(************************************************************************)

PROCEDURE Inside (x, y: INTEGER;  R: Rectangle): BOOLEAN;

    (* Returns TRUE iff point (x,y) is in (or on the border of) R.	*)

    BEGIN
	WITH R DO
	    RETURN (x >= left) AND (x <= right)
				AND (y >= bottom) AND (y <= top);
	END (*WITH*);
    END Inside;

(************************************************************************)

PROCEDURE Adjacent (R1, R2: Rectangle;
				VAR (*OUT*) union: Rectangle): BOOLEAN;

    (* If the union of R1 and R2 is itself a rectangle, returns TRUE	*)
    (* and sets "union" to be the combined rectangle.  Otherwise	*)
    (* returns FALSE, and the "union" result is meaningless.		*)

    BEGIN
	union := R1;
	IF R1.top = R2.top THEN
	    (* Possible left/right adjacency *)
	    IF R1.bottom = R2.bottom THEN
		IF R2.left = R1.right + 1 THEN
		    union.right := R2.right;
		    RETURN TRUE;
		ELSIF R1.left = R2.right + 1 THEN
		    union.left := R2.left;
		    RETURN TRUE;
		ELSE
		    RETURN FALSE;
		END (*IF*);
	    ELSE
		RETURN FALSE;
	    END (*IF*);
	ELSIF R1.left = R2.left THEN
	    (* Possible above/below adjacency *)
	    IF R1.right = R2.right THEN
		IF R2.bottom = R1.top + 1 THEN
		    union.top := R2.top;
		    RETURN TRUE;
		ELSIF R1.bottom = R2.top + 1 THEN
		    union.bottom := R2.bottom;
		    RETURN TRUE;
		ELSE
		    RETURN FALSE;
		END (*IF*);
	    ELSE
		RETURN FALSE;
	    END (*IF*);
	ELSE
	    RETURN FALSE;
	END (*IF*);
    END Adjacent;

(************************************************************************)

PROCEDURE SolveForX (end1, end2: Point;  y: INTEGER): INTEGER;

    (* Returns the x value for which the line with endpoints end1 and	*)
    (* end2 passes through the point (x,y).				*)

    VAR x1, x2, y1, y2, temp: LONGINT;

    BEGIN
	x1 := LONGINT(end1.x);  y1 := LONGINT(end1.y);
	x2 := LONGINT(end2.x);  y2 := LONGINT(end2.y);
	IF y1 > y2 THEN
	    temp := x1;  x1 := x2;  x2 := temp;
	    temp := y1;  y1 := y2;  y2 := temp;
	END (*IF*);
	RETURN INTEGER ((LONGINT(y)*(x2-x1)
			 - x2*y1 + x1*y2 + (y2-y1) DIV 2 + 1)
						DIV (y2-y1));
    END SolveForX;

(************************************************************************)

PROCEDURE SolveForY (end1, end2: Point;  x: INTEGER): INTEGER;

    (* Returns the y value for which the line with endpoints end1 and	*)
    (* end2 passes through the point (x,y).				*)

    VAR x1, x2, y1, y2, temp: LONGINT;

    BEGIN
	x1 := LONGINT(end1.x);  y1 := LONGINT(end1.y);
	x2 := LONGINT(end2.x);  y2 := LONGINT(end2.y);
	IF x1 > x2 THEN
	    temp := x1;  x1 := x2;  x2 := temp;
	    temp := y1;  y1 := y2;  y2 := temp;
	END (*IF*);
	RETURN INTEGER ((LONGINT(x)*(y2-y1) - y2*x1
				 + y1*x2 + (x2-x1) DIV 2 + 1)
							DIV (x2-x1));
    END SolveForY;

(************************************************************************)

PROCEDURE TrimLine (VAR (*INOUT*) end1, end2: Point;  R: Rectangle): BOOLEAN;

    (* Modifies end1 and end2, if necessary, to cut off the ends of	*)
    (* the line from end1 to end2 which do not fit in R.		*)
    (* Returns FALSE if none of the line passes through the rectangle.	*)

    VAR temp: Point;  result: INTEGER;

    BEGIN

	(* Check the bottom and top of the rectangle.	*)

	IF end1.y > end2.y THEN
	    temp := end1;  end1 := end2;  end2 := temp;
	END (*IF*);

	(* Exclude some special cases, to avoid overflow problems.	*)

	IF (end2.y < R.bottom) OR (end1.y > R.top) THEN
	    RETURN FALSE;
	END (*IF*);

	IF end1.y < R.bottom THEN
	    result := SolveForX (end1, end2, R.bottom);
	    IF (result < R.left) OR (result > R.right) THEN
		RETURN FALSE;
	    END (*IF*);
	    end1.x := result;
	    end1.y := R.bottom;
	END (*IF*);
	IF end2.y > R.top THEN
	    result := SolveForX (end1, end2, R.top);
	    IF (result < R.left) OR (result > R.right) THEN
		RETURN FALSE;
	    END (*IF*);
	    end2.x := result;
	    end2.y := R.top;
	END (*IF*);

	(* Check the left and right of the rectangle.	*)

	IF end1.x > end2.x THEN
	    temp := end1;  end1 := end2;  end2 := temp;
	END (*IF*);

	(* Exclude some special cases, to avoid overflow problems.	*)

	IF (end2.x < R.left) OR (end1.x > R.right) THEN
	    RETURN FALSE;
	END (*IF*);

	IF end1.x < R.left THEN
	    result := SolveForY (end1, end2, R.left);
	    IF (result < R.bottom) OR (result > R.top) THEN
		RETURN FALSE;
	    END (*IF*);
	    end1.y := result;
	    end1.x := R.left;
	END (*IF*);
	IF end2.x > R.right THEN
	    result := SolveForY (end1, end2, R.right);
	    IF (result < R.bottom) OR (result > R.top) THEN
		RETURN FALSE;
	    END (*IF*);
	    end2.y := result;
	    end2.x := R.right;
	END (*IF*);

	RETURN TRUE;

    END TrimLine;

END ScreenGeometry.
