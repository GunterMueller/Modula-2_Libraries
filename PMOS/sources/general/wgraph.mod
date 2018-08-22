IMPLEMENTATION MODULE WGraph;

	(********************************************************)
	(*							*)
	(*		Graph module For GWindows.		*)
	(*							*)
	(*  Programmer:		S.P.Lontis, P. Moylan.		*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:						*)
	(*	Conversion to TS version still not complete.	*)
	(*							*)
	(*	This started as a copy of the Lontis version,	*)
	(*	but by now I've modified it a fair bit.		*)
	(*							*)
	(*	Have converted the formatting to my standards,	*)
	(*	but still need to work through things like	*)
	(*	the program logic, confusing names, etc.	*)
	(*							*)
	(*	The algorithm for labelling logarithmic scales	*)
	(*	looks unsatisfactory to me.			*)
	(*							*)
	(********************************************************)

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateLock, Obtain, Release;

FROM MATHLIB IMPORT
    (* proc *)	Log;

FROM Conversions IMPORT
    (* proc *)	LongRealToF, atoi;

FROM GWindows IMPORT
    (* proc *)	Line2, PutPixel2, GString;

(************************************************************************)

TYPE
    str = ARRAY[0..10] OF CHAR;   (* string type *)

    (********************************************************************)
    (*									*)
    (* A Graph is implemented as a pointer to a record with fields:	*)
    (*	Win		the window in which the graph is written	*)
    (*	GT		graph type: linear, bar, etc.			*)
    (*	xs, ys		the bottom left corner of the rectangle in	*)
    (*			which the graph is written (in window-relative	*)
    (*			coordinates					*)
    (*	xe, ye		the top right corner of the rectangle		*)
    (*	xscale, yscale	scale factors used to convert the real user	*)
    (*			data into the screen ranges (xs..xe), (ys..ye)	*)
    (*	minx, maxx	the range of x values that can be plotted	*)
    (*	miny, maxy	the range of y values than can be plotted	*)
    (*	lastx, lasty	the last point added to the graph		*)
    (*	LT		line type: dots, joined, vertical, normal	*)
    (*	MT		marker type: diamond, plus, etc.		*)
    (*	NextGraph	used to hold all the graphs together in a	*)
    (*			linked list.  I'm not yet sure why.		*)
    (*	found		set to TRUE when the first point is added to	*)
    (*			the graph					*)
    (*									*)
    (********************************************************************)

    Graph = POINTER TO GraphInstance;
    GraphInstance = RECORD
			Win: Window;
			GT: GraphType;
			xs, ys, xe, ye: CARDINAL;
			xscale, yscale,
			minx, maxx,
			miny, maxy: LONGREAL;
			lastx, lasty: INTEGER;
			LT: LineType;
			MT: MarkType;
			NextGraph: Graph;
			found: BOOLEAN;
		    END (* RECORD *);

(************************************************************************)

VAR
    (* Lock to protect critical sections.  I presume that the		*)
    (* only critical section problem is with the linked list of graphs.	*)
    (* The only reason for linking all graphs together is for the	*)
    (* CloseDown procedure.						*)

    GraphAccess: Lock;

    (* Reference to the head of the linked list *)

    TopGraph: Graph;

(************************************************************************)
(*			MISCELLANEOUS UTILITIES				*)
(*									*)
(*  N.B. It's not clear why most of these should be needed.  I'll have	*)
(*  to look more carefully at how scales are labelled.			*)
(*									*)
(************************************************************************)

PROCEDURE TenToPower(n: INTEGER): LONGREAL;

    (* Returns the value 10^n.	*)	

    BEGIN
	RETURN atoi (10.0, n);
    END TenToPower;

(************************************************************************)

PROCEDURE Scale (VAR (*INOUT*) mantissa: LONGREAL;
			VAR (*INOUT*) exponent: INTEGER;
			power: CARDINAL;  lower, upper: LONGREAL);

    (* Adjusts mantissa so that lower <= mantissa < upper, while	*)
    (* keeping the quantity (mantissa * 10^exponent) invariant. To save	*)
    (* some calculation, the caller must ensure that upper = 10^power	*)
    (* and lower = 10^(-power).						*)

    BEGIN
	WHILE mantissa >= upper DO
	    INC (exponent, power);  mantissa := lower*mantissa;
	END (* WHILE *);

	WHILE mantissa < lower DO
	    DEC (exponent, power);  mantissa := upper*mantissa;
	END (* WHILE *)

    END Scale;

(************************************************************************)

PROCEDURE FindExponent (number: LONGREAL;  VAR (*OUT*) exponent: INTEGER);

    (* Separates the first argument into a mantissa and exponent part,	*)
    (* so that  number = mantissa * 10^exponent.  Returns the exponent.	*)

    VAR mantissa: LONGREAL;

    BEGIN
	IF number = 0.0 THEN exponent := 1
	ELSE
	    mantissa := number;  exponent := 0;
	    Scale (mantissa, exponent, 64, 1.0E-64, 1.0E64);
	    Scale (mantissa, exponent, 16, 1.0E-16, 1.0E16);
	    Scale (mantissa, exponent, 4, 1.0E-4, 1.0E4);
	    Scale (mantissa, exponent, 1, 1.0E-1, 1.0E1);
	END (* IF *);

	(* Not sure why the following modification is included. But	*)
	(* then I'm not sure either why this procedure is needed!	*)

	IF (exponent<0) THEN DEC(exponent, 1) END (* IF *)

    END FindExponent;

(************************************************************************)

PROCEDURE RealToStr (x: LONGREAL;  decplace: CARDINAL;
			VAR (*OUT*) string: ARRAY OF CHAR;
			VAR (*OUT*) count: CARDINAL);

    (* Converts the Real value x into a string; decplace specifies	*)
    (* the maximum number of places to allow after the decimal point.	*)
    (* On return, count gives the number of characters used.		*)

    BEGIN
	count := SIZE (string);
	LongRealToF (x, count, decplace, TRUE, string);
    END RealToStr;

(************************************************************************)

PROCEDURE PrintLogScale (G: Graph;  xaxis: BOOLEAN;  incr: LONGREAL;
					offset, Numbits: CARDINAL);

    (* Draws a labelled logarithmic scale.  The logic of this		*)
    (* procedure is still quite obscure to me, and I'm still in the	*)
    (* process of changing the unhelpful variable names and trying to	*)
    (* put in a few comments.						*)
    (* The labelling happens only at multiples of 10.  Depending on the	*)
    (* minimum values and increment, and depending also on roundoff	*)
    (* error, the labelling might not be done at all.  This seems to me	*)
    (* to be a fairly unsatisfactory situation, which perhaps calls for	*)
    (* a completely different algorithm.				*)

    VAR exponent: INTEGER;  xy, xybase, count: CARDINAL;
	value, z, k, step, max, temp1, logmin, scale: LONGREAL;
	label: str;
	w: Window;

    BEGIN
	WITH G^ DO
	    IF xaxis THEN
		value := minx;  max := maxx;
		logmin := Log(minx);  scale := xscale;
		xybase := xs;
	    ELSE
		value := miny;  max := maxy;
		logmin := Log(miny);  scale := yscale;
		xybase := ys;
	    END (*IF*);
	    w := Win;
	END (*WITH*);

	FindExponent (value, exponent);

	(* Compute z := 10^exponent	*)

	IF exponent >= 0 THEN
	    z := TenToPower(exponent)
	ELSE
	    z := 1.0 / TenToPower(-exponent)
	END (* IF *);

	(* Initially step = incr*10^exponent and k = 10.0*10^exponent.	*)
	(* As we go around the loop step and k are updated together, so	*)
	(* we have the invariant step = (incr/10.0) * k.		*)

	step := incr*z;  k := 10.0*z;

	(* Beyond this point the variable z is used only once, in a	*)
	(* comparison.  Its value is not altered.  The value of k is	*)
	(* however altered each time around the loop.			*)

	WHILE value <= max DO

	    temp1 := Log(value) - logmin;

	    (* logmin is not used beyond this point.	*)

	    (* Compute xy := xybase + scale*Log(value/min)	*)

	    xy := xybase + VAL(CARDINAL, scale*temp1 + 0.5);

	    (* Put the tick mark.	*)

	    WITH G^ DO
		IF xaxis THEN
		    Line2 (w, xy, ys-offset, xy, ys-offset-2)
		ELSE
		    Line2 (w, xs-offset-2, xy, xs-offset, xy)
		END (*IF*);
	    END (*WITH*);

	    (* Note k >= z always, so depending on the relationship	*)
	    (* between value and z (which is not yet clear) it is	*)
	    (* possible that the comparison below can be simplified.	*)
	    (* What is clear is that we are putting the labelling only	*)
	    (* when value is a multiple of 10.  By the way, the		*)
	    (* equality comparison is somewhat untrustworthy since it	*)
	    (* is sensitive to rounding error.				*)

	    IF (value = k) OR (value = z) THEN
		RealToStr (value, Numbits, label, count);
		IF xaxis THEN
		    GString (w, xy-4*count, G^.ys-offset-13, label);
		ELSE
		    GString (w, G^.xs-8*count-6-offset, xy-4, label);
		END (* IF *)
	    END (* IF *);

	    (* Update the step each time we pass a multiple of 10.	*)

	    IF value >= k THEN
		step := 10.0*step;  k := 10.0*k;
	    END (* IF *);

	    value := value + step;

	END (*WHILE*);

    END PrintLogScale;

(************************************************************************)

PROCEDURE PrintLinearScale (G: Graph;  xaxis: BOOLEAN;  incr: LONGREAL;
			offset, Numbits: CARDINAL;  WriteNumbers: BOOLEAN);

    (* Prints a linear scale for the graph, with incr distance between	*)
    (* the markers.  Numbers are written iff WriteNumbers is TRUE.	*)
    (* This procedure draws the tick marks and (if specified) the	*)
    (* labelling, but not the actual axis line.				*)

    VAR value, limit, nextmark, start, scale: LONGREAL;
	xy, xybase, NumOfChar: CARDINAL;
	text: str;

    BEGIN
	WITH G^ DO

	    IF xaxis THEN
		limit := maxx;  start := minx;
		scale := xscale;  xybase := xs;
	    ELSE
		limit := maxy;  start := miny;
		scale := yscale;  xybase := ys;
	    END (* IF *);
	    value := start;

	    WHILE value <= limit DO
	
		(* Find the position of the next marker point on the	*)
		(* axis i.e.  nextmark := (value-start)*scale.		*)

		nextmark := value - start;
		xy := xybase + VAL(CARDINAL, scale*nextmark + 0.5);

		IF xaxis THEN
		    Line2 (Win, xy, ys-offset, xy, ys-offset-2)
		ELSE
		    Line2 (Win, xs-offset-3, xy, xs-offset, xy)
		END (* IF *);

		IF WriteNumbers THEN

		    (* Convert value to string *)

		    RealToStr (value, Numbits, text, NumOfChar);

		    (* Display the value under or beside the marker.	*)

		    IF xaxis THEN
			GString (Win, xy-4*NumOfChar, ys-offset-13, text);
		    ELSE
			GString (Win, xs-offset-8*NumOfChar-6, xy-4, text);
		    END (*IF*);

		END (* IF *);
	
		value := value + incr;

	    END (* WHILE *)

	END (* WITH *)

    END PrintLinearScale;

(************************************************************************)

PROCEDURE BarGraphMarkers (G: Graph;  yoffset: CARDINAL);

    (* Draws the tick marks on the X axis of a bar graph.  The axis is	*)
    (* a distance yoffset screen points below the graph y origin.	*)

    VAR temp: LONGREAL;
	xcurrent, increment: CARDINAL;

    BEGIN
	WITH G^ DO

	    xcurrent:= xs + VAL(CARDINAL, 0.5*minx + 0.5);
	    increment := VAL(CARDINAL, maxx + minx + 0.5);

	    WHILE xcurrent <= xe DO
		Line2 (Win, xcurrent, ys-yoffset, xcurrent, ys-yoffset-3);
		INC (xcurrent, increment)
	    END (* WHILE *)

	END (*WITH*);

    END BarGraphMarkers;

(************************************************************************)

PROCEDURE DrawXAxis (G: Graph;  xinc: LONGREAL;  yoffset, DecPlaces: CARDINAL;
						numbering: BOOLEAN);

    (* Draws the x-axis for a graph; xinc is the distance between tick	*)
    (* marks, and yoffset gives the distance that the axis will lie	*)
    (* below the line y=ys (ys is defined in OpenGraph).  The option	*)
    (* yoffset <> 0 allows for the case where more than one graph	*)
    (* occupies the same window, since the graphs may have different	*)
    (* scales.  DecPlaces gives the number of decimal places that will	*)
    (* be used.  If numbering is FALSE, the axis is drawn but not	*)
    (* labelled.							*)

    BEGIN
	WITH G^ DO
	
	    (* Draw x-axis line *)

	    Line2 (Win, xs, ys-yoffset, xe, ys-yoffset);

	    IF GT = bar THEN
		BarGraphMarkers (G, yoffset);
	    ELSIF GT = linear THEN
		PrintLinearScale (G,TRUE,xinc,yoffset,DecPlaces,numbering)
	    ELSIF (GT=loglog) OR (GT=loglinear) THEN
		PrintLogScale (G, TRUE, xinc, yoffset, DecPlaces)
	    END (* IF *);

	END (* WITH *)

    END DrawXAxis;

(************************************************************************)

PROCEDURE DrawYAxis (G: Graph;  yinc: LONGREAL;  xoffset, DecPlaces: CARDINAL;
						numbering: BOOLEAN);

    (* Draws the y-axis for a graph; yinc is the space between tick	*)
    (* marks, and xoffset gives the distance that the axis will lie to	*)
    (* the left of the line x=xs (xs is defined in OpenGraph).		*)
    (* DecPlaces gives the number of decimal places that will be used.	*)
    (* If numbering is FALSE, the axis is drawn but not labelled.	*)

    BEGIN
	WITH G^ DO

	    Line2 (Win, xs-xoffset, ys, xs-xoffset, ye);

	    IF (GT=linear) OR (GT=loglinear) OR (GT=bar) THEN
		PrintLinearScale (G, FALSE, yinc, xoffset,
						DecPlaces, numbering)
	    ELSIF GT=loglog THEN
		PrintLogScale (G, FALSE, yinc, xoffset, DecPlaces)
	    END (* IF *);

	END (* WITH *);

    END DrawYAxis;

(************************************************************************)

PROCEDURE DrawAxes (G: Graph;
	xinc: LONGREAL;  yoffset, xdecplaces: CARDINAL;  numberx: BOOLEAN;
	yinc: LONGREAL;  xoffset, ydecplaces: CARDINAL;  numbery: BOOLEAN);

    (* Draws both the X and Y axes.  See the definitions of		*)
    (* DrawXAxis and DrawYAxis above.					*)

    BEGIN
	WITH G^ DO
	    DrawXAxis (G, xinc, yoffset, xdecplaces, numberx);
	    DrawYAxis (G, yinc, xoffset, ydecplaces, numbery);
	END (* WITH *)
    END DrawAxes;

(************************************************************************)

PROCEDURE OpenGraph (VAR (*OUT*) G: Graph;  w: Window;  GrphType: GraphType;
		    LnType: LineType; xst, yst, xen, yen: CARDINAL;
		    xmin, ymin, xmax, ymax: LONGREAL;  Mark: MarkType);

    (* Initialises a Graph, adding it to the top of the Graph stack.	*)

    VAR num: INTEGER;

    BEGIN
	NEW(G);
	WITH G^ DO
    
	    (* Add Graph to start of the stack *)

	    Obtain (GraphAccess);
	    NextGraph := TopGraph;
	    TopGraph := G;
	    Release (GraphAccess);
	    Win := w;

	    (* Set up graph variables *)

	    GT := GrphType;  LT := LnType;
	    minx := xmin;  miny := ymin;
	    maxx := xmax;  maxy := ymax;
	    xs := xst;  ys := yst;
	    xe := xen;  ye := yen;
	    MT := Mark;
	    found := FALSE;

	    (* Calculate the mapping values xscale and yscale. *)

	    IF GT <> bar THEN
		IF (GT<>loglinear) AND (GT<>loglog) THEN

		    (* xscale = Float(xe-xs) / (maxx-minx)	*)

		    xscale := VAL(LONGREAL, xe - xs) / (maxx - minx);

		ELSE

		    (* xscale:= (xe-xs)/(Log(maxx)-Log(minx)) *)
		    (* Used to be xscale:= (xe-xs-5)/(Log(maxx)-Log(minx)) *)

		    xscale := VAL(LONGREAL,xe-xs) / (Log(maxx) - Log(minx));

		END (* IF *);
	    END (* IF *);

	    IF GT <> loglog THEN

		(* yscale = Float(ye-ys) / (maxy-miny)	*)

		yscale := VAL(LONGREAL,ye - ys) / (maxy - miny);

	    ELSE

		(* yscale:= (ye-ys)/(Log(maxy)-Log(miny)) *)

		yscale := VAL(LONGREAL,ye-ys) / (Log(maxy) - Log(miny));	

	    END (*IF*);

	END (*WITH*)

    END OpenGraph;

(*************************************************************************)

PROCEDURE DrawMarker (G: Graph;  x, y: INTEGER);

    (* Puts a marker at point (x,y).  The coordinates are window-relative. *)

    BEGIN

	WITH G^ DO
	    CASE MT OF
		none:	IF (LT=dots) THEN
			    PutPixel2 (Win, x, y)
			END (* IF *);
	      |
		square:	Line2 (Win, x-2, y+1, x+2, y+1);
			Line2 (Win, x+2, y+1, x+2, y-1);
			Line2 (Win, x+2, y-1, x-2, y-1);
			Line2 (Win, x-2, y-1, x-2, y+1);
	      |
		plus:	Line2 (Win, x, y+2, x, y-2);
			Line2 (Win, x-3, y, x+3, y);
	      |
		cross:	Line2 (Win, x-2, y+2, x+2, y-2);
			Line2 (Win, x+2, y+2, x-2, y-2);
	      |
		diamond: Line2 (Win, x, y+2, x+3, y);
			Line2 (Win, x+3, y, x, y-2);
			Line2 (Win, x, y-2, x-3, y);
			Line2 (Win, x-3, y, x, y+2);

	    END (* CASE *);
	END (* WITH *);

    END DrawMarker;

(***********************************************************************)

PROCEDURE XScaled (G: Graph;  x0: LONGREAL): CARDINAL;

    (* Convert the number x0 in the range  (xmin <= x <= xmax) to a	*)
    (* value that is relative to the graph's screen range (xs to xe).	*)

    (* USED ONLY BY PROCEDURE Scaled, BELOW.	*)

    VAR temp: LONGREAL;

    BEGIN
	WITH G^ DO
	    IF (GT=loglog) OR (GT=loglinear) THEN

		(* Use a log mapping. *)

		temp := xscale * (Log(x0) - Log(minx));

	    ELSE

		(* use a linear mapping *)

		temp := xscale * (x0 - minx);

	    END (* IF *);

	    RETURN xs + VAL(CARDINAL, temp+0.5);

	END (* WITH *)

    END XScaled;

(************************************************************************)

PROCEDURE YScaled (G: Graph;  y0: LONGREAL): CARDINAL;

    (* Convert the number y0 in the range (ymin <= y <= ymax) to a	*)
    (* value that is relative to the graph's screen range (ys to ye).	*)

    VAR temp: LONGREAL;

    BEGIN
	WITH G^ DO
	    IF (GT=loglog) THEN

		(* use a Log mapping *)

		temp := yscale * (Log(y0) - Log(miny));

	    ELSE

  		(* use a linear mapping *)

		temp := yscale * (y0 - miny);

	    END (* IF *);

	    RETURN ys + VAL(CARDINAL, temp+0.5);

	END (* WITH *)

    END YScaled;

(************************************************************************)

PROCEDURE Scaled (G: Graph;  VAR (*OUT*) xscaled, yscaled: CARDINAL;
							x0, y0: LONGREAL);

    (* Scale the values of (x0,y0). See definition above of XScaled	*)
    (* and YScaled. 							*)

    BEGIN
	xscaled := XScaled (G, x0);
	yscaled := YScaled (G, y0);
    END Scaled;

(************************************************************************)

PROCEDURE ClipPoint (G: Graph;  VAR (*INOUT*) x, y: LONGREAL);

    (* Lets the values saturate to the permissible ranges for G.	*)

    BEGIN
	WITH G^ DO
	    IF x < minx THEN x := minx
	    ELSIF x > maxx THEN x := maxx;
	    END (* IF *);
	    IF y < miny THEN y := miny
	    ELSIF y > maxy THEN y := maxy
	    END (* IF *);
	END (* WITH *)
    END ClipPoint;

(************************************************************************)

PROCEDURE AddPoint (G: Graph;  x, y: LONGREAL);

    VAR ix, iy: CARDINAL;

    BEGIN
	WITH G^ DO
  
	    (* Clip point (x,y) if necessary. *)

	    ClipPoint (G, x, y);

	    (* scale the point (x,y) *)

	    Scaled (G, ix, iy, x, y);

	    (* Display the scaled point (ix,iy) *)

	    CASE LT OF
		joined:	IF found THEN Line2 (Win, lastx, lasty, ix, iy)
			END (* IF *);
	      |
		dots:	;
	      |
		vertical: Line2 (Win, ix, iy, ix, ys);
	    END (* CASE *);
	    DrawMarker (G, ix, iy);

	    (* Remember the last point drawn *)

	    found := TRUE;  lastx := ix;  lasty := iy;

	END (* WITH *)

    END AddPoint;

(************************************************************************)

PROCEDURE DrawLines(G: Graph;  x, y: ARRAY OF LONGREAL);

    VAR i: INTEGER;

    BEGIN
	WITH G^ DO
	    FOR i:= 0 TO HIGH(x) DO
		AddPoint (G, x[i], y[i])
	    END (* FOR *)
	END (* WITH *)
    END DrawLines;

(************************************************************************)

PROCEDURE DrawLine (G: Graph; x0, y0, x1, y1: LONGREAL);

    (* Used for drawing discontinuous lines.  The line is drawn between	*)
    (* (x0,y0) and (x1,y1) relative to graph G.				*)

    VAR xstart, ystart, xend, yend: CARDINAL;
   
    BEGIN
	ClipPoint (G, x0, y0);
	ClipPoint (G, x1, y1);
	Scaled (G, xstart, ystart, x0, y0);
	Scaled (G, xend, yend, x1, y1);
	Line2 (G^.Win, xstart, ystart, xend, yend);
	DrawMarker (G, xstart, ystart);
	DrawMarker (G, xend, yend)
    END DrawLine;

(************************************************************************)

PROCEDURE BarGraph (G: Graph;  y: ARRAY OF LONGREAL;
					del: CARDINAL;  Bt: BarType);

    (* Displays a BarGraph of the vector y.  Parameter del gives the	*)
    (* number of stripes per bar, in the case of striped fill patterns.	*)

    VAR index, BarStartPt, BarEndPt, incr, y1, barinc, i: CARDINAL;
	temp2, incr2, barwidth, bargap: LONGREAL;

    BEGIN
	barwidth := G^.minx+0.5;	(* width of a bar		*)
	bargap := G^.maxx;		(* distance between two bars	*)

	barinc := VAL(CARDINAL, barwidth) DIV del;

	(* incr := bargap + barwidth  *)

	incr2 := bargap + barwidth;
	incr := VAL(CARDINAL, incr2);

	WITH G^ DO
	    BarStartPt := xs;
	    index := 0;
	    WHILE index <= HIGH(y) DO

		y1 := YScaled (G, y[index]);
		temp2 := VAL(LONGREAL,BarStartPt) + barwidth;
		BarEndPt := VAL(CARDINAL, temp2);

		(* Draw one Bar of the Bar Graph *)

		Line2 (Win, BarStartPt, ys, BarStartPt, y1);
		Line2 (Win, BarStartPt, y1, BarEndPt, y1);
		Line2 (Win, BarEndPt, y1, BarEndPt, ys);

		(* Add pattern to the Bar Graph *)

		IF (Bt<>Four) THEN
		    IF (Bt=Two) THEN
			Line2 (Win, BarStartPt, ys, BarEndPt, y1);
			Line2 (Win, BarEndPt, ys, BarStartPt, y1)
		    ELSE
			IF (Bt=One)OR(Bt=Five) THEN
			    i:= BarStartPt + barinc;
			    WHILE (i<BarEndPt) DO
				Line2 (Win, i, ys, i, y1);
				INC(i, barinc)
			    END (* WHILE *)
			END (* IF *);
			IF (Bt=Three)OR(Bt=Five) THEN
			    i := ys + barinc;
			    WHILE (i<y1) DO
				Line2 (Win, BarStartPt, i, BarEndPt, i);
				INC(i, barinc)
			    END (* WHILE *)
			END (* IF *)
		    END (* IF *)
		END (* IF *);

		INC (BarStartPt, incr); INC (index)

	    END (* WHILE *)
	END (* WITH *)

    END BarGraph;

(***********************************************************************)

PROCEDURE CloseGraphs;

    (* Destroys all Graphs Created. *)

    VAR following: Graph;

    BEGIN
	Obtain (GraphAccess);
	WHILE TopGraph <> NIL DO
	    following := TopGraph^.NextGraph;
	    DISPOSE (TopGraph);
	    TopGraph := following;
	END (*WHILE*);
	Release (GraphAccess);
    END CloseGraphs;

(***********************************************************************)

BEGIN
    TopGraph := NIL;
    CreateLock (GraphAccess);
    SetTerminationProcedure(CloseGraphs);
END WGraph.
