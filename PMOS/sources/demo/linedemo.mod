MODULE LineDemo;

	(********************************************************)
	(*							*)
	(*		Demonstration of GWindows		*)
	(*							*)
	(*  Programmer:		S. Lontis, P. Moylan		*)
	(*  Last edited:	23 January 1994			*)
	(*  Status:		Working				*)
	(*							*)
	(*	Note: This is an EGA demonstration.  I've	*)
	(*	done some scaling to make it work with other	*)
	(*	screen resolutions, but I lost patience for	*)
	(*	covering the completely general case.		*)
	(*							*)
	(********************************************************)

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateTask, CreateLock, Obtain, Release;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM ScreenGeometry IMPORT
    (* type *)	Point;

FROM Graphics IMPORT
    (* type *)	ColourType,
    (* proc *)	GetScreenShape;

FROM GWindows IMPORT
    (* type *)	Window, BorderType,
    (* proc *)	OpenWindow, Line, PutPixel2, WriteString, ClearWindow,
		CloseWindow, WindowMemory, SetColour, InitGraphics;

FROM MATHLIB IMPORT
    (* proc *)	Sin, Cos, Exp;

FROM Keyboard IMPORT
    (* proc *)	InKey;

FROM Timer IMPORT
    (* proc *)	Sleep;

(************************************************************************)

CONST pi = 3.14159;

TYPE Vector2 = RECORD
		    x, y: LONGREAL;
	       END;

VAR
    Xmax, Ymax, CharHeight: CARDINAL;
    maxcolour: ColourType;

(************************************************************************)

(*
PROCEDURE Sleep (time: CARDINAL);

    (* Dummy procedure to prevent Timer.Sleep from being called.	*)
    (* Needed only during debugging.					*)

    BEGIN
    END Sleep;
*)

(************************************************************************)
(*		    "PRESS ANY KEY TO CONTINUE"				*)
(************************************************************************)

PROCEDURE Pause;

    VAR dummy: CHAR;  w: Window;

    BEGIN
	OpenWindow(w,Xmax DIV 2,30,Xmax DIV 2 + 205,40+CharHeight,0,3,single);
	WriteString (w, "Press any key to continue");
	dummy := InKey();
	CloseWindow (w);
    END Pause;

(************************************************************************)
(*			FLOATING POINT ROUND				*)
(************************************************************************)

PROCEDURE ROUND (x: LONGREAL): INTEGER;

    VAR negative: BOOLEAN;  result: INTEGER;

    BEGIN
	negative := FALSE;
	IF x < 0.0 THEN
	    x := -x;  negative := TRUE;
	END (*IF*);
	x := x + 0.5;
	IF x >= VAL(LONGREAL, MAX(INTEGER)) THEN result := MAX(INTEGER)
	ELSE result := VAL (INTEGER, x);
	END (*IF*);
	IF negative THEN RETURN -result
	ELSE RETURN result;
	END (*IF*);
    END ROUND;

(************************************************************************)

PROCEDURE Round2 (fpoint: Vector2): Point;

    (* Converts floating point (x,y) coordinates to cardinal.	*)

    VAR result: Point;

    BEGIN
	result.x := ROUND (fpoint.x);
	result.y := ROUND (fpoint.y);
	RETURN result;
    END Round2;

(************************************************************************)
(*			   CRO SIMULATION				*)
(************************************************************************)

VAR
    (* Data shared between the DataGenerator and CROdemo tasks. *)

    ToPlot: RECORD
		ready: Semaphore;
		access: Lock;
		datum: LONGREAL;
	    END (*RECORD*);

(************************************************************************)

PROCEDURE DataGenerator;

    (* Runs as a separate task, generating data for task CROdemo to plot.*)

    CONST tincr = 0.01;  M0 = 0.8;
	  alpha = 5.0;  omega = 15.0;

    VAR t, value, M: LONGREAL;

    BEGIN
	t := 0.0;  M := M0;
	LOOP
	    value := M*(1.0 - 2.0*Exp(-alpha*t)*Cos(omega*t));
	    WITH ToPlot DO
		Obtain (access);
		datum := value;
		Release (access);
		Signal (ready);
	    END (*WITH*);
	    t := t + tincr;
	    IF t > 1.0 THEN
		t := 0.0;  M := -M;
	    END (*IF*);
	    Sleep (120);
	END (*LOOP*);
    END DataGenerator;

(************************************************************************)

TYPE index = [10..190];

VAR data: ARRAY index OF CARDINAL;

PROCEDURE CROdemo;

    (* Runs as a separate task, plotting a moving waveform *)

    (* Colours on the default palette are:				*)
    (*	 0 black	 1 blue		 2 green	 3 cyan		*)
    (*	 4 red		 5 magenta	 6 brown	 7 white	*)
    (*	 8 grey		 9 lt blue	10 lt green	11 lt cyan	*)
    (*	12 lt red	13 lt magenta	14 yellow	15 very white	*)

    CONST colour = 10;

    VAR CRO: Window;  t: index;
	newval: LONGREAL;  scaledval: INTEGER;
	background: CARDINAL;

    PROCEDURE Blob (x, y: INTEGER);
	BEGIN
	    PutPixel2 (CRO, x, y);
	    PutPixel2 (CRO, x+1, y);
	    PutPixel2 (CRO, x, y+1);
	    PutPixel2 (CRO, x+1, y+1);
	    PutPixel2 (CRO, x, y-1);
	    PutPixel2 (CRO, x+1, y-1);
	END Blob;

    (********************************************************************)

    BEGIN
	IF maxcolour > 8 THEN background := 8
	ELSE background := 0
	END (*IF*);
	OpenWindow (CRO,Xmax-200,Ymax-CharHeight-140,Xmax,Ymax-CharHeight-10,
						0,background,single);
	WindowMemory (CRO, FALSE);
	WITH ToPlot DO
	    CreateSemaphore (ready, 0);
	    CreateLock (access);
	END (*WITH*);
	FOR t := MIN(index) TO MAX(index) DO
	    PutPixel2 (CRO, t, 30);
	    data[t] := 30;
	END (*FOR*);
	CreateTask (DataGenerator, 4, "Data for CRO");
	t := MIN(index);
	LOOP
	    WITH ToPlot DO
		Wait (ready);
		Obtain (access);
		newval := datum;
		Release (access);
	    END (*WITH*);
	    scaledval := 60 + VAL(INTEGER,30.0*newval);

(* Original version: non-shifting:

	    SetColour (CRO, background);
	    PutPixel2 (CRO, t, data[t]);
	    SetColour (CRO, colour);
	    PutPixel2 (CRO, t, scaledval);
	    data[t] := scaledval;
	    IF t = MAX(index) THEN t := MIN(index) ELSE INC(t) END(*IF*);
*)
	    (* New version: shift data in window *)

	    FOR t := MIN(index) TO MAX(index)-1 DO
		SetColour (CRO, background);
		PutPixel2 (CRO, t, data[t]);
		data[t] := data[t+1];
		SetColour (CRO, colour);
		PutPixel2 (CRO, t, data[t]);
	    END (*FOR*);
	    t := MAX(index);
	    SetColour (CRO, background);
	    Blob (t, data[t]);
	    data[t] := scaledval;
	    SetColour (CRO, 15);
	    Blob (t, scaledval);
	END (*LOOP*);
	CloseWindow (CRO);
    END CROdemo;

(************************************************************************)
(*			MISCELLANEOUS DEMO PLOTS			*)
(************************************************************************)

PROCEDURE Spiral (w: Window; centre: Point;
				radius, ang: LONGREAL; n: CARDINAL);

    CONST thinc = 0.05*pi;

    VAR theta, r: LONGREAL;
	i, ptnumber: CARDINAL;
	pt, pt1: Point;

    BEGIN
	theta := ang;  pt := centre;
	ptnumber := 40*n;
	FOR i := 1 TO ptnumber DO
	    theta := theta+thinc;  r:= radius*LONGREAL(i)/LONGREAL(ptnumber);
	    pt1.x := ROUND(3.0*LONGREAL(r)*Cos(theta)) + centre.x;
	    pt1.y := ROUND(2.0*LONGREAL(r)*Sin(theta)) + centre.y;
	    Line (w, pt, pt1);
	    pt := pt1;
	END (* FOR *)
    END Spiral;

(************************************************************************)

PROCEDURE Rose(w: Window; xoffset, yoffset: INTEGER;
				scale, m, n: CARDINAL);

    VAR inner, outer: ARRAY [1..100] OF Point;
	i, j: CARDINAL;
	r, theta, thinc: LONGREAL;

    BEGIN
	thinc := 2.0*pi/LONGREAL(n);
	FOR i := 1 TO n DO
	    inner[i].x := xoffset;
	    inner[i].y := yoffset;
	END (* FOR *);
	FOR j := 1 TO m DO
	    theta := -LONGREAL(j)*pi/LONGREAL(n);
	    r:=LONGREAL(j*scale)/LONGREAL(m);
	    FOR i := 1 TO n DO
		theta := theta+thinc;
		outer[i].x := xoffset + ROUND (3.0*LONGREAL(r)*Cos(theta));
		outer[i].y := yoffset + ROUND (2.0*LONGREAL(r)*Sin(theta));
	    END (* FOR *);
	    FOR i:= 1 TO n DO
		Line (w, outer[i], outer[(i MOD n) + 1]);
		Line (w, outer[(i MOD n) + 1], inner[i]);
		Line (w, inner[i], outer[i]);
		inner[i] := outer[i];
	    END (* FOR *);
	END (* FOR *)
    END Rose;

(************************************************************************)

PROCEDURE SquareinSquare (w: Window; xoffset, yoffset, size: LONGREAL);

    VAR pt, ptd: ARRAY[1..4] OF Vector2;
	i, j, nextj: INTEGER;
	mu, um: LONGREAL;
	pti: Point;

    BEGIN
	pt[1].x:=  xoffset+size*2.5; pt[1].y:=  yoffset+size*2.0;
	pt[2].x:=  xoffset+size*2.5; pt[2].y:=  yoffset;
	pt[3].x:=  xoffset; pt[3].y:=   yoffset;
	pt[4].x:=  xoffset; pt[4].y:=   yoffset+size*2.0;
	mu:= 0.1; um:= 1.0-mu;
	FOR i:= 1 TO 21 DO
	    pti := Round2 (pt[4]);
	    FOR j:= 1 TO 4 DO
		Line(w, Round2 (pt[j]), pti);
		pti := Round2 (pt[j]);
		nextj:= (j MOD 4) + 1;
		ptd[j].x:= um*pt[j].x + mu*pt[nextj].x;
		ptd[j].y:= um*pt[j].y + mu*pt[nextj].y
	    END (* FOR *);
	    FOR j:= 1 TO 4 DO  pt[j]:= ptd[j]   END (* FOR *)
	END (* FOR *)
    END SquareinSquare;

(************************************************************************)

PROCEDURE SpiroGraph(w: Window; xcenter, ycenter: INTEGER;
					a, b, d: CARDINAL);

    CONST Scale= 6.0;  steps = 50;

    VAR pt, oldpt: Point;
	i: CARDINAL;
	Xscale, phi, theta, thinc: LONGREAL;

    (********************************************************************)

    PROCEDURE hcf (i, j: CARDINAL) : CARDINAL;

	VAR remain: CARDINAL;

	BEGIN
	    REPEAT
		remain := i MOD j;
		i := j;  j := remain;
	    UNTIL remain = 0;
	    RETURN i;
	END hcf;

    (********************************************************************)

    BEGIN
	Xscale := 1.6*Scale;
	theta := 0.0;  thinc:= 2.0*pi/LONGREAL(steps);
	oldpt.x := INTEGER (xcenter) + ROUND(Xscale*LONGREAL(a - b + d));
	oldpt.y := ycenter;
	FOR i := 1 TO steps*(b DIV hcf(a,b)) DO
	    theta:= theta + thinc;  phi:= theta*LONGREAL(a-b)/LONGREAL(b);
	    pt.x := xcenter+ROUND(Xscale*(LONGREAL(a-b)*Cos(theta)
				+ LONGREAL(d)*Cos(phi)));
	    pt.y := ycenter+ROUND(Scale*(LONGREAL(a-b)*Sin(theta)
				- LONGREAL(d)*Sin(phi)));
	    Line (w, pt, oldpt);
	    oldpt := pt;
	END (* FOR *)
    END SpiroGraph;

(************************************************************************)

PROCEDURE StringPoly (w: Window; xcentre, ycentre, radius, n: CARDINAL);

    (* Draws a polygon of n vertices, with every vertex connected to	*)
    (* every other.							*)

    VAR pt: ARRAY[1..100] OF Point;
	i, j: CARDINAL;
	theta, thinc: LONGREAL;

    BEGIN
	theta:=0.0; thinc:= 2.0*pi/LONGREAL(n);
	FOR i := 1 TO n DO
	    pt[i].x := INTEGER(xcentre) + ROUND(LONGREAL(3*radius-20)*Cos(theta));
	    pt[i].y := INTEGER(ycentre) + ROUND(LONGREAL(2*radius)*Sin(theta));
	    theta:= theta + thinc;
	END (* FOR *);
	FOR i:= 1 TO n-1 DO
	    FOR j:= i+1 TO n DO
		Line (w, pt[i], pt[j]);
	    END (* FOR *)
	END (* FOR *);
    END StringPoly;

(************************************************************************)
(*			THE MAIN DEMONSTRATION TASK			*)
(************************************************************************)

PROCEDURE RunTheDemo;

    VAR w1, w2, w3, w4, w5: Window;
	i: [5..15];  b, c: [1..4];
	point: Point;
	xc, yc: CARDINAL;
	radius: LONGREAL;

    BEGIN
	OpenWindow(w5,15,Ymax-CharHeight-8,275,Ymax,1,3,single);
	WriteString (w5,' Demonstration of line graphics');

	IF Xmax > 400 THEN
	    xc := Xmax-201;
	    CreateTask (CROdemo, 3, "CRO demo");
	ELSE
	    xc := 3*Xmax DIV 4;
	END (*IF*);
	yc := 7*Ymax DIV 8;
	OpenWindow(w1,0,0,xc,yc,7,6,single);

	WriteString (w1,'Spiral');
	xc := xc DIV 2;  yc := yc DIV 2;
	IF xc DIV 3 < yc DIV 2 THEN
	    radius := 0.3*LONGREAL(xc);
	ELSE
	    radius := 0.5*LONGREAL(yc);
	END (*IF*);
	point.x := xc;  point.y := yc;
	Spiral (w1, point, radius, 0.0, 8);
	Sleep(2000);

	FOR i:= 5 TO 15 BY 2 DO
	    ClearWindow (w1);
	    WriteString (w1, 'Rose Pattern');
	    Rose (w1,xc,yc,CARDINAL(radius),i,i);
	    Sleep (1000);
	END (* FOR *);

	xc := xc DIV 2;
	yc := yc DIV 2;
	radius := 0.5*radius;
	OpenWindow(w4,Xmax DIV 3,Ymax DIV 3,
		Xmax DIV 3+2*xc,Ymax DIV 3+2*yc,0,3,double);
	StringPoly (w4,xc,yc,CARDINAL(radius),17);
	Sleep(1000);

	OpenWindow(w2,320,40,620,180,4,7,single);

	WriteString (w2,' Rotating Squares');
	SquareinSquare (w2, 125.0, 20.0, 50.0);
	SquareinSquare (w2, 15.0, 30.0, 40.0);
	Sleep(1000);

	OpenWindow(w3,20,20,240,180,14,6,double);
	FOR b := 2 TO 3 DO
	    ClearWindow(w3);
	    WriteString (w3,'SpiroGraphs');
	    FOR c := 2 TO 4 DO
		SpiroGraph(w3,110,70,9,b,c);
		Sleep(1000);
	    END (* FOR *)
	END (* FOR *);

	Pause;

	Sleep(1000);  CloseWindow (w1);
	Sleep(1000);  CloseWindow (w2);
	Sleep(1000);  CloseWindow (w3);
	Sleep(1000);  CloseWindow (w4);
	Sleep(1000);  CloseWindow (w5);

    END RunTheDemo;

(************************************************************************)

BEGIN
    (*InitGraphics(16);*)
    GetScreenShape (Xmax, Ymax, maxcolour, CharHeight);
    RunTheDemo;
END LineDemo.
