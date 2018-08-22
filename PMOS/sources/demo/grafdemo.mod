MODULE GrafDemo;

	(********************************************************)
	(*							*)
	(*		Test of module WGraph			*)
	(*		Draws some graphs			*)
	(*							*)
	(*  Programmer:		S.P. Lontis, P. Moylan.		*)
	(*  Last edited:	27 February 1995		*)
	(*  Status:		Working				*)
	(*	This was a copy of the Lontis version,		*)
	(*	but by now has been substantially modified.	*)
	(*							*)
	(********************************************************)

FROM GWindows IMPORT
    (* type *)	Window, BorderType,
    (* proc *)	WriteString, WriteLn, SetCursor,
		GString, GStringUp, Line, PutPixel, OpenWindow, CloseWindow,
		ClearWindow;

FROM WGraph IMPORT
    (* type *)	Graph, BarType, GraphType, LineType, MarkType,
    (* proc *)	OpenGraph, DrawLines, DrawAxes, DrawXAxis, DrawYAxis,
		AddPoint, BarGraph;

FROM MATHLIB IMPORT
    (* proc *)	Sin, Cos, Log, Exp, ATan;

FROM Timer IMPORT
    (* proc *)	Sleep;

(************************************************************************)

PROCEDURE Pause;

    BEGIN
	Sleep(1800);
    END Pause;

(************************************************************************)

PROCEDURE BodeDemo (w: Window);

    (* Plots a log-linear graph.	*)

    VAR G: Graph;
	i: [1..100];

    BEGIN
	OpenGraph(G,w,loglinear,joined,50,30,570,140,1.0,-20.0,100.0,0.0,none);
	ClearWindow(w);
	WriteString(w,'                   BODE DIAGRAM ');
	DrawAxes (G, 1.0, 0, 3, TRUE, 5.0, 0, 2, TRUE);
	GString (w, 200, 5, 'Frequency (Hz)');
	GStringUp (w, 10, 30, 'Magnitude (dB)');
	FOR i := 1 TO 100 BY 5 DO
	    IF (i<=20) THEN AddPoint (G, VAL(LONGREAL,i), 0.0)
	    ELSE
		AddPoint(G, VAL(LONGREAL,i), -0.25*VAL(LONGREAL,i-20))
	    END (* IF *)
	END (* FOR *);
	Pause;
    END BodeDemo;

(************************************************************************)

PROCEDURE TransferFunctionDemo;

    VAR w4, w5, w6, w7: Window;
	s1, s2, s3, s4: Graph;
	t, t1, t2: LONGREAL;

    BEGIN
	OpenWindow(w4,10,10,315,100,1,3,double);
	GString (w4, 100, 4, 'time (secs)');
	OpenGraph(s1,w4,linear,joined,40,30,275,70,0.0,0.0,0.5,10.0,none);
	DrawAxes (s1, 0.125, 0, 2, TRUE, 5.0, 0, 2, TRUE);

	OpenWindow(w5,325,10,630,100,1,3,double);
	WriteString(w5,'Transfer Function-');WriteLn(w5);WriteLn(w5);
	WriteString(w5,'   H(s) =   10 / ( s + 10) ');
	GString (w5, 100, 4, 'Re(H)');
	GStringUp (w5, 9, 20, 'Im(H)');
	OpenGraph(s4,w5,linear,dots,40,30,275,50,-20.0,0.0,0.0,5.0,cross);
	DrawAxes (s4, 10.0, 0, 2, TRUE, 5.0, 0, 2, TRUE);
	AddPoint (s4,-10.0,0.0);

	OpenWindow(w6,10,110,315,200,1,3,double);
	GString (w6, 100, 4, 'w (rad/sec)');
	GStringUp (w6, 9, 4, 'Mag H (dB)');
	OpenGraph (s2,w6,loglinear,joined,40,30,275,70,1.0,-50.0,100.0,0.0,none);
	DrawAxes (s2, 1.0, 0, 2, TRUE, 25.0, 0, 2, TRUE);

	OpenWindow(w7,325,110,630,200,1,3,double);
	GString (w7, 100, 4, 'w (rad/sec)');
	GStringUp (w7, 9, 20, 'Phase');
	OpenGraph (s3,w7,loglinear,joined,40,30,275,70,1.0,-90.0,1000.0,20.0,none);
	DrawAxes (s3, 1.0, 0, 2, TRUE, 45.0, 0, 2, TRUE);

	t := 0.0;  t1 := 1.0;  t2 := 1.0;

	WHILE (t<=0.5) OR (t1<=100.0) OR (t2<=1000.0) DO

	    AddPoint(s1,t,10.0*Exp(-10.0*t));
	    AddPoint(s2,t1,-10.0*Log(1.0+0.01*t1*t1));
	    AddPoint(s3,t2,-57.295*ATan(0.01*t2));
	    t := t + 0.025;
	    t1 := t1 + 5.0;
	    IF t2>100.0 THEN t2:=t2+100.0
	    ELSE t2:= t2 + 10.0
	    END (* IF *);

	END (* WHILE *);

	Pause;
	CloseWindow (w4);
	CloseWindow (w5);
	CloseWindow (w6);
	CloseWindow (w7);

    END TransferFunctionDemo;

(************************************************************************)

PROCEDURE LogLogDemo (w: Window);

    VAR i: INTEGER;
	G8: Graph;

    BEGIN
	OpenGraph(G8,w,loglog,joined,30,20,570,150,1.0,1.0,100.0,100.0,none);
	DrawAxes (G8, 1.0, 0, 3, TRUE, 1.0, 0, 3, TRUE);
	FOR i := 1 TO 100 BY 2 DO
	    IF i <= 8 THEN
		AddPoint (G8, VAL(LONGREAL,i), 1.0);
	    ELSE
		AddPoint (G8, VAL(LONGREAL,i), 10.0*Log(VAL(LONGREAL,i))-20.0)
	    END (* IF *)
	END (* FOR *);
	Pause;
    END LogLogDemo;

(************************************************************************)

PROCEDURE ScaleDemo;

    VAR a1, a2, a3, a4: Graph;
	w: Window;

    BEGIN
	OpenWindow (w, 55, 50, 580, 310, 14, 3, double);
	SetCursor (w, 7, 17);
	WriteString (w, 'Scales can be offset from the graph origin');
	OpenGraph(a1,w,linear,joined,100,60,400,250,0.0,0.0,5.0,5.0,none);
	OpenGraph(a2,w,linear,joined,100,60,400,250,-10.0,0.0,5.0,5.0,none);
	OpenGraph(a3,w,linear,joined,100,60,400,250,-5.0,-5.0,5.0,5.0,none);
	OpenGraph(a4,w,linear,joined,100,60,400,250,-5.0,-5.0,5.0,10.0,none);
	DrawAxes (a1, 1.0, 0, 2, TRUE, 1.0, 0, 2, TRUE);
	DrawXAxis (a2, 2.0, 15, 2, TRUE);
	DrawAxes (a3, 1.0, 30, 2, TRUE, 1.0, 28, 2, TRUE);
	DrawYAxis (a4, 1.0, 56, 2, TRUE);
	Pause;
	CloseWindow (w);
    END ScaleDemo;

(************************************************************************)

PROCEDURE RunTheDemo;

    (* Note that this procedure is badly designed (in the original	*)
    (* version, the entire demo was a single main program (!), and I	*)
    (* have not yet finished breaking it up into procedures.		*)

    VAR background, text, w3: Window;
	g1, g2, g3, g4, g5, g6: Graph;
	G4, G5, G6: Graph;
	i: [0..63];
	x: INTEGER;
	t: LONGREAL;
	qx, q1y, q2y, q3y: ARRAY [0..63] OF LONGREAL;

    BEGIN

	OpenWindow (background, 0, 0, 800, 500, 0, 7, single);
	OpenWindow (text, 15, 210, 620, 310, 14, 0, double);
	WriteString (text,'   Different types of graphs can be drawn. ');
	WriteLn (text);
	WriteString (text, '   For example:');  WriteLn(text);
	WriteString (text, '              Simple linear-linear Plots');       
	WriteLn (text);

	(* Straight-line graph: g1 in w3.	*)

	OpenWindow (w3, 25, 30, 610, 190, 2, 4, double);
	OpenGraph(g1,w3,linear,joined,30,20,570,150,0.0,0.0,10.0,10.0,none);
	DrawAxes(g1, 2.0, 0, 2, TRUE, 2.0, 0, 2, TRUE);
	FOR x := 0 TO 8 DO
	    AddPoint(g1, VAL(LONGREAL,x), VAL(LONGREAL,x-2))
	END (* FOR *);
	Pause;

	WriteString(text, '              with 5 different types of markers. ');
	WriteLn(text);

	(* Add a few more graphs in the same window.	*)

	OpenGraph(g2,w3,linear,joined,30,20,570,150,0.0,0.0,10.0,10.0,square);
	OpenGraph(g3,w3,linear,joined,30,20,570,150,0.0,0.0,10.0,10.0,cross);
	OpenGraph(g4,w3,linear,joined,30,20,570,150,0.0,0.0,10.0,10.0,plus);
	OpenGraph(g5,w3,linear,joined,30,20,570,150,0.0,0.0,10.0,10.0,diamond);

	FOR x := 0 TO 8 DO
	    AddPoint (g2, VAL(LONGREAL,x), VAL(LONGREAL,x-1));
	    AddPoint (g4, VAL(LONGREAL,x), VAL(LONGREAL,x+1));
	    AddPoint (g3, VAL(LONGREAL,x), VAL(LONGREAL,x));
	    AddPoint(g5, VAL(LONGREAL,x), VAL(LONGREAL,x+2));
	END (* FOR *);

	OpenGraph(G6,w3,linear,joined,30,20,570,150,0.0,0.0,10.0,10.0,none);
	OpenGraph(G5,w3,linear,joined,30,20,570,150,0.0,0.0,10.0,10.0,none);

	OpenGraph(G4,w3,linear,joined,30,20,570,150,0.0,0.0,10.0,10.0,none);

	t := 0.0;
	FOR i := 0 TO 63 DO
	    qx[i] := 4.0*Sin(t)+5.0;
	    q1y[i] := 4.0*Cos(t)+5.0;
	    q2y[i] := 4.0*Cos(3.0*t)+5.0;
	    q3y[i] := 4.0*Cos(5.0*t)+5.0;
	    t := t+0.1;
	END (* FOR *);

	Pause;
	ClearWindow(w3);
	WriteLn (text);
	WriteString(text,
		'   It is also possible to plot arrays of x and y values.');
	WriteLn(text);

	DrawAxes (G6, 2.0, 0, 2, TRUE, 2.0, 0, 2, TRUE);
	DrawLines(G6,qx,q1y); Pause; ClearWindow(w3);
	DrawAxes (G5, 2.0, 0, 2, TRUE, 2.0, 0, 2, TRUE);
	DrawLines(G5,qx,q2y); Pause; ClearWindow(w3);
	DrawAxes (G4, 2.0, 0, 2, TRUE, 2.0, 0, 2, TRUE);
	DrawLines(G4,qx,q3y); Pause;

	(* Demonstration of graphs with different scales.	*)

	ScaleDemo;

	ClearWindow(text);
	WriteString(text,'You can draw semi-log graphs ');

	BodeDemo (w3);

	WriteString(text,'and log-log graphs');
	ClearWindow(w3);
	LogLogDemo (w3);

	CloseWindow (w3);

	WriteLn(text);
	WriteString(text,'And of course you can have multiple graphs on the screen.');

	TransferFunctionDemo;

	WriteLn (text);
	WriteString (text,'End of graph demonstration');  Pause;
	CloseWindow (text);
	CloseWindow (background);

    END RunTheDemo;

(************************************************************************)

BEGIN
    RunTheDemo;
END GrafDemo.
