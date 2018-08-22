MODULE BarDemo;

	(********************************************************)
	(*							*)
	(*		Test of module WGraph			*)
	(*		Draws some bar graphs			*)
	(*							*)
	(*  Programmer:		S.P. Lontis, P. Moylan.		*)
	(*  Last edited:	27 January 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(*	This is a copy of the Lontis version,		*)
	(*	which I'm gradually modifying.			*)
	(*							*)
	(*	See comments inside the code.  This is		*)
	(*	basically working, but I'm still fiddling with	*)
	(*	the details.					*)
	(*							*)
	(********************************************************)

FROM Graphics IMPORT
    (* proc *)	GetScreenShape;

FROM GWindows IMPORT
    (* type *)	Window, BorderType,
    (* proc *)	InitGraphics, WriteString, WriteLn, GString, GStringUp,
		OpenWindow, CloseWindow, ClearWindow;

FROM WGraph IMPORT
    (* type *)	Graph, BarType, GraphType, LineType, MarkType,
    (* proc *)	OpenGraph, DrawXAxis, DrawAxes, BarGraph;

FROM MATHLIB IMPORT
    (* proc *)	Sin, Cos, Log;

FROM Keyboard IMPORT
    (* proc *)	InKey;

FROM Timer IMPORT
    (* proc *)	Sleep;

(************************************************************************)

PROCEDURE Pause;

    VAR t, i: INTEGER;
	dummy: CHAR;

    BEGIN
	Sleep (2000);
	(* dummy := InKey(); *)
(*
	FOR i := 1 TO 1000 DO
	    FOR t := 1 TO 250 DO
	    END (* FOR *);
	END (* FOR *);
*)
    END Pause;

(************************************************************************)

PROCEDURE RunTheDemo;

    VAR G1, G2, G3, G4, G5, G6: Graph;
	i : [0..4];
	w1, comments, w3: Window;
	x, y, z, t: ARRAY [0..4] OF LONGREAL;
	Xmax, Ymax, maxcolour, CharHeight: CARDINAL;
	barwidth: LONGREAL;
	ibw: CARDINAL;

    BEGIN
	GetScreenShape (Xmax, Ymax, maxcolour, CharHeight);

	(* Set up the demonstration data arrays.	*)

	FOR i := 0 TO 4 DO
	    y[i] := -5.0*Sin(6.24*LONGREAL(i))+1.0;
	    x[i] := 5.0*Cos(6.24*LONGREAL(i))+0.5;
	    t[i] := x[i] + LONGREAL(i)*0.6;
	    z[i] := 0.5*LONGREAL(i) + 1.0;
	END (* FOR *);

	(* Opening comments *)

	OpenWindow (comments, 0, 0, Xmax, Ymax DIV 4 - 1, 0, 2, double);
	WriteString (comments,'The Graph application also draws bar charts');
	WriteLn(comments);
	WriteString (comments, '   for example:'); WriteLn(comments);
	WriteString (comments, '           Simple bar graphs');
	Pause;

	(* A simple bar graph - G1 in w1.	*)

	OpenWindow (w1, 0, Ymax DIV 4, Xmax, Ymax, 7, 6, single);
	barwidth := LONGREAL(Xmax-180)/25.0;
	OpenGraph (G1,w1,bar,joined,100,30,Xmax-80,3*Ymax DIV 4 - 20,
				barwidth,0.0,4.0*barwidth,10.0,none);
	DrawAxes (G1, 0.0, 0, 2, TRUE, 1.0, 0, 2, TRUE);
	BarGraph (G1, y, 5, One);

	Pause;

	(* Graph G6 in w3 is another bar graph.	*)

	OpenWindow (w3, Xmax DIV 2, Ymax DIV 2, Xmax, Ymax, 1, 3, double);
	OpenGraph (G6,w3,bar,joined,50,40,Xmax DIV 2 - 30,Ymax DIV 2 - 20,
				10.0,0.0,30.0,10.0,none);
	DrawAxes (G6, 0.0, 0, 2, TRUE, 1.0, 0, 2, TRUE);
	BarGraph (G6,z,5,One);

	Pause;

	WriteString (comments,' - to which you can add labelling');
	WriteLn(comments);
	Pause;

	(* Add some text to G1.	*)

	GStringUp (w1, 20, 3*Ymax DIV 8 - 70, 'RainFall * 100 mm');
	GString (w1, Xmax DIV 2, 5, 'Months');
	GString (w1, Xmax DIV 2 -100, 3*Ymax DIV 4 - 50,
				'GRAPH OF MONTHLY RAINFALL');
	Pause;

	(* And then some text to G6.	*)

	GStringUp (w3, 20, Ymax DIV 4, 'Population');
	GString (w3, Xmax DIV 4, 5, 'Time in Days.');
	Pause;

	(* G2 is a copy of G1, in the same window, but with different	*)
	(* scale and bar pattern.					*)

	WriteString (comments, "           Here's a similar graph, with a different scale for the Y axis");
	WriteLn(comments);
	Pause;
	ibw := CARDINAL(barwidth);
	OpenGraph(G2,w1,bar,joined,100+ibw,30,Xmax-80,3*Ymax DIV 4 - 20,
				barwidth,0.0,4.0*barwidth,2.0,none);
	DrawAxes (G2, 0.0, 0, 2, TRUE, 0.25, 45, 2, TRUE);
	BarGraph (G2,y,5,Two);
	Pause;

	(*  G3, G4, and G5 are plotted on the same axes as G1	*)

	OpenGraph(G3,w1,bar,joined,100+2*ibw,30,Xmax-80,3*Ymax DIV 4 - 20,
				barwidth,0.0,4.0*barwidth,10.0,none);
	WriteString (comments, '           Extra Bars can be added to the existing graph.');
	WriteLn(comments);
	Pause;
	DrawXAxis (G3, 0.0, 0, 2, TRUE);
	BarGraph (G3,x,5,Three);
	Pause;

	OpenGraph(G4,w1,bar,joined,100+3*ibw,30,Xmax-80,3*Ymax DIV 4 - 20,
					barwidth,0.0,4.0*barwidth,10.0,none);
	OpenGraph (G5,w1,bar,joined,100+4*ibw,30,Xmax-80,3*Ymax DIV 4 - 20,
					barwidth,0.0,4.0*barwidth,10.0,none);

	WriteString (comments, '           With five different types of bar patterns.');
	WriteLn(comments);
	Pause;

	DrawXAxis (G4, 0.0, 0, 2, TRUE);
	BarGraph(G4,t,5,Four);
	DrawXAxis (G5, 0.0, 0, 2, TRUE);
	BarGraph(G5,z,5,Five);
	Pause;
	CloseWindow(w1);  Pause;
	CloseWindow(w3);  Pause;
	CloseWindow(comments);

    END RunTheDemo;

(************************************************************************)

BEGIN
    InitGraphics (18);
    RunTheDemo;
END BarDemo.
