MODULE ControlDemo;

	(********************************************************)
	(*							*)
	(*	   Implementation of a PI controller.		*)
	(*							*)
	(*  Programmers:	P.Moylan, R.Middleton, T.Wylie	*)
	(*  							*)
	(*	This is PJM's version, rewritten to work        *)
	(*      with the TopSpeed compiler.                     *)
	(*							*)
	(*  Last Edited:	9 March 1995			*)
	(*  Status:						*)
	(*	Working, except					*)
	(*		- the DMA bug still remains a mystery.	*)
	(*	Seems to have an excessive number of global	*)
	(*	 variables, may need cleaning up in terms of	*)
	(*	 data structure design.				*)
	(*							*)
	(********************************************************)

FROM Trace IMPORT
    (* proc*)	NYI, TraceOn;

FROM TransferFunctions IMPORT
    (* type *)	TransferFunction,
    (* proc *)	CreateTransferFunction, DestroyTransferFunction,
		UpdateSamplingInterval, Filter, EditTransferFunction;

FROM TextLines IMPORT
    (* type *)	LineType,
    (* proc *)	Box, HLine;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteLn, SetCursor,
		WriteChar, EditAborted;

FROM RealIO IMPORT
    (* proc *)	WriteReal, EditReal;

FROM ScreenEditor IMPORT
    (* type *)	Structure,
    (* proc *)	RealField, Combine, ScreenEdit, DeleteStructure;

FROM Menus IMPORT
    (* type *)	Menu, ItemText,
    (* proc *)	CreateMenu, PositionMenu, SelectFromMenu, DestroyMenu;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateTask, CreateLock, Obtain, Release;

FROM AnalogueIO IMPORT
    (* proc *)	StartPeriodicSampling, WaitForNextSample, AnalogueOut,
		StopPeriodicSampling,AnalogueInput;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM Timer IMPORT
    (* proc *)	Sleep;

FROM Calculator IMPORT
    (* proc *)	RunCalculator;

FROM Piano IMPORT
    (* proc *)	PlayPiano;

FROM Logger IMPORT
    (* proc *)	StartLogging, Log, StopLogging;

FROM Mouse IMPORT
    (* proc *)	MouseAvailable;

FROM UserInterface IMPORT
    (* type *)	UIWindow, Capability, CapabilitySet,
    (* proc *)	AllowMouseControl;

(************************************************************************)

    (* Set the following constant to FALSE if testing the software	*)
    (* on a machine lacking the A/D hardware.				*)

CONST RTIpresent = FALSE;

(************************************************************************)
(*				SHARED DATA				*)
(************************************************************************)

    (* Because the data used by the control task can be altered by	*)
    (* user options, a lot of it has to be global to the module.	*)
    (* Locks are used for critical section protection of all		*)
    (* global variables except for the Boolean flags and the transfer	*)
    (* functions.  For the transfer functions, we assume that module	*)
    (* TransferFunctions looks after its own critical section		*)
    (* protection.  The Boolean flags are not protected since there	*)
    (* don't seem to be any dangerous race conditions associated with	*)
    (* them - although it would be worth having another look to see	*)
    (* whether this assumption is justified.				*)

VAR
    (* ShutdownDesired is used to tell the control task to stop, and	*)
    (* semaphore ShutdownCompleted is used to signal that the shutdown	*)
    (* operation has been performed.  RestartDesired tells the		*)
    (* controller to re-initialise itself, with a new sampling interval	*)
    (* but not a new state.						*)

    ShutdownDesired, RestartDesired: BOOLEAN;
    ShutdownCompleted: Semaphore;

    FF, FB, C : TransferFunction;

    IOSignals:  RECORD
		    access: Lock;
		    Input, Output, SetPoint, Error: REAL;
    		END (*RECORD*);

    (* We are implementing the controller transfer function (K1 + K2/s)	*)
    (* with Anti-Integral Windup. IntegralGain and ProportionalGain	*)
    (* are the continuous time gains we wish to use. KpDiscrete		*)
    (* and IwDiscrete are the delta operator form discrete proportional	*)
    (* gain, and integral rate ( rad/sec ) obtained using a bilinear	*)
    (* approximation of the continuous controller.			*)
    (*	KpDiscrete = ProportionalGain + IntegralGain * delta * 0.5	*)
    (*	IwDiscrete = IntegralGain / KpDiscrete				*)
    (*	KtDiscrete = TachoGain						*)
    (* where delta is the sampling interval.				*)

    ControllerParameters:
		    RECORD
			access: Lock;
			delta: REAL;	(* seconds *)
			IntegralGain, ProportionalGain, TachoGain: REAL;
			KpDiscrete, IwDiscrete, KtDiscrete: REAL;
		    END;

    (* When LoggingInProgress is TRUE, the raw data from the A/D	*)
    (* converter are saved in a disk file.				*)

    LoggingInProgress: BOOLEAN;

(************************************************************************)
(*			    UTILITY ROUTINES				*)
(************************************************************************)

PROCEDURE VoltToInteger (v: REAL): INTEGER;

    (* This function converts a real number, v, into an integer		*)
    (* suitable for output to a 12 bit two's complement D/A converter.	*)
    (* This function is only valid for the factory setting of Bipolar	*)
    (* (i.e. +/- 10V) output, and 2's complement output data.		*)

    CONST MaxVolts = 9.99;

    VAR negative: BOOLEAN;  absvalue: CARDINAL;

    BEGIN
	negative := v < 0.0;
	IF negative THEN v := -v END(*IF*);
	IF v >= MaxVolts THEN v := MaxVolts; END(*IF*);
	absvalue := TRUNC (204.8*v + 0.5);
	IF negative THEN RETURN -INTEGER(absvalue)
	ELSE RETURN absvalue
	END (*IF*);
    END VoltToInteger;

(***********************************************************************)

PROCEDURE IntegerToVolt (c: INTEGER): REAL;

    (* This function converts an integer into a real number		*)
    (* representing the voltage. This function works correctly only if	*)
    (* the RTI-815 board is set at the factory settings, i.e., +/-10V	*)
    (* operation, 2's complement digital output.  Note that, even	*)
    (* though the A/D converter is a 12-bit converter, it sign-extends	*)
    (* correctly to give a 16-bit 2's complement number.		*)

    CONST scale = 1.0/204.8;

    BEGIN
	IF c >= 0 THEN RETURN scale*FLOAT(c);
	ELSE RETURN -scale*FLOAT(-c);
	END (* IF *);
    END IntegerToVolt;

(************************************************************************)

PROCEDURE CalcDiscrete;

    (* Procedure to recompute the discrete time controller gains: 	*)
    (*		KpDiscrete, IwDiscrete, KtDiscrete			*)
    (* in the global ( to this module ) record : ControllerParameters.	*)
    (* This procedure should be called every time the parameters, or	*)
    (* the sampling rate is changed.  Assumption: execution of this	*)
    (* procedure has been made indivisible by critical section		*)
    (* protection applied by the caller.				*)

    BEGIN
	WITH ControllerParameters DO
	    KpDiscrete := ProportionalGain + delta * 0.5 * IntegralGain ;
	    IwDiscrete := IntegralGain / ProportionalGain ;
	    KtDiscrete := TachoGain;
	END (* WITH *)
    END CalcDiscrete ;
 
(************************************************************************)
(*			   THE MONITORING TASK				*)
(************************************************************************)

PROCEDURE MonitorTask;

    (* This task displays the plant input and output on the screen.	*)
    (* We run it at a low priority, so that it will not interfere with	*)
    (* the controller.							*)

    VAR w: Window;  error, input, output, setpoint: REAL;
	flipflop,Count: CARDINAL;  dummy: UIWindow;

    BEGIN
	flipflop := 0;
	OpenWindow (w, yellow, cyan, 0, 5, 4, 43, simpleframe, nodivider);
	IF MouseAvailable() THEN
	    dummy := AllowMouseControl (w, "Monitor plant",
				CapabilitySet {wshow, wmove, whide});
	END (*IF*);
	WriteString (w, "Plant input :");  WriteLn (w);
	WriteString (w, "Plant output:"); WriteLn (w);
	WriteString (w, "Set point   :"); WriteLn (w);
	WriteString (w, "Error       :");
	REPEAT
	    WITH IOSignals DO
		Obtain (access);
		output := Output;  input := Input;
		setpoint := SetPoint; error := Error;
		Release (access);
	    END (*WITH*);
	    SetCursor (w, 1, 15);  WriteReal (w, input, 8);
	    SetCursor (w, 2, 15);  WriteReal (w, output, 8);
	    SetCursor (w, 3, 15);  WriteReal (w, setpoint, 8);
	    SetCursor (w, 4, 15);  WriteReal (w, error, 8);
	    SetCursor (w, 2, 28);
	    INC ( flipflop );
	    CASE flipflop OF
		1 : WriteString ( w, '-  -');
			|
		2 : WriteString ( w, '\  /');
			|
		3 : WriteString ( w, '|  |');
			|
		4 : WriteString ( w, '/  \');
			flipflop := 0;
		ELSE WriteString ( w, '***');
	    END (* CASE flipflop *);
	    Sleep (100);
	UNTIL ShutdownDesired;
	CloseWindow (w);
	Signal (ShutdownCompleted);
    END MonitorTask;

(************************************************************************)
(*			   THE CONTROLLER TASK				*)
(************************************************************************)

PROCEDURE ControlTask;

    (* This procedure runs as a separate task, and it implements the	*)
    (* control algorithm.						*)

    CONST MaxInput = 10.0;  MinInput = -10.0;

	(* The above constants define the maximum and minimum input	*)
	(* used in the anti-integral-windup scheme.			*)

    VAR	PIstate: REAL;  SamplePeriod: LONGCARD;
	controlvalue: INTEGER;
 	Measurement : ARRAY [0..3] OF INTEGER;
		(* Raw A/D Converter Data goes into Measurement[0..2]	*)
		(* D/A value copied into Measurement[3], for logging.	*)
	PlantInput, TachoFB, gain: REAL;

    BEGIN
	PIstate := 0.0;
	REPEAT
	    WITH ControllerParameters DO
		Obtain (access);
		CalcDiscrete;
		SamplePeriod := VAL(LONGCARD,1.0E06*delta);
		Release (access);
	    END;

	    (* Insertion for testing *)

	    IF NOT RTIpresent THEN
		Measurement[0] := 7876;
		Measurement[1] := -36;
		Measurement[2] := 30605;
	    END (*IF*);
   	    StartPeriodicSampling (0,2,SamplePeriod,0,Measurement);
	    REPEAT

		(* The following test allows the software to be tested	*)
		(* in the absence of the RTI A/D hardware.		*)

		IF RTIpresent THEN
		    WaitForNextSample;
		ELSE
		    Sleep (CARDINAL(SamplePeriod DIV 1000));
		END (*IF*);

		WITH IOSignals DO
		    Obtain (access);
		    Output := IntegerToVolt( Measurement[0] );
		    SetPoint := IntegerToVolt( Measurement[1] );
		    TachoFB := IntegerToVolt( Measurement[2] );

		    (* Form error signal		    		*)
		    (*		==> Input to Control Compensator.	*)

		    Error := Filter(FF, SetPoint) - Filter(FB, Output);

		    (* Pass error signal through control	*)
		    (* compensator and PI controller.		*)

		    WITH ControllerParameters DO
			Obtain (access);
			gain := KpDiscrete;
			Release (access);
		    END (*WITH*);

		    Input := gain * Filter(C, Error) + PIstate;

		    IF Input > MaxInput THEN Input := MaxInput;
		    ELSIF Input < MinInput THEN Input := MinInput;
		    END (* IF *);

		    WITH ControllerParameters DO
			Obtain (access);
			PIstate := PIstate + delta*IwDiscrete*(Input-PIstate);
			PlantInput := Input - KtDiscrete*TachoFB;
			Release (access);
		    END (*WITH*);

		    Release (access);
		END (*WITH*);

		controlvalue := VoltToInteger (PlantInput);

		AnalogueOut (0, controlvalue);
		Measurement[3] := controlvalue;

		IF LoggingInProgress THEN
		    Log (Measurement);
		END (*IF*);

	    UNTIL RestartDesired OR ShutdownDesired;

	    StopPeriodicSampling;
	    RestartDesired := FALSE;

	UNTIL ShutdownDesired;

	AnalogueOut(0,0);	(* to stop servo *)
	Signal (ShutdownCompleted);

    END ControlTask;

(************************************************************************)
(*			    THE USER INTERFACE				*)
(************************************************************************)

PROCEDURE ChangeSamplingInterval;

    (* Allows the keyboard user to change the analogue sampling interval. *)

    CONST Return = CHR(13);

    VAR w: Window;  dummy: UIWindow;
	millisecs, newdelta: REAL;

    BEGIN
	WITH ControllerParameters DO
	    Obtain (access);
	    millisecs := 1000.0*delta;
	    Release (access);
	END (*WITH*);

	OpenWindow (w, white, magenta, 13, 15, 20, 61, simpleframe, nodivider);
	IF MouseAvailable() THEN
	    dummy := AllowMouseControl (w, "Set sampling interval",
				CapabilitySet {wshow, wmove, wescape});
	END (*IF*);
	WriteString (w, "Sampling interval:          milliseconds");
	SetCursor (w, 1, 20);
	EditReal (w, millisecs, 8);
	IF NOT EditAborted() THEN
	    newdelta := 0.001*millisecs;

	    WITH ControllerParameters DO
		Obtain (access);
		delta := newdelta;
		Release (access);
	    END (*WITH*);

	    UpdateSamplingInterval (FF, newdelta);
	    UpdateSamplingInterval (FB, newdelta);
	    UpdateSamplingInterval (C, newdelta);

	    RestartDesired := TRUE;
	END (*IF*);
	CloseWindow (w);

    END ChangeSamplingInterval;

(************************************************************************)

PROCEDURE ChangeControllerGains;

    (* Allows the keyboard user to change the controller gains.		*)

    VAR w: Window;  dummy: UIWindow;  S: Structure;
	Gain1, Gain2, Gain3: REAL;  abort: BOOLEAN;

    BEGIN

	(* Initial screen display. *)

	OpenWindow (w, white, magenta, 12, 16, 45, 75, simpleframe, nodivider);
	IF MouseAvailable() THEN
	    dummy := AllowMouseControl (w, "Set gains",
				CapabilitySet {wshow, wmove, wescape});
	END (*IF*);
	WriteString (w, "Proportional gain :");  WriteLn (w);
	WriteString (w, "    Integral gain :");  WriteLn (w);
	WriteString (w, "       Tacho gain :");

	(* Take a copy of the current continuous-time gains.	*)

	WITH ControllerParameters DO
	    Obtain (access);
	    Gain1 := ProportionalGain;
	    Gain2 := IntegralGain;
	    Gain3 := TachoGain;
	    Release (access);
	END (*WITH*);

	(* Create the editing structure used by ScreenEditor.	*)

	WITH ControllerParameters DO
	    S := RealField (Gain1, 1, 21, 8);
	    Combine (S, RealField (Gain2, 2, 21, 8));
	    Combine (S, RealField (Gain3, 3, 21, 8));
	END (*WITH*);

	(* The actual editing: *)

	ScreenEdit (w, S, abort);

	(* End of user interaction.	*)

	DeleteStructure (S);
	CloseWindow (w);

	(* Convert the new values to discrete-time form.	*)

	IF NOT abort THEN
	    WITH ControllerParameters DO
		Obtain (access);
		ProportionalGain := Gain1;
		IntegralGain := Gain2;
		TachoGain := Gain3;
		CalcDiscrete;
		Release (access);
	    END (*WITH*);
	END (*IF*);

    END ChangeControllerGains;

(************************************************************************)

PROCEDURE HandleOptions;

    TYPE OptionRange = [0..10];

    VAR M: Menu;  Messages: ARRAY OptionRange OF ItemText;
	finished: BOOLEAN;

    BEGIN
	Messages[0] := "     Options";
	Messages[1] := "Sampling interval";
	Messages[2] := "PI controller gains";
	Messages[3] := "Feedforward Compensator";
	Messages[4] := "Feedback Compensator";
	Messages[5] := "Control Compensator";
	Messages[6] := "Calculator";
	Messages[7] := "Piano";
	Messages[8] := "Start logging";
	Messages[9] := "Stop logging";
	Messages[10] := "Exit";
	CreateMenu (M, 1, Messages, MAX(OptionRange));
	PositionMenu (M, 0, 3+MAX(OptionRange), 50, 75);
	finished := FALSE;
	REPEAT
	    CASE SelectFromMenu (M)  OF
		    0:	finished := TRUE;
		|
		    1:	ChangeSamplingInterval;
		|
		    2:	ChangeControllerGains;
		|
		    3:  EditTransferFunction (FF, "Feedforward Compensator");
		|
		    4:  EditTransferFunction (FB, "Feedback Compensator");
		|
		    5:  EditTransferFunction (C, "Control Compensator");
		|
		    6:	RunCalculator;
		|
		    7:	PlayPiano;
		|
		    8:	LoggingInProgress := StartLogging();
		|
		    9:	LoggingInProgress := FALSE;
			StopLogging;
		|
		   10:	finished := TRUE;
		|
		  ELSE
			NYI ("that option");
	    END (*CASE*);
	UNTIL finished;
	ShutdownDesired := TRUE;
	DestroyMenu (M);
    END HandleOptions;

(************************************************************************)
(*				INITIALISATION				*)
(************************************************************************)

PROCEDURE BlockDiagram (w: Window);

    (* Draws a picture in window w showing the controller configuration. *)

    BEGIN
	Box (w, 1, 6, 8, 3, single);
	SetCursor (w, 2, 2);  WriteString(w,"Out");
	HLine (w, 3, 2, 6, single);
	SetCursor (w, 2, 8);  WriteString(w,"Feed");
	SetCursor (w, 3, 8);  WriteString(w,"Back");

	Box (w, 6, 6, 8, 3, single);
	HLine (w, 7, 2, 6, single);
	SetCursor (w, 8, 2);  WriteString(w,"Ref");
	SetCursor (w, 7, 8);  WriteString(w,"Feed");
	SetCursor (w, 8, 7);  WriteString(w,"Forward");

	Box (w, 1, 19, 8, 8, single);
	HLine (w, 3, 14, 19, single);
	HLine (w, 7, 14, 19, single);
	SetCursor (w, 3, 21);  WriteChar (w, "-");
	SetCursor (w, 5, 23);  WriteChar (w, "ä");
	SetCursor (w, 7, 21);  WriteChar (w, "+");

	Box (w, 3, 32, 8, 3, single);
	SetCursor (w, 4, 33);  WriteString(w,"Control");
	SetCursor (w, 5, 34);  WriteString(w,"Comp");

	HLine (w, 4, 27, 32, single);

	Box (w, 3, 45, 7, 3, single);
	SetCursor (w, 4, 46);  WriteString(w,"  PI  ");

	HLine (w, 4, 40, 45, single);
	HLine (w, 4, 52, 57, single);

	SetCursor (w, 5, 56);  WriteString(w,"In");
	SetCursor (w, 9, 30);  WriteString(w,"General Controller Structure");

    END BlockDiagram;

(***********************************************************************)

PROCEDURE RunTheController;

    CONST DefaultDelta = 0.100;	(* seconds *)

    VAR titlewindow, diagram: Window;  dummy: UIWindow;

    BEGIN

	(* Create some initial messages. *)

	OpenWindow (titlewindow, black, white, 7, 12, 0, 46,
					doubleframe, nodivider);
	IF MouseAvailable() THEN
	    dummy := AllowMouseControl (titlewindow, "Title window",
				CapabilitySet {wshow, wmove, whide});
	END (*IF*);
	SetCursor (titlewindow, 1, 14);
	WriteString (titlewindow, "Control Laboratory");
	SetCursor (titlewindow, 2, 4);
	WriteString (titlewindow, "_______________________________________");

	SetCursor (titlewindow, 4, 4);
	WriteString (titlewindow, "PJM's test version of the PI controller");

	OpenWindow (diagram,white,red,14,24,10,69,doubleframe,nodivider);
	IF MouseAvailable() THEN
	    dummy := AllowMouseControl (diagram, "Block diagram",
				CapabilitySet {wshow, wmove, whide});
	END (*IF*);
	BlockDiagram (diagram);

	(* Initialization of global variables. *)

	LoggingInProgress := FALSE;
	ShutdownDesired := FALSE;  RestartDesired := FALSE;
	CreateSemaphore (ShutdownCompleted, 0);

	WITH ControllerParameters DO
	    CreateLock (access);
	    Obtain (access);
	    delta := DefaultDelta;
	    IntegralGain := 0.0;  ProportionalGain := 1.0;
	    TachoGain := 0.0;
	    Release (access);
	END (*WITH*);

	WITH IOSignals DO
	    CreateLock (access);
	    Obtain (access);
	    Output := 0.0;  Input := 0.0;  SetPoint := 0.0;
	    Release (access);
	END (*WITH*);

	CreateTransferFunction (FF, DefaultDelta);
	CreateTransferFunction (FB, DefaultDelta);
	CreateTransferFunction (C, DefaultDelta);

	(* Start the monitor task.	*)

	CreateTask (MonitorTask, 1, "Control Monitor");

	(* Start the controller running. *)

	CreateTask (ControlTask, 2, "Control demo");

	HandleOptions;

	IF LoggingInProgress THEN
	    LoggingInProgress := FALSE;  StopLogging;
	END (*IF*);
	Wait (ShutdownCompleted);	(* Twice, because we have to	*)
	Wait (ShutdownCompleted);	(* wait for two tasks to exit.	*)

	DestroyTransferFunction (FF);
	DestroyTransferFunction (FB);
	DestroyTransferFunction (C);
	CloseWindow (diagram);
	CloseWindow (titlewindow);

    END RunTheController;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    (*TraceOn (19, 24, 40, 79);*)
    RunTheController;
END ControlDemo.
