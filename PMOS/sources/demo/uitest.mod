MODULE UITest;

	(****************************************************************)
	(*								*)
	(*		Test of module UserInterface			*)
	(*								*)
	(*	Programmer:	M. Walsh				*)
	(*	Modifications:	P. Moylan				*)
	(*	Last edited:	26 July 1994				*)
	(*	Status:		Working					*)
	(*								*)
	(****************************************************************)

FROM Conversions IMPORT
    (* proc *)	CardinalToString;

FROM TaskControl IMPORT
    (* proc *)	CreateTask;

FROM Timer IMPORT
    (* proc *)	Sleep;

FROM SoundEffects IMPORT Beep;

FROM Mouse IMPORT
    (* type *)	Buttons, ButtonSet,
    (* proc *)	GetTextMouseStatus, GetTextMousePosition,
		ShowMouseCursor, HideMouseCursor;

FROM Windows IMPORT
    (* type *)	Window,Colour,FrameType,DividerType,
		MaxRowNumber,MaxColumnNumber, ColumnRange,RowRange,
    (* proc *)	OpenWindow, WriteString, SetCursor;

FROM UserInterface IMPORT
    (* type *)	UIWindow, Capability, CapabilitySet,
    (* proc *)	AllowMouseControl, AddActiveRegion;

CONST
    LeftOnly = ButtonSet {LeftButton};
    RightOnly = ButtonSet {RightButton};

(************************************************************************)

PROCEDURE BEEP (w: Window;  r: RowRange;  c: ColumnRange);

    BEGIN
	Beep;
    END BEEP;

(************************************************************************)

PROCEDURE BEEPBEEP (w: Window;  r: RowRange;  c: ColumnRange);

    BEGIN
	Beep;  Beep;
    END BEEPBEEP;

(************************************************************************)

PROCEDURE Spawned;

    VAR w: Window;  Test: UIWindow;

    BEGIN
	OpenWindow (w,white,black,1,12,1,18,doubleframe,nodivider);
	Test := AllowMouseControl (w, "Spawned",
					CapabilitySet{wmove, whide, wshow});
	AddActiveRegion(Test,1,10,1,16,LeftOnly,BEEP);
	AddActiveRegion(Test,1,10,1,16,RightOnly,BEEPBEEP);
	LOOP
	    WriteString(w,'Testing and it appears to work - Amazing');
	    Sleep(100);
	END (*LOOP*);
    END Spawned;

(************************************************************************)

PROCEDURE Spawned2;

    VAR w: Window;  Test: UIWindow;

    BEGIN
	OpenWindow (w,white,black,14,23,1,18,doubleframe,nodivider);
	Test := AllowMouseControl (w, "Spawned as well",
				CapabilitySet {wmove, whide, wshow});
	AddActiveRegion(Test,1,8,1,16,LeftOnly,BEEP);
	LOOP
	    WriteString (w,'Testing and also working ---------- !!!!!!!!!');
	    Sleep(100);
	END (*LOOP*);
    END Spawned2;

(************************************************************************)

PROCEDURE MouseWindow;

    VAR w: Window;  MousePos: UIWindow;
	X: ColumnRange;  Y: RowRange;  LastX, LastY: CARDINAL;
	string: ARRAY [0..2] OF CHAR;

    BEGIN
	OpenWindow (w,black,white,15,18,20,27,doubleframe,nodivider);
	MousePos := AllowMouseControl (w, "MousePos",
				CapabilitySet {wmove, whide, wshow});
	SetCursor (w, 1, 2);  WriteString (w, "X");
	SetCursor (w, 2, 2);  WriteString (w, "Y");
	AddActiveRegion(MousePos,1,2,1,6,LeftOnly,BEEP);
	LastX := MAX(ColumnRange) + 1;
	LOOP
	    GetTextMousePosition (X, Y);
	    IF (LastX <> X) OR (LastY <> Y) THEN
		HideMouseCursor;
		CardinalToString (X, string, 3);
		SetCursor (w, 1, 3);  WriteString (w, string);
		CardinalToString (Y, string, 3);
		SetCursor (w, 2, 3);  WriteString (w, string);
		ShowMouseCursor;
		LastX := X;
		LastY := Y;
	    END (*IF*);
	    Sleep(50);
	END (*LOOP*);
    END MouseWindow;

(************************************************************************)

PROCEDURE ScrollWindowtask;

    VAR w: Window;  LoopCount:CARDINAL;  ScrollWindow: UIWindow;

    BEGIN
	OpenWindow (w,intensewhite,red,2,12,40,75,doubleframe,nodivider);
	ScrollWindow := AllowMouseControl (w, "ScrollWindow",
				CapabilitySet {wmove, whide, wshow});
	AddActiveRegion(ScrollWindow,1,9,1,34,LeftOnly,BEEP);
	Sleep(100);
	LOOP
	    FOR LoopCount := 1 TO 10 DO
		WriteString(w,"Scrolling Text Is Fast. Faster than you can read !");
	    END (*FOR*);
	    Sleep(100);
	END (*LOOP*);

    END ScrollWindowtask;

(************************************************************************)

PROCEDURE ScrollWindow2task;

    VAR LoopCount:CARDINAL;  ScrollWindow2: UIWindow;  w: Window;

    BEGIN
	OpenWindow (w,blue,black,14,20,40,75,doubleframe,nodivider);
	ScrollWindow2 := AllowMouseControl (w, "ScrollWindow Too",
				CapabilitySet {wmove, whide, wshow});
	AddActiveRegion(ScrollWindow2,1,5,1,34,LeftOnly,BEEP);
	Sleep(100);
	LOOP
	    FOR LoopCount := 1 TO 10 DO
		WriteString(w,"Scrolling Text Is Fast. Faster than you can read !");
	    END (*FOR*);
	    Sleep(100);
	END (*LOOP*);

    END ScrollWindow2task;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

VAR buttons: ButtonSet;  X:ColumnRange;  Y:RowRange;

BEGIN
    CreateTask(MouseWindow,5,"Mouse");
    CreateTask(ScrollWindowtask,2,"SW");
    CreateTask(ScrollWindow2task,3,"SW2");
    CreateTask(Spawned,2,"Spawned");
    CreateTask(Spawned2,2,"Spawned2");
    REPEAT
        Sleep(40);
        GetTextMouseStatus(buttons, X, Y);
    UNTIL ButtonSet{LeftButton,RightButton} <= buttons;

END UITest.