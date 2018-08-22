MODULE Pong;

(****************************************************************)	
(*								*)
(*	Original author:	probably Michael Walsh		*)
(*		(The copy I got came without comments)		*)
(*	Debugging by:	Peter Moylan				*)
(*	Last edited:	7 December 1993				*)
(*	Status:							*)
(*		- not yet clear what this program is supposed	*)
(*		  to do.  It makes pretty pictures on the	*)
(*		  screen, but purpose is unclear.		*)
(*		- apparently not responding to keyboard.	*)
(*								*)
(****************************************************************)	

FROM Storage IMPORT
	ALLOCATE,DEALLOCATE;

FROM SoundEffects IMPORT
    (* proc *)	Beep;

FROM Graphics IMPORT
    (* type *)	ColourType,
    (* proc *)	Fill,SetMode,PlotMark,PlotLine,GetScreenShape;

FROM Keyboard IMPORT
    (* proc *)	InKey, KeyPressed;

CONST
	Up = 'k';
	Down = 'm';
	NewBall = ' ';
	Quit = 'q';
	None = 'a';
	PaddleSpeed = 1;
	SpeedOfBall = 1;
	
TYPE


	BallPtr=POINTER TO Ball;

	Ball=RECORD
		Xcoord:CARDINAL;
		Ycoord:CARDINAL;
		Xdir:INTEGER;
		Ydir:INTEGER;
		Colour:ColourType;
		NextBall:BallPtr;
	END; (*Ball Record*)
VAR
	Balls,LastBall:BallPtr;
	KeyIn:CHAR;
	Xmax,Ymax,PaddlePos,PaddleMoveCount:CARDINAL;
	LastColour,LastCol,ColourMax:ColourType;
	DirLast:INTEGER;
	EndOfGame,FirstBall:BOOLEAN;


PROCEDURE MovePaddle(Direction:CHAR; VAR Position:CARDINAL);
VAR
	Dir:INTEGER;
BEGIN
	IF Direction = Up
	THEN
		Dir := 5;
	ELSE
		Dir := -5;
	END(*IF*);
	IF NOT (((Position <= 5) AND (Direction = Down))
		OR ((Position + 55 >= Ymax) AND (Direction = Up)))
	THEN
		Fill(10,Position,10,Position + 50,0);
		IF Dir > 0 THEN
		       Position := Position + ORD(Dir);
		ELSE
		      Position := Position - ORD(Dir);
		END(*iF*);
		Fill(10,Position,10,Position + 50,3);
	END(*IF*);
	
END MovePaddle;

PROCEDURE EraseBall(Ball:BallPtr);
BEGIN
	Fill(Ball^.Xcoord,Ball^.Ycoord,(Ball^.Xcoord + 10),(Ball^.Ycoord + 10),0);
END EraseBall;

PROCEDURE DrawBall(Ball:BallPtr);
	
BEGIN
	Fill(Ball^.Xcoord,Ball^.Ycoord,(Ball^.Xcoord + 10),(Ball^.Ycoord + 10),(Ball^.Colour));
END DrawBall;

PROCEDURE CreateNewBall(VAR BallList,LastBallOnList:BallPtr;
		VAR First:BOOLEAN;
		VAR LastDir:INTEGER;
		VAR LastCol:ColourType);	
BEGIN
	
				IF First THEN
                                	NEW ( LastBall );
				        BallList := LastBallOnList;
					First := FALSE;
                                ELSE
					NEW ( LastBallOnList^.NextBall );
					LastBallOnList := LastBallOnList^.NextBall;
				END; (* if *)
				
				WITH LastBallOnList^ DO
					NextBall := NIL;
					Xcoord := Xmax DIV 2;
					Ycoord := Ymax DIV 2;
					Xdir := ((LastDir MOD 2) *2) -1;
					Ydir := ((LastDir DIV 2) * 2) -1;
					Colour := LastCol;
					LastCol:= (((LastCol + 3) MOD 7) + 1);
				END (*WITH*);
				DirLast :=(DirLast +1) MOD 4;
END CreateNewBall;

PROCEDURE MoveBalls(StartOfList:BallPtr;PaddlePosition,BallSpeed:CARDINAL);
	VAR
		CurrentBall:BallPtr;
				

	BEGIN
	CurrentBall := StartOfList;

	WHILE	CurrentBall <> NIL
	DO
		
		WITH CurrentBall^ DO
			IF (Xcoord >= 2*Xmax - 10 - BallSpeed) OR (Xcoord  <= BallSpeed)
			THEN
				Xdir := -(Xdir);
				Colour := ((Colour +1) MOD 7) + 1;
			END (*IF*);

			IF 	(Ycoord >= (Ymax -10 - BallSpeed)) OR (Ycoord <= BallSpeed)
			THEN 	
				Ydir := -(Ydir);
				Colour := ((Colour + 1) MOD 7) + 1;
			END(*IF*);
		
			IF ((Ycoord+10 > PaddlePosition)
				AND (Ycoord < PaddlePosition + 50)
				AND (Xcoord <= 10 + BallSpeed)
				AND (Xdir = -1))
			THEN
				Xdir := -(Xdir);
			END(*IF*);
		END(*WITH*);
		
		EraseBall(CurrentBall);

		WITH CurrentBall^ DO
		IF Xdir > 0 THEN
		   Xcoord := Xcoord + BallSpeed;
		ELSE
		   Xcoord := Xcoord  - BallSpeed;
		END(*IF*);
		IF Ydir > 0 THEN
		   Ycoord := Ycoord + BallSpeed;
		ELSE
		   Ycoord := Ycoord - BallSpeed;
		END(*IF*);
		END(*WITH*);
		
		DrawBall(CurrentBall);
		CurrentBall := CurrentBall^.NextBall;
	END (*DO*);	
 			
	END MoveBalls;

(****************************************************************)	

PROCEDURE RunTheGame;

    BEGIN
	PlotLine(0,0,0,Ymax,3);
	PlotLine(0,Ymax,Xmax,Ymax,3);
	PlotLine(Xmax,Ymax,Xmax,0,3);
	PlotLine(Xmax,0,0,0,3);
	FirstBall := TRUE;
	Balls := NIL;
	DirLast := 0;
	LastColour := 5;
	PaddlePos := 1;
	KeyIn := None;
	REPEAT	
	    (*PaddleMoveCount := 0;
	    REPEAT
		CASE KeyIn OF
		    Up 	: MovePaddle(Up,PaddlePos);|
		    Down	: MovePaddle(Down,PaddlePos);|
		    Quit  	: EndOfGame := TRUE;|
		    NewBall : CreateNewBall(Balls,LastBall,FirstBall,DirLast,LastColour);|
		    None : |
		ELSE
		    CreateNewBall(Balls,LastBall,FirstBall,DirLast,LastColour);
		END(*CASE*);
		IF KeyPressed() THEN
		    Beep;  KeyIn := InKey();
		ELSE
		    KeyIn := None;
		END(*IF*);
		PaddleMoveCount := PaddleMoveCount + 1;
	    UNTIL (EndOfGame) OR (PaddleMoveCount = PaddleSpeed);*)
	    CreateNewBall(Balls,LastBall,FirstBall,DirLast,LastColour);
	    MoveBalls(Balls,PaddlePos,SpeedOfBall);
		
	UNTIL EndOfGame;
    END RunTheGame;

(****************************************************************)	

BEGIN
    SetMode(16, TRUE);
    GetScreenShape(Xmax,Ymax,ColourMax);
    RunTheGame;
END Pong.