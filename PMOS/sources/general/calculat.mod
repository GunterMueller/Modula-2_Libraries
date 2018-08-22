IMPLEMENTATION MODULE Calculator;

	(********************************************************)
	(*							*)
	(*	Simple calculator (Algebraic notation version)	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	13 December 1994		*)
	(*  Status:		Working				*)
	(*							*)
	(*	Note that this module includes preprocessor	*)
	(*	  directives for compiler-dependent code.	*)
	(*							*)
	(********************************************************)

FROM Keyboard IMPORT
    (* proc *)	InKey, PutBack, StuffKeyboardBuffer, LockStatus, SetLocks;

FROM KBdriver IMPORT
    (* const*)	NumLockLED;

FROM SoundEffects IMPORT
    (* proc *)	Beep;

FROM TextLines IMPORT
    (* type *)	LineType,
    (* proc *)	Box;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType, Rectangle,
		RowRange, ColumnRange,
    (* proc *)	OpenWindow, CloseWindow, ChangeScrollingRegion,
		WriteChar, WriteString, WriteLn, SetCursor,
		ShiftWindowRel, WindowLocation, Hide, PutOnTop,
		IdentifyTopWindow;

(*<UseMouse*)
FROM Mouse IMPORT
    (* type *)	Buttons, ButtonSet,
    (* proc *)	HideMouseCursor, ShowMouseCursor;

FROM UserInterface IMPORT
    (* type *)	UIWindow, Capability, CapabilitySet,
    (* proc *)	AllowMouseControl, AddActiveRegion;
(*>*)

FROM Menus IMPORT
    (* type *)	Menu, ItemText,
    (* proc *)	CreateMenu, PositionMenu, SelectFromMenu, DestroyMenu;

FROM RealIO IMPORT
    (* proc *)	WriteLongReal;

FROM Str IMPORT
    (* proc *)	Copy;

FROM LowLevel IMPORT
    (* proc *)	IAND;

FROM MATHLIB IMPORT
    (* proc *)	Pow, Sqrt, Exp, Log, Log10, Sin, Cos,
		Tan, ASin, ACos, ATan, SinH, CosH, TanH;

(************************************************************************)
(*		    MISCELLANEOUS GLOBAL DEFINITIONS			*)
(************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST
    EmptyMarker = CHR(0);  EndMarker = ' ';
    Enter = CHR(0DH);  Esc = CHR(01BH);
    UnaryOperatorSet = CharSet {'%', ')', 's', 'S'};
    UnknownOperatorPriority = 255;
    numberstart = 8;	(* the x cursor position for displaying numbers	*)
    numberwidth = 9;	(* The field size for displaying numbers	*)

VAR calc, help: Window;
    baserow, basecol, helprow, helpcol: CARDINAL;

(************************************************************************)
(*			THE CALCULATOR STATE				*)
(************************************************************************)

CONST MaxRegisterNumber = 6;
      MaxMemoryNumber = 3;
      DisplayedRegisters = 3;

TYPE RegisterNumber = [0..MaxRegisterNumber];
     MemoryNumber = [0..MaxMemoryNumber];

VAR
    (* Array "Register" is a combined operand and operator stack.  It	*)
    (* would be more conventional to have separate stacks for the	*)
    (* operators and operands, but we adopt this slightly unusual stack	*)
    (* format because it makes it easier to maintain a user-friendly	*)
    (* screen display.							*)

    Register: ARRAY RegisterNumber OF
		RECORD
		    operator: CHAR;
		    ParenCount: CARDINAL;
		    value: LONGREAL;
		END (*RECORD*);

    (* NumberPresent = FALSE means that Register[0].value is displayed	*)
    (* as space characters rather than as a numeric string.		*)

    NumberPresent: BOOLEAN;

    (* In addition to the stack, there is a set of "memory" registers	*)
    (* in which the user can save calculation results.			*)

    MemoryValue: ARRAY MemoryNumber OF LONGREAL;

(************************************************************************)
(*			THE PREFIX UNARY FUNCTIONS			*)
(************************************************************************)

(* To simplify the calculator logic, and to keep the display readable,	*)
(* all prefix unary functions are stored in the calculator stack in	*)
(* terms of the all-purpose binary function "f".  The first argument	*)
(* of f is the function number, and the second argument of f is the	*)
(* true argument of the original unary function.			*)

CONST
    MaxFunctionNumber = 13;	(* number of built-in functions		*)
    functionnamewidth = 5;	(* # of characters in a function name	*)

TYPE
    FunctionType = [0..MaxFunctionNumber];
    NameText = ARRAY [0..functionnamewidth-1] OF CHAR;
    NameArray = ARRAY FunctionType OF NameText;
    MathProc = PROCEDURE (LONGREAL): LONGREAL;
    FunctionArray = ARRAY FunctionType OF MathProc;

PROCEDURE Negative (x: LONGREAL): LONGREAL;
    BEGIN RETURN -x; END Negative;

CONST
    (* Array FunctionName gives the function names as displayed.	*)

    FunctionName = NameArray ('     ', 'SQRT ', 'EXP  ', 'LN   ', 'LOG10',
				'SIN  ', 'COS  ', 'TAN  ', 'ASIN ', 'ACOS ',
				'ATAN ', 'SINH ', 'COSH ', 'TANH ');

    (* Array Function is the set of built-in functions.  This is a	*)
    (* constant array, but I have to set it up as a variable to mollify	*)
    (* compilers that can't handle constant arrays of procedures.	*)

    (*<~TopSpeed3
    VAR Function: FunctionArray;
    >*)

    (*<TopSpeed3*)
    CONST Function = FunctionArray (Negative, Sqrt, Exp, Log, Log10,
			Sin, Cos, Tan, ASin, ACos, ATan, SinH, CosH, TanH);
    (*>*)

(************************************************************************)
(*			    DISPLAY ROUTINES				*)
(************************************************************************)

PROCEDURE MoveWindow (code: CHAR;  w: Window);

    (* Parameter "code" is the second character of a function key	*)
    (* sequence.  This procedure moves window "w" on the screen if	*)
    (* the key turns out to be an arrow key (and does nothing if it	*)
    (* isn't).								*)

    BEGIN
	IF code = "H" THEN	(* cursor up *)
	    ShiftWindowRel (w, -1, 0);
	ELSIF code = "P" THEN	(* cursor down *)
	    ShiftWindowRel (w, 1, 0);
	ELSIF code = "M" THEN	(* cursor right *)
	    ShiftWindowRel (w, 0, 1);
	ELSIF code = "K" THEN	(* cursor left *)
	    ShiftWindowRel (w, 0, -1)
	END (*IF*);
    END MoveWindow;

(************************************************************************)

PROCEDURE CreateHelpWindow;

    (* Puts a help message on the screen. *)

    (*<UseMouse*)
    VAR dummy: UIWindow;
    (*>*)

    BEGIN
	OpenWindow (help, intensewhite, blue, helprow, helprow+10,
				helpcol, helpcol+28, simpleframe, nodivider);
	Hide (help);

	(*<UseMouse*)
	dummy := AllowMouseControl (help, "Help for calculator",
				CapabilitySet {wshow, wmove, whide});
	(*>*)

	WriteString (help, "Operators: + - * / ^ % ( )");
	WriteLn (help);
	WriteString (help, "  S   store to memory");
	WriteLn (help);
	WriteString (help, "  M   load from memory");
	WriteLn (help);
	WriteString (help, "  F   function (from menu)");
	WriteLn (help);
	WriteString (help, "  P   3.14159...");
	WriteLn (help);
	WriteString (help, "Arrow keys: move calculator");
	WriteLn (help);
	WriteString (help, "Backspace: delete last");
	WriteLn (help);
	WriteString (help, "= or Enter: evaluate result");
	WriteLn (help);
	WriteString (help, "Esc: exit from calculator");
    END CreateHelpWindow;

(************************************************************************)

PROCEDURE HelpVisible(): BOOLEAN;

    (* Returns TRUE iff some part of the help window is visible on the	*)
    (* screen.  We have to work this out each time, because the		*)
    (* visibility of the help window can be affected by things that	*)
    (* this module doesn't know about.					*)

    VAR R: Rectangle;  w: Window;
	r, row: RowRange;  c, col: ColumnRange;

    BEGIN
	R := WindowLocation (help);
	row := R.top;
	LOOP
	    col := R.left;
	    LOOP
		r := row;  c := col;
		IF IdentifyTopWindow(w, r, c) AND (w = help) THEN
		    RETURN TRUE;
		ELSIF col = R.right THEN
		    EXIT (*LOOP*);
		ELSE
		    INC (col);
		END (*IF*);
	    END (*LOOP*);
	    IF row = R.bottom THEN
		RETURN FALSE;
	    END (*IF*);
	    INC (row);
	END (*LOOP*);
    END HelpVisible;

(************************************************************************)

PROCEDURE LiteralAccumulatorDisplay (VAR (*IN*) Buffer: ARRAY OF CHAR);

    (* Displays "Buffer" as a text string at the screen location	*)
    (* reserved for the accumulator.					*)

    BEGIN
	SetCursor (calc, DisplayedRegisters+4, numberstart);
	WriteString (calc, Buffer);
    END LiteralAccumulatorDisplay;

(************************************************************************)

PROCEDURE WriteSpaces (N: CARDINAL);

    (* Writes a string of N spaces at the current cursor location.	*)

    VAR k: CARDINAL;

    BEGIN
	FOR k := 1 TO N DO
	    WriteChar (calc, " ");
	END (*FOR*);
    END WriteSpaces;

(************************************************************************)

PROCEDURE WriteParentheses (count: CARDINAL);

    (* Writes "count" left parentheses on the screen, with an	*)
    (* abbreviated display if count > 4.			*)

    VAR j: CARDINAL;

    BEGIN
	IF count > 4 THEN
	    WriteString (calc, "(..(");
	ELSE
	    FOR j := 1 TO count DO
		WriteChar (calc, "(");
	    END (*FOR*);
	    WriteSpaces (4-count);
	END (*IF*);
    END WriteParentheses;

(************************************************************************)

PROCEDURE DisplayRegister (j: RegisterNumber);

    (* Refreshes the display of the left parentheses, the value, and	*)
    (* the trailing operator for register j.				*)

    VAR row: CARDINAL;  operator: CHAR;

    BEGIN
	IF j = 0 THEN
	    row := DisplayedRegisters+4;
	ELSE
	    row := DisplayedRegisters+3-j;
	END (*IF*);
	SetCursor (calc, row, 2);
	IF Register[j].operator = EmptyMarker THEN
	    WriteSpaces (numberwidth+6);
	ELSE
	    IF Register[j].operator = "f" THEN
		IF Register[j+1].value = 0.0 THEN
		    operator := "-";
		ELSE
		    operator := " ";
		END (*IF*);
	    ELSE
		operator := Register[j].operator;
	    END (*IF*);
	    WriteChar (calc, operator);
	    WriteParentheses (Register[j].ParenCount);
	    SetCursor (calc, row, numberstart);
	    IF (j > 0) AND (Register[j-1].operator = "f") THEN
		WriteSpaces (numberwidth - functionnamewidth);
		WriteString (calc,
				FunctionName[FunctionType(Register[j].value)]);
	    ELSIF (j = 0) AND NOT NumberPresent THEN
		WriteSpaces (numberwidth);
	    ELSE
		WriteLongReal (calc, Register[j].value, numberwidth);
	    END (*IF*);
	END (*IF*);
    END DisplayRegister;

(************************************************************************)

PROCEDURE DisplayStack;

    (* Refreshes the display of the stack of registers.	*)

    VAR j: RegisterNumber;

    BEGIN
	FOR j := 0 TO DisplayedRegisters DO
	    DisplayRegister (j);
	END (*FOR*);

	(* If stack is empty, display help message *)

	IF Register[1].operator = EmptyMarker THEN
	    SetCursor (calc, 3, numberwidth DIV 2 - 1);
	    WriteString (calc, "Press F1 key");
	    SetCursor (calc, 4, numberwidth DIV 2 + 1);
	    WriteString (calc, "for help");
	END (*IF*);

    END DisplayStack;

(************************************************************************)

PROCEDURE DisplayMemory (j: MemoryNumber);

    (* Refreshes the display of "memory" register j.	*)

    BEGIN
	SetCursor (calc, DisplayedRegisters+j+6, numberstart);
	WriteLongReal (calc, MemoryValue[j], numberwidth);
    END DisplayMemory;

(************************************************************************)

PROCEDURE InitialDisplay;

    (* Assumption: the calculator window calc is already open.  This	*)
    (* procedure puts the initial picture of the calculator onto the	*)
    (* screen.								*)

    VAR mem: MemoryNumber;

    BEGIN
	(* Draw a box for the accumulator.	*)

	Box (calc, DisplayedRegisters+3, numberstart-1,
					numberwidth+1, 2, single);

	(* Display the register contents.	*)

	DisplayStack;

	(* Display the memory values.	*)

	FOR mem := 0 TO MAX(MemoryNumber) DO
	    SetCursor (calc, DisplayedRegisters+mem+6, 2);
	    WriteChar (calc, "M");  WriteChar (calc, CHR(ORD("0")+mem));
	    DisplayMemory (mem);
	END (*FOR*);

    END InitialDisplay;

(************************************************************************)
(*			    NUMERIC INPUT				*)
(************************************************************************)

PROCEDURE AcceptNumber (nextchar: CHAR);

    (* Reads a number from the keyboard.  On entry, nextchar holds the	*)
    (* first digit or the decimal point.  On exit, the input value is	*)
    (* in Register[0].value.						*)

    TYPE BufferSubscript = [1..numberwidth];

    VAR placevalue: LONGREAL;
	j: BufferSubscript;
	Buffer: ARRAY BufferSubscript OF CHAR;
	BufferFull: BOOLEAN;

    (********************************************************************)

    PROCEDURE GetNextChar;

	(* Displays the input so far (as a text string if it will fit,	*)
	(* otherwise by a call to WriteReal), and then reads nextchar.	*)

	BEGIN
	    IF BufferFull THEN
		DisplayRegister(0);
	    ELSE
		LiteralAccumulatorDisplay (Buffer);
	    END (*IF*);
	    nextchar := InKey();
	    IF NOT BufferFull THEN
		IF Buffer[1] <> " " THEN
		    BufferFull := TRUE;
		ELSE
		    FOR j := 1 TO numberwidth-1 DO
			Buffer[j] := Buffer[j+1];
		    END (*FOR*);
		    Buffer[numberwidth] := nextchar;
		END (*IF*);
	    END (*IF*);
	END GetNextChar;

    (********************************************************************)

    BEGIN
	Register[0].value := 0.0;  BufferFull := FALSE;
	FOR j := 1 TO numberwidth-1 DO
	    Buffer[j] := " ";
	END (*FOR*);
	NumberPresent := TRUE;
	Buffer[numberwidth] := nextchar;

	(* Read the part before the decimal point.	*)

	WITH Register[0] DO
	    WHILE nextchar IN CharSet {"0".."9"} DO
		value := 10.0*value + LONGREAL(ORD(nextchar) - ORD("0"));
		GetNextChar;
	    END (*WHILE*);
	END (*WITH*);

	(* Now the part after the decimal point, if any.	*)

	IF nextchar = "." THEN
	    GetNextChar;  placevalue := 0.1;
	    WHILE nextchar IN CharSet {"0".."9"} DO
		Register[0].value := Register[0].value
			+ placevalue*(LONGREAL(ORD(nextchar) - ORD("0")));
		placevalue := 0.1*placevalue;
		GetNextChar;
	    END (*WHILE*);
	END (*IF*);

	(* Correct for overshoot in input.	*)

	PutBack (nextchar);

    END AcceptNumber;

(************************************************************************)

PROCEDURE priority (operator: CHAR): CARDINAL;

    (* Returns the priority of an operator.	*)

    BEGIN
	CASE operator OF
		EndMarker:	RETURN 0;
	    |
		Enter,"=":	RETURN 1;
	    |
		"+","-":	RETURN 2;
	    |
		"*","/":	RETURN 3;
	    |
		"f":		IF Register[1].value = 0.0 THEN RETURN 7
				ELSE RETURN 4;
				END (*IF*);
	    |
		"x":		RETURN 5;
	    |
		"^":		RETURN 6;
	    |
		ELSE
				RETURN UnknownOperatorPriority;
	END (*CASE*);
    END priority;

(************************************************************************)

PROCEDURE TopOperatorPriority(): CARDINAL;

    (* TopOperatorPriority is normally the priority of the operator in	*)
    (* Register[0].  However any left parenthesis in Register[0]	*)
    (* overrides this; in that case we return an answer of 0.		*)

    BEGIN
	IF Register[0].ParenCount > 0 THEN RETURN 0
	ELSE RETURN priority (Register[0].operator)
	END (*IF*);
    END TopOperatorPriority;

(************************************************************************)
(*			    STACK MANIPULATION				*)
(************************************************************************)

PROCEDURE PushStack (LatestOperator: CHAR);

    (* Pushes the register stack, clearing the top one.  The argument	*)
    (* ends up as the operator in Register[0].  If the stack overflows	*)
    (* we give an audible alarm, but perform the push anyway.		*)

    VAR j: RegisterNumber;

    BEGIN
	IF Register[MaxRegisterNumber].operator <> EmptyMarker THEN
	    Register[MaxRegisterNumber-1].operator := EndMarker;
	    Beep;
	END (*IF*);
	FOR j := MaxRegisterNumber TO 1 BY -1 DO
	    Register[j] := Register[j-1];
	END (*FOR*);
	WITH Register[0] DO
	    operator := LatestOperator;  value := 0.0;  ParenCount := 0;
	END (*WITH*);
	NumberPresent := FALSE;
	DisplayStack;
    END PushStack;

(************************************************************************)

PROCEDURE PopStack;

    (* Pops the register stack, clearing the bottom register.	*)

    VAR j: RegisterNumber;

    BEGIN
	FOR j := 0 TO MaxRegisterNumber-1 DO
	    Register[j] := Register[j+1];
	END (*FOR*);
	WITH Register[MaxRegisterNumber] DO
	    ParenCount := 0;  value := 0.0;  operator := EmptyMarker;
	END (*WITH*);
	DisplayStack;
    END PopStack;

(************************************************************************)
(*			OPERATIONS ON THE MEMORIES			*)
(************************************************************************)

PROCEDURE GetMemoryNumber (): MemoryNumber;

    (* Returns the value of a one-digit memory number typed from the	*)
    (* keyboard.  Also wipes the "memory number" prompt on the display.	*)
    (* Assumes memory number 0 (and does not consume the typed key) if	*)
    (* no valid memory number is specified.				*)

    VAR ch: CHAR;

    BEGIN
	ch := InKey();
	SetCursor (calc, DisplayedRegisters+5, 3);
	WriteString (calc, " ");
	IF ch IN CharSet{"0"..CHR(ORD("0")+MaxMemoryNumber)} THEN
	    RETURN ORD(ch) - ORD("0");
	ELSE
	    PutBack(ch);  RETURN 0;
	END (*IF*);
    END GetMemoryNumber;

(************************************************************************)

PROCEDURE StoreToMemory;

    (* Gets a memory number from the keyboard, stores the accumulator	*)
    (* value in that memory register.					*)

    VAR mem: MemoryNumber;

    BEGIN
	SetCursor (calc, DisplayedRegisters+5, 3);
	WriteChar (calc, CHR(25));
	mem := GetMemoryNumber();
	MemoryValue[mem] := Register[0].value;  DisplayMemory(mem);
    END StoreToMemory;

(************************************************************************)
(*				OPERATIONS				*)
(************************************************************************)

PROCEDURE Divide0 (first, second: LONGREAL): LONGREAL;

    (* Computes first/second, except that division by zero gives 0.0.	*)

    BEGIN
	IF second = 0.0 THEN RETURN 0.0
	ELSE RETURN first/second;
	END (*IF*);
    END Divide0;

(************************************************************************)

PROCEDURE BinaryOperation;

    (* Performs the binary operation requested by Register[0].operator.	*)

    VAR x, y, result: LONGREAL;
	command: CHAR;

    BEGIN
	command := Register[0].operator;
	x := Register[1].value;  y := Register[0].value;
	result := x;
	IF command = "+" THEN result := result + y
	ELSIF command = "-" THEN result := result - y
	ELSIF (command = "*") OR (command = "x") THEN result := result * y
	ELSIF command = "/" THEN result := Divide0 (x, y)
	ELSIF command = "^" THEN result := Pow (x, y)
	ELSIF command = "f" THEN
	    result := Function[VAL(FunctionType,x)] (y);
	ELSE Beep;
	END (*IF*);
	Register[1].value := result;
	PopStack;
    END BinaryOperation;

(************************************************************************)

PROCEDURE PostfixUnaryOperation (code: CHAR);

    (* Performs the unary operation requested by code.	*)

    BEGIN
	IF code = "%" THEN
	    Register[0].value := 0.01*Register[0].value*Register[1].value;
	ELSIF code = ")" THEN
	    IF Register[0].ParenCount > 0 THEN
		DEC (Register[0].ParenCount);
	    ELSIF Register[0].operator <> EndMarker THEN
		BinaryOperation;  PutBack (")");
	    ELSE
		Beep;
	    END (*IF*);
	ELSIF (code="s") OR (code="S") THEN
	    StoreToMemory;
	ELSE
	    Beep;
	END (*IF*);
	DisplayRegister(0);
    END PostfixUnaryOperation;

(************************************************************************)
(*		GETTING A FUNCTION NAME BY MENU SELECTION		*)
(************************************************************************)

PROCEDURE ReadBuiltinFunctionName;

    (* Allows the user to select a function name from a menu.  We then	*)
    (* load the stack with the function number, and the special		*)
    (* "binary operator" f.						*)

    VAR funcmenu: Menu;  menutext: ARRAY FunctionType OF ItemText;
	function: FunctionType;

    BEGIN
	menutext[0] := "    Function";
	FOR function := 1 TO MaxFunctionNumber DO
	    Copy (menutext[function], FunctionName[function]);
	END (*FOR*);
	CreateMenu (funcmenu, 3, menutext, MaxFunctionNumber);
	PositionMenu (funcmenu, 14, 22, 60, 78);
	function := SelectFromMenu (funcmenu);
	DestroyMenu (funcmenu);
	IF function <> 0 THEN
	    Register[0].value := VAL(LONGREAL, function);
	    PushStack ("f");
	END (*IF*);
    END ReadBuiltinFunctionName;

(************************************************************************)
(*			THE CALCULATOR CONTROL LOGIC			*)
(************************************************************************)

PROCEDURE HandleFunctionKey;

    (* Looks after the cases where the keyboard input code was CHR(0).	*)
    (* In the present version, the arrow keys and F1 key are looked	*)
    (* after, and all other function keys are ignored.			*)

    VAR code: CHAR;

    BEGIN
	code := InKey();
	IF (code = ";") OR (code = "T") THEN	(* F1 or Shift/F1 *)
	    PutOnTop (help);
	ELSIF HelpVisible() THEN		(* check for arrow key *)
	    MoveWindow (code, help);
	ELSE					(* check for arrow key *)
	    MoveWindow (code, calc);
	END (*IF*);
    END HandleFunctionKey;

(************************************************************************)

PROCEDURE LoadAccumulator (VAR (*OUT*) nextchar: CHAR);

    (* Loads the accumulator with a number, also accepting and keeping	*)
    (* track of any opening parentheses.  Unary operations are also	*)
    (* dealt with by this procedure; and this could lead to the		*)
    (* evaluation of entire subexpressions, because we treat a closing	*)
    (* parenthesis as a unary postfix operator.  On return, nextchar	*)
    (* holds the following keyboard character (usually an operator, but	*)
    (* it could also be Esc, Return, or an illegal keystroke).  Most of	*)
    (* the complexity of this procedure lies in the fact that the user	*)
    (* can also type Backspace at any time, which has the effect of	*)
    (* cancelling the latest number, left parenthesis, or unevaluated	*)
    (* operator, as appropriate.					*)

    (* It is possible that the user will enter no value before the	*)
    (* operator.  In this case, the previous accumulator contents are	*)
    (* retained, unless they have been wiped out by a backspace.	*)
    (* Conversely, the user can override a number which is already	*)
    (* present.  We try to give a legal meaning, wherever possible, to	*)
    (* any user input.							*)

    CONST Backspace = CHR(8);
          Starters = CharSet {"(", ".", "0".."9", "f", "F", "m", "M",
				"p", "P"};
	  Misc = CharSet {CHR(0), "e", "E", EndMarker, Backspace};
	  HandledHere = Starters + Misc + UnaryOperatorSet;

    BEGIN
	LOOP
	    nextchar := InKey();

	    (* If the input is such as to imply that a new number is	*)
	    (* to be entered, discard any number already in the		*)
	    (* accumulator - i.e. allow the user to override any	*)
	    (* previous input.						*)

	    IF nextchar IN Starters THEN
		NumberPresent := FALSE;
		DisplayRegister(0);
	    END (*IF*);

	    (* On seeing a "-", we have to decide whether it is a	*)
	    (* unary minus (if so, handle it here) or a binary minus.	*)

	    IF nextchar = "-" THEN
		IF NumberPresent THEN EXIT(*LOOP*)
		ELSE
		    Register[0].value := 0.0;
		    PushStack ("f");
		END (*IF*);

	    ELSIF NOT (nextchar IN HandledHere) THEN
		EXIT (*LOOP*);

	    (* Any character which, by coincidence, has the	*)
	    (* same character code as EndMarker is ignored.	*)

	    ELSIF nextchar = EndMarker THEN (* do nothing *)

	    (* Function key? *)

	    ELSIF nextchar = CHR(0) THEN HandleFunctionKey

	    (* Read prefix unary operator.  We don't evaluate it here;	*)
	    (* it's put on the stack to look like a binary operator.	*)

	    ELSIF CAP(nextchar) = "F" THEN ReadBuiltinFunctionName;

	    (* Handle postfix unary operator.	*)

	    ELSIF nextchar IN UnaryOperatorSet THEN
		PostfixUnaryOperation (nextchar);
		NumberPresent := TRUE;

	    (* Handle opening parenthesis.	*)

	    ELSIF nextchar = "(" THEN
		INC (Register[0].ParenCount);
		DisplayRegister(0);

	    (* P means the constant PI.	*)

	    ELSIF CAP(nextchar) = "P" THEN
		Register[0].value := 3.14159265359;
		NumberPresent := TRUE;
		DisplayRegister(0);

	    (* Fetch a number.	*)

	    ELSIF nextchar IN CharSet {"0".."9", "."} THEN
		AcceptNumber (nextchar);

	    (* We use the calculator itself to evaluate "E" notation.	*)

	    ELSIF CAP(nextchar) = "E" THEN
		IF NOT NumberPresent THEN
		    Register[0].value := 1.0;
		    NumberPresent := TRUE;
		END (*IF*);
		PushStack ("x");
		Register[0].value := 10.0;
		PutBack ("^");

	    (* Or an operand from memory.	*)

	    ELSIF CAP(nextchar) ="M" THEN
		SetCursor (calc, DisplayedRegisters+5, 3);
		WriteChar (calc, CHR(24));
		Register[0].value := MemoryValue[GetMemoryNumber()];
		NumberPresent := TRUE;
		DisplayRegister(0);

	    (* Now the hard part: handle Backspace.	*)

	    ELSIF nextchar = Backspace THEN

		(* The effect of a backspace depends on whether the	*)
		(* accumulator holds a user-supplied number at this	*)
		(* stage.  This depends on things like whether the	*)
		(* user has typed several backspaces in a row.		*)

		IF NumberPresent THEN

		    (* Delete the number in the accumulator.	*)

		    Register[0].value := 0.0;  NumberPresent := FALSE;

		ELSIF Register[0].ParenCount > 0 THEN

		    (* Remove one left parenthesis.	*)

		    DEC (Register[0].ParenCount);

		ELSE    (* Delete the last outstanding operator, if any. *)

		    IF Register[0].operator = EndMarker THEN
			Beep;
		    ELSIF Register[0].operator = "f" THEN
			PopStack;
			Register[0].value := 0.0;
			NumberPresent := FALSE;
		    ELSE
			PopStack;
			NumberPresent := TRUE;
		    END (*IF*);

		END (*IF*);

		DisplayRegister(0);

	    END (*IF*);

	END (*LOOP*);

    END LoadAccumulator;

(************************************************************************)

PROCEDURE PerformCalculation;

    (* This procedure consists of a loop which is repeated until an	*)
    (* Esc character is encountered.  Each time around the loop, we	*)
    (* pick up an operand followed by an operator.  (Fetching the	*)
    (* operand, which is done by procedure LoadAccumulator, may itself	*)
    (* involve some subexpression evaluation, because the operand can	*)
    (* include things like opening and closing parentheses, prefix and	*)
    (* postfix functions, and the like.  Procedure LoadAccumulator also	*)
    (* allows some of the preceding input to be deleted via the		*)
    (* Backspace key.)  The operator may be a binary operator, or Esc,	*)
    (* or Enter, or '='.  (These last two are considered to be		*)
    (* equivalent.)  Anything else is considered to be an unknown	*)
    (* operator, and results in an audible Beep.			*)
    (* A calculation step, or possibly a whole sequence of steps, is	*)
    (* triggered if there are more closing parentheses than opening	*)
    (* parentheses, or if the operator has lower priority than the last	*)
    (* stacked operator.						*)

    VAR operator: CHAR;

    BEGIN
	LOOP
	    LoadAccumulator (operator);

	    (* The Esc key drops us out of the calculator - unless the	*)
	    (* help window is on display, in which case Esc simply	*)
	    (* removes the help window from the display.		*)

	    IF operator = Esc THEN
		IF HelpVisible() THEN
		    Hide (help);  operator := Enter;
		ELSE
		    EXIT (*LOOP*);
		END (*IF*);
	    END (*IF*);

	    (* Perform any pending operations. *)

	    NumberPresent := TRUE;
	    WHILE TopOperatorPriority() >= priority(operator) DO
		BinaryOperation;
	    END (*WHILE*);

	    (* Push the latest operator, unless it marks the end	*)
	    (* of the calculation.					*)

	    IF priority(operator) = UnknownOperatorPriority THEN
		Beep;
	    ELSIF (operator <> Enter) AND (operator <> "=") THEN
		PushStack (operator);
	    END(*IF*);

	END (*LOOP*);

    END PerformCalculation;

(************************************************************************)

PROCEDURE ForceExit (w: Window;  row: RowRange;  col: ColumnRange);

    (* This procedure is triggered by a mouse click on the "hide"	*)
    (* button.  We turn this into a keyboard Esc character, or two Esc	*)
    (* characters if the help window is visible.			*)

    BEGIN
	IF HelpVisible() THEN StuffKeyboardBuffer (Esc) END(*IF*);
	StuffKeyboardBuffer (Esc);
    END ForceExit;

(************************************************************************)

PROCEDURE ForceF1 (w: Window;  row: RowRange;  col: ColumnRange);

    (* This procedure is triggered by a mouse click on the "F1"		*)
    (* message.  We turn this into a keyboard F1 keypress.		*)

    BEGIN
	StuffKeyboardBuffer (CHR(0));
	StuffKeyboardBuffer (';');
    END ForceF1;

(************************************************************************)
(*			INTERFACE TO THE CALLER				*)
(************************************************************************)

PROCEDURE RunCalculator;

    (* Displays a calculator window on the screen; this can be operated	*)
    (* from the numeric keypad.  On exit, the screen window is closed,	*)
    (* but calculation results are saved for the next invocation of	*)
    (* this procedure.							*)

    VAR KeyboardLocks: CARDINAL;  R: Rectangle;
	(*<UseMouse*)
	UIW: UIWindow;
	(*>*)

    BEGIN
	(* Set the NumLock state, if not already set.	*)

	KeyboardLocks := LockStatus();
	IF ORD(IAND (KeyboardLocks, NumLockLED)) = 0 THEN
	    SetLocks (KeyboardLocks + NumLockLED);
	END (*IF*);

	OpenWindow (calc, yellow, blue,
			baserow, baserow+DisplayedRegisters+MaxMemoryNumber+7,
			    basecol, basecol+numberwidth+9,
				simpleframe, doubledivider);
	WriteString (calc, "   Calculator");
	ChangeScrollingRegion (calc, 3, DisplayedRegisters+MaxMemoryNumber+6);

	(*<UseMouse*)
	UIW := AllowMouseControl (calc, "Calculator",
				CapabilitySet {wshow, wmove, whide});
	AddActiveRegion (UIW, 0, 0, numberwidth+7, numberwidth+7,
				ButtonSet {LeftButton}, ForceExit);
	AddActiveRegion (UIW,3,3, numberwidth DIV 2 + 5, numberwidth DIV 2 + 6,
				ButtonSet {LeftButton}, ForceF1);
	(*>*)

	CreateHelpWindow;
	InitialDisplay;
	PerformCalculation;
	R := WindowLocation (help);
	helprow := R.top;  helpcol := R.left;
	(*<UseMouse*) HideMouseCursor; (*>*)
	CloseWindow (help);
	R := WindowLocation (calc);
	baserow := R.top;  basecol := R.left;
	CloseWindow (calc);
	(*<UseMouse*) ShowMouseCursor; (*>*)

	SetLocks (KeyboardLocks);

    END RunCalculator;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

PROCEDURE ClearCalculatorState;

    (* Clears all of the working registers of the calculator.	*)

    VAR j: RegisterNumber;  mem: MemoryNumber;

    BEGIN
	FOR j := 0 TO MaxRegisterNumber DO
	    WITH Register[j] DO
		ParenCount := 0;
		value := 0.0;
		operator := EmptyMarker;
	    END (*WITH*);
	END (*FOR*);
	Register[0].operator := EndMarker;
	NumberPresent := FALSE;
	FOR mem := 0 TO MAX(MemoryNumber) DO
	    MemoryValue[mem] := 0.0;
	END (*FOR*);
    END ClearCalculatorState;

(************************************************************************)

BEGIN
    baserow := 0;  basecol := 70 - numberwidth;
    helprow := 7;  helpcol := 26;
    ClearCalculatorState;
    (*<~TopSpeed3
    Function[0] := Negative;
    Function[1] := Sqrt;
    Function[2] := Exp;
    Function[3] := Log;
    Function[4] := Log10;
    Function[5] := Sin;
    Function[6] := Cos;
    Function[7] := Tan;
    Function[8] := ASin;
    Function[9] := ACos;
    Function[10] := ATan;
    Function[11] := SinH;
    Function[12] := CosH;
    Function[13] := TanH;
    >*)
END Calculator.
