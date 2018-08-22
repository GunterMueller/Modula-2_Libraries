IMPLEMENTATION MODULE PPExpr;

	(********************************************************)
	(*							*)
	(*	   Expression handler for preprocessor		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	9 February 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(*	Could possibly be improved in terms of code	*)
	(*	compactness and clarity; the present version	*)
	(*	duplicates many code sections because of the	*)
	(*	fact that we don't know in advance whether	*)
	(*	we're dealing with a Boolean-valued result.	*)
	(*							*)
	(********************************************************)

FROM PPMisc IMPORT
    (* proc *)	StringMatch, CopyString, Message, EndOfMessage;

(************************************************************************)

TYPE
    SymbolTableIndex = [0..50];
    String = ARRAY [0..31] OF CHAR;
    CharSet = SET OF CHAR;

CONST
    AlphaNumerics = CharSet {"A".."Z", "a".."z", "0".."9"};

VAR
    (* The symbol table is a table of string-valued variables.  In this	*)
    (* application the table is likely always to be quite small, so we	*)
    (* can get away with an unsorted table and a crude linear search.	*)
    (* NextEntry is the location of the first unused entry.		*)

    SymTable: ARRAY SymbolTableIndex OF
		    RECORD
			sym, val: String;
		    END (*RECORD*);

    NextEntry: SymbolTableIndex;

(************************************************************************)
(*			  SYMBOL TABLE HANDLING				*)
(************************************************************************)

PROCEDURE LocateSymbol (VAR (*IN*) symbol: ARRAY OF CHAR;
			VAR (*OUT*) location: SymbolTableIndex): BOOLEAN;

    BEGIN
	location := 0;
	LOOP
	    IF location = NextEntry THEN
	        RETURN FALSE;
	    END (*IF*);
	    IF StringMatch (symbol, SymTable[location].sym) THEN
	        RETURN TRUE;
	    END (*IF*);
	    INC (location);
	END (*LOOP*);
    END LocateSymbol;

(************************************************************************)

PROCEDURE Lookup (VAR (*IN*) symbol: ARRAY OF CHAR;
				VAR (*OUT*) value: ARRAY OF CHAR);

    (* Gets the value of "symbol" from the symbol table.  If the symbol	*)
    (* is not there, the result is the string value of "symbol" itself.	*)

    VAR found: BOOLEAN;  place: SymbolTableIndex;

    BEGIN
	IF LocateSymbol (symbol, place) THEN
	    CopyString (SymTable[place].val, value);
	ELSE
	    CopyString (symbol, value);
	END (*IF*);
    END Lookup;

(************************************************************************)

PROCEDURE DefineSymbol (VAR (*IN*) symbol: ARRAY OF CHAR;
					value: ARRAY OF CHAR);

    (* Stores "value" as the value of variable "symbol".  The variable	*)
    (* might or might not already exist in the symbol table.		*)

    VAR found: BOOLEAN;  place: SymbolTableIndex;

    BEGIN
	found := LocateSymbol (symbol, place);
	IF NOT found THEN
	    place := NextEntry;  INC(NextEntry);
	    CopyString (symbol, SymTable[place].sym);
	END (*IF*);
	CopyString (value, SymTable[place].val);
    END DefineSymbol;

(************************************************************************)

PROCEDURE DumpSymbolTable;

    (* Writes the current contents of the symbol table to the screen. *)

    VAR j: SymbolTableIndex;

    BEGIN
	IF NextEntry = 0 THEN
	    Message ("Warning: No symbols currently defined");
	ELSE
	    Message ("LIST OF SYMBOLS CURRENTLY DEFINED");
	    EndOfMessage;  EndOfMessage;
	    FOR j := 0 TO NextEntry-1 DO
		Message ("      ");
		WITH SymTable[j] DO
		    Message (sym);  Message (" = ");
		    Message (val);
		END (*WITH*);
		EndOfMessage;
	    END (*FOR*);
	END (*IF*);
	EndOfMessage;
    END DumpSymbolTable;

(************************************************************************)
(*			  EXPRESSION EVALUATION				*)
(*									*)
(* In each of the following procedures, the source string starts at	*)
(* Buffer[Place], Place is updated so that on exit Buffer[Place] is the	*)
(* first non-alphanumeric character found.				*)
(* Note: short-circuit evaluation is NOT used, since in all cases we	*)
(* want to scan to the end of the input string.				*)
(*									*)
(************************************************************************)

PROCEDURE Id (VAR (*IN*) Buffer: ARRAY OF CHAR;
		VAR (*INOUT*) Place: CARDINAL;
		VAR (*OUT*) result: ARRAY OF CHAR);

    (* Picks up an alphanumeric string starting at Buffer[Place]. *)

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	WHILE Buffer[Place] IN AlphaNumerics DO
	    result[j] := Buffer[Place];
	    INC (Place);  INC(j);
	END (*WHILE*);
	IF j <= HIGH(result) THEN
	    result[j] := CHR(0);
	END (*IF*);
    END Id;

(************************************************************************)

PROCEDURE EvalId (VAR (*IN*) Buffer: ARRAY OF CHAR;
		VAR (*INOUT*) Place: CARDINAL;
		VAR (*OUT*) result: ARRAY OF CHAR);

    (* Picks up and evaluates an alphanumeric string starting at	*)
    (* Buffer[Place].  The difference between this procedure and Id is	*)
    (* that Id returns the symbol literally, whereas this procedure	*)
    (* returns the result of evaluating the string.			*)

    VAR symbol: String;

    BEGIN
	Id (Buffer, Place, symbol);
	Lookup (symbol, result);
    END EvalId;

(************************************************************************)

PROCEDURE Factor (VAR (*IN*) Buffer: ARRAY OF CHAR;
		VAR (*INOUT*) Place: CARDINAL): BOOLEAN;

    (* Evaluates a Boolean expression consisting of a single identifier	*)
    (* or an expression in parentheses.					*)

    VAR NOTcount: CARDINAL;  value, value2: String;  result: BOOLEAN;

    BEGIN
	(* To evaluate "NOT" conditions, I can avoid recursion by	*)
	(* simply waltzing my ~ until there are no more.		*)

	NOTcount := 0;
	WHILE Buffer[Place] = "~" DO
	    INC (NOTcount);  INC(Place);
	END (*WHILE*);

	(* The remaining cases we have to handle are parenthesised	*)
	(* expressions, comparisons, and simple identifiers.		*)

	IF Buffer[Place] = "(" THEN
	    INC (Place);
	    result := Expr (Buffer, Place);
	    IF Buffer[Place] = ")" THEN
		INC (Place);
	    ELSE
		Message ("Error: Mismatched () in expression.");
		EndOfMessage;
	    END (*IF*);
	ELSE
	    EvalId (Buffer, Place, value);
	    IF Buffer[Place] = "=" THEN
		INC (Place);
		EvalId (Buffer, Place, value2);
	    ELSIF (Buffer[Place] = "<") AND (Buffer[Place+1] = ">") THEN
		INC (Place, 2);
		EvalId (Buffer, Place, value2);
		INC (NOTcount);
	    ELSE
		CopyString ("TRUE", value2);
	    END (*IF*);
	    result := StringMatch (value, value2);
	END (*IF*);

	IF ODD (NOTcount) THEN result := NOT result END (*IF*);
	RETURN result;

    END Factor;

(************************************************************************)

PROCEDURE StringFactor (VAR (*IN*) Buffer: ARRAY OF CHAR;
		VAR (*INOUT*) Place: CARDINAL;
		VAR (*OUT*) result: ARRAY OF CHAR);

    (* Evaluates a string-valued factor starting at Buffer[Place]. *)

    VAR value2: String;

    BEGIN
	IF Buffer[Place] IN AlphaNumerics THEN

	    (* We have a simple Id, or a comparison. *)

	    EvalId (Buffer, Place, result);
	    IF Buffer[Place] = "=" THEN
		INC (Place);
		EvalId (Buffer, Place, value2);
		IF StringMatch (result, value2) THEN
		    CopyString ("TRUE", result);
		ELSE
		    CopyString ("FALSE", result);
		END (*IF*);
	    ELSIF (Buffer[Place] = "<") AND (Buffer[Place+1] = ">") THEN
		INC (Place, 2);
		EvalId (Buffer, Place, value2);
		IF StringMatch (result, value2) THEN
		    CopyString ("FALSE", result);
		ELSE
		    CopyString ("TRUE", result);
		END (*IF*);
	    END (*IF*);

	ELSE

	    (* It's definitely a Boolean factor. *)

	    IF Factor(Buffer,Place) THEN
		CopyString ("TRUE", result);
	    ELSE
		CopyString ("FALSE", result);
	    END (*IF*);

	END (*IF*);

    END StringFactor;

(************************************************************************)

PROCEDURE Term (VAR (*IN*) Buffer: ARRAY OF CHAR;
		VAR (*INOUT*) Place: CARDINAL): BOOLEAN;

    (* Evaluates a Boolean expression consisting of one or more factors	*)
    (* connected by logical AND operations (indicated by "&").		*)

    VAR result: BOOLEAN;

    BEGIN
	result := Factor (Buffer, Place);
	WHILE Buffer[Place] = "&" DO
	    INC (Place);
	    result := Factor(Buffer,Place) AND result;
	END (*WHILE*);
	RETURN result;
    END Term;

(************************************************************************)

PROCEDURE StringTerm (VAR (*IN*) Buffer: ARRAY OF CHAR;
		VAR (*INOUT*) Place: CARDINAL;
		VAR (*OUT*) result: ARRAY OF CHAR);

    (* Evaluates a string-valued term starting at Buffer[Place]. *)

    BEGIN
	StringFactor (Buffer, Place, result);
	IF Buffer[Place] = "&" THEN
	    INC (Place);
	    IF Expr(Buffer,Place) AND StringMatch(result,"TRUE") THEN
		CopyString ("TRUE", result);
	    ELSE
		CopyString ("FALSE", result);
	    END (*IF*);
	END (*IF*);
    END StringTerm;

(************************************************************************)

PROCEDURE Expr (VAR (*IN*) Buffer: ARRAY OF CHAR;
		VAR (*INOUT*) Place: CARDINAL): BOOLEAN;

    (* Evaluates a Boolean expression starting at Buffer[Place].	*)
    (* On exit Place is updated so that Line[Place] is the first	*)
    (* non-alphanumeric character found.				*)

    VAR result: BOOLEAN;

    BEGIN
	result := Term (Buffer, Place);
	WHILE Buffer[Place] = "|" DO
	    INC (Place);
	    result := Term(Buffer,Place) OR result;
	END (*WHILE*);
	RETURN result;
    END Expr;

(************************************************************************)

PROCEDURE StringExpr (VAR (*IN*) Buffer: ARRAY OF CHAR;
		VAR (*INOUT*) Place: CARDINAL;
		VAR (*OUT*) result: ARRAY OF CHAR);

    (* Evaluates a string-valued expression starting at Buffer[Place]. *)

    BEGIN
	StringTerm (Buffer, Place, result);
	IF Buffer[Place] = "|" THEN
	    INC (Place);
	    IF Expr(Buffer,Place) OR StringMatch(result,"TRUE") THEN
		CopyString ("TRUE", result);
	    ELSE
		CopyString ("FALSE", result);
	    END (*IF*);
	END (*IF*);
    END StringExpr;

(************************************************************************)

VAR s: CHAR;

BEGIN
    NextEntry := 0;
    s := "T";  DefineSymbol (s, "TRUE");
    s := "F";  DefineSymbol (s, "FALSE");
END PPExpr.
