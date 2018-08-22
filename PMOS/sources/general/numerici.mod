IMPLEMENTATION MODULE NumericIO;

	(****************************************************************)
	(*								*)
	(*		Numeric I/O using windows.			*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	18 February 1994			*)
	(*  Status:		OK					*)
	(*								*)
	(****************************************************************)

FROM Windows IMPORT
    (* type *)	ColumnRange,
    (* proc *)	WriteChar, WriteString, ReadChar, LookaheadChar,
		ReadCharWithoutEcho, EditString, SaveCursor, SetCursor;

FROM Conversions IMPORT
    (* type *)	HexDigit, EightChar,
    (* proc *)	CardinalToString, StringToCardinal, StringToHex,
		LongCardToString, HexToChar, HexToString, LongHexToString;

FROM LowLevel IMPORT
    (* proc *)	SEGMENT, OFFSET,
		HighByte, LowByte, HighWord, LowWord,
		IANDB, RSB;

FROM Keyboard IMPORT
    (* proc *)	InKey, PutBack;

(************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST Digits = CharSet {"0".."9"};

(************************************************************************)
(*				OUTPUT					*)
(************************************************************************)

PROCEDURE WriteHexDigit (w: Window;  number: BYTE);

    (* Writes a one-digit hexadecimal number.	*)

    BEGIN
	WriteChar (w, HexToChar(VAL(HexDigit, number)));
    END WriteHexDigit;

(************************************************************************)

PROCEDURE WriteHexByte (w: Window;  number: BYTE);

    (* Writes the second argument as a two-digit hexadecimal number.	*)

    BEGIN

	(* The obscure function names from LowLevel are:	*)
	(*	RSB = right shift	IANDB = logical AND	*)

	WriteHexDigit (w, RSB(number, 4));
	WriteHexDigit (w, IANDB(number,15));

    END WriteHexByte;

(************************************************************************)

PROCEDURE WriteHexWord (w: Window;  number: CARDINAL);

    (* Writes the second argument as a four-digit hexadecimal number.	*)

    BEGIN
	WriteHexByte (w, HighByte(number));
	WriteHexByte (w, LowByte(number));
    END WriteHexWord;

(************************************************************************)

PROCEDURE WriteHexLongword (w: Window;  number: LONGCARD);

    (* Writes the second argument as an eight-digit hexadecimal number.	*)

    BEGIN
	WriteHexWord (w, HighWord(number));
	WriteHexWord (w, LowWord(number));
    END WriteHexLongword;

(************************************************************************)

PROCEDURE WriteAddress (w: Window;  addr: ADDRESS);

    (* Writes a segmented address to the screen.	*)

    BEGIN
	WriteHexWord (w, SEGMENT(addr));  WriteChar (w, ":");
	WriteHexWord (w, OFFSET(addr));
    END WriteAddress;

(************************************************************************)

PROCEDURE WriteLongCard (w: Window;  number: LONGCARD);

    (* Writes the second argument as a decimal number.	*)

    VAR remainder: CARDINAL;

    BEGIN
	IF number > 9 THEN
	    WriteLongCard (w, number DIV 10);
	END (*IF*);
	remainder := CARDINAL(number MOD 10);
	WriteChar (w, CHR(remainder + ORD("0")));
    END WriteLongCard;

(************************************************************************)

PROCEDURE WriteCard (w: Window;  number: CARDINAL);

    (* Writes the second argument as a decimal number.	*)

    BEGIN
	WriteLongCard (w, VAL(LONGCARD,number));
    END WriteCard;

(************************************************************************)

PROCEDURE WriteShortCard (w: Window;  number: SHORTCARD);

    (* Writes the second argument as a decimal number.	*)

    BEGIN
	WriteLongCard (w, VAL(LONGCARD,number));
    END WriteShortCard;

(************************************************************************)

PROCEDURE WriteInt (w: Window;  number: INTEGER);

    (* Writes the second argument as a decimal number.	*)

    BEGIN
	IF number < 0 THEN
	    WriteChar (w, '-');  number := -number;
	END (*IF*);
	WriteCard (w, number);
    END WriteInt;

(************************************************************************)

PROCEDURE WriteRJLongCard (w: Window;  number: LONGCARD; fieldsize: CARDINAL);

    (* Like WriteLongCard, but the result is right justified in a field	*)
    (* of fieldsize characters.						*)

    VAR buffer: ARRAY [0..79] OF CHAR;

    BEGIN
	LongCardToString (number, buffer, fieldsize);
	buffer[fieldsize] := CHR(0);
	WriteString (w, buffer);
    END WriteRJLongCard;

(************************************************************************)

PROCEDURE WriteRJCard (w: Window;  number, fieldsize: CARDINAL);

    (* Like WriteCard, but the result is right justified in a field	*)
    (* of fieldsize characters.						*)

    VAR buffer: ARRAY [0..79] OF CHAR;

    BEGIN
	CardinalToString (number, buffer, fieldsize);
	buffer[fieldsize] := CHR(0);
	WriteString (w, buffer);
    END WriteRJCard;

(************************************************************************)

PROCEDURE WriteRJShortCard (w: Window;  number: SHORTCARD;
					fieldsize: CARDINAL);

    (* Like WriteShortCard, but the result is right justified in a	*)
    (* field of fieldsize characters.					*)

    BEGIN
	WriteRJCard (w, CARDINAL(number), fieldsize);
    END WriteRJShortCard;

(************************************************************************)
(*			CHECK FOR Esc KEY				*)
(************************************************************************)

PROCEDURE EditOK (): BOOLEAN;

    (* Returns TRUE unless the Esc key was pressed.	*)

    CONST Esc = CHR(27);

    VAR ch: CHAR;

    BEGIN
	ch := InKey();  PutBack(ch);
	RETURN ch <> Esc;
    END EditOK;

(************************************************************************)
(*			  HEXADECIMAL INPUT				*)
(************************************************************************)

PROCEDURE EditHexByte (w: Window;  VAR (*INOUT*) value: BYTE);

    (* Screen editing of a 2-digit hexadecimal value *)

    VAR buffer: ARRAY [0..1] OF CHAR;

    BEGIN
	HexToString (CARDINAL(value), buffer);
	EditString (w, buffer, 2);
	IF EditOK() THEN
	    value := BYTE (StringToHex (buffer));
	END (*IF*);
    END EditHexByte;

(************************************************************************)

PROCEDURE EditHexWord (w: Window;  VAR (*INOUT*) value: CARDINAL);

    (* Screen editing of a 4-digit hexadecimal value *)

    VAR buffer: ARRAY [0..3] OF CHAR;

    BEGIN
	HexToString (value, buffer);
	EditString (w, buffer, 4);
	IF EditOK() THEN
	    value := CARDINAL (StringToHex (buffer));
	END (*IF*);
    END EditHexWord;

(************************************************************************)

PROCEDURE EditHexLongword (w: Window;  VAR (*INOUT*) value: LONGCARD);

    (* Screen editing of an 8-digit hexadecimal value *)

    VAR buffer: EightChar;

    BEGIN
	LongHexToString (value, buffer);
	EditString (w, buffer, 8);
	IF EditOK() THEN
	    value := StringToHex (buffer);
	END (*IF*);
    END EditHexLongword;

(************************************************************************)
(*			    DECIMAL INPUT				*)
(************************************************************************)

PROCEDURE ReadCard (w: Window;  VAR (*OUT*) number: CARDINAL);

    (* Reads a decimal number, skipping over all non-numeric input.	*)

    VAR ch: CHAR;

    BEGIN
	number := 0;

	(* Skip over any leading non-numeric input.	*)

	WHILE NOT (LookaheadChar(w) IN Digits) DO
	    ReadCharWithoutEcho (w, ch);
	END (*WHILE*);

	(* Now read and convert the desired input.	*)

	WHILE LookaheadChar(w) IN Digits DO
	    ReadChar (w, ch);
	    number := 10*number + ORD(ch) - ORD("0");
	END (*WHILE*);
    END ReadCard;

(*************************************************************************)

PROCEDURE ReadBufferedCardinal (w: Window;  fieldsize: CARDINAL): CARDINAL;

    (* Reads a decimal number.  The difference between this and		*)
    (* ReadCard is that the user is given a reverse-video field of a	*)
    (* fixed width to work in, and is able to use the cursor control	*)
    (* keys to edit within that field.					*)

    VAR buffer: ARRAY ColumnRange OF CHAR;
	result: CARDINAL;  row, column: CARDINAL;

    BEGIN
	SaveCursor (w, row, column);
	buffer := "";
	EditString (w, buffer, fieldsize);
	result := StringToCardinal (buffer);
	SetCursor (w, row, column);
	WriteRJCard (w, result, fieldsize);
	RETURN result;
    END ReadBufferedCardinal;

(*************************************************************************)

PROCEDURE EditCardinal (w: Window;  VAR (*INOUT*) value: CARDINAL;
						fieldsize: CARDINAL);

    (* Screen editing of a decimal number. *)

    VAR buffer: ARRAY ColumnRange OF CHAR;
	row, column: CARDINAL;

    BEGIN
	SaveCursor (w, row, column);
	CardinalToString (value, buffer, fieldsize);
	EditString (w, buffer, fieldsize);
	IF EditOK() THEN
	    value := StringToCardinal (buffer);
	END (*IF*);
	SetCursor (w, row, column);
	WriteRJCard (w, value, fieldsize);
    END EditCardinal;

(*************************************************************************)

END NumericIO.
