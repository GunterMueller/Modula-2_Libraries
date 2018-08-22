IMPLEMENTATION MODULE Conversions;

	(********************************************************)
	(*							*)
	(*		Miscellaneous type conversions		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	23 July 1994			*)
	(*  Status:		Working				*)
	(*							*)
	(*	Have not fully tested the cases where the field	*)
	(*	 size is too small.				*)
	(*	Seems to be a loss of accuracy when converting	*)
	(*	 E-format numbers; for example 123.456E7 is	*)
	(*	 converted to 1234559999 when passed through	*)
	(*	 StringToReal and then RealToString.  I'm not	*)
	(*	 yet sure where this is happening, but suspect	*)
	(*	 that it requires some deep error analysis.	*)
	(*							*)
	(********************************************************)

FROM LowLevel IMPORT
    (* proc *)	IAND, RS, LowWord, HighWord;

CONST tab = CHR(9);

TYPE CharSet = SET OF CHAR;

(************************************************************************)
(*			   BUFFER MANIPULATION				*)
(************************************************************************)

PROCEDURE ShiftRight (VAR (*INOUT*) buffer: ARRAY OF CHAR;
				first, last, amount: CARDINAL);

    (* Moves the contents of buffer[first..last] right by the specified	*)
    (* number of characters, space filling at the left and discarding	*)
    (* characters shifted out at the right.				*)

    VAR j: CARDINAL;

    BEGIN
	IF amount > 0 THEN
	    FOR j := last TO first+amount BY -1 DO
		buffer[j] := buffer[j-amount];
	    END (*FOR*);
	    FOR j := first TO first+amount-1 DO
		buffer[j] := " ";
	    END (*FOR*);
	END (*IF*);
    END ShiftRight;

(************************************************************************)
(*			REAL NUMBER TO CARDINAL POWER			*)
(************************************************************************)

PROCEDURE atoi (a: LONGREAL;  i: CARDINAL): LONGREAL;

    (* Calculates a**i.  This procedure does not really belong in this	*)
    (* module, but for now it doesn't seem to have any other suitable	*)
    (* home.								*)

    VAR result: LONGREAL;

    BEGIN
	result := 1.0;

	(* Desired answer is result*(a)**i.  The loop below keeps this	*)
	(* quantity invariant while reducing i down to zero.		*)

	LOOP
	    IF ODD(i) THEN
		DEC(i);  result := a*result;
	    END (*IF*);
	    IF i=0 THEN EXIT(*LOOP*) END(*IF*);
	    i := i DIV 2;  a := a*a;
	END (*LOOP*);
	RETURN result;
    END atoi;

(************************************************************************)

PROCEDURE TenToPower (N: CARDINAL): LONGREAL;

    (* Calculates 10**N.	*)

    BEGIN
	RETURN atoi (10.0, N);
    END TenToPower;

(************************************************************************)
(*			CARDINAL-TO-STRING CONVERSIONS			*)
(************************************************************************)

PROCEDURE HexToChar (number: HexDigit): CHAR;

    (* Converts a one-digit hexadecimal number to its readable form.	*)

    BEGIN
	IF number < 10 THEN
	    RETURN CHR(ORD("0")+number)
	ELSE
	    RETURN CHR(ORD("A")+number-10)
	END (*IF*);
    END HexToChar;

(************************************************************************)

PROCEDURE HexByteToString (value: SHORTCARD;
			VAR (*OUT*) buffer: ARRAY OF CHAR;  pos: CARDINAL);

    (* Converts a byte value to 2-character hexadecimal, with the	*)
    (* result stored at buffer[pos] and buffer[pos+1].			*)

    BEGIN
	buffer[pos] := HexToChar (VAL(HexDigit, value DIV 16));
	buffer[pos+1] := HexToChar (VAL(HexDigit, value MOD 16));
    END HexByteToString;

(************************************************************************)

PROCEDURE HexToString (value: CARDINAL;  VAR (*OUT*) buffer: ARRAY OF CHAR);

    VAR j: CARDINAL;

    BEGIN
	FOR j := HIGH(buffer) TO 0 BY -1 DO
	    buffer[j] := HexToChar (IAND(value,0FH));
	    value := RS (value, 4);
	END (*FOR*);
    END HexToString;

(************************************************************************)

PROCEDURE LongHexToString (value: LONGCARD;  VAR (*OUT*) buffer: EightChar);

    VAR j: [0..3];  highpart: ARRAY [0..3] OF CHAR;

    BEGIN
	HexToString (LowWord(value), buffer);
	HexToString (HighWord(value), highpart);
	FOR j := 0 TO 3 DO
	    buffer[j] := highpart[j];
	END (*FOR*);
    END LongHexToString;

(************************************************************************)

PROCEDURE LongCardToString (number: LONGCARD;
					VAR (*OUT*) buffer: ARRAY OF CHAR;
					fieldsize: CARDINAL);

    (* Converts the number to a decimal character string in array	*)
    (* "buffer", right-justified in a field of fieldsize characters.	*)

    VAR j, remainder: CARDINAL;

    BEGIN
	IF number < 10 THEN
	    IF fieldsize > 1 THEN
		FOR j := 0 TO fieldsize-2 DO
		    buffer[j] := " ";
		END (*FOR*);
	    END (*IF*);
	    buffer[fieldsize-1] := CHR(ORD(number) + ORD("0"));
	ELSIF fieldsize = 1 THEN
	    buffer[0] := "*";
	ELSE
	    LongCardToString (number DIV 10, buffer, fieldsize-1);
	    remainder := CARDINAL (number MOD 10);
	    buffer[fieldsize-1] := CHR(remainder + ORD("0"));
	END (*IF*);
    END LongCardToString;

(*********************************************************************)

PROCEDURE CardinalToString (number: CARDINAL;
					VAR (*OUT*) buffer: ARRAY OF CHAR;
					fieldsize: CARDINAL);

    (* Converts the number to a decimal character string in array	*)
    (* "buffer", right-justified in a field of fieldsize characters.	*)

    BEGIN
	LongCardToString (LONGCARD(number), buffer, fieldsize);
    END CardinalToString;

(*********************************************************************)

PROCEDURE ShortCardToString (number: SHORTCARD;
					VAR (*OUT*) buffer: ARRAY OF CHAR;
					fieldsize: CARDINAL);

    (* Converts the number to a decimal character string in array	*)
    (* "buffer", right-justified in a field of fieldsize characters.	*)

    BEGIN
	LongCardToString (LONGCARD(number), buffer, fieldsize);
    END ShortCardToString;

(*********************************************************************)

PROCEDURE AssembleLongCardinal (number: LONGCARD;
				VAR (*OUT*) buffer: ARRAY OF CHAR;
				VAR (*INOUT*) place: CARDINAL;
				VAR (*OUT*) error: BOOLEAN);

    (* Converts number to decimal, putting it in buffer starting at	*)
    (* buffer[place].  On return, place has been updated to be just	*)
    (* beyond the last digit put in the buffer.				*)

    BEGIN
	IF number > 9 THEN
	    AssembleLongCardinal (number DIV 10, buffer, place, error);
	    IF error THEN RETURN END(*IF*);
	END (*IF*);
	error := place > HIGH(buffer);
	IF NOT error THEN
	    buffer[place] := CHR (CARDINAL(number MOD 10) + ORD("0"));
	    INC (place);
	END (*IF*);
    END AssembleLongCardinal;

(************************************************************************)
(*			REAL-TO-STRING CONVERSIONS			*)
(************************************************************************)

PROCEDURE AssembleExponent (number: INTEGER;
			VAR (*OUT*) buffer: ARRAY OF CHAR;
			VAR (*INOUT*) position: CARDINAL;
			VAR (*OUT*) error: BOOLEAN);

    (* Puts a field of the format Ennn or E-nnn into the buffer,	*)
    (* starting at buffer[position].  On return, position has been	*)
    (* updated so that buffer[position] is the first character not	*)
    (* altered by this procedure.					*)

    BEGIN
	error := FALSE;
	IF number <> 0 THEN
	    error := position > HIGH(buffer);
	    IF NOT error THEN
		buffer[position] := "E";  INC(position);
		IF number < 0 THEN
		    error := position > HIGH(buffer);
		    IF NOT error THEN
			buffer[position] := "-";  INC(position);
			number := -number;
		    END (*IF*);
		END (*IF*);
	    END (*IF*);
	    IF NOT error THEN
		AssembleLongCardinal (LONGCARD(number),
					buffer, position, error);
	    END (*IF*);
	END (*IF*);
    END AssembleExponent;

(************************************************************************)

PROCEDURE Roundup (VAR (*INOUT*) buffer: ARRAY OF CHAR;
					first, last: CARDINAL);

    (* Takes the decimal number in buffer[first..last] and increments	*)
    (* its least significant digit, propagating the carry upwards as	*)
    (* far as necessary.						*)

    VAR position, pointposition: CARDINAL;
	code: CHAR;

    BEGIN
	position := last+1;  pointposition := position;
	REPEAT
	    DEC (position);
	    code := buffer[position];
	    IF code = "9" THEN buffer[position] := "0"
	    ELSIF code = "." THEN
		pointposition := position;  code := "9";
	    ELSE
		INC (buffer[position]);
	    END (*IF*);
	UNTIL (code <> "9") OR (position = first);

	(* The job is now done, except for one special case.  If we	*)
	(* have left the above loop after incrementing a "9", the carry	*)
	(* has propagated off the left end of the number.  In that case	*)
	(* every digit must have been a "9", so the result is 10000...	*)
	(* with a decimal point inserted at the appropriate place.	*)

	IF code = "9" THEN
	    IF pointposition <= last THEN
		buffer[pointposition] := "0";
		IF pointposition < last THEN
		    INC (pointposition);  buffer[pointposition] := ".";
		END (*IF*);
	    END (*IF*);
	    buffer[first] := "1";
	END (*IF*);

    END Roundup;

(************************************************************************)

PROCEDURE Fformat (number: LONGREAL;  VAR (*OUT*) buffer: ARRAY OF CHAR;
			start: CARDINAL;  VAR (*INOUT*) finish: CARDINAL;
			LeftJustified: BOOLEAN;  VAR (*OUT*) error: BOOLEAN);

    (* Formats the second argument as a decimal number, left or right	*)
    (* justified depending on the value of LeftJustified, in		*)
    (* buffer[start..finish].  This procedure is known to be called	*)
    (* only with start=0 or start=1 with a sign in buffer[0]; so we	*)
    (* perform the justification on all of buffer[0..finish] if right	*)
    (* justification is specified.  In the case of left justification,	*)
    (* finish is updated to show the last buffer position actually	*)
    (* used; and this character position is followed by one or more NUL	*)
    (* characters, except in the case where we have used the entire	*)
    (* field to hold the result.					*)

    VAR position: CARDINAL;
	integerpart: LONGCARD;  nextdigit: [0..9];

    BEGIN
	position := start;
	integerpart := VAL (LONGCARD, number);
	AssembleLongCardinal (integerpart, buffer, position, error);
	IF error THEN RETURN END(*IF*);

	IF position <= finish THEN
	    buffer[position] := ".";
	    INC (position);
	    number := number - VAL (LONGREAL, integerpart);

	    WHILE (position <= finish) DO
		number := 10.0*number;
		nextdigit := VAL (CARDINAL, number);
		buffer[position] := CHR(ORD("0") + nextdigit);
		INC (position);
		number := number - VAL (LONGREAL, nextdigit);
	    END (*WHILE*);

	    (* If the remainder is 0.5 or more, adjust the result by	*)
	    (* rounding up.						*)

	    IF number >= 0.5 THEN
		Roundup (buffer, start, finish);
	    END (*IF*);

	    (* Strip off the trailing zeros.	*)

	    DEC (position);
	    WHILE buffer[position] = '0' DO
		buffer[position] := CHR(0);
		DEC (position);
	    END (*WHILE*);

	    (* If we are left with a whole number, strip off the	*)
	    (* decimal point.						*)

	    IF buffer[position] = '.' THEN
		buffer[position] := CHR(0);
		DEC (position);
	    END (*IF*);

	    (* Right justify the result or modify finish, as specified.	*)

	    IF LeftJustified THEN
		finish := position;
	    ELSE
		ShiftRight (buffer, 0, finish, finish-position);
	    END (*IF*);

	END (*IF*);

    END Fformat;

(************************************************************************)

PROCEDURE Scale (VAR (*INOUT*) mantissa: LONGREAL;
			VAR (*INOUT*) exponent: INTEGER;
			power: CARDINAL;  lower, upper: LONGREAL);

    (* Adjusts mantissa so that lower <= mantissa < upper, while	*)
    (* keeping the quantity  (mantissa * 10^exponent) invariant.  To	*)
    (* save us some calculation, the caller must ensure that		*)
    (* upper = 10^power and lower = 10^(-power).			*)

    BEGIN
	WHILE mantissa >= upper DO
	    INC (exponent, power);  mantissa := lower*mantissa;
	END (*WHILE*);

	WHILE mantissa < lower DO
	    DEC (exponent, power);  mantissa := upper*mantissa;
	END (*WHILE*);
    END Scale;

(************************************************************************)

PROCEDURE Separate (number: LONGREAL;  VAR (*OUT*) mantissa: LONGREAL;
					VAR (*OUT*) exponent: INTEGER);

    (* Separates the first argument into a mantissa and exponent part,	*)
    (* so that  number = mantissa * 10^exponent.			*)

    BEGIN
	mantissa := number;  exponent := 0;
	Scale (mantissa, exponent, 256, 1.0E-256, 1.0E256);
	Scale (mantissa, exponent, 64, 1.0E-64, 1.0E64);
	Scale (mantissa, exponent, 16, 1.0E-16, 1.0E16);
	Scale (mantissa, exponent, 4, 1.0E-4, 1.0E4);
	Scale (mantissa, exponent, 1, 1.0E-1, 1.0E1);
    END Separate;

(************************************************************************)

PROCEDURE Eformat (number: LONGREAL;  VAR (*OUT*) buffer: ARRAY OF CHAR;
					start, finish: CARDINAL;
					VAR (*OUT*) error: BOOLEAN);

    (* Puts number into buffer[start..finish] in E format, with the	*)
    (* whole of buffer[0..finish] right justified.			*)

    VAR mantissa: LONGREAL;  exponent: INTEGER;
	position: CARDINAL;

    BEGIN
	Separate (number, mantissa, exponent);

	(* Put the exponent into the buffer first, in order to find out	*)
	(* how much space will be left for the mantissa.		*)

	position := start;
	AssembleExponent (exponent, buffer, position, error);
	error := error OR (position > finish);

	IF error THEN
	    IF finish < HIGH(buffer) THEN
		buffer[finish+1] := CHR(0);
	    END (*IF*);
	ELSE
	    ShiftRight (buffer, start, finish, finish-position+1);

	    (* Now assemble the mantissa into the buffer.	*)

	    DEC (finish, position-start);
	    Fformat (mantissa, buffer, start, finish, FALSE, error);
	END (*IF*);

    END Eformat;

(************************************************************************)
(*		CONVERSION OF REAL NUMBER TO CHARACTER STRING		*)
(************************************************************************)

PROCEDURE LongRealToString (number: LONGREAL;
					VAR (*OUT*) buffer: ARRAY OF CHAR;
					fieldsize: CARDINAL);

    (* Converts the number to a decimal character string in array	*)
    (* "buffer", right-justified in a field of "places" characters.	*)

    VAR start, finish, j: CARDINAL;  small: LONGREAL;  error: BOOLEAN;

    BEGIN
	IF fieldsize = 0 THEN RETURN END(*IF*);

	start := 0;  finish := start+fieldsize-1;  error := FALSE;

	(* Make sure that the string will fit into the buffer, and that	*)
	(* it will be properly terminated.				*)

	IF finish > HIGH(buffer) THEN
	    DEC (fieldsize, finish-HIGH(buffer));
	    finish := HIGH(buffer);
	ELSIF finish < HIGH(buffer) THEN
	    buffer[finish+1] := CHR(0);
	END (*IF*);

	(* For a negative number, insert a minus sign.	*)

	IF number < 0.0 THEN
	    IF fieldsize <= 1 THEN
		error := TRUE;
	    ELSE
		buffer[0] := "-";  INC (start);  DEC(fieldsize);
		number := -number;
	    END (*IF*);
	END (*IF*);

	IF NOT error THEN

	    (* Now decide on whether to use E format, based on the	*)
	    (* value to be converted.					*)

	    small := 100.0 / TenToPower(fieldsize);
	    IF number = 0.0 THEN
		Fformat (number, buffer, start, finish, FALSE, error);
	    ELSIF (number >= TenToPower(fieldsize))
		    OR (number > VAL(LONGREAL, MAX(LONGCARD)))
			OR (number < small) THEN
	        Eformat (number, buffer, start, finish, error);
	    ELSE
		Fformat (number, buffer, start, finish, FALSE, error);
	    END (*IF*);

	END (*IF*);

	IF error THEN
	    FOR j := 0 TO finish DO
		buffer[j] := '*';
	    END (*FOR*);
	END (*IF*);

    END LongRealToString;

(************************************************************************)

PROCEDURE RealToString (number: REAL;  VAR (*OUT*) buffer: ARRAY OF CHAR;
					fieldsize: CARDINAL);

    (* Like LongRealToString, except for argument type.	*)

    BEGIN
	LongRealToString (LONGREAL(number), buffer, fieldsize);
    END RealToString;

(************************************************************************)

PROCEDURE LongRealToF (number: LONGREAL;  VAR (*INOUT*) fieldsize: CARDINAL;
			decimalplaces: CARDINAL;  LeftJustified: BOOLEAN;
			VAR (*OUT*) buffer: ARRAY OF CHAR);

    (* Converts the number to an F-format string, of up to fieldsize	*)
    (* characters with decimalplaces digits after the decimal point.	*)
    (* The result is left justified if LeftJustified = TRUE is		*)
    (* specified by the caller, and right justified with space fill	*)
    (* otherwise.  On return fieldsize gives the number of character	*)
    (* positions actually used.  The result string is terminated with	*)
    (* at least one CHR(0) (which is not counted in fieldsize), except	*)
    (* where the result fills the entire buffer.			*)

    VAR start, finish, j: CARDINAL;  scalefactor: LONGREAL;  error: BOOLEAN;

    BEGIN
	IF fieldsize = 0 THEN RETURN END(*IF*);

	start := 0;  finish := start+fieldsize-1;  error := FALSE;

	(* Make sure that the string will fit into the buffer, and that	*)
	(* it will be properly terminated.				*)

	IF finish > HIGH(buffer) THEN
	    DEC (fieldsize, finish-HIGH(buffer));
	    finish := HIGH(buffer);
	ELSIF finish < HIGH(buffer) THEN
	    buffer[finish+1] := CHR(0);
	END (*IF*);

	(* For a negative number, insert a minus sign.	*)

	IF number < 0.0 THEN
	    IF fieldsize <= 1 THEN
		error := TRUE;
	    ELSE
		buffer[0] := "-";  INC (start);  DEC(fieldsize);
		number := -number;
	    END (*IF*);
	END (*IF*);

	IF NOT error THEN

	    (* Round the number to the desired number of decimal places. *)

	    scalefactor := TenToPower (decimalplaces);
	    number := scalefactor*number + 0.5;
	    number := VAL(LONGREAL, VAL(LONGCARD, number)) / scalefactor;

	    (* Perform the conversion.	*)

	    Fformat (number, buffer, start, finish, LeftJustified, error);

	END (*IF*);

	IF error THEN
	    FOR j := 0 TO finish DO
		buffer[j] := '*';
	    END (*FOR*);
	END (*IF*);

	fieldsize := finish + 1;

    END LongRealToF;

(************************************************************************)

PROCEDURE RealToF (number: REAL;  VAR (*INOUT*) fieldsize: CARDINAL;
			decimalplaces: CARDINAL;  LeftJustified: BOOLEAN;
			VAR (*OUT*) buffer: ARRAY OF CHAR);

    (* Like LongRealToF, except for argument type.	*)

    BEGIN
	LongRealToF (LONGREAL(number), fieldsize, decimalplaces,
					LeftJustified, buffer);
    END RealToF;

(************************************************************************)
(*		    CONVERSION OF STRING TO CARDINAL			*)
(************************************************************************)

PROCEDURE StringToHex (string: ARRAY OF CHAR): LONGCARD;

    (* Converts a hexadecimal character string to numeric, stopping at	*)
    (* the first non-digit character.  Leading spaces are permitted.	*)

    CONST HexChars = CharSet {"0".."9", "a".."f", "A".."F"};

    VAR position: CARDINAL;  value: LONGCARD;

    BEGIN
	position := 0;
	WHILE (position <= HIGH(string)) AND (string[position] = ' ') DO
	    INC (position);
	END (*WHILE*);
	value := 0;
	WHILE (position <= HIGH(string)) AND (string[position] IN HexChars) DO
	    value := 16*value;
	    IF string[position] IN CharSet{"a".."f"} THEN
		value := value + 10
			+ LONGCARD(ORD(string[position]) - ORD('a'));
	    ELSIF string[position] IN CharSet{"A".."F"} THEN
		value := value + 10
			+ LONGCARD(ORD(string[position]) - ORD('A'));
	    ELSE
		value := value + LONGCARD(ORD(string[position]) - ORD('0'));
	    END (*IF*);
	    INC (position);
	END (*WHILE*);
	RETURN value;
    END StringToHex;

(************************************************************************)

PROCEDURE StringToLongCard (string: ARRAY OF CHAR): LONGCARD;

    (* Converts a character string to decimal, stopping at the first	*)
    (* non-digit character.  Leading spaces are permitted.		*)

    VAR position: CARDINAL;  value: LONGCARD;

    BEGIN
	position := 0;
	WHILE (position <= HIGH(string)) AND (string[position] = ' ') DO
	    INC (position);
	END (*WHILE*);
	value := 0;
	WHILE (position <= HIGH(string)) AND (string[position] >= '0')
		AND (string[position] <= '9') DO
	    value := 10*value + VAL(LONGCARD, ORD(string[position]) - ORD('0'));
	    INC (position);
	END (*WHILE*);
	RETURN value;
    END StringToLongCard;

(************************************************************************)

PROCEDURE StringToCardinal (string: ARRAY OF CHAR): CARDINAL;

    (* Converts a character string to decimal, stopping at the first	*)
    (* non-digit character.  Leading spaces are permitted.		*)

    VAR position: CARDINAL;  value: CARDINAL;

    BEGIN
	RETURN VAL(CARDINAL, StringToLongCard(string));
    END StringToCardinal;

(************************************************************************)
(*			CONVERSION OF STRING TO REAL			*)
(************************************************************************)

PROCEDURE StringToLongReal (string: ARRAY OF CHAR): LONGREAL;

    (* Converts a decimal text string (with optional leading minus	*)
    (* sign) to real.  Leading blanks are ignored.  The conversion	*)
    (* stops at the end of the array or at the first character which	*)
    (* cannot be part of the number, and in the latter case all		*)
    (* subsequent characters are ignored.				*)

    VAR result, placevalue, temp: LONGREAL;
	position: CARDINAL;
	nextchar: CHAR;
	exponent: CARDINAL;  negative, negativeexp: BOOLEAN;

    (********************************************************************)

    PROCEDURE GetNextChar;

	(* Puts the next character in the input into variable nextchar,	*)
	(* or sets nextchar := CHR(0) if there is no next character.	*)
	(* The position in the array is updated.			*)

	CONST EndMarker = CHR(0);

	BEGIN
	    IF position > HIGH(string) THEN
		nextchar := EndMarker;
	    ELSE
		nextchar := string[position];  INC (position);
	    END (*IF*);
	END GetNextChar;

    (********************************************************************)

    BEGIN
	result := 0.0;  position := 0;  negative := FALSE;

	(* Skip leading spaces and tabs.	*)

	REPEAT
	    GetNextChar;
	UNTIL (nextchar <> " ") AND (nextchar <> tab);

	(* Check for a sign.	*)

	IF (nextchar = "-") OR (nextchar = "+") THEN
	    negative := (nextchar = "-");

	    (* There might be some more spaces to skip.	*)

	    REPEAT
		GetNextChar;
	    UNTIL (nextchar <> " ") AND (nextchar <> tab);
	END (*IF*);

	(* Read the part before the decimal point.	*)

	WHILE nextchar IN CharSet {"0".."9"} DO
	    result := 10.0*result + VAL(LONGREAL, ORD(nextchar)-ORD("0") );
	    GetNextChar;
	END (*WHILE*);

	(* Now the part after the decimal point, if any.	*)

	IF nextchar = "." THEN
	    GetNextChar;  placevalue := 0.1;
	    WHILE nextchar IN CharSet {"0".."9"} DO
		result := result +
			placevalue * VAL(LONGREAL, ORD(nextchar)-ORD("0") );
		placevalue := 0.1*placevalue;
		GetNextChar;
	    END (*WHILE*);
	END (*IF*);

	(* Check for Ennn part.	*)

	IF (nextchar = "E") OR (nextchar = "e") THEN
	    GetNextChar;
	    exponent := 0;  negativeexp := FALSE;
	    IF nextchar = "+" THEN
		GetNextChar;
	    ELSIF nextchar = "-" THEN
		negativeexp := TRUE;  GetNextChar;
	    END (*IF*);
	    WHILE nextchar IN CharSet {"0".."9"} DO
		exponent := 10*exponent + ORD(nextchar) - ORD("0");
		GetNextChar;
	    END (*WHILE*);
	    IF negativeexp THEN
		result := result / TenToPower(exponent);
	    ELSE
		result := result * TenToPower(exponent);
	    END (*IF*);
	END (*IF*);

	IF negative THEN
	    result := -result;
	END (*IF*);
	RETURN result;

    END StringToLongReal;

(************************************************************************)

PROCEDURE StringToReal (string: ARRAY OF CHAR): REAL;

    (* Like StringToLongReal except for the result type.	*)

    BEGIN
	RETURN REAL (StringToLongReal(string));
    END StringToReal;

(************************************************************************)

END Conversions.
