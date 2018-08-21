IMPLEMENTATION MODULE CharUtils;
(*====================================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Component: Tool- Primitive Utility - Character Utilities

    INTRODUCTION
    This module provides elementary character operations.
    The JPI version was converted from the MacModula-2 version of
    Alpha (V2.01), which was also by the same author.

    REVISION HISTORY
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    LOCAL CONSTANTS
    * LowercaseOffset
      Numeric difference between the ordinal values of the lower-
      and upper-case ASCII characters, such that when added to the
      ordinal value of an uppercase character will yield the lowercase
      letter equivalent.

    INFORMATION HIDDEN
    * Specifics related to the ASCII character set.

			  IMPLEMENTATION DESIGN NOTES
    * The ASCII character set is assumed.

    * Procedure call overhead in the MacMETH compiler is excessive for small
      routines (few parameters, 0-4 bytes of locals, 1-4 lines of source code,
      no procedure calls within the routine).  It is much larger than for a
      functionally equivalent assembly language version of the routine.

    * In [2], using Ada, the operator IN can be used for membership tests on a
      subtype or subrange, see sample usage on pg. 388.

      To achieve the equivalent in Modula-2 one could:
      * define a character set ADT module, since most M2 compilers
	support SET types with a small maximum cardinality, (16 to
	32 elements), when 128/256 elements are needed for the ASCII
	characters. The problem is space: 8/16 words for each
	character set variable plus the bit manipulation code to
	implement IN (set membership test).

      * hide within the implementation module tests to determine
	whether a given character lies within the subrange bounds.

      For efficiency reasons the second option was chosen.

    * Routines that make a procedure call to another routine within
      the module to accomplish a character test, should be transformed
      to the in-line equivalent, in the interests of efficiency, which
      (hopefully) will promote using the module.

      The duplicate comparisons should not adversely effect the
      maintainance effort for these reasons:
	a. module is already specific to the ASCII characters;
	b. the ASCII character definition is an ISO standard,
	   ‘freezing’ the specification and internal constraints;
	c. no change to module functionality is anticipated.
	d. statements are simple character comparisons against constant value(s).

    REFERENCES and SUPPORTING DOCUMENTS
    [1] Niklaus Wirth, Programming in Modula-2, 3rd ed., pg. 162,
	Springer-Verlag Berlin Heidelberg, 1985.
    [2] Grady Booch, Software Components in Ada, pp. 386-391,
	Benjamin/Cummings Publishing Co, 1987.

			    PROPRIETARY NOTICES
    CharUtils.MOD Copyright (C) 1989 Charles A. Lins.
====================================================================*)

CONST	LowercaseOffset =   VAL(INTEGER,MIN(LowercaseCharacter))
			  - VAL(INTEGER,MIN(UppercaseCharacter));

       (*--------------------*)

PROCEDURE MakeUppercase  (VAR theChar : CHAR	(*-- inout *));
BEGIN
  IF IsLowercase(theChar) THEN
    (*-- Yes, convert to uppercase equivalent *)
    theChar := CAP(theChar);
  END (*--if*);
END MakeUppercase;
(*--------------------*)

PROCEDURE MakeLowercase  (VAR theChar : CHAR	(*-- inout *));
BEGIN
  IF IsUppercase(theChar) THEN
    (*-- Yes, convert to lowercase equivalent *)
    theChar := CHR(ORD(theChar) + LowercaseOffset);
  END (*--if*);
END MakeLowercase;
(*--------------------*)


PROCEDURE Uppercase	 (    theChar : CHAR	(*-- in    *))
				      : CHAR	(*-- out   *);
BEGIN
  theChar := CAP(theChar);
  RETURN theChar;
END Uppercase;
(*--------------------*)

PROCEDURE Lowercase	 (    theChar : CHAR	(*-- in    *))
				      : CHAR	(*-- out   *);
BEGIN
  IF IsUppercase(theChar) THEN
    (*-- Yes, convert to lowercase equivalent *)
    theChar := CHR(ORD(theChar) + LowercaseOffset);
  END (*--if*);
  RETURN theChar;
END Lowercase;
(*--------------------*)


PROCEDURE IsAscii	 (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN (MIN(AsciiCharacter) <= theChar) & (theChar <= MAX(AsciiCharacter));
END IsAscii;
(*--------------------*)

PROCEDURE IsControl	 (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN (MIN(ControlCharacter) <= theChar) & (theChar <= MAX(ControlCharacter));
END IsControl;
(*--------------------*)

PROCEDURE IsGraphic	 (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN (MIN(GraphicCharacter) <= theChar) & (theChar <= MAX(GraphicCharacter));
END IsGraphic;
(*--------------------*)

PROCEDURE IsLowercase	 (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN (MIN(LowercaseCharacter) <= theChar) & (theChar <= MAX(LowercaseCharacter));
END IsLowercase;
(*--------------------*)

PROCEDURE IsUppercase	 (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN (MIN(UppercaseCharacter) <= theChar) & (theChar <= MAX(UppercaseCharacter));
END IsUppercase;
(*--------------------*)

PROCEDURE IsAlphabetic	 (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN IsLowercase(theChar) OR IsUppercase(theChar);
END IsAlphabetic;
(*--------------------*)

PROCEDURE IsDigit	 (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN (MIN(DigitCharacter) <= theChar) & (theChar <= MAX(DigitCharacter))
	 OR
	 (MIN(HexCharacter) <= theChar) & (theChar <= MAX(HexCharacter));
END IsDigit;
(*--------------------*)

PROCEDURE IsBinaryDigit  (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN (MIN(BinaryCharacter) <= theChar) & (theChar <= MAX(BinaryCharacter));
END IsBinaryDigit;
(*--------------------*)

PROCEDURE IsOctalDigit	 (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN (MIN(OctalCharacter) <= theChar) & (theChar <= MAX(OctalCharacter));
END IsOctalDigit;
(*--------------------*)

PROCEDURE IsDecimalDigit (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN (MIN(DigitCharacter) <= theChar) & (theChar <= MAX(DigitCharacter));
END IsDecimalDigit;
(*--------------------*)

PROCEDURE IsHexDigit	 (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  MakeUppercase(theChar);
  RETURN (MIN(HexCharacter) <= theChar) & (theChar <= MAX(HexCharacter));
END IsHexDigit;
(*--------------------*)

PROCEDURE IsAlphanumeric (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN IsDigit(theChar) OR IsLowercase(theChar) OR IsUppercase(theChar);
END IsAlphanumeric;
(*--------------------*)

PROCEDURE IsSpecial	 (    theChar : CHAR	(*-- in    *))
				      : BOOLEAN (*-- out   *);
BEGIN
  RETURN ~(IsDigit(theChar) OR IsAlphabetic(theChar)) OR IsControl(theChar);
END IsSpecial;
(*--------------------*)


PROCEDURE ValueOf	 (    theChar : CHAR	(*-- in    *))
				      : Digit	(*-- out   *);

CONST MinDecCharValue = VAL(CARDINAL,MIN(DigitCharacter));
      MinHexCharValue = VAL(CARDINAL,MIN(HexCharacter));

BEGIN
  IF IsDecimalDigit(theChar) THEN
    RETURN ORD(theChar) - MinDecCharValue;

  ELSIF IsHexDigit(theChar) THEN
    RETURN ORD(theChar) - MinHexCharValue + 10;
  END (*--if*);
END ValueOf;
(*--------------------*)

PROCEDURE ImageOf	 (    theDigit: Digit	(*-- in    *))
				      : CHAR	(*-- out   *);
BEGIN
  IF (theDigit <= VAL(Digit,MAX(DecimalDigit))) THEN
    RETURN CHR(ORD(MIN(DigitCharacter)) + VAL(CARDINAL,theDigit));
  ELSE
    DEC(theDigit,MAX(DecimalDigit)+1);
    RETURN CHR(ORD(MIN(HexCharacter)) + VAL(CARDINAL,theDigit));
  END (*--if*);
END ImageOf;
(*--------------------*)


PROCEDURE IndexOf	 (    theChar : CHAR	(*-- in    *))
				      : Letter	(*-- out   *);
BEGIN
  IF IsLowercase(theChar) THEN
    RETURN ORD(theChar) - ORD(MIN(LowercaseCharacter)) + 1;
  ELSIF IsUppercase(theChar) THEN
    RETURN ORD(theChar) - ORD(MIN(UppercaseCharacter)) + 1;
  END (*--if*);
END IndexOf;
(*--------------------*)

PROCEDURE UppercaseOf	 (    theLetter: Letter (*-- in    *))
				       : CHAR	(*-- out   *);
BEGIN
  RETURN CHR(ORD(MIN(UppercaseCharacter)) + VAL(CARDINAL,theLetter - 1) );
END UppercaseOf;
(*--------------------*)

PROCEDURE LowercaseOf	 (    theLetter: Letter (*-- in    *))
				       : CHAR	(*-- out   *);
BEGIN
  RETURN CHR(ORD(MIN(LowercaseCharacter)) + VAL(CARDINAL,theLetter - 1) );
END LowercaseOf;
(*--------------------*)


PROCEDURE IsEqual	 (    left	   : CHAR     (*-- in	 *);
			      right	   : CHAR     (*-- in	 *);
			      caseSensitive: BOOLEAN  (*-- in	 *))
					   : BOOLEAN  (*-- out	 *);
BEGIN
  IF caseSensitive THEN
    RETURN (left = right);
  END (*--if*);
  RETURN (CAP(left) = CAP(right));
END IsEqual;
(*--------------------*)

PROCEDURE IsLessThan	 (    left	   : CHAR     (*-- in	 *);
			      right	   : CHAR     (*-- in	 *);
			      caseSensitive: BOOLEAN  (*-- in	 *))
					   : BOOLEAN  (*-- out	 *);
BEGIN
  IF caseSensitive THEN
    RETURN (left < right);
  END (*--if*);
  RETURN (CAP(left) < CAP(right));
END IsLessThan;
(*--------------------*)

PROCEDURE IsGreaterThan  (    left	   : CHAR     (*-- in	 *);
			      right	   : CHAR     (*-- in	 *);
			      caseSensitive: BOOLEAN  (*-- in	 *))
					   : BOOLEAN  (*-- out	 *);
BEGIN
  IF caseSensitive THEN
    RETURN (left > right);
  END (*--if*);
  RETURN (CAP(left) > CAP(right));
END IsGreaterThan;
(*--------------------*)

END CharUtils.
