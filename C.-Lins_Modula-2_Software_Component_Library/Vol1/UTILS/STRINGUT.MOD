IMPLEMENTATION MODULE StringUtils;
(*==========================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Component: Tool - String Utilities
    Code Size: R-  bytes

    Revision History
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    Introduction
    This module provides elementary String operations.

    Proprietary Notices
      Copyright (C) 1989 Charles A. Lins. All rights reserved. 
==========================================================*)

FROM Str IMPORT
    (*--Proc*) Insert, Delete, Length, Compare;

IMPORT CharUtils;

       (*--------------------*)

PROCEDURE MakeUppercase  (VAR theString    : ARRAY OF CHAR (*--inout*));

VAR   index: CARDINAL; (*-- loop index over characters in the string *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    CharUtils.MakeUppercase(theString[index]);
    INC(index);
  END (*--while*);
END MakeUppercase;
(*--------------------*)

PROCEDURE MakeLowercase  (VAR theString    : ARRAY OF CHAR (*-- inout *));

VAR   index: CARDINAL; (*-- loop index over characters in the string *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    CharUtils.MakeLowercase(theString[index]);
    INC(index);
  END (*--while*);
END MakeLowercase;
(*--------------------*)

PROCEDURE Replace	 (    theChar	   : CHAR	   (*--in   *);
			      withChar	   : CHAR	   (*--in   *);
			  VAR theString    : ARRAY OF CHAR (*--inout*);
			      caseSensitive: BOOLEAN	   (*--in   *));
VAR   index: CARDINAL;
BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    IF CharUtils.IsEqual(theChar, theString[index], caseSensitive) THEN
      theString[index] := withChar;
    END (*--if*);
    INC(index);
  END (*--while*);
END Replace;
(*--------------------*)

PROCEDURE Append	 (    theString    : ARRAY OF CHAR (*--in   *);
			  VAR toTheString  : ARRAY OF CHAR (*--inout*));

VAR   srcLimit : CARDINAL; (*-- Source string high index bound *)
      srcIndex : CARDINAL; (*-- Source string working index *)
      destLimit: CARDINAL; (*-- Destination string high index bound *)
      destIndex: CARDINAL; (*-- Destination string working index *)

BEGIN
  destLimit := HIGH(toTheString);
  destIndex := 0;

  (*-- Find the end of the string that is being appended to *)
  WHILE (destIndex <= destLimit) & (toTheString[destIndex] # EOS) DO
    INC(destIndex);
  END(*--while*);

  srcLimit := HIGH(theString);
  srcIndex := 0;

  (*-- Append source to end of destination *)
  WHILE (destIndex < destLimit) & (srcIndex <= srcLimit) &
	(theString[srcIndex] # EOS) DO
    toTheString[destIndex] := theString[srcIndex];
    INC(destIndex);
    INC(srcIndex);
  END(*--while*);
  toTheString[destIndex] := EOS;
END Append;
(*--------------------*)

PROCEDURE Fill		 (    theChar	   : CHAR	   (*--in   *);
			  VAR inTheString  : ARRAY OF CHAR (*--out  *));

VAR   index : INTEGER; (*-- loop index over characters in the string *)

BEGIN
  FOR index:= 0 TO HIGH(inTheString) DO
    inTheString[index] := theChar;
  END(*--for*);
END Fill;
(*--------------------*)

PROCEDURE FillSpaces	 (VAR inTheString  : ARRAY OF CHAR (*--out   *));

VAR   index : INTEGER; (*-- loop index over characters in the string *)

BEGIN
  FOR index:= 0 TO HIGH(inTheString) DO
    inTheString[index] := ' ';
  END(*--for*);
END FillSpaces;
(*--------------------*)

PROCEDURE FillPartial	 (    withTheChar  : CHAR	   (*--in   *);
			      atTheIndex   : CARDINAL	   (*--in   *);
			      forTheLength : CARDINAL	   (*--in   *);
			  VAR inTheString  : ARRAY OF CHAR (*--out  *));

VAR   lastIndex: CARDINAL;

BEGIN
  lastIndex := HIGH(inTheString);
  WHILE (atTheIndex <= lastIndex) & (inTheString[atTheIndex] # EOS) &
	(forTheLength > 0) DO
    inTheString[atTheIndex] := withTheChar;
    INC(atTheIndex);
    DEC(forTheLength);
  END (*--while*);
END FillPartial;
(*--------------------*)

PROCEDURE InsertChar	 (    withTheChar  : CHAR	   (*--in   *);
			      atTheIndex   : CARDINAL	   (*--in   *);
			      forTheLength : CARDINAL	   (*--in   *);
			  VAR inTheString  : ARRAY OF CHAR (*--out  *));

VAR   insertCount     : CARDINAL;
      insertCharString: ARRAY [0..0] OF CHAR;

BEGIN
  IF atTheIndex <= VAL(CARDINAL,HIGH(inTheString)) THEN
    insertCharString[0] := withTheChar;
    insertCount := 0;
    WHILE insertCount < forTheLength DO
      Insert(insertCharString, inTheString, atTheIndex);
      INC(insertCount);
    END (*--while*);
  END (*--if*);
END InsertChar;
(*--------------------*)

PROCEDURE Centered	 (VAR theString    : ARRAY OF CHAR (*--inout*);
			      inWidth	   : CARDINAL	   (*--in   *);
			      withTheFiller: CHAR	   (*--in   *));

VAR   leftMargin : CARDINAL;
      rightMargin: CARDINAL;
      sourceLen  : CARDINAL;
      sourceLimit: CARDINAL;

BEGIN
  sourceLen := Length(theString);

  IF inWidth > sourceLen THEN
    sourceLimit := HIGH(theString);
    IF (inWidth > sourceLimit) THEN
      inWidth := sourceLimit;
    END (*--if*);

    leftMargin	:= (inWidth - sourceLen) DIV 2;
    rightMargin := inWidth - sourceLen - leftMargin;

    (*-- Fill at the left *)
    InsertChar(withTheFiller,0,leftMargin,theString);

    (*-- Fill at the right *)
    InsertChar(withTheFiller,sourceLen+leftMargin,rightMargin,theString);

  END (*--if*);
END Centered;
(*--------------------*)

PROCEDURE LeftJustified  (VAR theString    : ARRAY OF CHAR (*--inout*);
			      inWidth	   : CARDINAL	   (*--in   *);
			      withTheFiller: CHAR	   (*--in   *));

VAR   index: CARDINAL; (*-- loop index over characters of the string *)

BEGIN
  index := 0;
  WHILE (index < HIGH(theString)) & (theString[index] # EOS) DO
    INC(index);
  END (*--while*);

  WHILE (index < HIGH(theString)) & (VAL(CARDINAL, index) < inWidth) DO
    theString [index] := withTheFiller;
    INC(index);
  END (*--while*);

  theString[index] := EOS;
END LeftJustified;
(*--------------------*)

PROCEDURE RightJustified (VAR theString    : ARRAY OF CHAR (*--inout*);
			      inWidth	   : CARDINAL	   (*--in   *);
			      withTheFiller: CHAR	   (*--in   *));
VAR   srcIndex : CARDINAL;
      srcLength: CARDINAL;
      moveWidth: CARDINAL;

BEGIN
  srcLength := Length(theString);

  IF (srcLength < inWidth) THEN

    (*-- If given width exceeds limit of string, readjust to the limit *)
    IF VAL(CARDINAL,HIGH(theString)) < inWidth THEN
      inWidth := HIGH(theString);
    END (*--if*);

    (*-- Calculate how many positions to added to the string *)
    moveWidth := inWidth - srcLength;

    (*-- Shift the input string right, based on given width *)
    FOR srcIndex := inWidth TO moveWidth BY -1 DO
      theString[srcIndex] := theString[srcIndex-moveWidth];
    END (*--for*);

    (*-- Fill the front of the string with the given filler character *)
    DEC(moveWidth);
    FOR srcIndex := 0 TO moveWidth DO
      theString[srcIndex] := withTheFiller;
    END (*--for*);
  END (*--if*);
END RightJustified;
(*--------------------*)

PROCEDURE Stripped	 (VAR fromTheString: ARRAY OF CHAR (*--inout*);
			      theChar	   : CHAR	   (*--in   *);
			      caseSensitive: BOOLEAN	   (*--in   *));

VAR   srcIndex	 : CARDINAL;
      maxIndex	 : CARDINAL;
      stripIndex : CARDINAL;
      stripCount : CARDINAL;

BEGIN
  maxIndex := HIGH(fromTheString);
  srcIndex := 0;
  WHILE (srcIndex <= maxIndex) & (fromTheString[srcIndex] # EOS) DO
    IF CharUtils.IsEqual(theChar, fromTheString[srcIndex], caseSensitive) THEN
      stripIndex := srcIndex;
      stripCount := 1;
      INC(srcIndex);
      WHILE (srcIndex <= maxIndex) & (fromTheString[srcIndex] # EOS) &
	    CharUtils.IsEqual(theChar, fromTheString[srcIndex], caseSensitive) DO
	INC(stripCount);
	INC(srcIndex);
      END (*--while*);
      Delete(fromTheString, stripIndex, stripCount);
      DEC(srcIndex, stripCount);

    ELSE
      INC(srcIndex);
    END (*--if*);
  END (*--while*);
END Stripped;
(*--------------------*)

PROCEDURE StripLeading	 (VAR fromTheString: ARRAY OF CHAR (*--inout*);
			      theChar	   : CHAR	   (*--in   *);
			      caseSensitive: BOOLEAN	   (*--in   *));

VAR   srcIndex	 : CARDINAL;
      maxIndex	 : CARDINAL;
      stripIndex : CARDINAL;
      stripCount : CARDINAL;

BEGIN
  maxIndex   := HIGH(fromTheString);
  srcIndex   := 0;
  stripCount := 0;
  WHILE (srcIndex <= maxIndex) & (fromTheString[srcIndex] # EOS) &
	CharUtils.IsEqual(theChar, fromTheString[srcIndex], caseSensitive) DO
    INC(stripCount);
    INC(srcIndex);
  END (*--while*);
  Delete(fromTheString, 0, stripCount);
END StripLeading;
(*--------------------*)

PROCEDURE StripTrailing  (VAR fromTheString: ARRAY OF CHAR (*--inout*);
			      theChar	   : CHAR	   (*--in   *);
			      caseSensitive: BOOLEAN	   (*--in   *));

VAR   srcIndex: CARDINAL;
      srcLen  : CARDINAL;
      newEOS  : CARDINAL;

BEGIN
  srcIndex := Length(fromTheString);
  IF srcIndex > 0 THEN
    REPEAT
      newEOS := srcIndex;
      DEC(srcIndex);
    UNTIL (srcIndex = 0) OR
	  ~CharUtils.IsEqual(theChar, fromTheString[srcIndex], caseSensitive);
    IF newEOS <= VAL(CARDINAL,HIGH(fromTheString)) THEN
      fromTheString[newEOS] := EOS;
    END (*--if*);
  END (*--if*);
END StripTrailing;
(*--------------------*)


PROCEDURE Uppercase	 (    theString    : ARRAY OF CHAR (*--in   *);
			  VAR newString    : ARRAY OF CHAR (*--out  *));

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) &
	(index <= HIGH(newString)) DO
    newString[index] := CharUtils.Uppercase(theString[index]);
    INC(index);
  END (*--while*);
  newString[index] := EOS;
END Uppercase;
(*--------------------*)

PROCEDURE Lowercase	 (    theString    : ARRAY OF CHAR (*--in   *);
			  VAR newString    : ARRAY OF CHAR (*--out  *));

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) &
	(index <= HIGH(newString)) DO
    newString[index] := CharUtils.Lowercase(theString[index]);
    INC(index);
  END (*--while*);
  newString[index] := EOS;
END Lowercase;
(*--------------------*)

PROCEDURE Capitalized	 (    theString    : ARRAY OF CHAR (*--in   *);
			  VAR newString    : ARRAY OF CHAR (*--out  *));

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) &
	(index <= HIGH(newString)) DO
    INC(index);
  END (*--while*);
  newString[index] := EOS;
END Capitalized;
(*--------------------*)

PROCEDURE Uncapitalized  (    theString    : ARRAY OF CHAR (*--in   *);
			  VAR newString    : ARRAY OF CHAR (*--out  *));

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) &
	(index <= HIGH(newString)) DO
    INC(index);
  END (*--while*);
  newString[index] := EOS;
END Uncapitalized;
(*--------------------*)

PROCEDURE Replaced	 (    theChar	   : CHAR	   (*--in   *);
			      withChar	   : CHAR	   (*--in   *);
			      inTheString  : ARRAY OF CHAR (*--in   *);
			      caseSensitive: BOOLEAN	   (*--in   *);
			  VAR newString    : ARRAY OF CHAR (*--out  *));

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(inTheString)) & (inTheString[index] # EOS) &
	(index <= HIGH(newString)) DO
    IF CharUtils.IsEqual(theChar, inTheString[index], caseSensitive) THEN
      newString[index] := withChar;
    ELSE
      newString[index] := inTheString[index];
    END (*--if*);
    INC(index);
  END (*--while*);
  newString[index] := EOS;
END Replaced;
(*--------------------*)

PROCEDURE IsEmpty	 (    theString    : ARRAY OF CHAR (*--in   *))
					   : BOOLEAN	   (*--out  *);
BEGIN
  RETURN (HIGH(theString) = 0) & (theString[0] = EOS);
END IsEmpty;
(*--------------------*)

PROCEDURE IsAscii	 (    theString    : ARRAY OF CHAR (*--in   *))
					   : BOOLEAN	   (*--out  *);

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    IF ~CharUtils.IsAscii(theString[index]) THEN
      RETURN FALSE;
    END (*--if*);
    INC(index);
  END (*--while*);
  RETURN TRUE;
END IsAscii;
(*--------------------*)

PROCEDURE IsControl	 (    theString    : ARRAY OF CHAR (*--in   *))
					   : BOOLEAN	   (*--out  *);

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    IF ~CharUtils.IsControl(theString[index]) THEN
      RETURN FALSE;
    END (*--if*);
    INC(index);
  END (*--while*);
  RETURN TRUE;
END IsControl;
(*--------------------*)

PROCEDURE IsGraphic	 (    theString    : ARRAY OF CHAR (*--in   *))
					   : BOOLEAN	   (*--out  *);

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    IF ~CharUtils.IsGraphic(theString[index]) THEN
      RETURN FALSE;
    END (*--if*);
    INC(index);
  END (*--while*);
  RETURN TRUE;
END IsGraphic;
(*--------------------*)

PROCEDURE IsLowercase	 (    theString    : ARRAY OF CHAR (*--in   *))
					   : BOOLEAN	   (*--out  *);

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    IF ~CharUtils.IsLowercase(theString[index]) THEN
      RETURN FALSE;
    END (*--if*);
    INC(index);
  END (*--while*);
  RETURN TRUE;
END IsLowercase;
(*--------------------*)

PROCEDURE IsUppercase	 (    theString    : ARRAY OF CHAR (*--in   *))
					   : BOOLEAN	   (*--out  *);

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    IF ~CharUtils.IsUppercase(theString[index]) THEN
      RETURN FALSE;
    END (*--if*);
    INC(index);
  END (*--while*);
  RETURN TRUE;
END IsUppercase;
(*--------------------*)

PROCEDURE IsDigit	 (    theString    : ARRAY OF CHAR (*--in   *))
					   : BOOLEAN	   (*--out  *);

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    IF ~CharUtils.IsDigit(theString[index]) THEN
      RETURN FALSE;
    END (*--if*);
    INC(index);
  END (*--while*);
  RETURN TRUE;
END IsDigit;
(*--------------------*)

PROCEDURE IsAlphabetic	 (    theString    : ARRAY OF CHAR (*--in   *))
					   : BOOLEAN	   (*--out  *);

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    IF ~CharUtils.IsAlphabetic(theString[index]) THEN
      RETURN FALSE;
    END (*--if*);
    INC(index);
  END (*--while*);
  RETURN TRUE;
END IsAlphabetic;
(*--------------------*)

PROCEDURE IsAlphanumeric (    theString    : ARRAY OF CHAR (*--in   *))
					   : BOOLEAN	   (*--out  *);

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    IF ~CharUtils.IsAlphanumeric(theString[index]) THEN
      RETURN FALSE;
    END (*--if*);
    INC(index);
  END (*--while*);
  RETURN TRUE;
END IsAlphanumeric;
(*--------------------*)

PROCEDURE IsSpecial	 (    theString    : ARRAY OF CHAR (*--in   *))
					   : BOOLEAN	   (*--out  *);

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  index := 0;
  WHILE (index <= HIGH(theString)) & (theString[index] # EOS) DO
    IF ~CharUtils.IsSpecial(theString[index]) THEN
      RETURN FALSE;
    END (*--if*);
    INC(index);
  END (*--while*);
  RETURN TRUE;
END IsSpecial;
(*--------------------*)

PROCEDURE NumberOf	 (    theChar	   : CHAR	   (*--in   *);
			      inTheString  : ARRAY OF CHAR (*--in   *);
			      caseSensitive: BOOLEAN	   (*--in   *))
					   : CARDINAL	   (*--out  *);

VAR   index: CARDINAL;  (*-- loop index over characters of theString *)
      count: CARDINAL;

BEGIN
  count := 0;
  index := 0;
  WHILE (index <= HIGH(inTheString)) & (inTheString[index] # EOS) DO
    IF CharUtils.IsEqual(theChar, inTheString[index], caseSensitive) THEN
      INC(count);
    END (*--if*);
    INC(index);
  END (*--while*);
  RETURN count;
END NumberOf;
(*--------------------*)

PROCEDURE LocationOf	 (    theChar	   : CHAR	   (*--in   *);
			      inTheString  : ARRAY OF CHAR (*--in   *);
			      caseSensitive: BOOLEAN	   (*--in   *);
			      forward	   : BOOLEAN	   (*--in   *))
					   : CARDINAL	   (*--out  *);

VAR   index: CARDINAL; (*-- loop index over characters of theString *)

BEGIN
  IF forward THEN
    index := 0;
    WHILE (index <= HIGH(inTheString)) & (inTheString[index] # EOS) DO
      IF CharUtils.IsEqual(theChar, inTheString[index], caseSensitive) THEN
	RETURN index;
      END (*--if*);
      INC(index);
    END (*--while*);
  ELSE
    index := Length(inTheString);
    WHILE (index >= 0) DO
      IF CharUtils.IsEqual(theChar, inTheString[index], caseSensitive) THEN
	RETURN index;
      END (*--if*);
      DEC(index);
    END (*--while*);
  END (*--if*);
  RETURN 0;
END LocationOf;
(*--------------------*)

PROCEDURE Scan		 (    theString    : ARRAY OF CHAR (*--in   *);
			      forAnyChar   : ARRAY OF CHAR (*--in   *);
			      matchEqual   : BOOLEAN	   (*--in   *);
			  VAR atIndex	   : CARDINAL	   (*--inout*))
					   : BOOLEAN	   (*--out  *);

VAR   length,		(*-- maximum index of source string array *)
      initialIndex,	(*-- index upon entry to the routine *)
      cindex,		(*-- current index of pattern array *)
      clength: CARDINAL;(*-- maximum index of pattern array *)

BEGIN
  length      :=HIGH(theString);
  clength     :=HIGH(forAnyChar);
  initialIndex:=atIndex;

  (*-- Traverse the input string *)
  WHILE (atIndex <= length) & (theString[atIndex] # EOS) DO

    cindex:=0;
    IF matchEqual THEN

      (*-- Traverse the scanning string to find first match *)
      WHILE (cindex <= clength) & (forAnyChar[cindex] # EOS) DO
	IF (theString[atIndex] = forAnyChar[cindex]) THEN
	  RETURN TRUE;
	END (*--if*);
	INC(cindex);
      END (*--while*);

    ELSE
      (*-- Traverse the scanning string to find first non-match  *)
      WHILE (cindex <= clength) & (forAnyChar[cindex] # EOS) DO
	IF (theString[atIndex] # forAnyChar[cindex]) THEN
	  RETURN TRUE;
	END (*--if*);
	INC(cindex);
      END (*--while*);
    END (*--if*);

    INC(atIndex);	   (*-- Advance to next character of the input string *)
  END (*--while*);

  atIndex := initialIndex; (*-- restore index value upon failure *)
  RETURN FALSE;
END Scan;
(*--------------------*)

PROCEDURE IsEqual	 (    left	   : ARRAY OF CHAR (*--in   *);
			      right	   : ARRAY OF CHAR (*--in   *);
			      caseSensitive: BOOLEAN	   (*--in   *))
					   : BOOLEAN	   (*--out  *);
BEGIN
  IF ~caseSensitive THEN
    MakeLowercase(left);
    MakeLowercase(right);
  END (*--if*);
  RETURN Compare(left, right) = 0;
END IsEqual;
(*--------------------*)

PROCEDURE IsLessThan	 (    left	   : ARRAY OF CHAR (*--in   *);
			      right	   : ARRAY OF CHAR (*--in   *);
			      caseSensitive: BOOLEAN	   (*--in   *))
					   : BOOLEAN	   (*--out  *);
BEGIN
  IF ~caseSensitive THEN
    MakeLowercase(left);
    MakeLowercase(right);
  END (*--if*);
  RETURN Compare(left, right) = -1;
END IsLessThan;
(*--------------------*)

PROCEDURE IsGreaterThan  (    left	   : ARRAY OF CHAR (*--in   *);
			      right	   : ARRAY OF CHAR (*--in   *);
			      caseSensitive: BOOLEAN	   (*--in   *))
					   : BOOLEAN	   (*--out  *);
BEGIN
  IF ~caseSensitive THEN
    MakeLowercase(left);
    MakeLowercase(right);
  END (*--if*);
  RETURN Compare(left, right) = +1;
END IsGreaterThan;
(*--------------------*)

END StringUtils.
