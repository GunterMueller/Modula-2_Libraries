IMPLEMENTATION MODULE InOutUtils;
(*=======================================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Code Size: R-  bytes

    Component Class: Tool
    Component Form : Standard I/O Support Utility - InOut Utilities

    REVISION HISTORY
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    INTRODUCTION
    This module implements support routines for simplifying use of InOut.

    LOCAL VARIABLES
    * yesNoString   Array of strings Yes and No indexed by a boolean.

    Copyright (C) 1989 Charles A. Lins. All rights reserved.
=======================================================================*)

FROM IO        IMPORT (*--Proc*) RdChar, WrChar, WrStr, WrLn,
				 WrCard, WrInt, WrLngInt,
				 WrLngCard, WrReal;
FROM ASCII     IMPORT (*--Cons*) nul, cr, lf, bs, bel, del;
FROM CharUtils IMPORT (*--Proc*) MakeUppercase, Uppercase;

(*--------------------*)

PROCEDURE CarriageReturn;
BEGIN
  WrChar(cr);
END CarriageReturn;
(*--------------------*)

PROCEDURE LineFeed;
BEGIN
  WrChar(lf);
END LineFeed;
(*--------------------*)

PROCEDURE WriteNChars	  (    c: CHAR		      (*-- in *);
			       n: CARDINAL	      (*-- in *));
VAR   i: CARDINAL;
BEGIN
  FOR i:=1 TO n DO
    WrChar(c);
  END(*--for*);
END WriteNChars;
(*--------------------*)

PROCEDURE BackSpace	  (    n: CARDINAL	      (*-- in *));
BEGIN
  WriteNChars(bs,n);
END BackSpace;
(*--------------------*)

PROCEDURE Erase 	  (    n: CARDINAL	      (*-- in *));
BEGIN
  WriteNChars(del,n);
END Erase;
(*--------------------*)

PROCEDURE Beep		  (    n: CARDINAL	      (*-- in *));
BEGIN
  WriteNChars(bel,n);
END Beep;
(*--------------------*)

PROCEDURE WaitForUser;
VAR   ch: CHAR;
BEGIN
  WrStr("Press any key to continue...");
  ch:=RdChar();
  WrChar(ch);
  WrLn;
  IF Uppercase(ch) = 'A' THEN
    HALT; (*-- Terminate program *)
  END(*--if*);
END WaitForUser;
(*--------------------*)

PROCEDURE WriteBlanks	  (    n: CARDINAL	      (*-- in *));
BEGIN
  WriteNChars(' ',n);
END WriteBlanks;
(*--------------------*)

PROCEDURE WriteBlankLines (    n: CARDINAL	      (*-- in *));
VAR   i: CARDINAL;
BEGIN
  FOR i:=1 TO n DO
    WrLn;
  END(*--for*);
END WriteBlankLines;
(*--------------------*)

PROCEDURE WriteLnString   (    string : ARRAY OF CHAR (*-- in *));
BEGIN
  WrStr(string);
  WrLn;
END WriteLnString;
(*--------------------*)

PROCEDURE WriteLnsString  (    string : ARRAY OF CHAR (*-- in *);
			       nLines : CARDINAL      (*-- in *));
BEGIN
  WrStr(string);
  WriteBlankLines(nLines);
END WriteLnsString;
(*--------------------*)

VAR   yesNoString: ARRAY BOOLEAN OF ARRAY [0..4] OF CHAR;

PROCEDURE WriteYesOrNo	  (    logical: BOOLEAN       (*-- in *));
BEGIN
  WrStr(yesNoString[logical]);
END WriteYesOrNo;
(*--------------------*)

PROCEDURE WriteLblBoolean (    s : ARRAY OF CHAR    (*--in   *);
			       b : BOOLEAN	    (*--in   *));
BEGIN
    WrStr(s);
    WrStr(yesNoString[b]);
END WriteLblBoolean;
(*--------------------*)

PROCEDURE WriteLblCardinal (	s : ARRAY OF CHAR   (*--in   *);
				c : CARDINAL	    (*--in   *);
				w : CARDINAL	    (*--in   *));
BEGIN
    WrStr(s);
    WrCard(c, w);
END WriteLblCardinal;
(*--------------------*)

PROCEDURE WriteLblInteger  (	s : ARRAY OF CHAR   (*--in   *);
				n : INTEGER	    (*--in   *);
				w : CARDINAL	    (*--in   *));
BEGIN
    WrStr(s);
    WrInt(n, w);
END WriteLblInteger;
(*--------------------*)

PROCEDURE WriteLblReal	  (    s : ARRAY OF CHAR    (*--in   *);
			       r : REAL 	    (*--in   *);
			       n : CARDINAL	    (*--in   *);
			       d : CARDINAL	    (*--in   *);
			       fixed : BOOLEAN	    (*--in   *));
BEGIN
    WrStr(s);
    WrReal(r, n, d);
END WriteLblReal;
(*--------------------*)

PROCEDURE WriteLblLongCard(    s : ARRAY OF CHAR    (*--in   *);
			       c : LONGCARD	    (*--in   *);
			       w : CARDINAL	    (*--in   *));
BEGIN
    WrStr(s);
    WrLngCard(c, w);
END WriteLblLongCard;
(*--------------------*)

PROCEDURE WriteLblLongInt (    s : ARRAY OF CHAR    (*--in   *);
			       n : LONGINT	    (*--in   *);
			       w : CARDINAL	    (*--in   *));
BEGIN
    WrStr(s);
    WrLngInt(n, w);
END WriteLblLongInt;
(*--------------------*)

PROCEDURE AnswerIsYesTo   (    query  : ARRAY OF CHAR (*-- in *))
				      : BOOLEAN       (*-- out *);
VAR   ch: CHAR;
BEGIN
  WrStr(query);
  WrStr(" [y/n]? ");
  REPEAT
    ch:=RdChar();
    MakeUppercase(ch);
  UNTIL (ch = "Y") OR (ch = "N");
  RETURN ch = "Y";
END AnswerIsYesTo;
(*--------------------*)

PROCEDURE InputString	  (VAR string : ARRAY OF CHAR (*-- out *));
VAR   currentChar : CHAR;
      currentIndex: CARDINAL;
      maximumIndex: CARDINAL;
BEGIN
  currentIndex := 0;
  maximumIndex := HIGH(string);
  LOOP
    currentChar:=RdChar();
    WrChar(currentChar);
    IF (currentChar = bs) THEN
      IF (0 < currentIndex) THEN
	DEC(currentIndex);
      END (*--if*);

    ELSIF (currentChar = lf) OR (currentChar = cr) THEN
      EXIT (*--loop*);

    ELSIF (currentIndex < maximumIndex)  THEN
      string[currentIndex] := currentChar;
      INC(currentIndex);
    END (*--if*);
  END (*--loop*);
  string[currentIndex] := nul;
END InputString;
(*--------------------*)


BEGIN (*~~~~~ Module Initializarion ~~~~~*)
  yesNoString[FALSE]:="FALSE";
  yesNoString[TRUE ]:="TRUE";
END InOutUtils.
