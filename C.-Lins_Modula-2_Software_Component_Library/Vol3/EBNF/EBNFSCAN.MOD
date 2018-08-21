IMPLEMENTATION MODULE EBNFScanner;
(*====================================================================
    Version  : 1.0d2  06 Feb 1989  C. Lins
    Compiler : JPI's TopSpeed Modula-2 Compiler
    Component: EBNFScanner

    INTRODUCTION
    This module defines a scanner for EBNF.

    REVISION HISTORY
    v1.0d1  04 Feb 1989  C. Lins:
      Initial implementation derived from Wirth's Programming in
                        Modula-2, 4th edition, pp 96-100.
    v1.0d2  06 Feb 1989  C. Lins:
      Improved (speeded up) construction of unbounded string by
      creating all at once rather than adding a character at a time.
    v1.1    02 Dec 1989  I. S. C. Houston
      Adapted to JPI TopSpeed compiler and library
      and changed to take the language definition input
      from a named file.
      ====================================================================*)

FROM    CharItems IMPORT
        (*--cons*)      NullItem,
        (*--type*)      Item;

FROM    StrCSUMI IMPORT
        (*--cons*)      NullString,
        (*--type*)      String,
        (*--proc*)      Construct;

FROM    FIO IMPORT
        (*--var*)       EOF,
        (*--proc*)      RdChar;

FROM    IO    IMPORT
        (*--proc*)      WrChar, WrCharRep, WrStr, WrInt, WrLn;

IMPORT ASCII;

VAR     ch : CHAR;
        currentChar : INTEGER;
        colHiWater: INTEGER;
        errColIndex: INTEGER;

CONST   LineWidth = 256;
VAR     line: ARRAY [0..LineWidth-1] OF CHAR;

CONST   kMaxIdentifier = 30;

PROCEDURE GetLine;
BEGIN
        IF (errColIndex > 0) THEN
                (*--terminate active error report line*)
                WrLn;
                errColIndex := 0;
        END (*--if*);

        (*--ignore linefeed*)
        REPEAT ch := RdChar(source)
        UNTIL (ch # ASCII.lf);

        IF EOF OR (ch = ASCII.sub) THEN
                line[0] := ASCII.del;
                colHiWater := 1
        ELSE
                (*--not at end of file*)
                INC(lineNo);
                WrInt(lineNo, 5);
                WrChar(" ");
                colHiWater := 0;

                LOOP
                        WrChar(ch);
                        line[colHiWater] := ch;
                        INC(colHiWater);

                        ch := RdChar(source);
                        IF EOF THEN ch := ASCII.nul END;

                        IF (ch = ASCII.cr) OR
                           (ch = ASCII.nul) THEN
                                EXIT (*--loop*)
                        END (*--if*);

                END (*--loop*);
                WrLn
        END (*--if*);
END GetLine;
(*--------------------*)

PROCEDURE GetChar;
BEGIN
        WHILE (colHiWater = currentChar) DO
                currentChar := 0;
                GetLine;
        END (*--while*);
        ch := line[currentChar];
        INC(currentChar);
END GetChar;
(*--------------------*)


PROCEDURE GetIdentifier;

VAR     theString : ARRAY [0..kMaxIdentifier] OF Item;
        index     : INTEGER;

BEGIN
        theSymbol := literal;
        index := 0;
        REPEAT
                theString[index] := ch;
                INC(index);
                IF (ch > "Z") THEN
                        theSymbol := identifier;
                END (*--if*);
                GetChar;
        UNTIL (CAP(ch) < "A") OR (CAP(ch) > "Z");
        theString[index] := NullItem; (*-- terminate the Modula string *)
        theIdent := NullString; (*-- make sure the previous identifier isn't
                                  -- automatically deallocated *)
        Construct(theIdent, theString);
END GetIdentifier;
(*--------------------*)

PROCEDURE GetLiteral (    termCh : CHAR);

VAR             theString : ARRAY [0..kMaxIdentifier] OF Item;
                index     : INTEGER;

BEGIN
        theSymbol := literal;
        GetChar;
        index := 0;
        WHILE (ch # termCh) DO
                theString[index] := ch;
                INC(index);
                GetChar;
        END (*--while*);
        theString[index] := NullItem; (*-- terminate the Modula string *)
        theIdent := NullString; (*-- make sure the previous identifier isn't
                                  -- automatically deallocated *)
        Construct(theIdent, theString);
        GetChar;
END GetLiteral;
(*--------------------*)


PROCEDURE WrSymbol;
BEGIN
        CASE theSymbol OF
        identifier:    WrStr("identifier");
        | literal:     WrStr("literal");
        | lparen:      WrStr("lparen");
        | rparen:      WrStr("rparen");
        | lbrace:      WrStr("lbrace");
        | rbrace:      WrStr("rbrace");
        | lbraket:     WrStr("lbraket");
        | rbraket:     WrStr("rbraket");
        | bar:         WrStr("bar");
        | eql:         WrStr("eql");
        | period:      WrStr("period");
        ELSE           WrStr("other");
        END (*--case*);
        WrLn;
END WrSymbol;

PROCEDURE       GetSymbol;
BEGIN
        WHILE (ch = ASCII.space) OR
              (ch = ASCII.cr) DO
                GetChar;
        END (*--while*);
        IF (ch = ASCII.slash) THEN
                SkipLine;
                WHILE (ch = ASCII.space) OR
                      (ch = ASCII.cr) DO
                        GetChar;
                END (*--while*);
        END (*--if*);

        IF (CAP(ch) >= "A") & (CAP(ch) <= "Z") THEN
                GetIdentifier;
        ELSIF (ch = ASCII.quote) OR (ch = ASCII.quotequote) THEN
                GetLiteral(ch);
        ELSIF (ch = ASCII.equalsign) THEN
                theSymbol := eql;
                GetChar;
        ELSIF (ch = ASCII.lparen) THEN
                theSymbol := lparen;
                GetChar;
        ELSIF (ch = ASCII.rparen) THEN
                theSymbol := rparen;
                GetChar;
        ELSIF (ch = ASCII.lbracket) THEN
                theSymbol := lbraket;
                GetChar;
        ELSIF (ch = ASCII.rbracket) THEN
                theSymbol := rbraket;
                GetChar;
        ELSIF (ch = ASCII.lbrace) THEN
                theSymbol := lbrace;
                GetChar;
        ELSIF (ch = ASCII.rbrace) THEN
                theSymbol := rbrace;
                GetChar;
        ELSIF (ch = ASCII.bar) THEN
                theSymbol := bar;
                GetChar;
        ELSIF (ch = ASCII.period) THEN
                theSymbol := period;
                GetChar;
        ELSIF (ch = ASCII.del) THEN
                theSymbol := other;
        ELSE
                theSymbol := other;
                GetChar;
        END (*--if*);
END GetSymbol;
(*--------------------*)

PROCEDURE       MarkError (    errorNo : INTEGER);
BEGIN
        errorsFound := TRUE;
        IF (errColIndex = 0) THEN (*--start error report line*)
                WrChar("*");
                WrCharRep(" ",3);
        END (*--if*);
        WrCharRep(" ",currentChar);
        WrChar("^");
        WrInt(errorNo, 1);
        INC(errColIndex, 2);
END MarkError;
(*--------------------*)

PROCEDURE SkipLine;
VAR thisLineNo: INTEGER;
BEGIN
        thisLineNo := lineNo;
        WHILE (thisLineNo = lineNo) DO
                GetChar
        END (*--while*);
        GetChar;
END SkipLine;
(*--------------------*)

BEGIN
        theIdent       := NullString;
        theSymbol      := other;
        errorsFound    := FALSE;
        lineNo         := 0;
        currentChar    := 0;
        colHiWater     := 0;
        errColIndex    := 0;
        ch             := " ";

END EBNFScanner.
