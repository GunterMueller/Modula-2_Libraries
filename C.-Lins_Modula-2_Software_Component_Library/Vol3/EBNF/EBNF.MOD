MODULE EBNF;
(*====================================================================
    Version  : 1.00  04 Feb 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2 Compiler
    Component: EBNF

    INTRODUCTION
    This is the main module to the tool EBNF. It reads a language
    specification in EBNF format from a file. It writes to standard
    output two lists: one of terminal symbols, the other of nonterminals.

    The input source is checked for conformity with the EBNF syntax. The
    errors that can result from an ill-formed specification are listed
    in the procedure WrErrorCodes in this module.

    REVISION HISTORY
    v1.00  04 Feb 1989  C. Lins:
      Initial implementation derived from Wirth's Programming in
                        Modula-2, 4th edition, pp 96-100.
    v1.01  06 Dec 1989  Iain Houston:
      Adapted to JPI TopSpeed Modula2 compiler.
      Improved error reporting.
====================================================================*)

FROM    EBNFScanner IMPORT
        (*--type*)      Symbol,
        (*--var *)      source, theSymbol, lineNo, theIdent, errorsFound,
        (*--proc*)      GetSymbol, MarkError, SkipLine;

FROM    EBNFTable IMPORT
        (*--type*)      SymbolTable,
        (*--proc*)      Create, Record, Tabulate, Overflow;

FROM    FIO IMPORT
        (*--proc*)      AssignBuffer,
        (*--const*)     BufferOverhead;

FROM    IO IMPORT
        (*--proc*)      WrLn, WrStr;

FROM    FIO IMPORT
        (*--var*)      EOF,
        (*--proc*)     Exists, Open, Close, File, RdChar;

FROM    Lib IMPORT
        (*--proc*)      ParamStr;

VAR     terminals       : SymbolTable;
        nonTerminals    : SymbolTable;
        fileName        : ARRAY [0..64] OF CHAR;


PROCEDURE SkipToSymbol (    errorNo : INTEGER (*--in   *));
BEGIN
        MarkError(errorNo);
        WHILE (theSymbol < lparen) OR (theSymbol > period) DO
                GetSymbol;
        END (*--while*);
END SkipToSymbol;

PROCEDURE Expression;

        PROCEDURE Term;

                PROCEDURE Factor;
                BEGIN
                        IF (theSymbol = identifier) THEN
                                Record(nonTerminals, theIdent, lineNo);
                                GetSymbol;

                        ELSIF (theSymbol = literal) THEN
                                Record(terminals, theIdent, lineNo);
                                GetSymbol;

                        ELSIF (theSymbol = lparen) THEN
                                GetSymbol;
                                Expression;
                                IF (theSymbol = rparen) THEN
                                        GetSymbol;
                                ELSE
                                        SkipToSymbol(2);
                                END (*--if*);

                        ELSIF (theSymbol = lbraket) THEN
                                GetSymbol;
                                Expression;
                                IF (theSymbol = rbraket) THEN
                                        GetSymbol;
                                ELSE
                                        SkipToSymbol(3);
                                END (*--if*);

                        ELSIF (theSymbol = lbrace) THEN
                                GetSymbol;
                                Expression;
                                IF (theSymbol = rbrace) THEN
                                        GetSymbol;
                                ELSE
                                        SkipToSymbol(4);
                                END (*--if*);

                        ELSE
                                SkipToSymbol(5);
                        END (*--if*);
                END Factor;

        BEGIN           (*-- Term --*)
                Factor;
                WHILE (theSymbol < bar) DO
                        Factor;
                END (*--while*);
        END Term;

BEGIN           (*-- Expression --*)
        Term;
        WHILE (theSymbol = bar) DO
                GetSymbol;
                Term;
        END (*--while*);
END Expression;

PROCEDURE Production;
BEGIN
        (*-- theSymbol = identifier --*)
        Record(nonTerminals, theIdent, -lineNo);
        GetSymbol;
        IF (theSymbol = eql) THEN
                GetSymbol;
        ELSE
                SkipToSymbol(7);
        END (*--if*);
        Expression;
        IF (theSymbol # period) THEN
                MarkError(8);
                SkipLine;
        END (*--if*);
        GetSymbol;
END Production;

PROCEDURE WrErrorCodes;
BEGIN
        WrLn;WrLn;WrLn;
        WrStr('Syntax errors were found. Errors were coded as follows:');
        WrLn;WrLn;
        WrStr('1.  <reserved>                                      ');WrLn;
        WrStr('2.  Missing right parentheses                       ');WrLn;
        WrStr('3.  Missing right bracket                           ');WrLn;
        WrStr('4.  Missing right brace                             ');WrLn;
        WrStr('5.  Identifier, literal, "(", "[", or "{" expected  ');WrLn;
        WrStr('6.  Symbol table overflow                           ');WrLn;
        WrStr('7.  Equal sign expected                             ');WrLn;
        WrStr('8.  Period expected                                 ');WrLn
END WrErrorCodes;

PROCEDURE Main;
BEGIN   (*--Main*)
        terminals   := Create();
        nonTerminals:= Create();

        GetSymbol;
        WHILE (theSymbol = identifier) & ~Overflow() DO
                Production;
        END (*--while*);
        IF Overflow() THEN
                WrLn;
                WrStr("### EBNF: Symbol Table Overflow");
                WrLn;
        END (*--if*);
        WrLn;WrLn;
        WrStr("Non-Terminal Symbols");WrLn;
        Tabulate(nonTerminals);WrLn;
        WrStr("Terminal Symbols");WrLn;
        Tabulate(terminals);
        IF errorsFound THEN WrErrorCodes END
END Main;

CONST   toolName= "EBNF Tool";
CONST   version = "  OS/2 v1.1  02 Dec 1989";
CONST   author1 = "  by Iain Houston,";
CONST   author2 = "  after Chuck Lins.";

VAR sBuff: ARRAY [0..(2*512+BufferOverhead)] OF BYTE;

BEGIN
        WrStr(toolName);
        WrStr(version);WrLn;
        WrStr(author1);WrLn;
        WrStr(author2);WrLn;
        WrLn;

        ParamStr( fileName, 1);   (*--find fileName from invocation argument*)
        IF Exists( fileName ) THEN
                source := Open( fileName );
                WrStr('Reading a language definition from ');
                WrStr(fileName);
                WrLn;WrLn;
                AssignBuffer(source, sBuff);
                Main;
                Close(source);
                WrLn;
                WrStr("Normal program termination.")
        ELSE
                WrStr("Couldn't find the file ");
                WrStr(fileName);
                WrLn;
                WrStr("Program terminated.");
        END     (*--IF Exists*)
END EBNF.
