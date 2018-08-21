IMPLEMENTATION MODULE EBNFTable;
(*====================================================================
    Version  : 1.0d2  06 Feb 1989  C. Lins
    Compiler : JPI's TopSpeed Modula-2 Compiler
    Component: EBNFTable

    INTRODUCTION
    This module defines a symbol table for identifiers and line numbers
    for the EBNF tool.

    The act of tabulating a symbol table causes the table to be destroyed.

    REVISION HISTORY
    v1.0d1  04 Feb 1989  C. Lins:
      Initial implementation derived from Wirth's Programming in
                        Modula-2, 4th edition, pp 96-100.
    v1.0d2  06 Feb 1989  C. Lins:
                        Changed to use k-balanced tree where k = 3.
    v1.1    01 Dec 1989  I. S. C. Houston
                        Changed to use JPI Compiler and library.
====================================================================*)

FROM    JPIStorage IMPORT
        (*--proc*) Allocate, Deallocate;

FROM    StrCSUMI IMPORT
        (*--type*) String;

FROM    Relations IMPORT
        (*--type*) Relation;

IMPORT  StrCSUMI;
IMPORT  TreeTypes;
IMPORT  IPBSUMI;
IMPORT  TypeManager;
IMPORT  CharItems;

FROM    IO IMPORT
        (*--proc*)  WrChar, WrCharRep, WrInt, WrLn;

TYPE    SymbolTable = IPBSUMI.Tree;

VAR     listOverflow : BOOLEAN;


PROCEDURE StringAssign (    theKey : TreeTypes.Key) : TreeTypes.Key;
VAR       newString : String;
BEGIN
        (* Ensure that target string doesn't contain garbage that Assign might
                 interpret as a legal string and try and deallocate it. *)
        newString := StrCSUMI.NullString;
        (* Assign the string value from source to target *)
        StrCSUMI.Assign( String(theKey), newString);
        (* Return the new string copy *)
        RETURN TreeTypes.Key(newString);
END StringAssign;
(*--------------------*)

PROCEDURE StringCompare (    left, right : TreeTypes.Key) : Relation;
BEGIN
        RETURN StrCSUMI.Compare(String(left), String(right));
END StringCompare;
(*--------------------*)

PROCEDURE StringDispose (VAR theKey : TreeTypes.Key);
VAR             s : String;
BEGIN
        (* Coerce into proper type for use as VAR parameter *)
        s := String(theKey);
        (* Deallocate the dynamic string. "s" is changed to NIL *)
        StrCSUMI.Destroy(s);
        (* Store NIL back into the key *)
        theKey := TreeTypes.Key(s);
END StringDispose;
(*--------------------*)

TYPE    List = POINTER TO ListNode;
TYPE    ListNode = RECORD
             lineNo : INTEGER;
             next   : List;
        END (*--ListNode*);

(* We must provide an assignment routine but know that it won't be used. *)
PROCEDURE ListAssign (list : TreeTypes.Data) : TreeTypes.Data;
BEGIN
 RETURN list
END ListAssign;
(*--------------------*)

PROCEDURE ListDispose (VAR list : TreeTypes.Data);
VAR             l : List;
BEGIN
        l := List(list);
        Deallocate(l,SIZE(ListNode)); (* returns NIL *)
        list := TreeTypes.Data(l);    (* output null list of coerced type *)
END ListDispose;
(*--------------------*)

PROCEDURE NewListNode (    theLineNo : INTEGER) : List;
VAR             l : List;
BEGIN
        Allocate(l, SIZE(ListNode));
        IF (l = NIL) THEN
                listOverflow := TRUE;
        ELSE
                l^.lineNo := theLineNo;
                l^.next   := NIL;
        END (*--if*);
        RETURN l;
END NewListNode;


PROCEDURE Create  () : SymbolTable  (*--out  *);

VAR             theTable : SymbolTable;
         stringKeyTypeID : TypeManager.TypeID;
         listDataTypeID  : TypeManager.TypeID;

BEGIN
        stringKeyTypeID := TypeManager.Create("string",
                                          StringAssign,
                                          StringCompare,
                                          StringDispose);
        listDataTypeID  := TypeManager.Create("list",
                                          ListAssign,
                                          TypeManager.NoCompareProc,
                                          ListDispose);
        theTable := SymbolTable(
                    IPBSUMI.Create(stringKeyTypeID,
                    listDataTypeID, 3));
        RETURN theTable;
END Create;
(*--------------------*)

PROCEDURE Found (    theKey  : TreeTypes.Key;
                 VAR theList : TreeTypes.Data;
                     newData : TreeTypes.Data);

VAR             newList : List;

BEGIN
        newList := List(newData);
        newList^.next := List(theList);
        theList := TreeTypes.Data(newList);
END Found;
(*--------------------*)

PROCEDURE Record  (    inTable  : SymbolTable (*--inout*);
                       theID    : String      (*--in   *);
                       lineNo   : INTEGER     (*--in   *));

VAR             t : IPBSUMI.Tree;
                        newList : List;

BEGIN
        newList := NewListNode(lineNo);
        IF NOT listOverflow THEN
                t := IPBSUMI.Tree(inTable);
                IPBSUMI.Insert(t,
                               TreeTypes.Key(theID),
                               TreeTypes.Data(newList),
                               Found);
        END (*--if*);
     (* CursorControl.SpinCursor(8); *)
END Record;
(*--------------------*)


PROCEDURE OutputString (c : CharItems.Item);
BEGIN
        WrChar(c);
END OutputString;
(*--------------------*)

PROCEDURE WriteTable (    theKey : TreeTypes.Key  (*--in   *);
                      VAR theData: TreeTypes.Data (*--in   *));

CONST   width = 6;
    lineWidth = 120;
    wordWidth = 30;
            N = (lineWidth - wordWidth) DIV width;

VAR  q         : List;
     i         : INTEGER;
     keyLength : INTEGER;

BEGIN
     (* CursorControl.SpinCursor(-8); *)
        StrCSUMI.Traverse( String(theKey), OutputString);
        keyLength := StrCSUMI.LengthOf( String(theKey) );
        IF (keyLength < wordWidth) THEN
                WrCharRep(" ",wordWidth-keyLength);
        END (*--if*);
        WrChar(" ");

        q := List(theData);
        i := N;
        WHILE (q # NIL) DO
                IF (i = 0) THEN
                        WrLn;
                        WrCharRep(" ",wordWidth+1);
                        i := N;
                END (*--if*);
                WrInt(q^.lineNo, width);
                q := q^.next;
                DEC(i);
             (* CursorControl.SpinCursor(-4); *)
        END (*--while*);
        WrLn;
END WriteTable;
(*--------------------*)

PROCEDURE Tabulate (    theTable: SymbolTable (*--in   *));
VAR     t : IPBSUMI.Tree;
BEGIN
        IPBSUMI.Inorder( IPBSUMI.Tree(theTable), WriteTable);
        t := IPBSUMI.Tree(theTable);
        IPBSUMI.Destroy(t);
END Tabulate;
(*--------------------*)

PROCEDURE Overflow () : BOOLEAN     (*--out  *);
BEGIN
        RETURN (IPBSUMI.TreeError() # TreeTypes.noerr) OR listOverflow;
END Overflow;
(*--------------------*)

BEGIN
        listOverflow := FALSE;
END EBNFTable.
