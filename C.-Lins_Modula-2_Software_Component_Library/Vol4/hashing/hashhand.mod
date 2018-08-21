IMPLEMENTATION MODULE HashHand; 
(*==============================================================
    Version  : 2.0 16 Sep 1990 C. Lins
    Compiler : Generic pc Modula-2
    Component: Hash Table Exception Handlers Utility

    REVISION HISTORY
    v1.00  20 Apr 1989  C. Lins:
        Initial implementation
    v1.01  22 Apr 1989  C. Lins:
        Added Iterate operation.
    v1.02  21 Aug 1989  C. Lins:
        Added HashOf to list of operations.
   v2.00  16 Sep 1990  C. Lins
     Created generic pc version
   (C) Copyright 1990 Charles A. Lins
==============================================================*)

FROM ErrorHandling IMPORT
   (*--type*) ErrorCode,
   (*--proc*) Unpack;

FROM HashTypes IMPORT
   (*--type*) Exceptions, Operations;

FROM Utilities IMPORT
   (*--proc*) SysBeep;

FROM InOut IMPORT
   (*--proc*) WriteString, WriteLn, WriteCard;


PROCEDURE WriteHandler (    theError : ErrorCode (*--in   *));

VAR   theModule   : CARDINAL;
      theOperation: Operations;
      theException: Exceptions;

BEGIN
  SysBeep(0);
  Unpack(theError, theModule, theOperation, theException);

  WriteString('### Error "');

  CASE theException OF
    noerr        : WriteString('No Error');
  | duplicatekey : WriteString('Duplicate Key');
  | overflow     : WriteString('Overflow');
  | notfound     : WriteString('Key not found');
  | undefined    : WriteString('Undefined Hash Table');
  END (*--case*);

  WriteString('" raised in Routine "');

  CASE theOperation OF
    assign   : WriteString('Assign');
  | create   : WriteString('Create');
  | destroy  : WriteString('Destroy');
  | clear    : WriteString('Clear');
  | insert   : WriteString('Insert');
  | remove   : WriteString('Remove');
  | update   : WriteString('Update');
 
  | isempty    : WriteString('IsEmpty');
  | typeof     : WriteString('TypeOf');
  | hashof     : WriteString('HashOf');
  | sizeof     : WriteString('SizeOf');
  | extentof   : WriteString('ExtentOf');
  | ispresent  : WriteString('IsPresent');
   
  | loopover   : WriteString('LoopOver');
  | traverse   : WriteString('Traverse');
  | iterate    : WriteString('Iterate');
  ELSE
    WriteString('Unknown');
  END (*--case*);

  WriteString('" in Module ');
  WriteCard(theModule, 1);
  WriteLn;
END WriteHandler;
(*-------------------------*)

END HashHand.