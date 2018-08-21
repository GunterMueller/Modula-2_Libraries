IMPLEMENTATION MODULE StrHandlers;
(*==============================================================
    Version  : 1.00  30 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: String Exception Handlers Utility

    REVISION HISTORY
    v1.00  30 Apr 1989  C. Lins:
        Initial implementation

    (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM StrEnum IMPORT
	(*--type*) Exceptions, Operations;

FROM IO IMPORT
	(*--proc*) WrStr, WrLn, WrCard;


PROCEDURE WriteHandler (    theModule 	: CARDINAL;
	                    theOperation: Operations;
	  	            theException: Exceptions);
BEGIN
  WrStr('### Error "');

  CASE theException OF
    noerr      : WrStr('No Error');
  | typeerror  : WrStr('Type mismatch');
  | overflow   : WrStr('Overflow');
  | positionerr: WrStr('Position Error');
  | undefined  : WrStr('Undefined String');
  END (*--case*);

  WrStr('" raised in Routine "');

  CASE theOperation OF
    assign   : WrStr('Assign');
  | create   : WrStr('Create');
  | destroy  : WrStr('Destroy');
  | clear    : WrStr('Clear');
  | prepend  : WrStr('Prepend');
  | append   : WrStr('Append');
  | insert   : WrStr('Insert');
  | delete   : WrStr('Delete');
  | replace  : WrStr('Replace');
  | setitem  : WrStr('SetItem');
  | construct: WrStr('Construct');

  | isempty    : WrStr('IsEmpty');
  | isequal    : WrStr('IsEqual');
  | typeof     : WrStr('TypeOf');
  | lengthof   : WrStr('LengthOf');
  | sizeof     : WrStr('SizeOf');
  | compare    : WrStr('Compare');
  | itemof     : WrStr('ItemOf');
  | substringof: WrStr('SubstringOf');
  | sliceof    : WrStr('SliceOf');
	
  | loopover   : WrStr('LoopOver');
  | loopchange : WrStr('LoopChange');
  | traverse   : WrStr('Traverse');
  | travchange : WrStr('TravChange');
  ELSE
    WrStr('?????');
  END (*--case*);

  WrStr('" in Module ');
  WrCard(theModule, 1);
  WrLn;
END WriteHandler;

END StrHandlers.