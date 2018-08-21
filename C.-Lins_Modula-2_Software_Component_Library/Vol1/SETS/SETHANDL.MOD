IMPLEMENTATION MODULE SetHandlers;
(*==============================================================
    Version  : 1.00  02 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Set Exception Handlers Utility

    REVISION HISTORY
    v1.00  02 May 1989  C. Lins:
        Initial TopSpeed Modula-2 implementation

    (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM SetEnum IMPORT
	(*--type*) Exceptions, Operations;

FROM IO IMPORT
	(*--proc*) WrStr, WrLn, WrCard;


PROCEDURE WriteHandler (    theModule 	: CARDINAL;
	  	                    theOperation: Operations;
	  	                    theException: Exceptions);

BEGIN
  WrStr('### Error "');

  CASE theException OF
  	noerr     : WrStr('No Error');
  |	domainerr : WrStr('Domain Error');
  | iteminset : WrStr('Item in Set');
  | notinset  : WrStr('Item not in Set');
  | overflow  : WrStr('Overflow');
  | typeerror : WrStr('Type mismatch between sets');
  | undefined : WrStr('Undefined Set');
  END (*--case*);

  WrStr('" raised in Routine "');

  CASE theOperation OF
  	assign        : WrStr('Assign');
  | create        : WrStr('Create');
  | destroy       : WrStr('Destroy');
  | clear         : WrStr('Clear');
  | include       : WrStr('Include');
  | exclude       : WrStr('Exclude');
  | inclrange     : WrStr('Include Range');
  | exclrange     : WrStr('Exclude Range');
  | union         : WrStr('Union');
  | intersection  : WrStr('Intersection');
  | difference    : WrStr('Difference');
  | symdifference : WrStr('Symmetric Difference');
  | complement    : WrStr('Complement');
  | construct     : WrStr('Construct');

  | isempty       : WrStr('IsEmpty');
  | isequal       : WrStr('IsEqual');
  | typeof        : WrStr('TypeOf');
  | sizeof        : WrStr('SizeOf');
  | nummembers    : WrStr('NumMembers');
  | ismember      : WrStr('IsMember');
  | issubset      : WrStr('IsSubset');
  | ispropersubset: WrStr('IsProperSubset');
  | universeof    : WrStr('UniverseOf');
	
  | loopover      : WrStr('LoopOver');
  | traverse      : WrStr('Traverse');

  ELSE
    WrStr('?????');
  END (*--case*);

  WrStr('" in Module ');
  WrCard(theModule, 1);
  WrLn;
END WriteHandler;

END SetHandlers.