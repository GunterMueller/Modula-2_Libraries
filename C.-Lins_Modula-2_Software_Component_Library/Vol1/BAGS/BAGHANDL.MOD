IMPLEMENTATION MODULE BagHandlers;
(*==============================================================
    Version  : 1.00  02 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Set Exception Handlers Utility

    REVISION HISTORY
    v1.00  02 May 1989  C. Lins:
        Initial implementation

    (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM BagEnum IMPORT
	(*--type*) Exceptions, Operations;

FROM IO IMPORT
	(*--proc*) WrStr, WrLn, WrCard;


PROCEDURE WriteHandler (    theModule  : CARDINAL   (*--in   *);
                            theRoutine : Operations (*--in   *);
                            theError   : Exceptions (*--in   *));
BEGIN
  WrStr('### Error "');

  CASE theError OF
  	noerr     : WrStr('No Error');
  |	domainerr : WrStr('Domain Error');
  | notinbag  : WrStr('Item not in Bag');
  | overflow  : WrStr('Overflow');
  | typeerror : WrStr('Type mismatch between sets');
  | undefined : WrStr('Undefined Set');
  END (*--case*);

  WrStr('" raised in Routine "');

  CASE theRoutine OF
  	assign        : WrStr('Assign');
  | create        : WrStr('Create');
  | destroy       : WrStr('Destroy');
  | clear         : WrStr('Clear');
  | include       : WrStr('Include');
  | exclude       : WrStr('Exclude');
  | union         : WrStr('Union');
  | intersection  : WrStr('Intersection');
  | difference    : WrStr('Difference');
  | symdifference : WrStr('Symmetric Difference');

  | isempty       : WrStr('IsEmpty');
  | isequal       : WrStr('IsEqual');
  | typeof        : WrStr('TypeOf');
  | sizeof        : WrStr('SizeOf');
  | nummembers    : WrStr('NumMembers');
  | uniquemembers : WrStr('UniqueMembers');
  | ismember      : WrStr('IsMember');
  | issubset      : WrStr('IsSubset');
  | ispropersubset: WrStr('IsProperSubset');
  | numberof      : WrStr('NumberOf');
	
	| loopover      : WrStr('LoopOver');
	| traverse      : WrStr('Traverse');

  ELSE
    WrStr('?????');
  END (*--case*);

  WrStr('" in Module ');
	WrCard(theModule, 1);
  WrLn;
END WriteHandler;

END BagHandlers.