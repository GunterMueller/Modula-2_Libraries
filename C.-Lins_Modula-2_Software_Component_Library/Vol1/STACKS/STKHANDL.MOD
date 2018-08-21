IMPLEMENTATION MODULE StkHandlers;
(*==============================================================
    Version  : 1.00  28 Apr 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Component: Stack Exception Handlers Utility

    REVISION HISTORY
    v1.00  28 Apr 1989  C. Lins:
        Initial implementation
==============================================================*)

FROM StackEnum IMPORT
	(*--type*) Exceptions, Operations;


FROM IO IMPORT
	(*--proc*) WrStr, WrLn, WrCard;


PROCEDURE WriteHandler (    theModule 	: CARDINAL;
	  	theOperation: Operations;
	  	theException: Exceptions;

BEGIN
  WrStr('### Error "');

  CASE theException OF
    noerr     : WrStr('No Error');
  | initfailed: WrStr('Initialization failure');
  | typeerror : WrStr('Type mismatch between Stacks');
  | overflow  : WrStr('Stack Overflow');
  | underflow : WrStr('Stack Underflow');
  | undefined : WrStr('Undefined Stack');
  END (*--case*);

  WrStr('" raised in Routine "');

  CASE theOperation OF
    assign   : WrStr('Assign');
  | create   : WrStr('Create');
  | destroy  : WrStr('Destroy');
  | clear    : WrStr('Clear');
  | push     : WrStr('Push');
  | pop      : WrStr('Pop');
  | poptopof : WrStr('PopTopOf');

  | isempty    : WrStr('IsEmpty');
  | isequal    : WrStr('IsEqual');
  | typeof     : WrStr('TypeOf');
  | sizeof     : WrStr('SizeOf');
  | topof      : WrStr('TopOf');
  | depthof    : WrStr('DepthOf');
	
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

END StkHandlers.