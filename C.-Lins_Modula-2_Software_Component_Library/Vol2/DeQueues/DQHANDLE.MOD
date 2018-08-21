IMPLEMENTATION MODULE DQHandlers; 
(*==============================================================
    Version  : 1.00  16 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Deque Exception Handlers Utility

    REVISION HISTORY
    v1.00  16 May 1989  C. Lins:
	Initial TopSpeed Modula-2 implementation
==============================================================*)

FROM QEnum IMPORT
	(*--type*) Exceptions, Operations;

FROM IO IMPORT
	(*--proc*) WrStr, WrLn, WrCard;


PROCEDURE WriteHandler (    theModule 	 : CARDINAL   (*--in   *);
							theOperation : Operations (*--in   *);
							theException : Exceptions (*--in   *));

BEGIN
  WrStr('### Error "');

  CASE theException OF
  	noerr     : WrStr('No Error');
  |	initfailed: WrStr('Initialization failure');
  | notfound  : WrStr('Item not found in Deque');
  | overflow  : WrStr('Deque Overflow');
  | typeerror : WrStr('Type mismatch between Deques');
  | underflow : WrStr('Deque Underflow');
  | undefined : WrStr('Undefined Deque');
  END (*--case*);

  WrStr('" raised in Routine "');

  CASE theOperation OF
  	assign   : WrStr('Assign');
  | create   : WrStr('Create');
  | destroy  : WrStr('Destroy');
  | clear    : WrStr('Clear');
  | arrive   : WrStr('Arrive');
  | depart   : WrStr('Depart');
  | leave    : WrStr('Leave');
 
  | isempty    : WrStr('IsEmpty');
  | isequal    : WrStr('IsEqual');
  | typeof     : WrStr('TypeOf');
  | sizeof     : WrStr('SizeOf');
  | lengthof   : WrStr('LengthOf');
  | frontof    : WrStr('FrontOf');
  | rearof     : WrStr('RearOf');
  | endof      : WrStr('EndOf');
  | positionof : WrStr('PositionOf');
	
	| loopover   : WrStr('LoopOver');
	| traverse   : WrStr('Traverse');
	
  ELSE
    WrStr('?????');
  END (*--case*);

  WrStr('" in Module ');
  WrCard(theModule, 1);
  WrLn;
END WriteHandler;

END DQHandlers.