MODULE TTransfer;

	(****************************************************************)
	(*								*)
	(*		Test of TopSpeed TRANSFER routine		*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	7 October 1992				*)
	(*  Status:		Just starting				*)
	(*								*)
	(****************************************************************)

FROM SYSTEM IMPORT
    (* type *)	PROCESS,
    (* proc *)	NEWPROCESS, TRANSFER;

FROM IO IMPORT
    (* proc *)	WrLn, WrStr;

(************************************************************************)
(*			    GLOBAL VARIABLES				*)
(************************************************************************)

VAR Stack2: ARRAY [0..1023] OF SHORTCARD;
    P1, P2: PROCESS;

(************************************************************************)
(*			    SUBSIDIARY TASK				*)
(************************************************************************)

PROCEDURE Task2;

    VAR x: REAL;

    BEGIN
	WrLn;  WrStr ("Now in Task2");
	x := 1.0;
	WrLn;  WrStr ("About to transfer to main task");
	TRANSFER (P2, P1);
    END Task2;

(************************************************************************)
(*			MAIN PROGRAM AND MAIN TASK			*)
(************************************************************************)

VAR y: REAL;

BEGIN
    y := 1.0;
    NEWPROCESS (Task2, FarADR(Stack2), SIZE(Stack2), P2);
    WrLn;  WrStr ("About to transfer to Task2");
    TRANSFER (P1, P2);
    WrLn;  WrStr ("Now back in main task");
END TTransfer.
