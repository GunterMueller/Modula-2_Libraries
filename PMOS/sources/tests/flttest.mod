MODULE FltTest;
    (* The sole purpose of this module is to let me check what the	*)
    (* TopSpeed floating point library is doing to corrupt the stack	*)
    (* segment.								*)
    (* Conclusion so far: locations ss:[0001] to ss:[0004] inclusive	*)
    (* are being used by Exp and Sqrt; so we're forced to make sure	*)
    (* that those locations are actually within the task stack space.	*)

FROM SYSTEM IMPORT NEWPROCESS, TRANSFER;
FROM MATHLIB IMPORT Log, Tan, Sqrt, Exp;

VAR x, y: LONGREAL;
    P0, P1: ADDRESS;
    WkSpace: ARRAY [0..2047] OF CHAR;

PROCEDURE proc1;
    VAR x, y: LONGREAL;
    BEGIN
	y := 1.0;
	x := Log(y);
	x := Tan(y);
	x := Sqrt(y);
	TRANSFER (P1,P0);
    END proc1;

BEGIN
    NEWPROCESS (proc1, ADR(WkSpace), SIZE(WkSpace), P1);
    y := 1.0;
    x := Exp (y);
    x := Log(-5.0);
    x := Tan(y);
    x := Sqrt(y);
    TRANSFER (P0,P1);
END FltTest.
