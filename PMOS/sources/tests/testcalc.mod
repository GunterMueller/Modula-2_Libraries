MODULE TestCalculator;

	(************************************************)
	(*						*)
	(*	Test of my calculator module.		*)
	(*						*)
	(*  Programmer:		P. Moylan		*)
	(*  Last edited:	6 September 1993	*)
	(*  Status:		OK			*)
	(*						*)
	(************************************************)
(*
FROM Trace IMPORT
    (* proc *)	NYI, InTrace, OutTrace, TraceOn;
*)
FROM Calculator IMPORT
    (* proc *)	RunCalculator;

(************************************************************************)

BEGIN
(*    TraceOn (10, 24, 0, 39); *)
    RunCalculator;
END TestCalculator.
