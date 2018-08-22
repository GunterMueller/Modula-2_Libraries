MODULE BoolTest;

(* This test shows that TRUE=1, FALSE=0. *)
(* (By examination of the compiled code). *)
(* In tests, TRUE=nonzero *)

VAR x: BOOLEAN;

BEGIN
    x := TRUE;
    x := FALSE;
    IF x THEN
	x := NOT x;
    END (*IF*)
END BoolTest.
