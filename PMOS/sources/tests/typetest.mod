MODULE TypeTest;

	(********************************************************)
	(*							*)
	(*							*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	22 February 1995		*)
	(*  Status:						*)
	(*							*)
	(*	This program produces no output, it's simply	*)
	(*	something I can look at with the debugger.	*)
	(*							*)
	(********************************************************)

FROM Types IMPORT
    (* type *)	FarPointer, FarWordPointer;

(************************************************************************)

PROCEDURE P;

    BEGIN

    END P;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

VAR fp: FarPointer;  addr: ADDRESS;
    fwp: FarWordPointer;  wp: POINTER TO WORD;

BEGIN
    (* fwp := wp; *)	(* ALWAYS ILLEGAL *)
    (* fp := addr; *)	(* LEGALITY DEPENDS ON MEMORY MODEL *)
    (* fp := wp; *)	(* LEGALITY DEPENDS ON MEMORY MODEL *)
    fwp := fp;
    addr := wp;
END TypeTest.
