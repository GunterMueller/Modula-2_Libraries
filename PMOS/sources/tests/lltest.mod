MODULE LLTest;

	(********************************************************)
	(*							*)
	(*	Test of some functions in module LowLevel	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	11 March 1995			*)
	(*  Status:		OK				*)
	(*	All results appear to be correct for the	*)
	(*	Small memory model (which, for now, is the	*)
	(*	only one that I need to check).			*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* type *)	ADDRESS,
    (* proc *)	ADR;

FROM Types IMPORT
    (* type *)	FarPointer;

FROM LowLevel IMPORT
    (* proc *)	Far, SEGMENT, FarSEGMENT, OFFSET, MakePointer, Physical,
		Virtual, AddOffset, SubtractOffset, FarAddOffset,
		FarSubtractOffset, Copy, FarCopy;

VAR G: CARDINAL;

(************************************************************************)

PROCEDURE BasicTest;

    VAR L, seg, off: CARDINAL;  phys: LONGCARD;
	p: ADDRESS;  fp: FarPointer;

    BEGIN
	p := ADR(L);
	fp := Far(p);
	seg := SEGMENT(p);
	off := OFFSET (p);
	fp := MakePointer (seg, off);
	seg := FarSEGMENT(fp);
	phys := Physical(p);
	fp := Virtual(phys);
	p := AddOffset (p, 1);
	p := SubtractOffset (p, 1);
	fp := FarAddOffset (fp, 1);
	fp := FarSubtractOffset (fp, 1);
    END BasicTest;

(************************************************************************)

PROCEDURE CopyTest;

    VAR src, dst: ARRAY [0..15] OF CHAR;
	j: CARDINAL;

    BEGIN
	FOR j := 0 TO 15 DO src[j] := CHR(j+ORD('A')); END (*FOR*);
	Copy (ADR(src), ADR(dst), 16);
	FOR j := 0 TO 15 DO src[j] := CHR(j+ORD('a')); END (*FOR*);
	FarCopy (Far(ADR(src)), Far(ADR(dst)), 16);
    END CopyTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    BasicTest;
    CopyTest;
END LLTest.
