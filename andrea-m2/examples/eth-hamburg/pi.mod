MODULE TestPi;

(* Test the CalcPi routine *)
(* J. Andrea, 1992 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM RealInOut IMPORT FWriteReal;
FROM InOut     IMPORT WriteLn;
FROM MoreMath  IMPORT CalcPi;

VAR
  x :REAL;

BEGIN

x := CalcPi();

FWriteReal( x, 9, 7 ); WriteLn;

END TestPi.
