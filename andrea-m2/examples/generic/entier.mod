MODULE TestEntier;

(* test the entier function *)
(* revitalized, J. Andrea, Aug.12/91 *)
(* J. Andrea, 1984 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut     IMPORT WriteString, WriteInt, WriteReal, WriteLn;
FROM MoreMath  IMPORT entier;

VAR
   x :REAL;
   i :INTEGER;

BEGIN

  WriteLn;

  x := -2.5;
  FOR i := 1 TO 9 DO

     WriteString( ' for x = '); WriteReal( x, 0 );

     WriteString( '  entier(x)=' ); WriteInt( entier(x), 12 );
     WriteLn;

     x := x + 0.5;
  END; (* for *)

  WriteLn;

END TestEntier.
