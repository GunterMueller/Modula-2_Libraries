MODULE TestLookup;

(* test the procedure Lookup, and show how its used *)
(* J. Andrea, 1984 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut      IMPORT WriteLn, WriteString;
FROM FileSystem IMPORT File, Lookup, Done;

VAR
   x : File;

BEGIN (* TestLookup *)

WriteString('about to open the file "X.TMP"'); WriteLn;

Lookup( x, 'X.TMP', TRUE );
IF Done() THEN
  WriteString(' worked ok '); WriteLn;
ELSE
  WriteString(' didnt work '); WriteLn;
END; (* if *)

END TestLookup.
