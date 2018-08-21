MODULE Permute;

(* Example program from Programming In Modula-2, N. Wirth, pg. 54 *)

FROM InOut IMPORT Read, Write, WriteLn;

VAR
  n       :CARDINAL;
  ch      :CHAR;
  a       :ARRAY [1..20] OF CHAR;

  (* ----------------------------------------------------- *)
  PROCEDURE output;

  VAR
    i  :CARDINAL;

  BEGIN (* output *)

     FOR i := 1 TO n DO
        Write( a[i] );
     END; (* for *)

     WriteLn;

  END output;

  (* ----------------------------------------------------- *)
  PROCEDURE permute( k :CARDINAL );

  VAR
     i    :CARDINAL;
     t    :CHAR;

  BEGIN (* permute *)

     IF k = 1 THEN
       output
     ELSE
       permute( k - 1 );
       FOR i := 1 TO k - 1 DO
          t := a[i];
          a[i] := a[k];
          a[k] := t;
          permute( k - 1 );
          t := a[i];
          a[i] := a[k];
          a[k] := t;
       END; (* for *)
     END; (* if *)

  END permute;

BEGIN (* Permute *)

Write( '>' );
n := 0;
Read( ch );

WHILE ch > ' ' DO
  n := n+ 1;
  a[n] := ch;
  Write( ch );
  Read( ch );
END; (* while *)

WriteLn;
permute( n );

END Permute.
