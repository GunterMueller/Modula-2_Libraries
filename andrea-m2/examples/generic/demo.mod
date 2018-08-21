MODULE Demo;    

(* demonstrate the scope of:
      variables, procedures, and action across modules *)
(* J. Andrea, 1983 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn;
FROM DemoModule IMPORT const_a, type_b, var_c, proc_d, proc_e;

CONST
    a = 10;

TYPE
    b = ARRAY ['a'..'z'] OF CARDINAL;

VAR
    c : CARDINAL;
   bb : type_b;

    (* ------------------------------------------------ *)
    PROCEDURE d;

    BEGIN (* d *)

      WriteLn;
      WriteString('-begin Demo.d ');
      WriteLn;
      WriteString('-var_c =');
      WriteCard(var_c,5);
      WriteLn;
      WriteString('-end Demo.d ');
      WriteLn;
 
    END d;

BEGIN (* Demo *)

WriteLn;
WriteString('begin Demo');
WriteLn;

d;

WriteString('inside Demo - change var_c ');
WriteLn;
var_c := 42;

d;
proc_d;

proc_e;

WriteLn;
WriteString('end Demo');
WriteLn;

END Demo.
