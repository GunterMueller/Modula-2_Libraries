MODULE PowersOf2;

(* Example program from Programming In Modula-2, Niklaus Wirth ,pg.38 *)

FROM InOut IMPORT Write , WriteLn , WriteString , WriteCard;

CONST
     m = 11;
     n = 32; (* m ~= n * log(2) *)
VAR
     i, j, k, exp, c, r, t :CARDINAL;
     d, f                  :ARRAY [0..n] OF CARDINAL;

BEGIN (* PowersOf2 *)

d[0] := 1;
k := 1;

FOR exp := 1 TO n DO     (* compute d =  2^exp by   d := 2*d  *)

    c := 0;  (* carry *)

    FOR i := 0 TO k-1 DO
        t := 2 * d[i] + c;
        IF t >= 10 THEN
           d[i] := t - 10;
           c := 1
        ELSE
           d[i] := t;
           c := 0
        END (* if *)
     END; (* for *)
     
     IF c > 0 THEN
        d[k] := 1;
        k := k + 1;
     END; (* if *)

     (* output d[k-1]..d[0] *)
     i := m;
     REPEAT
       i := i - 1;
       Write(' ')
     UNTIL i = k;
     REPEAT
       i := i - 1;
       Write(CHR(d[i] + ORD('0')))
     UNTIL i = 0;

     WriteCard(exp,4);
     WriteString('  ');

     (* compute and output   f := 2^(-exp) ,by f := f DIV 2 *)
     WriteString('0.');
     r := 0; (* remainder *)
     FOR j := 1 TO exp-1 DO
         r := 10 * r + f[j];
         f[j] := r DIV 2;
         r := r MOD 2;
         Write(CHR(f[j] + ORD('0')));
     END; (* for *)

     f[exp] := 5;
     Write('5');
     WriteLn;

   END; (* for *)

END PowersOf2.
