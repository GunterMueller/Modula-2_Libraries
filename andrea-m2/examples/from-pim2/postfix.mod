MODULE Postfix;

(* Example program from Programming In Modula-2, N. Wirth., pg. 56, *)
(*  - no WINDOWS in this example *)

(* this program translates a small language into postfix form
 *  the language is
 *
 *           expression = term { [ '+' | '-' ] term }
 *
 *           term = factor { [ '*' | '/' ] factor }
 *
 *           factor = letter | '(' expression ')'
 *
 *   try as input
 *      a+b
 *      a*b+c
 *      a+b*c
 *      a*(b/(c-d))
 *)

FROM InOut      IMPORT Read, Write, WriteLn, EOL, in;
FROM FileSystem IMPORT Eof;

TYPE
    range = [1..80];

VAR
   ch       :CHAR;
   done     :BOOLEAN;
   i, index :range;
   out_line :ARRAY range OF CHAR;   

   (* -------------------------------------- *)
   PROCEDURE expression;

   VAR
      addop :CHAR;

      (* ------------------------------------ *)
      PROCEDURE term;

      VAR 
         mulop :CHAR;

         (* --------------------------------- *)
         PROCEDURE factor;

         BEGIN (* factor *)

             IF ch = '(' THEN
    
                Read(ch);
                expression;
                WHILE ch # ')' DO
                   Read(ch)
                END (* while *)

             ELSE    

                WHILE (ch < 'a') OR (ch > 'z') DO
                   Read(ch)
                END; (* while *)
                out_line[index] := ch;
                index := index + 1;

             END; (* if *)

             Read(ch);

         END factor;

      BEGIN (* term *)

         factor;
         WHILE (ch = '*') OR (ch = '/') DO
            mulop := ch;
            Read(ch);
            factor;
            out_line[index] := mulop;
            index := index + 1
         END; (* while *)

      END term;

   BEGIN (* expression *)

     term;
     WHILE (ch = '+') OR (ch = '-') DO
        addop := ch;
        Read(ch);
        term;
        out_line[index] := addop;
        index := index + 1
     END; (* while *)

   END expression;
   
BEGIN (* Postfix *)

  index := 1;

  Write('>');
  Read(ch);
  WHILE ch > ' ' DO

     expression;

     FOR i := 1 TO index-1 DO
       Write(out_line[i]);
     END; (* for *)
     WriteLn;
     index := 1;

     Write('>');
     Read(ch)

  END; (* while *)

END Postfix.
