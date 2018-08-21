MODULE NumberPuzzle;

(*
 Find a 9 digit number containing the digits 1 to 9 once and only once
 such that the first 1 digit is a multiple of 1, the first 2 digits form a
 number that is a multiple of 2, the first 3 digits form a number that is
 a multiple of 3, etc...
*)
(* John Andrea, Dec.11/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn;

VAR
  digit_taken            :ARRAY [1..9] OF BOOLEAN;
  attempts, solutions, i :CARDINAL;

  (* -------------------------------------------------- *)
  PROCEDURE TrySolve( value, level :CARDINAL );
  VAR i :CARDINAL;
  BEGIN

    attempts := attempts + 1;

    (* does the value so far satisfy the conditions *)

    IF value MOD level = 0 THEN
      (* ok, it works so far *)

      IF level = 9 THEN
        (* all digits taken, this must be a solution *)

        solutions := solutions + 1;
        WriteCard( value, 0 ); WriteLn;

      ELSE
        (* try all the  available digits *)

        FOR i := 1 TO 9 DO
           IF NOT digit_taken[i] THEN
             digit_taken[i] := TRUE;

             TrySolve( value * 10 + i, level + 1 );

             digit_taken[i] := FALSE;
           END;
        END;
      END;
    END;

  END TrySolve;

BEGIN

   attempts := 0;  solutions := 0;

   FOR i := 1 TO 9 DO  digit_taken[i] := FALSE;  END;

   WriteString( 'Starting to solve the puzzle...' ); WriteLn;

   WriteLn;
   FOR i := 1 TO 9 DO
      digit_taken[i] := TRUE;

      TrySolve( i, 1 );

      digit_taken[i] := FALSE;
   END;
   WriteLn;

   IF solutions = 0 THEN
     WriteString( 'impossible' );
   ELSE
     WriteCard( solutions, 0 ); WriteString( ' solutions' );
   END;
   WriteLn;
   
   WriteString( 'after ' ); WriteCard( attempts, 0 ); WriteString( ' attempts' );  WriteLn;

END NumberPuzzle.
