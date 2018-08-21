MODULE TestStacks;

(* Test the procedures in the module Stacks, and show how they work. *)
(* J. Andrea, Oct.3/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut  IMPORT WriteString, WriteLn;
FROM Stacks IMPORT InitStack, DelStack, Pop, Push, Stack;

VAR
   a, b  :ARRAY [0..9] OF CHAR;
   stack :Stack;
   error :BOOLEAN;
   i     :CARDINAL;

   (* -------------------------------------------------- *)
   PROCEDURE Show( string :ARRAY OF CHAR );
   BEGIN
      WriteString( 'string is >' ); WriteString( string ); WriteString( '<' );
      WriteLn;
   END Show;

BEGIN

       (*0123456789*)
   a := 'abcdefghji';
   b := '          ';

   InitStack( stack );

   Show( a );

   WriteString( 'push ' ); WriteLn;
   Push( stack, a );

   WriteString( 'pop ' ); WriteLn;
   Pop( stack, b, error );

   IF error THEN
     WriteString( 'somethings wrong, shouldnt be empty' ); WriteLn;
   ELSE

     Show( b );

     error := FALSE;
     FOR i := 0 TO 9 DO
        IF a[i] # b[i] THEN
          error := TRUE;
        END;
     END;
     IF error THEN
       WriteString( 'compared strings are different !' );
     ELSE
       WriteString( 'strings ok' );
     END;
     WriteLn;

     WriteLn;
     Pop( stack, b, error );
     IF error THEN
       WriteString( 'ok, empty as expected' );
     ELSE
       WriteString( 'the stack should be empty but isnt' );
     END;
     WriteLn;

     DelStack( stack );

  END;

END TestStacks.
