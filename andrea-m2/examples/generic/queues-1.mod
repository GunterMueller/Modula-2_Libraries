MODULE TestQueues;

(* Test the procedures in the module Queues, and show how they work. *)
(* J. Andrea, Oct.11/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut  IMPORT WriteCard, WriteString, WriteLn;
FROM Queues IMPORT Queue, Directions, InitQueue, DelQueue, Pop, Push, QueueInfo;

VAR
   a, b  :ARRAY [0..9] OF CHAR;
   queue :Queue;
   error :BOOLEAN;
   i, j  :CARDINAL;

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

   WriteString( 'create a queue' ); WriteLn;
   InitQueue( queue );
   QueueInfo( queue, Top, i, j );
   WriteString( 'queue has ' ); WriteCard( i, 0 ); WriteString( ' items' );
   WriteString( ' and top has ' ); WriteCard( j, 0 ); WriteString( ' bytes' );
   WriteLn;

   WriteString( 'now just delete that queue' ); WriteLn;

   DelQueue( queue );

   WriteLn;  WriteLn;  WriteLn;
   WriteString( 'create a queue' ); WriteLn;
   InitQueue( queue );

   Show( a );

   WriteString( 'push ' ); WriteLn;
   Push( queue, Top, a );

   WriteString( 'push ' ); WriteLn;
   Push( queue, Top, a );

   WriteString( 'push ' ); WriteLn;
   Push( queue, Top, a );

   QueueInfo( queue, Top, i, j );
   WriteString( 'queue has ' ); WriteCard( i, 0 ); WriteString( ' items' );
   WriteString( ' and top has ' ); WriteCard( j, 0 ); WriteString( ' bytes' );
   WriteLn;

   WriteString( 'delete that queue' ); WriteLn;
   DelQueue( queue );


   WriteLn;  WriteLn;   WriteLn;
   WriteString( 'create a queue' ); WriteLn;
   InitQueue( queue );

   Show( a );

   WriteString( 'push ' ); WriteLn;
   Push( queue, Top, a );

   WriteString( 'pop ' ); WriteLn;
   Pop( queue, Top, b, error );

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
     Pop( queue, Top, b, error );
     IF error THEN
       WriteString( 'ok, empty as expected' );
     ELSE
       WriteString( 'the queue should be empty but isnt' );
     END;
     WriteLn;

     DelQueue( queue );

  END;

END TestQueues.
