MODULE TestQueues;

(* Test the procedures in the module Queues, and show how they work. *)
(* J. Andrea, Oct.11/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut  IMPORT WriteString, WriteCard, WriteLn;
FROM Queues IMPORT Queue, Directions, InitQueue, DelQueue, Pop, Push,
                   QueueInfo;

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


   WriteString( 'create a queue, and push TOP one char at a time' ); WriteLn;
   InitQueue( queue );

   FOR i := 0 TO 9 DO
     WriteString( 'push ' ); WriteString(a[i]); WriteString( ',' );
     Push( queue, Top, a[i] );
   END;
   WriteLn;

   QueueInfo( queue, Top, i, j );
   WriteString( 'queue has ' ); WriteCard( i, 0 ); WriteString( ' items' );
   WriteString( ' and top is ' ); WriteCard( j, 0 ); WriteString( ' bytes' );
   WriteLn;

   WriteString( 'now pop one char at a time, from TOP' ); WriteLn;

   FOR i := 0 TO 9 DO
     WriteString( 'pop ' );
     Pop( queue, Top, b[i], error ); WriteString(b[i]); WriteString(',');
   END;
   WriteLn;

   WriteString( 'they should be in reverse order'); WriteLn;
   Show(a); Show(b);

   WriteString( 'delete that queue' ); WriteLn;
   DelQueue( queue );



   WriteLn;  WriteLn;  WriteLn;
   WriteString( 'create a queue, and push BOTTOM one char at a time' ); WriteLn;
   InitQueue( queue );

   FOR i := 0 TO 9 DO
     WriteString( 'push ' ); WriteString(a[i]); WriteString( ',' );
     Push( queue, Bottom, a[i] );
   END;
   WriteLn;

   QueueInfo( queue, Bottom, i, j );
   WriteString( 'queue has ' ); WriteCard( i, 0 ); WriteString( ' items' );
   WriteString( ' and top is ' ); WriteCard( j, 0 ); WriteString( ' bytes' );
   WriteLn;

   WriteString( 'now pop one char at a time, from BOTTOM' ); WriteLn;

   FOR i := 0 TO 9 DO
     WriteString( 'pop ' );
     Pop( queue, Bottom, b[i], error ); WriteString(b[i]); WriteString(',');
   END;
   WriteLn;

   WriteString( 'they should be in reverse order'); WriteLn;
   Show(a); Show(b);

   WriteString( 'delete that queue' ); WriteLn;
   DelQueue( queue );


   WriteLn;  WriteLn;  WriteLn;
   InitQueue( queue );
   WriteString( 'create a queue, and push TOP one char at a time' ); WriteLn;

   FOR i := 0 TO 9 DO
     WriteString( 'push ' ); WriteString(a[i]); WriteString( ',' );
     Push( queue, Top, a[i] );
   END;
   WriteLn;

   QueueInfo( queue, Top, i, j );
   WriteString( 'queue has ' ); WriteCard( i, 0 ); WriteString( ' items' );
   WriteString( ' and top has ' ); WriteCard( j, 0 ); WriteString( ' bytes' );
   WriteLn;

   WriteString( 'now pop one char at a time, BOTTOM end of queue' ); WriteLn;

   FOR i := 0 TO 9 DO
     WriteString( 'pop ' );
     Pop( queue, Bottom, b[i], error ); WriteString(b[i]); WriteString( ',' );
   END;
   WriteLn;

   WriteString( 'they should be in the same order' ); WriteLn;
   Show(a); Show(b);

   WriteString( 'delete that queue' ); WriteLn;
   DelQueue( queue );


   WriteLn;  WriteLn;  WriteLn;
   InitQueue( queue );
   WriteString( 'create a queue, and push BOTTOM one char at a time' ); WriteLn;

   FOR i := 0 TO 9 DO
     WriteString( 'push ' ); WriteString(a[i]); WriteString( ',' );
     Push( queue, Bottom, a[i] );
   END;
   WriteLn;

   QueueInfo( queue, Top, i, j );
   WriteString( 'queue has ' ); WriteCard( i, 0 ); WriteString( ' items' );
   WriteString( ' and top has ' ); WriteCard( j, 0 ); WriteString( ' bytes' );
   WriteLn;

   WriteString( 'now pop one char at a time, TOP end of queue' ); WriteLn;

   FOR i := 0 TO 9 DO
     WriteString( 'pop ' );
     Pop( queue, Top, b[i], error ); WriteString(b[i]); WriteString( ',' );
   END;
   WriteLn;

   WriteString( 'they should be in the same order' ); WriteLn;
   Show(a); Show(b);

   WriteString( 'delete that queue' ); WriteLn;
   DelQueue( queue );

END TestQueues.
