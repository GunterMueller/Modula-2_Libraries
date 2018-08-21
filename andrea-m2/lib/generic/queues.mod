IMPLEMENTATION MODULE Queues;

(* Procedures for non-homogeneous multi-direction queue data structure *)
(* John Andrea, Oct.11/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SYSTEM IMPORT BYTE;
FROM MoreMath IMPORT MinCard;
FROM Storage  IMPORT ALLOCATE, DEALLOCATE;

CONST
   max_data   = 511;

TYPE
   DataPtr   = POINTER TO QueueData;
   QueueData = RECORD
                 data        :ARRAY [0..max_data] OF BYTE;
                 size        :CARDINAL;
                 top, bottom :DataPtr;
               END;

   Queue     = POINTER TO AQueue;
   AQueue    = RECORD
                 top, bottom :DataPtr;
                 n           :CARDINAL;
               END;

VAR
   q      :DataPtr;
   max, i :CARDINAL;

   (* ------------------------------------------- *)
   PROCEDURE InitQueue( VAR a_queue :Queue );
   (* Create a queue, this must be called before the queue is used *)
   BEGIN

     NEW( a_queue );
     a_queue^.n      := 0;
     a_queue^.top    := NIL;
     a_queue^.bottom := NIL;

   END InitQueue;

   (* ------------------------------------------- *)
   PROCEDURE Push( a_queue :Queue; which_end :Directions; data :ARRAY OF BYTE );
   (* Push an item onto the queue *)
   BEGIN

     NEW( q );

     max     := MinCard( HIGH(data), max_data );
     q^.size := max;

     FOR i := 0 TO max DO
        q^.data[i] := data[i];
     END;

     (* and setup the correct pointers *)

     a_queue^.n := a_queue^.n + 1;

     IF a_queue^.n = 1 THEN

       q^.top          := NIL;
       q^.bottom       := NIL;

       (* this is the only item on the queue, top and bottom are same place *)
       a_queue^.top    := q;
       a_queue^.bottom := q;

     ELSE

       IF which_end = Top THEN

         q^.top    := NIL;
         q^.bottom := a_queue^.top;

         (* the new item is the top of the old top *)
         a_queue^.top^.top := q;

         (* the new item is the new top *)
         a_queue^.top := q;

       ELSE

         q^.top    := a_queue^.bottom;
         q^.bottom := NIL;

         (* the new item is the bottom of the old bottom *)
         a_queue^.bottom^.bottom := q;

         (* the new item is the new bottom *)
         a_queue^.bottom := q;

       END;
     END;

   END Push;

   (* ------------------------------------------- *)
   PROCEDURE Pop( a_queue :Queue; which_end :Directions;
                  VAR data :ARRAY OF BYTE; VAR nothing_to_pop :BOOLEAN );
   (* Get an item from the queue *)
   BEGIN

     IF a_queue^.n = 0 THEN
       nothing_to_pop := TRUE;
     ELSE
       nothing_to_pop := FALSE;

       IF which_end = Top THEN
         q := a_queue^.top;
       ELSE
         q := a_queue^.bottom;
       END;

       (* move the data *)

       max := MinCard( HIGH(data), q^.size );
       FOR i := 0 TO max DO
          data[i] := q^.data[i];
       END;

       (* reset the pointers *)

       a_queue^.n := a_queue^.n - 1;

       IF a_queue^.n = 0 THEN
         (* nothing left in the queue, set both top and bottom to empty *)
         a_queue^.top    := NIL;
         a_queue^.bottom := NIL;

       ELSE

         IF which_end = Top THEN
           a_queue^.top := q^.bottom;
         ELSE
           a_queue^.bottom := q^.top;
         END;

       END;

       (* get rid of the item which was just pop'ed *)

       DISPOSE( q );

     END;

   END Pop;

   (* ------------------------------------------- *)
   PROCEDURE DelQueue( VAR a_queue :Queue );
   (* Complete delete a queue *)
   BEGIN

      WHILE a_queue^.top # NIL DO
         q := a_queue^.top^.bottom;
         DISPOSE( a_queue^.top );
         a_queue^.top := q;
      END;

      DISPOSE( a_queue );

   END DelQueue;

   (* ------------------------------------------- *)
   PROCEDURE QueueInfo( a_queue :Queue; which_end :Directions;
                        VAR n_items, size :CARDINAL );
   (* Return some info about a queue *)
   BEGIN

     n_items := a_queue^.n;
     IF n_items = 0 THEN
       size := 0;
     ELSE

       IF which_end = Top THEN
         size := a_queue^.top^.size;
       ELSE
         size := a_queue^.bottom^.size;
       END;

       size := size + 1; (* to account for the zero offset *)

     END;

   END QueueInfo;

BEGIN
END Queues.
