IMPLEMENTATION MODULE Stacks;

(* Procedures for non-homogeneous stack data structure *)
(* V1.2, J.Andrea, Jun.22/93 -more compiler generic *)
(* V1.1, John Andrea, Oct.11/91 - modified to use the generic Queue module *)
(* V1.0, John Andrea, Oct.3/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM SYSTEM IMPORT BYTE;
IMPORT Queues;

                                              (* Some compilers don't allow  *)
                                              (* type  Stack  = Queues.Queue *)
                                              (* So build in a second layer  *)
TYPE
   Stack     = POINTER TO StackData;
   StackData = RECORD
                 queue :Queues.Queue;
               END;

   (* ------------------------------------------- *)
   PROCEDURE InitStack( VAR a :Stack );
   (* Create a stack, this must be called before the stack is used *)
   BEGIN

     NEW( a );
     Queues.InitQueue( a^.queue );

   END InitStack;

   (* ------------------------------------------- *)
   PROCEDURE Push( a :Stack; data :ARRAY OF BYTE );
   (* Push an item onto the stack *)
   BEGIN

     Queues.Push( a^.queue, Queues.Top, data );

   END Push;

   (* ------------------------------------------- *)
   PROCEDURE Pop( a :Stack; VAR data :ARRAY OF BYTE;
                  VAR nothing_to_pop :BOOLEAN );
   (* Get an item from the stack *)
   BEGIN

     Queues.Pop( a^.queue, Queues.Top, data, nothing_to_pop );

   END Pop;

   (* ------------------------------------------- *)
   PROCEDURE DelStack( VAR a :Stack );
   (* Complete delete a stack *)
   BEGIN

      Queues.DelQueue( a^.queue );
      DISPOSE( a );

   END DelStack;

   (* ------------------------------------------- *)
   PROCEDURE StackInfo( a :Stack; VAR n_items, size :CARDINAL );
   (* Return some info about a stack *)
   BEGIN

     Queues.QueueInfo( a^.queue, Queues.Top, n_items, size );

   END StackInfo;

BEGIN
END Stacks.
