IMPLEMENTATION MODULE DemoModule;

(* J. Andrea, 1983 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn;

VAR
   local : CARDINAL;  (* notice how this totally local var stays alive
                         while the calling module is active            *)

   (* ---------------------------------------------------- *)
   PROCEDURE proc_d;

   BEGIN (* proc_d *)

      WriteLn;
      WriteString(' -----begin DemoModule.proc_d');
      WriteLn;
      WriteString(' -----var_c = ');
      WriteCard(var_c,5);
      WriteLn;
      WriteString(' -----end DemoModule.proc_d');
      WriteLn;

   END proc_d;

   (* ---------------------------------------------------- *)
   PROCEDURE proc_e;

   BEGIN (* proc_e *)

      WriteLn;
      WriteString(' -----begin DemoModule.proc_e');
      WriteLn;
      WriteString(' -----local=');
      WriteCard(local,3);
      WriteLn;
      WriteString(' -----end DemoModule.proc_e');
      WriteLn;

   END proc_e;

BEGIN (* DemoModule *)

(* notice now all this action takes place BEFORE the calling module
   is activated                                                     *)

WriteLn;
WriteString(' --begin DemoModule');
WriteLn;

local := 700;
WriteString(' --local=');
WriteCard(local,3);
WriteLn;
WriteString(' --now change local');
local := local + 1;
WriteLn;

var_c := 21;
WriteString(' --var_c = ');
WriteCard(var_c,5);
WriteLn;

WriteLn;
WriteString(' --end DemoModule');
WriteLn;

END DemoModule.
