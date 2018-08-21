IMPLEMENTATION MODULE MemoryStatus;

(* Return some information about memory use *)
(* J. Andrea, Sept.4/92 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM JPIDefinitions IMPORT JPI$_FREPTECNT, JPI$_PGFLQUOTA;
FROM FortranJPI IMPORT For$LibJPI;

VAR
   status :CARDINAL;
   string :ARRAY [0..1] OF CHAR;
   len    :CARDINAL;
   value  :CARDINAL;


   (* ---------------------------------------------- *)
   PROCEDURE Used() :CARDINAL;
   BEGIN
     For$LibJPI( JPI$_PGFLQUOTA, value, string, len, status );
     RETURN ( value * 512 ) - Available();
   END Used;

   (* ---------------------------------------------- *)
   PROCEDURE Available() :CARDINAL;
   BEGIN
     For$LibJPI( JPI$_FREPTECNT, value, string, len, status );
     RETURN value * 512;
   END Available;

BEGIN
END MemoryStatus.
