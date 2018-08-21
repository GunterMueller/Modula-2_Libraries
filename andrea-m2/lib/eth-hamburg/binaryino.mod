IMPLEMENTATION MODULE BinaryInOut;

FROM SYSTEM IMPORT BYTE;
FROM FileSystem IMPORT File, ReadRecord, WriteRecord;

(* V1.0, Jaa, Sept.4/92 *)

VAR
  how_many_bytes :CARDINAL;

   (* --------------------------------------- *)
   PROCEDURE ReadBlock( in :File; VAR rec :ARRAY OF BYTE );
   BEGIN
      how_many_bytes := HIGH( rec ) + 1;
      ReadRecord( in, rec, how_many_bytes );
   END ReadBlock;

   (* --------------------------------------- *)
   PROCEDURE WriteBlock( out :File; rec :ARRAY OF BYTE );
   BEGIN
      how_many_bytes := HIGH( rec ) + 1;
      WriteRecord( out, rec, how_many_bytes );
   END WriteBlock;

BEGIN
END BinaryInOut.
