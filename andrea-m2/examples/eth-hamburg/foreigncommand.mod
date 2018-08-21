MODULE TestForeignCommand;

(* This program shows how to use the module ForeignCommand *)
(* V1.0, J. Andrea, May.4/92 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM ForeignCommand IMPORT ReadCommand, NOptions, GetOption;
FROM InOut IMPORT WriteCard, WriteString, WriteLn;

VAR
   string :ARRAY [1..80] OF CHAR;
   len, i :CARDINAL;
   
BEGIN

   (* setup the command first *)

   ReadCommand( 'this is a test prompt ? ' );

   WriteString( 'detected ' ); WriteCard( NOptions(), 0 ); 
   WriteString( ' options' ); WriteLn;

   (* then show the first ten options *)

   FOR i := 1 TO 10 DO
      WriteString( 'option #' ); WriteCard( i, 2 );

      GetOption( i, string, len );

      IF len = 0 THEN
        WriteString( ' --none-- ' );
      ELSE
        WriteString ( ' is >' );
        WriteString( string );
        WriteString( '<' );
      END;
      WriteLn;
   END;

END TestForeignCommand.
