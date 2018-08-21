MODULE TillEof;

(*
 This program reads a file and counts the number of characters in
 each line. It is intended to be an example of using read statements
 to read through a file.
*)
(* John Andrea, Oct.15/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT OpenInput, CloseInput, Read, EOL, in, Done,
                  WriteString, WriteCard, WriteLn;
FROM FileSystem IMPORT Eof;

VAR
  line, count :CARDINAL;
  c           :CHAR;

BEGIN

  OpenInput( '.TXT' );
  IF Done THEN

    line := 0;

    Read( c );

    WHILE NOT Eof( in ) DO
       line := line + 1;

       count := 0;

       WHILE NOT Eof( in ) & ( c # EOL ) DO
         count := count + 1;
         Read( c );
       END;

       WriteString( 'line #' ); WriteCard( line, 0 ); WriteString( ' has ' );
       WriteCard( count, 0 ); WriteString( ' characters' ); WriteLn;

       Read( c );
    END;

    CloseInput;
  END;

END TillEof.
