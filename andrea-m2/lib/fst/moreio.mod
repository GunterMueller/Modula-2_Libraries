IMPLEMENTATION MODULE MoreIO;

(* John Andrea, Mar.27/92 *)
(* This code may NOT be sold, it may be freely used and distributed *)

FROM ASCII IMPORT EOL, EOF;
FROM InOut IMPORT Read, Done;

  (* ----------------------------------------------------- *)
  PROCEDURE ReadString( VAR string :ARRAY OF CHAR );

  CONST
     tab = 11C;

  VAR
     c :CHAR;
     i, max :CARDINAL;

     (*------------------------------------------*)
     PROCEDURE Eos( ch :CHAR ) :BOOLEAN;
     BEGIN
        RETURN ( ch <= ' ' ) OR ( ch = EOL );
     END Eos;

  BEGIN

    max := HIGH( string );

    (* skip leading spaces *)
    Read(c);
    WHILE ( c # EOF ) & ( ( c = ' ' ) OR ( c = tab ) OR ( c = EOL ) ) DO
      Read(c);
    END;

    IF Done THEN
        i := 0;
        WHILE Done & NOT Eos(c) & ( i <= max ) DO
           string[i] := c;
           Read(c);  i := i + 1;
        END;

        IF i > max THEN
          (* the end of the string has been reached *)
          (* but maybe the input hasn't, keep reading any input string *)
          WHILE Done & NOT Eos(c) DO
             Read(c);
          END;
        ELSE
         string[i] := 0C;
        END;
    END;
  END ReadString;

BEGIN
END MoreIO.
