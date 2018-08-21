IMPLEMENTATION MODULE MoreIO;

(* misc. extra i/o functions *)
(* J. Andrea, Feb.18/92 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT Read, ReadLn, EOL, WriteString;
FROM StringOperations IMPORT Length;

VAR
   i, n :CARDINAL;


   (* ---------------------------------------------------- *)
   PROCEDURE WriteLeftString( string :ARRAY OF CHAR; width :CARDINAL );
   BEGIN
      WriteFixString( string, width, TRUE );
   END WriteLeftString;


   (* ---------------------------------------------------- *)
   PROCEDURE WriteRightString( string :ARRAY OF CHAR; width :CARDINAL );
   BEGIN
      WriteFixString( string, width, FALSE );
   END WriteRightString;


   (* ---------------------------------------------------- *)
   PROCEDURE ReadLine( VAR line :ARRAY OF CHAR );

   VAR
      c :CHAR;

   BEGIN (* ReadLine *)

      n := HIGH( line ) + 1;

      i := 0;
      Read( c );

      WHILE ( c # EOL ) & ( i < n ) DO
         line[i] := c;  i := i + 1;
         Read( c );
      END;
      ReadLn;

      IF i < n THEN line[i] := 0C; END;

   END ReadLine;


   (* ---------------------------------------------------- *)
   PROCEDURE WriteFixString( string :ARRAY OF CHAR; width :CARDINAL;
                             left_justify :BOOLEAN );
   BEGIN (* WriteFixString *)
      IF width > 0 THEN

        n := Length( string );

        IF n <= width THEN

          IF left_justify THEN WriteString( string ); END;

          FOR i := n + 1 TO width DO
             WriteString( ' ' );
          END;

          IF NOT left_justify THEN WriteString( string ); END;

        ELSE

          FOR i := 0 TO width - 1 DO
             WriteString( string[i] );
          END;

        END;

      END;
   END WriteFixString;

BEGIN
END MoreIO.
