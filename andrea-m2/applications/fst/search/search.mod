MODULE Search;

(*
 Search the file given by argument 1
 for any line which contains all of the strings defined by arguments 2,3,...
*)
(* John Andrea, June.27/92 *)
(* This code may be freely distributed, it may not be sold *)

FROM Break IMPORT EnableBreak;
FROM System IMPORT GetArg;
FROM InOut IMPORT Write, WriteString, WriteLn, RedirectInput, Read, WriteCard, Done;
FROM ASCII IMPORT EOL, EOF;

CONST
  max_size = 30;
  max_args = 11;
  max_line = 133;

VAR
  i       :CARDINAL;

  n_args  :CARDINAL;
  arg     :ARRAY [1..max_args] OF ARRAY [1..max_size] OF CHAR;
  size    :ARRAY [1..max_args] OF CARDINAL;

  line    :ARRAY [1..max_line] OF CHAR;
  len     :CARDINAL;

  eof     :BOOLEAN;

  (* ------------------------------------------ *)
  PROCEDURE ReadLine;
  VAR
    c :CHAR;
  BEGIN

    Read( c );

    IF c = EOF THEN
      eof := TRUE;
    ELSE

      len := 1;

      WHILE ( len < max_line ) & ( c # EOL ) & ( c # EOF ) DO
        line[len] := c;
        Read( c );
        len := len + 1;
      END;

      IF len <= max_line THEN
       line[len] := 0C;
      ELSE
       (* finish reading this line, beacuse its too big to fit into the buffer *)
       Read( c );
       WHILE c # EOL DO
         Read( c );
       END;
      END;

      len := len - 1;

    END;

  END ReadLine;

    (* -------------------------------------------------------- *)
    PROCEDURE Contains( pattern :ARRAY OF CHAR; len_p :CARDINAL ) :BOOLEAN;
    VAR
       s, p :CARDINAL;
       done, found :BOOLEAN;
    BEGIN

       IF ( len_p = 0 ) OR ( len_p > len ) THEN
         RETURN FALSE;
       ELSE

         found := FALSE;
         done  := FALSE;

         s := 1;

         WHILE ( NOT done ) & ( NOT found ) DO

           (* find the next occurance of the first char of the pattern *)
           WHILE ( s <= len ) & ( CAP(line[s]) # CAP(pattern[0]) ) DO
              s := s + 1;
           END;

           IF s > len THEN
             (* didn't find another first character match *)
             done := TRUE;
           ELSE

             (* try to determine if the pattern fits here *)

             IF len - s + 1 < len_p THEN
               (* the pattern won't fit in the remainder if the source *)
               done := TRUE;
             ELSE

               (* ok, try to make a match *)

               p := 0;
               WHILE ( p < len_p ) & ( CAP(line[s+p]) = CAP(pattern[p]) ) DO
                  p := p + 1;
               END;

               found := p = len_p;

             END;

           END;

           s := s + 1;

         END; (* while *)

         RETURN found;

       END;

    END Contains;

BEGIN EnableBreak;

  n_args := 0;
  REPEAT
    n_args := n_args + 1;
    GetArg( arg[n_args], size[n_args] );
  UNTIL ( n_args = max_args ) OR ( size[n_args] = 0 );

  n_args := n_args - 1;
  IF n_args > 1 THEN

    eof := FALSE;

    RedirectInput( arg[1] );
    IF Done THEN

      ReadLine;
      WHILE NOT eof DO

        (* only try to search though the line if is isn't all blank *)
        IF len > 0 THEN

          (* stop searching if one of the arg strings isn't in this line *)
          i := 2;
          WHILE ( i <= n_args ) & Contains( arg[i], size[i] ) DO
            i := i + 1;
          END;

          IF i > n_args THEN
            WriteString( line ); WriteLn;
          END;

        END;
        ReadLine;
      END;

    ELSE
      WriteString( 'cant open input file ' ); WriteString( arg[1] ); WriteLn;
    END;

  END;
END Search.