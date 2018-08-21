IMPLEMENTATION MODULE MoreStrings;

(* misc. extra string functions *)
(* see definition module for algorithm notes *)
(* V2.0, J. Andrea, Jun.17/93 -do only generic things here, move some VMS
                               specific functions to a new ExtendStrings *)
(* V1.0, J. Andrea, 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM StringOperations IMPORT Length;

    (* -------------------------------------------------------- *)
    PROCEDURE Contains( source, pattern :ARRAY OF CHAR ) :BOOLEAN;
    VAR
       len_s, len_p, s, p :CARDINAL;
       done, found :BOOLEAN;
    BEGIN

       len_s := Length( source );

       IF len_s = 0 THEN
         RETURN FALSE;
       ELSE

         len_p := Length( pattern );

         IF ( len_p = 0 ) OR ( len_p > len_s ) THEN
           RETURN FALSE;
         ELSE

           found := FALSE;
           done  := FALSE;

           s := 0;

           WHILE ( NOT done ) & ( NOT found ) DO

             (* find the next occurance of the first char of the pattern *)
             WHILE ( s < len_s ) & ( CAP(source[s]) # CAP(pattern[0]) ) DO
                s := s + 1;
             END;

             IF s = len_s THEN
               (* didn't find another first character match *)
               done := TRUE;
             ELSE

               (* try to determine if the pattern fits here *)

               IF len_s - s + 1 < len_p THEN
                 (* the pattern won't fit in the remainder if the source *)
                 done := TRUE;
               ELSE

                 (* ok, try to make a match *)

                 p := 0;
                 WHILE ( p < len_p ) & ( CAP(source[s+p]) = CAP(pattern[p]) ) DO
                    p := p + 1;
                 END;

                 found := p = len_p;

               END;

             END;

             s := s + 1;

           END; (* while *)

           RETURN found;

         END;
       END;

    END Contains;


    (* -------------------------------------------- *)
    PROCEDURE Locate( source, pattern :ARRAY OF CHAR;
                      start :CARDINAL ) :CARDINAL;
    VAR
      len_s, len_p, s, p :CARDINAL;
     
    BEGIN

       (* start is not zero offset, its the start'th character *)
       IF start = 0 THEN start := 1; END;

       len_s := Length( source );

       IF ( len_s = 0 ) OR ( start > len_s ) THEN
         RETURN 0;
       ELSE

         len_p := Length( pattern );

         IF ( len_p = 0 ) OR ( len_p > len_s ) THEN
           RETURN 0;
         ELSE

           (* convert start back to zero offset for the open arrays *)
           start := start - 1;
           
           (* keep trying to make comparisons till the pattern
              will no longer fit at the end of the source *)

           s := start;
           WHILE len_s - s + 1 >= len_p DO

             (* are the strings equal from this position to the end *)

             p := 0;
             WHILE ( p < len_p ) & ( source[s+p] = pattern[p] ) DO
                  p := p + 1;
             END;

             IF p = len_p THEN   (* they are equal *)
                RETURN s + 1;
             END;

             s := s + 1;
           END;

           RETURN 0;   (* if outside the loop, then not found *)

         END;
       END;
    END Locate;


    (* -------------------------------------------- *)
    PROCEDURE EqualSubString( a, b :ARRAY OF CHAR;
                              start, len :CARDINAL ) :BOOLEAN;
    VAR
      i, n :CARDINAL;
    BEGIN

       IF len = 0 THEN
         RETURN FALSE;
       ELSE

         n := start + len - 1;

         IF n > Length( a ) THEN
           RETURN FALSE;
         ELSE

           IF n > Length( b ) THEN
             RETURN FALSE;
           ELSE

             i := start - 1;
             WHILE ( i < n ) & ( a[i] = b[i] ) DO
               i := i + 1;
             END;

             RETURN i = n;

           END;
         END;
       END;

    END EqualSubString;


BEGIN
END MoreStrings.
