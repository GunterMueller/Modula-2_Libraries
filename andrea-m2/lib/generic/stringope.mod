IMPLEMENTATION MODULE StringOperations;

(* J. Andrea, Jun.17/93 - use internal Length rather than non-standard
                          built in function LEN, and correct global variables *)
(* J. Andrea, Jul.2/92 - NextBlank and NextNonBlank use same scan routine *)
(* J. Andrea, Sep.5/91 - add compare *)
(* J. Andrea, Aug.12/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM MoreMath IMPORT MinCard;

CONST
   nul   = 0C;
   space = ' ';
   tab   = 11C;

TYPE
   CharScanProc = PROCEDURE( CHAR ) :BOOLEAN;

(* ------------------------------------------------------- *)
PROCEDURE Blank( c :CHAR ) :BOOLEAN;
(* is this character a blank or tab *)
BEGIN
  RETURN ( c = space ) OR ( c = tab );
END Blank;

(* ------------------------------------------------------- *)
PROCEDURE NonBlank( c :CHAR ) :BOOLEAN;
(* is this character a blank or tab *)
BEGIN
  RETURN NOT Blank( c );
END NonBlank;

    (* -------------------------------------------- *)
    PROCEDURE Equal( a, b :ARRAY OF CHAR ) :BOOLEAN;
    VAR
      i, n :CARDINAL;
    BEGIN

       n := Length( a );

       IF n # Length( b ) THEN
         RETURN FALSE;
       ELSE

         i := 0;
         WHILE ( i < n ) & ( a[i] = b[i] ) DO
           i := i + 1;
         END;

         RETURN i = n;

       END;

    END Equal;



    (* ---------------------------------------------------------------- *)
    PROCEDURE Compare( a, operation, b :ARRAY OF CHAR ) :BOOLEAN;
    (*
     This nice algorithm comes from:
                 Western Research Laboratory, Digital Equipment Corp.
                 100 Hamilton Ave., Palo Alto, CA  94301
     and is subject to their copyrights: this may be used, copied
     and modified but may not be sold.
     Some minor changes and a bug fix by J. Andrea, Sep.5/91
    *)

    TYPE
       Operations = ( eq, ne, gt, ge, lt, le, nop );

    VAR
       compare_op :Operations;

       last_a, last_b :CHAR;
       len_a,  len_b  :CARDINAL;
       ok             :BOOLEAN;
       i, n           :CARDINAL;

    BEGIN (* Compare *)

       n := Length( operation );

       IF n < 1 THEN
         compare_op := nop;

       ELSIF n = 1 THEN

         IF operation[0] = '=' THEN
           compare_op := eq;
         ELSIF operation[0] = '#' THEN
           compare_op := ne;
         ELSIF operation[0] = '>' THEN
           compare_op := gt;
         ELSIF operation[0] = '<' THEN
           compare_op := lt;
         ELSE
           compare_op := nop;
         END;

       ELSIF n = 2 THEN

         IF operation[0] = '>' THEN

           IF operation[1] = '=' THEN
             compare_op := ge;
           ELSE
             compare_op := nop;
           END;

         ELSIF operation[0] = '<' THEN

           IF operation[1] = '=' THEN
             compare_op := le;
           ELSIF operation[1] = '>' THEN
             compare_op := ne;
           ELSE
             compare_op := nop;
           END;

         ELSIF operation[0] = '=' THEN

           IF operation[1] = '<' THEN
             compare_op := le;
           ELSIF operation[1] = '>' THEN
             compare_op := ge;
           ELSE
             compare_op := nop;
           END;

         ELSE
           compare_op := nop;
         END;

       ELSE
         compare_op := nop;
       END;

       IF compare_op = nop THEN
         RETURN FALSE;
       ELSE

         len_a  := Length( a );  len_b  := Length( b );
         last_a := nul;          last_b := nul;

         IF len_a = len_b THEN
           n := len_a;
           IF n # 0 THEN
             last_a := a[n-1];
             last_b := b[n-1];
           END;
         ELSIF len_a < len_b THEN
           n      := len_a;
           last_b := b[n-1];
         ELSE
           n      := len_b;
           last_a := a[n-1];
         END;

         i := 0; ok := TRUE;
         WHILE ok & ( i < n ) DO
           IF a[i] # b[i] THEN
             ok     := FALSE;
             last_a := a[i];  last_b := b[i];
           ELSE
             i := i + 1;
           END;
         END;

         CASE compare_op OF

         eq: RETURN last_a = last_b;
        |ne: RETURN last_a # last_b;
        |gt: RETURN last_a > last_b;
        |lt: RETURN last_a < last_b;
        |ge: RETURN last_a >= last_b;
        |le: RETURN last_a <= last_b;

         END;

       END;

    END Compare;



    (* -------------------------------------------- *)
    PROCEDURE Index( source, pattern :ARRAY OF CHAR ) :CARDINAL;
    (*
       This algorithm is a simple string search.
       Help from D. MacDonald for a fix to string lengths.
    *)

    VAR
       len_s, len_p, s, p :CARDINAL;

    BEGIN (* Index *)

       len_s := Length( source );

       IF len_s = 0 THEN
         RETURN 0;
       ELSE

         len_p := Length( pattern );

         IF ( len_p = 0 ) OR ( len_p > len_s ) THEN
           RETURN 0;
         ELSE

           (* keep trying to make comparisons till the pattern
              will no longer fit at the end of the source *)

           s := 0;
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

    END Index;


    (* -------------------------------------------- *)
    PROCEDURE Assign( a :ARRAY OF CHAR; VAR b :ARRAY OF CHAR );
    VAR
      i, n, max :CARDINAL;
    BEGIN

       n := Length( a );

       IF n = 0 THEN
         b[0] := nul;
       ELSE

         (* determine how many characters can be copied *)

          max := HIGH( b ) + 1;      (* maximum characters in 'c' *)

          n := MinCard( n, max );    (* minimum required to copy *)

          FOR i := 0 TO n - 1 DO     (* copy those characters *)
             b[i] := a[i];
          END;

          IF n < max THEN b[n] := nul; END;

       END;

    END Assign;


    (* ---------------------------------------------------------------- *)
    PROCEDURE Append( a :ARRAY OF CHAR; VAR b :ARRAY OF CHAR );
    VAR
       i, n, max, out, remain :CARDINAL;
    BEGIN

       n := Length( a );

       IF n > 0 THEN

         max := HIGH( b ) + 1;   (* maximum number of characters in 'b' *)
         out := Length( b );

         (* calculate the minimum number of characters which must be moved *)

         remain := max - out;

         n := MinCard( n, remain );

         FOR i := 0 TO n-1 DO           (* copy those characters *)
            b[out] := a[i];
            out    := out + 1;
         END;

         IF out < max THEN b[out] := nul; END;

       END;

    END Append;


    (* ---------------------------------------------------------------- *)
    PROCEDURE Concat( a, b :ARRAY OF CHAR; VAR c :ARRAY OF CHAR );
    BEGIN

        Assign( a, c );
        Append( b, c );

    END Concat;


    (* ---------------------------------------------------------------- *)
    PROCEDURE SubString( a :ARRAY OF CHAR; start, len :CARDINAL;
                         VAR b :ARRAY OF CHAR );
    VAR
      i, n, max :CARDINAL;
    BEGIN

       IF len = 0 THEN  (* don't copy anything *)
         b[0] := nul;
       ELSE

         IF start = 0 THEN start := 1; END;

         n := Length( a );

         IF start > n THEN  (* start off the end, don't copy anything *)
           b[0] := nul;
         ELSE

           max := HIGH( b ) + 1;   (* maximum number of characters in 'b' *)

           (* determine how many characters can be copied *)

           (* don't run off the end of the output string *)
           len := MinCard( len, max );

           (* and don't run off the end of the input string *)
           IF start + len - 1 > n THEN
             len := n - start + 1;
           END;

           (* change 'start' back to zero offset *)
           start := start - 1;

           FOR i := 0 TO len - 1 DO
              b[i] := a[start+i];
           END;

           IF len < max THEN b[len] := nul; END;

        END;
      END;

    END SubString;



    (* ---------------------------------------------------------------- *)
    PROCEDURE Insert( a :ARRAY OF CHAR; start :CARDINAL;
                      VAR b :ARRAY OF CHAR );
    VAR
       i, j, n, m, len_b, max, top, remain :CARDINAL;
    BEGIN

       n := Length( a );

       IF n > 0 THEN

         len_b := Length( b );

         IF start = 0 THEN start := 1; END;

         IF start <= len_b THEN

           (* this operation must be done in two steps *)
           (* 1. shift the end section of 'b' back beyond where 'a' will go *)
           (* 2. move the characters of 'a' into 'b' *)

           max := HIGH( b ) + 1;   (* maximum number of characters in 'b' *)


           (* begin part 1 *)

           top := start + n - 1; (* last character of the insert section *)

           (* don't shift anything if the insert will fill the remainder *)

           IF top < max THEN

             (* how many characters should be shifted *)
             m := len_b - start + 1;

             (* how many places remain after the insert *)
             remain := max - top;

             (* move only as many characters as will fit at the end *)
             m := MinCard( remain, m );

             (* the motion must go from the back to the front *)
             (* or else characters may get written over before they are moved *)

             (* this really reads j := start + m - 1;               *)
             (* but the strings are zero offset, so the extra -1 is *)
             (* propagated out of the loop to the initial value     *)

             j := start + m - 2;
             FOR i := 1 TO m DO
                b[n+j] := b[j];
                j := j - 1;
             END;

           END;


           (* begin part 2 *)
           (* determine how many characters of 'a' can be copied in *)

           m := MinCard( n, max - start + 1 );

           (* convert 'start' back to zero offset *)
           start := start - 1;

           FOR i := 0 TO m-1 DO           (* copy those characters *)
              b[start+i] := a[i];
           END;

           top := len_b + n;
           IF top < max THEN b[top] := nul; END;

        END;
      END;

    END Insert;


    (* ---------------------------------------------------------------- *)
    PROCEDURE Replace( a :ARRAY OF CHAR; start :CARDINAL; VAR b :ARRAY OF CHAR );
    VAR
      i, n, len_a, len_b :CARDINAL;
    BEGIN

       len_b := Length( b );

       IF len_b > 0 THEN

         len_a := Length( a );

         IF len_a > 0 THEN

           IF start = 0 THEN start := 1; END;

           IF start <= len_b THEN

             (* find the number of characters which can be replaced *)
             n := MinCard( len_a, len_b - start + 1 );

             IF n > 0 THEN

               n := n - 1;   start := start - 1;

               FOR i := 0 TO n DO
                  b[start+i] := a[i];
               END;

             END;
           END;
        END;
      END;

    END Replace;


    (* ---------------------------------------------------------------- *)
    PROCEDURE Delete( VAR a :ARRAY OF CHAR; start, len :CARDINAL );

    VAR
       i, n, top, remain :CARDINAL;

    BEGIN

       IF len > 0 THEN

         n := Length( a );

         IF start = 0 THEN start := 1; END;

         IF start <= n THEN

           top := start + len - 1;    (* last character of the delete section *)

           IF top >= n THEN
             (* all the remaining characters are to be deleted *)
             (* so just end the string here *)

             a[start-1] := nul;

           ELSE

             (* determine how many characters must be shifted down from *)
             (* above the delete section *)

             n := n - top;

             (* change 'start' back to zero offset *)
             start := start - 1;

             FOR i := 0 TO n - 1 DO
                a[start+i] := a[top+i];
             END;

             a[start+n] := nul;

           END;

        END;
      END;

    END Delete;


    (* ---------------------------------------------------------------- *)
    PROCEDURE Upper( VAR string :ARRAY OF CHAR );
    VAR
      i, n :CARDINAL;
      c    :CHAR;
    BEGIN

       n := Length( string );

       FOR i := 0 TO n-1 DO

          c := string[i];

          IF ( c >= 'a' ) & ( c <= 'z' ) THEN
            string[i] := CAP( c );
          END;

       END;

    END Upper;

    (* ---------------------------------------------------------------- *)
    PROCEDURE Lower( VAR string :ARRAY OF CHAR );
    VAR
      i, n :CARDINAL;
      c    :CHAR;
    BEGIN

       n := Length( string );

       FOR i := 0 TO n-1 DO

          c := string[i];

          IF ( c >= 'A' ) & ( c <= 'Z' ) THEN
            string[i] := CHR( ORD( c ) + 32 );
          END;

       END;

    END Lower;


    (* -------------------------------------------- *)
    PROCEDURE NextScan( source :ARRAY OF CHAR; start :CARDINAL;
                        ScanFor :CharScanProc ) :CARDINAL;
    VAR
      i, n :CARDINAL;
    BEGIN

       n := Length( source );

       IF n = 0 THEN
         RETURN 0;
       ELSE

         IF start = 0 THEN start := 1; END;

         IF start > n THEN
           RETURN 0;
         ELSE

           i := start - 1;

           WHILE ( i < n ) & ScanFor( source[i] ) DO
             i := i + 1;
           END;

           IF i = n THEN
             RETURN 0;
           ELSE
             RETURN i + 1;
           END;

         END;
       END;

    END NextScan;

    (* -------------------------------------------- *)
    PROCEDURE NextBlank( source :ARRAY OF CHAR;
                         start  :CARDINAL ) :CARDINAL;
    BEGIN
      RETURN NextScan( source, start, NonBlank );
    END NextBlank;


    (* -------------------------------------------- *)
    PROCEDURE NextNonBlank( source :ARRAY OF CHAR;
                            start  :CARDINAL ) :CARDINAL;
    BEGIN
      RETURN NextScan( source, start, Blank );
    END NextNonBlank;


    (* ---------------------------------------------------------------- *)
    PROCEDURE Empty( VAR string :ARRAY OF CHAR );
    VAR
      i, n :CARDINAL;
    BEGIN

       n := HIGH( string );

       FOR i := 0 TO n DO
          string[i] := nul;
       END;

    END Empty;


    (* ---------------------------------------------------------------- *)
    PROCEDURE Trim( VAR string :ARRAY OF CHAR );

    VAR
       n, on_input :CARDINAL;

    BEGIN

       on_input := Length( string );

       IF on_input # 0 THEN

         on_input := on_input - 1;
         n        := on_input;

         (* start at the end and run backwards *)

         WHILE ( n > 0 ) & Blank( string[n] ) DO
           n := n - 1;
         END;

         (* has the length of the string actually changed *)

         IF n # on_input THEN
           (* end the string properly *)

           IF n = 0 THEN
             IF Blank( string[0] ) THEN string[0] := nul; END;
           ELSE
             string[n+1] := nul;
           END;

         END;

      END;

    END Trim;

    (* ---------------------------------------------------------------- *)
    PROCEDURE Length( string :ARRAY OF CHAR ) :CARDINAL;
    VAR
      i, n :CARDINAL;
    BEGIN

       n := HIGH( string );

       i := 0;
       WHILE ( i <= n ) & ( string[i] # nul ) DO
          i := i + 1;
       END;

       RETURN i;

    END Length;

BEGIN
END StringOperations.
