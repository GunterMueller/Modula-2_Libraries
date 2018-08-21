IMPLEMENTATION MODULE ExtendStrings;

(* misc. extra string functions *)

(* see definition module for algorithm notes *)
(* V1.0, J. Andrea, Jun.17/93 -taken from MoreStrings *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM STRDefinitions IMPORT STR$_NOMATCH, STR$_MATCH;
FROM StringHandlingProcedures IMPORT STR$COMPARE, STR$CASE_BLIND_COMPARE,
                                     STR$MATCH_WILD;

VAR
   result :INTEGER;

    (* ---------------------------------------------------------------- *)
    PROCEDURE CaseCompare( a, b :ARRAY OF CHAR; case_specific :BOOLEAN ) :CHAR;
    BEGIN

       IF case_specific THEN
         result := STR$COMPARE( a, b );
       ELSE
         result := STR$CASE_BLIND_COMPARE( a, b );
       END;

       IF result = 0 THEN
         RETURN '=';
       ELSE
         IF result > 0 THEN
           RETURN '>';
         ELSE
           RETURN '<';
         END;
       END;

    END CaseCompare;


    (* ---------------------------------------------------------------- *)
    PROCEDURE WildcardMatch( string, pattern :ARRAY OF CHAR ) :BOOLEAN;
    BEGIN
      RETURN STR$MATCH_WILD( string, pattern ) = STR$_MATCH;
    END WildcardMatch;

BEGIN
END ExtendStrings.
