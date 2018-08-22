MODULE WC2Test;

        (********************************************************)
        (*                                                      *)
        (*              Test of the WildC2 module               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            11 May 2003                     *)
        (*  Last edited:        11 May 2003                     *)
        (*  Status:             Was working in OS/2, now doing  *)
        (*              a DOS version so that I can translate   *)
        (*              it to C.                                *)
        (*                                                      *)
        (********************************************************)

FROM IO IMPORT
    (* proc *)  WrStr, WrLn;

FROM WildC2 IMPORT
    (* proc *)  WildMatch;

(************************************************************************)
(*                          TEST CODE                                   *)
(************************************************************************)

PROCEDURE TestPair (input, template: ARRAY OF CHAR);

    BEGIN
        WrStr (input);
        IF WildMatch (input, template) THEN
            WrStr (" matches ");
        ELSE
            WrStr (" doesn't match ");
        END (*IF*);
        WrStr ("template ");  WrStr (template);
        WrLn;
    END TestPair;

(************************************************************************)

PROCEDURE RunTheTest;

    BEGIN
        WrStr ("Testing wildcard matches");  WrLn;
        TestPair ("a", "*a");
        TestPair ("a", "A");
        TestPair ("a", "b*");
        TestPair ("a", "*");
        TestPair ("ab", "*b*");
        TestPair ("a", "**");
        TestPair ("a", "**b");
        TestPair ("xyabuvdefabcmmdefmmm", "**abc*def*");
        TestPair ("xyz.uvw.abc.zip", "*.ZIP");
    END RunTheTest;

(************************************************************************)
(*                            MAIN PROGRAM                              *)
(************************************************************************)

BEGIN
    RunTheTest;
END WC2Test.

