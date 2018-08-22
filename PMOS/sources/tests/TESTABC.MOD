MODULE TestABC;

FROM IO IMPORT
    (* proc *)  WrStr, WrLn;

VAR x: CARDINAL;

BEGIN
    x := 1;
    WrStr ("Hello");
    WrLn;
    INC (x);
END TestABC.
