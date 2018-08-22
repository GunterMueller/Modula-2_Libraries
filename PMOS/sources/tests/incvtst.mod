MODULE INCVtst;

FROM LowLevel IMPORT INCV, DECV;

FROM Keyboard IMPORT InKey;

FROM GlassTTY IMPORT WriteString, WriteCard, WriteLn;

VAR x, y: CARDINAL;  dummy: CHAR;

BEGIN
    x := 40000;  y := 30000;
    IF INCV (x,y) THEN WriteString ("Overflow ");  END(*IF*);
    WriteCard (x);  WriteString ("   ");  WriteCard (y);
    WriteLn;
    x := 40000;  y := 30000;
    IF DECV (x,y) THEN WriteString ("Underflow ");  END(*IF*);
    WriteCard (x);  WriteString ("   ");  WriteCard (y);
    WriteLn;
    dummy := InKey();
END INCVtst.
