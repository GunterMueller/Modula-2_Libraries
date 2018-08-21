MODULE TestSay;

(* Make a cardinal number into a phrase of words of numbers *)
(* V1.0, J. Andrea, Oct.11/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteString, WriteLn, WriteCard, ReadCard, ReadLn;
FROM SayNumbers IMPORT SayCard;

VAR
  x    :CARDINAL;
  word :ARRAY [1..200] OF CHAR;

BEGIN

  WriteString( 'a number ? ' );
  ReadCard( x );  ReadLn;
  WHILE x # 0 DO
    WriteCard( x, 0 ); WriteString( ' ' );
    SayCard( x, word );
    WriteString( word ); WriteLn;
    WriteLn;
    WriteString( 'a number (zero to exit) ? ' );
    ReadCard( x ); ReadLn;
  END;
  WriteLn;

END TestSay.
