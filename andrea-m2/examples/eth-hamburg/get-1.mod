MODULE TestGet;

(* an example of how to use the GetCharacter procedures *)
(* J. Andrea, Aug.21/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM GetCharacter IMPORT StartGet, StopGet, Get, GetNoWait;
FROM InOut IMPORT WriteString, WriteLn, WriteCard;

VAR
  c :CHAR;
  i :CARDINAL;

BEGIN

WriteString( 'about to enter single character mode' ); WriteLn;
WriteString( 'hit any key, Q to quit' ); WriteLn;

StartGet;

Get( c );
WHILE CAP( c ) # 'Q' DO
  WriteString( c ); WriteLn; 
  Get( c );
END;

StopGet;

WriteLn; WriteLn;
WriteString( 'about to enter single character mode with no waiting' ); WriteLn;
WriteString( 'hit any key to restart, hit Q to quit' ); WriteLn;

StartGet;

GetNoWait( c );  i := 0;
WHILE CAP( c ) # 'Q' DO

  IF c # 0C THEN
    i := 0;
  END;

  WriteCard( i, 0 ); WriteLn;

  GetNoWait( c );   i := i + 1;
END;

StopGet;

END TestGet.
