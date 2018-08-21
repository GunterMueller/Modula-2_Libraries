MODULE Test;

(* Test the procedure ReadKey from ScreenIO *)
(* J.Andrea, Jun.1993 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteString, WriteLn;
FROM ScreenIO IMPORT ReadKey;
FROM Break IMPORT EnableBreak;

VAR
  ch :CHAR;
  shift, control, alt, function, arrow, special :BOOLEAN;

BEGIN EnableBreak;

  WriteString( 'hit any key and see what description gets displayed ' );
  WriteLn;
  WriteString( 'the program stops when anything involving an X is hit' );
  WriteLn; WriteLn;

  REPEAT
     ReadKey( ch, shift, control, alt, function, arrow, special );
     IF shift THEN WriteString( 'shift ' ) END;
     IF control THEN WriteString( 'control ' ) END;
     IF alt THEN WriteString( 'alt ' ) END;
     IF function THEN WriteString( 'function ' ) END;
     IF arrow THEN WriteString( 'arrow ' ) END;
     IF special THEN WriteString( 'special ' ) END;
     IF ch = ' ' THEN
       WriteString( 'space' );
     ELSE
       WriteString( ch );
     END;
     WriteLn;
  UNTIL ch = 'x';

END Test.
