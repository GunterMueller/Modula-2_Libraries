IMPLEMENTATION MODULE Serial;

(* Deal with a serial port. *)
(* V1.0, J.Andrea, Jun.3/93 *)
(* This code may be freely used and distributed, it may not be sold *)

IMPORT RS232Code;
FROM Strings IMPORT Length;

VAR
  init_done :BOOLEAN;

  (* ------------------------------------ *)
  PROCEDURE InitPort( VAR done :BOOLEAN );
  BEGIN
    RS232Code.Init(     (* com port 1 *)
         9600,  (* baud *)
         2,     (* stop bits *)
         FALSE, (* parity enable *)
         TRUE,  (* even parity *)
         7,     (* char size *)
         done );
     IF done THEN
       RS232Code.StartReading;
       init_done := TRUE;
     END;
  END InitPort;

  (* ----------------------------------- *)
  PROCEDURE ClosePort;
  BEGIN
    IF init_done THEN
      RS232Code.StopReading;
      init_done := FALSE;
    END;
  END ClosePort;

  (* ----------------------------------- *)
  PROCEDURE Get( VAR ch :CHAR );
  BEGIN
    IF init_done THEN
      RS232Code.Read( ch );
    ELSE
      ch := 0C;
    END;
  END Get;

  PROCEDURE Put( ch :CHAR );
  BEGIN
    IF init_done THEN
      RS232Code.Write( ch );
    END;
  END Put;

  PROCEDURE PutString( s :ARRAY OF CHAR );
  VAR i, n :CARDINAL;
  BEGIN
     n := Length( s );
     IF n # 0 THEN
       FOR i := 0 TO n-1 DO
         Put( s[i] );
       END;
     END;
  END PutString;

BEGIN
  init_done := FALSE;
END Serial.
