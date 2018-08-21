IMPLEMENTATION MODULE GetCharacter;

(* John Andrea, Mar.27/92 *)
(* This code may NOT be sold, it may be freely used and distributed *)

FROM TermIO IMPORT Read, KeyPressed;

  (* --------------------------------------------------------------- *)
  PROCEDURE Get( VAR ch: CHAR) ;
  BEGIN (* Get *)
     Read( ch );
  END Get;

  (* --------------------------------------------------------------- *)
  PROCEDURE GetNoWait( VAR ch: CHAR );
  BEGIN
    IF KeyPressed() THEN
      Read( ch );
    ELSE
      ch := 0C;
    END;
  END GetNoWait;
 
  (* --------------------------------------------------------------- *)
  PROCEDURE StartGet;
  BEGIN
  END StartGet;

  (* --------------------------------------------------------------- *)
  PROCEDURE StopGet;
  BEGIN
  END StopGet;

BEGIN
END GetCharacter.
