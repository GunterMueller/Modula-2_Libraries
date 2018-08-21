IMPLEMENTATION MODULE GetCharacter;

(* GET a single character from the terminal *)
(* J. Andrea, May.18/92 -dos version *)
(* bastardization of M. Mall's TTIO from ETH by Jaa, 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)
  
FROM Terminal IMPORT Read, KeyPressed;                

  (* --------------------------------------------------------------- *)
  PROCEDURE Get( VAR ch: CHAR );
  BEGIN
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
