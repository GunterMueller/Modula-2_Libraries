IMPLEMENTATION MODULE RealInOut;

(* see the definition module for details *)
(* Implemented by J. Andrea, Aug.8/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

IMPORT InOut;
FROM FortranFormat IMPORT For$FFormat, For$EFormat;

VAR string :ARRAY [0..132] OF CHAR;

(* ---------------------------------------------------------------- *)
PROCEDURE ReadReal( VAR x :REAL );
BEGIN
   InOut.ReadReal( x );            Done := InOut.Done;
END ReadReal;


(* ---------------------------------------------------------------- *)
PROCEDURE WriteReal( x :REAL; width :CARDINAL );
BEGIN
   InOut.WriteReal( x, width );    Done := InOut.Done;
END WriteReal;


(* ---------------------------------------------------------------- *)
PROCEDURE FWriteReal( x :REAL; width, decimals :CARDINAL);
BEGIN (* FWriteReal *)

   ToFString( x, width, decimals, string );

   InOut.WriteString( string );     Done := InOut.Done;

END FWriteReal;


(* ---------------------------------------------------------------- *)
PROCEDURE EWriteReal( x :REAL; width, decimals :CARDINAL);
BEGIN (* EWriteReal *)

   ToEString( x, width, decimals, string );

   InOut.WriteString( string );     Done := InOut.Done;

END EWriteReal;


(* ---------------------------------------------------------------- *)
PROCEDURE ToFString( x :REAL; width, decimals :CARDINAL;
                     VAR string :ARRAY OF CHAR );

VAR
   len :CARDINAL;

BEGIN (* ToFString *)

  IF width = 0 THEN
    (* impossible width, return nothing *)
    string[0] := 0C;
  ELSE

    len := HIGH( string );

    (* this set of code is an attempt to give something to the *)
    (* user, even if the user has specified an invalid set of parameters *)

    (* so the values of width and decimals which get used may may be *)
    (* different here, from what was passed in as parameters *)

    IF width > len THEN
      (* if the specified width is longer than the output string *)
      (* then choose a smaller width *)
      width := len;
    END;

    IF decimals >= width THEN
      (* decimal places can't be more than the total width *)
      (* and there has to be one place for the decimal point too *)
      decimals := width - 1;
    END;

    For$FFormat( x, width, decimals, string );

    (* if possible, add a proper end to the output string *)
    IF width < len THEN
      string[width] := 0C;
    END;

  END;

  Done := TRUE;

END ToFString;


(* ---------------------------------------------------------------- *)
PROCEDURE ToEString( x :REAL; width, decimals :CARDINAL;
                     VAR string :ARRAY OF CHAR );

VAR
   len :CARDINAL;

BEGIN (* ToEString *)

  IF width = 0 THEN
    (* impossible width, return nothing *)
    string[0] := 0C;
  ELSE

    len := HIGH( string );

    (* this set of code is an attempt to give something to the *)
    (* user, even if the user has specified an invalid set of parameters *)

    (* so the values of width and decimals which get used may may be *)
    (* different here, from what was passed in as parameters *)

    IF width > len THEN
      (* if the specified width is longer than the output string *)
      (* then choose a smaller width *)
      width := len;
    END;

    (* allow the lower level routine to handle an invalid 'decimals' *)

    For$EFormat( x, width, decimals, string );

    (* if possible, add a proper end to the output string *)
    IF width < len THEN
      string[width] := 0C;
    END;

  END;

  Done := TRUE;

END ToEString;

BEGIN (* RealInOut *)
END RealInOut.
