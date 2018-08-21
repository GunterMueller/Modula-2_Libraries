IMPLEMENTATION MODULE CmdLine;

(* J. Andrea, Oct.13/92 -vms version *)
(* Implementation by J. Andrea, Jun.28/92 -dos version *)
(* This code may be freely used and distributed, it may not be sold *)

IMPORT ForeignCommand;

VAR
  length :CARDINAL;


  (* ------------------------------------------- *)
  PROCEDURE WholeCommandLine( VAR line :ARRAY OF CHAR );
  BEGIN
     ForeignCommand.GetAll( line, length );
     IF length = 0 THEN
       line[0] := 0C;
     END;
  END WholeCommandLine;

  (* ------------------------------------------- *)
  PROCEDURE Nargs() :CARDINAL;
  BEGIN
    RETURN ForeignCommand.NOptions();
  END Nargs;

  (* ------------------------------------------- *)
  PROCEDURE GetArg( n :CARDINAL; VAR arg :ARRAY OF CHAR );
  BEGIN
    ForeignCommand.GetOption( n, arg, length );
    IF length = 0 THEN
      arg[0] := 0C;
    END;
  END GetArg;

BEGIN
END CmdLine.



PROCEDURE ReadCommand( prompt :ARRAY OF CHAR );
(* First step in getting options, this must be called before GetOption   *)
(* and should be called only once. The prompt is shown if the user types *)
(* the command line with no options. *)


PROCEDURE NOptions() :CARDINAL;
(* return the number of options given *)


PROCEDURE GetOption( i :CARDINAL; VAR option :ARRAY OF CHAR;
                                  VAR length :CARDINAL );
(* Get the i'th option string, need not be called in order *)
(* A returned 'length' of zero means that that option is not available. *)
