IMPLEMENTATION MODULE CmdLine;

(* Implementation by J. Andrea, Jun.28/92 -dos version *)
(* With help on PSP addresses from Pat Terry *)
(* This code may be freely used and distributed, it may not be sold *)

FROM RTSMain IMPORT cmdLinePtr;
FROM StringOperations IMPORT Assign, SubString, NextBlank, NextNonBlank;

CONST
  max_args = 30;   (* don't expect any more than this many arguments *)
  max_size = 256;  (* max size of command line *)

VAR
  cmd_line        :ARRAY [1..max_size] OF CHAR;
  cmd_len, n_args :CARDINAL;
  
  start, stop     :ARRAY [1..max_args] OF CARDINAL;
  i, j            :CARDINAL;
  
  (* ------------------------------------------- *)
  PROCEDURE WholeCommandLine( VAR line :ARRAY OF CHAR );
  BEGIN
     Assign( cmd_line, line );
  END WholeCommandLine;
  
  (* ------------------------------------------- *)
  PROCEDURE Nargs() :CARDINAL;
  BEGIN
    RETURN n_args;
  END Nargs;
  
  (* ------------------------------------------- *)
  PROCEDURE GetArg( n :CARDINAL; VAR arg :ARRAY OF CHAR );    
  BEGIN
    IF ( n < 1 ) OR ( n > n_args ) THEN
      arg[0] := 0C;
    ELSE
    
      i := stop[n] - start[n] + 1;  (* length of this arg *)

      SubString( cmd_line, start[n], i, arg );

    END;
  END GetArg;
  
  (* ------------------------------------------- *)
  PROCEDURE SetupArgs;
  (* at program startup time, precompute the start and end positions *)
  (* of all the arguments *)
  BEGIN
  
     i := NextNonBlank( cmd_line, 1 );
     WHILE ( n_args < max_args ) & ( i # 0 ) DO
        j := NextBlank( cmd_line, i );

        IF j = 0 THEN
          (* no more blanks, end of arg must be last character *)
          j := cmd_len + 1;
        END;
        
        n_args := n_args + 1;
        start[n_args] := i;
        stop[n_args]  := j - 1;

        i := NextNonBlank( cmd_line, j );
     END;

  END SetupArgs;

  (* ------------------------------------------- *)
  PROCEDURE GetCommandLine;
  (* at program startup time, get the command line from the DOS PSP buffer *)
  TYPE
    Psp = POINTER TO ARRAY [0..max_size-1] OF CHAR;
    
  VAR
    psp_ptr :Psp;
    
  BEGIN
  
     psp_ptr := cmdLinePtr.addr;

     cmd_len := ORD( psp_ptr^[128] );
       
     FOR i := 1 TO cmd_len DO
       cmd_line[i] := psp_ptr^[i+128];
     END;

     IF cmd_len < max_size THEN
       cmd_line[cmd_len+1] := 0C;
     END;
     
   END GetCommandLine;

BEGIN

  n_args      := 0;
  cmd_line[1] := 0C;
  cmd_len     := 0;
  
  GetCommandLine;
  SetupArgs;
    
END CmdLine.
