IMPLEMENTATION MODULE ForeignCommand;

(* Get options from the command line *)
(* V1.0, J. Andrea, May.4/92 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM CommonInputOutputProcedures IMPORT LIB$GET_FOREIGN;
FROM StringOperations IMPORT NextBlank, NextNonBlank, SubString, Assign;

CONST
   max_options  = 30;      (* maximum number of options on a line *)
   max_line_len = 255;     (* maximum size of the command line    *)

VAR
   command_line :ARRAY [1..max_line_len] OF CHAR;

   endpoints :ARRAY [1..max_options] OF RECORD
                                          start, end :CARDINAL;
                                        END;

   command_len, n_options :CARDINAL;
   command_read           :BOOLEAN;


   (* -------------------------------------------------- *)
   PROCEDURE ReadCommand( prompt :ARRAY OF CHAR );
   VAR
      status        :CARDINAL;
      prompt_forced :BOOLEAN;
      start, end    :CARDINAL;
   BEGIN
      IF NOT command_read THEN
        command_read := TRUE;

        prompt_forced := FALSE;

        status := LIB$GET_FOREIGN( command_line, prompt, command_len,
                                   prompt_forced );


        (* now find all the option strings *)

        start := NextNonBlank( command_line, 1 );
        WHILE ( n_options < max_options ) & ( start # 0 ) DO
           end := NextBlank( command_line, start );

           n_options := n_options + 1;
           endpoints[n_options].start := start;
           endpoints[n_options].end   := end - 1;

           start := NextNonBlank( command_line, end );
        END;

      END;
   END ReadCommand;

   (* -------------------------------------------------- *)
   PROCEDURE NOptions() :CARDINAL;
   BEGIN
      RETURN n_options;
   END NOptions;

   (* -------------------------------------------------- *)
   PROCEDURE GetAll( VAR whole_line :ARRAY OF CHAR;
                     VAR length :CARDINAL );
   BEGIN
      IF command_read THEN
        length := command_len;
        Assign( command_line, whole_line );
      ELSE
        length := 0;
      END;
   END GetAll;

   (* -------------------------------------------------- *)
   PROCEDURE GetOption( i :CARDINAL; VAR option :ARRAY OF CHAR;
                                     VAR length :CARDINAL );
   BEGIN
      IF command_read THEN
        IF n_options >= i THEN

          length := endpoints[i].end - endpoints[i].start + 1;

          SubString( command_line, endpoints[i].start, length, option );

        ELSE
          length := 0;
        END;
      ELSE
        length := 0;
      END;

   END GetOption;


BEGIN
   command_read := FALSE;
   command_len  := 0;
   n_options    := 0;
END ForeignCommand.
