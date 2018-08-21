MODULE UnStream;

(* Convert a file from Stream_LF format to standard text,
   and break long lines as well *)
(* J. Andrea, Jun.17/92 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM ForeignCommand IMPORT ReadCommand, GetOption;
IMPORT BlockIO;
IMPORT TextIO;

CONST
  out_max = 132;

VAR
  in_f        :BlockIO.BlockFile;
  out_f       :TextIO.File;
  filename    :ARRAY [0..80] OF CHAR;
  block_data       :ARRAY [1..512] OF CHAR;
  block_count, block_size :CARDINAL;

  out_line, save_line :ARRAY [1..out_max+1] OF CHAR;
  out_len,  saved     :CARDINAL;

  (* ----------------------------------------------------------- *)
  PROCEDURE DoDump;
  VAR
     p, i, j :CARDINAL;

        (* ----------------------------------------------------------- *)
        PROCEDURE Output( length :CARDINAL );
        BEGIN
           IF length > 0 THEN
             out_line[length+1] := 0C;
             TextIO.WriteString( out_f, out_line );
           END;
           TextIO.WriteLn( out_f );
        END Output;

        (* ----------------------------------------------------------- *)
        PROCEDURE NewLine( c :CHAR ) :BOOLEAN;
        CONST
           lf  = 12C;
           cr  = 15C;
        BEGIN
           RETURN ( c = lf ) OR ( c = cr );
        END NewLine;

   BEGIN

     FOR p := 1 TO block_size DO
 
       IF NewLine( block_data[p] ) THEN
         Output( out_len );
         out_len := 0;
       ELSE

         IF out_len < out_max THEN
           out_len := out_len + 1;
           out_line[out_len] := block_data[p];
         ELSE

           (* its going to be too big to fit into the output record,
              so break the line at the beginning of this word *)

           (* go backwards to the the last blank *)
           i := out_len;
           WHILE ( i > 0 ) & ( out_line[i] > ' ' ) DO
             i := i - 1;
           END;

           IF i = 0 THEN
             (* couldn't find a place to break, force out all we've got *)
             Output( out_len );
             out_len := 0;
           ELSE

             (* save the end part of the line *)
             saved := 0;
             FOR j := i+1 TO out_len DO
                saved := saved + 1;
                save_line[saved] := out_line[j];
             END;

             Output( i );

             out_line := save_line;
             out_len  := saved;
           END;

           (* and add in the current character *)
           out_len := out_len + 1;
           out_line[out_len] := block_data[p];

         END;
       END;

     END;

  END DoDump;

BEGIN

ReadCommand( 'input filename ? ' );
GetOption( 1, filename, block_size );

BlockIO.OpenIn( in_f, filename );
IF BlockIO.Done() THEN

  TextIO.OpenOut( out_f, 'out.unstream' );
  IF TextIO.Done THEN

    out_len := 0;

    block_count := 1;
    BlockIO.Read( in_f, block_count, block_data, block_size );

    WHILE BlockIO.Done() DO

      DoDump;

      block_count := block_count + 1;
      BlockIO.Read( in_f, block_count, block_data, block_size );
    END;

    BlockIO.Close( in_f );

  ELSE
    TextIO.WriteString( TextIO.tty, 'output file not opened' ); TextIO.WriteLn( TextIO.tty );
  END;
ELSE
  TextIO.WriteString( TextIO.tty, 'file not opened' ); TextIO.WriteLn( TextIO.tty );
END;

END UnStream.
