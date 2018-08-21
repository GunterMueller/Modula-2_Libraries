MODULE Dump;

(*
 show an example of using BlockIO,
 do a file dump
*)
(* J. Andrea, July 1991 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn, ReadString, ReadLn;
FROM BlockIO IMPORT BlockFile, OpenIn, Read, Close, Done;

VAR
  f           :BlockFile;
  filename    :ARRAY [0..80] OF CHAR;
  block       :ARRAY [0..511] OF CHAR;
  count, size :CARDINAL;

  (* ----------------------------------------------------------- *)
  PROCEDURE DoDump;
  VAR
     i, j, n :CARDINAL;
  BEGIN

    i := 0;
    WHILE i < size + 20 DO
       IF i + 20 > size THEN
         n := size - i - 1;
       ELSE
          n := 20;
       END;
 
       (* this first line of the dump is the displayable characters *)
       FOR j := i TO i + n DO
          WriteString( '  ' );
          IF ( block[j] >= ' ' ) & ( block[j] <= '~' ) THEN
            WriteString( block[j] );
          ELSE
            WriteString( '.' );
          END;
       END;
       WriteLn;

       (* this second line is the ascii values *)
       FOR j := i TO i + n DO
          WriteCard( ORD( block[j] ), 3 );
       END;
       WriteLn;

       i := i + 20;
     END;

  END DoDump;

BEGIN (* Test *)

WriteString( ' input file ? ' );
ReadString( filename ); ReadLn;

OpenIn( f, filename );

IF Done() THEN

  count := 1;
  Read( f, count, block, size );

  WHILE Done() DO
    WriteCard( count, 1 ); WriteString( ' block = ' );
    WriteCard( size, 1 );  WriteString( ' bytes.' ); WriteLn;

    DoDump;

    count := count + 1;
    Read( f, count, block, size );
  END;

  Close( f );

ELSE
  WriteString( 'file not opened' ); WriteLn;
END;

END Dump.
