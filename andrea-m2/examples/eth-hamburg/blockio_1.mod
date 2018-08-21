MODULE FileCopy;

(*
 show an example of using BlockIO,
 copy from one file to another,
 then use VMS DIFFERENCE to make sure they are the same
 use this to copy .EXE files because the file type created by the OpenOut
 procedure is the same as for .exe files
*)
(* J. Andrea, July 1991 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn, ReadString, ReadLn;
FROM BlockIO IMPORT BlockFile, OpenIn, OpenOut, Read, Write, Close, Done;

VAR
  inf, outf      :BlockFile;
  infile,outfile :ARRAY [0..80] OF CHAR;
  block          :ARRAY [0..511] OF CHAR;
  count, size    :CARDINAL;
  i, j, n        :CARDINAL;

BEGIN (* Test *)

WriteString( ' input file ? ' );
ReadString( infile ); ReadLn;

WriteString( ' output file ? ' );
ReadString( outfile ); ReadLn;

OpenIn( inf, infile );

IF Done() THEN

  OpenOut( outf, outfile );

  IF Done() THEN

    count := 1;
    Read( inf, count, block, size );

    WHILE Done() DO
      WriteCard( count, 1 ); WriteString( ' block = ' );
      WriteCard( size, 1 );  WriteString( ' bytes.' ); WriteLn;

      Write( outf, count, block, size );

      count := count + 1;
      Read( inf, count, block, size );
    END;

    Close( outf );

  ELSE
     WriteString( 'output file not opened' ); WriteLn;
  END;

  Close( inf );

ELSE
  WriteString( 'input file not opened' ); WriteLn;
END;

END FileCopy.
