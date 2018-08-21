MODULE TestDirFiles2;

(* test direct access files *)
(* J. Andrea, Aug.12/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn;

FROM Randomly IMPORT Choose_1_To_N;

FROM DirectAccessFiles IMPORT DirectFile,   DirFileDone, DirFileStatus,
                              DirectCreate,
                              DirectWrite,  DirectRead,  DirectClose;


CONST
   n_rec = 10;
   width = 20;

TYPE
   String = ARRAY [0..width] OF CHAR;

VAR
   test_string   :String;
   i             :CARDINAL;
   file          :DirectFile;

BEGIN (* TestDirFiles *)

WriteLn;

WriteString( 'creating a file' ); WriteLn;

DirectCreate( file, 'TESTDIR.TMP', n_rec );

IF DirFileDone THEN

  WriteString( 'filling the file with junk' ); WriteLn;

  test_string := 'JUNKJUNKJUNKJUNKJUNK';

  FOR i := 1 TO n_rec DO
    DirectWrite( file, i, test_string, width );
  END;

  WriteString( 'now choose a record at random, and put something there' );
  WriteLn;

  i := Choose_1_To_N( n_rec );

  WriteString( 'record chosen = ' ); WriteCard( i, 0 ); WriteLn;

  test_string := '--here is a record--';
  DirectWrite( file, i, test_string, width );

  IF DirFileDone THEN

    WriteString( 'read back to whole file in sequence' ); WriteLn;
    WriteString( 'and display the file to see the non junk' ); WriteLn;

    WriteLn;

    FOR i := 1 TO n_rec DO
      DirectRead( file, i, test_string, width );
      WriteString( test_string ); WriteLn;
    END;

  ELSE
    WriteString('didnt write to file'); WriteLn;
    DirFileStatus;
  END;

  DirectClose( file );
 
ELSE
   WriteString('didnt create file'); WriteLn;
   DirFileStatus;
END;

WriteLn; WriteString('all done'); WriteLn; WriteLn;

END TestDirFiles2.
