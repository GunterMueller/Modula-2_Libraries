MODULE TestDirFiles;

(* test direct access files *)
(* revitalized, J. Andrea, Aug.12/91 *)
(* J. Andrea, 1984 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteLn;

FROM DirectAccessFiles IMPORT DirectFile,   DirFileDone, DirFileStatus,
                              DirectCreate, DirectOpen,
                              DirectWrite,  DirectRead,  DirectClose;


TYPE
   String = ARRAY [0..80] OF CHAR;

VAR
   test_string   :String;
   record_number :CARDINAL;
   unit_1        :DirectFile;

BEGIN (* TestDirFiles *)

WriteLn;

DirectCreate( unit_1, 'TESTDIR.TMP', 10 );
IF NOT DirFileDone THEN
   WriteString('didnt create file'); WriteLn;
   DirFileStatus;
   HALT;
END;

test_string := 'this here is a string';

DirectWrite( unit_1, 5, test_string, 11 );
IF NOT DirFileDone THEN
   WriteString('didnt write to file'); WriteLn;
   DirFileStatus;
   DirectClose(unit_1);
   HALT;
END;

test_string := '_____________________________';

DirectRead( unit_1, 5, test_string, 11 );
IF NOT DirFileDone THEN
   WriteString('didnt read from file'); WriteLn;
   DirFileStatus;
   DirectClose(unit_1);
   HALT;
END;

WriteString(test_string); WriteLn;

DirectClose(unit_1);

WriteString('all done'); WriteLn; WriteLn;

END TestDirFiles.
