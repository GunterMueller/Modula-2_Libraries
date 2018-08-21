IMPLEMENTATION MODULE DirectAccessFiles;

(* Use direct access files - see the definition module for details *)
(* J. Andrea, Aug.8/91 - revitalized *)
(* J. Andrea, 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SYSTEM IMPORT BYTE, ADR;

FROM InOut IMPORT WriteLn, WriteString, WriteCard;

FROM FortranDirectAccess IMPORT For$DirCreate, For$DirOpen, For$DirClose,
                                For$DirRead, For$DirWrite;

CONST
    min_file = 10;   max_file = 90;   (* allow for 80 files in this range *)

TYPE
    FilesReady = ( available, notavailable );

VAR
    last_file      :DirectFile;
    status         :CARDINAL;
    file_slots     :ARRAY [min_file..max_file] OF FilesReady;

   (* -------------------------------------------------- *)
   PROCEDURE DirectCreate( VAR file :DirectFile;
                           name     :ARRAY OF CHAR;
                           maxrec   :CARDINAL );
   (* create a file, and leave it in an open state *)

   BEGIN (* DirectCreate *)

        (* find the first open file *)
        file := min_file;
        WHILE ( file <= max_file ) & ( file_slots[file] # available ) DO
           file := file + 1;
        END; (* while *)

        IF file > max_file THEN

           last_file := 0;   status := 1;

           DirFileDone := FALSE;

        ELSE

          last_file := file;

          For$DirCreate( file, name, maxrec, status );

          DirFileDone := status = 0;

          IF DirFileDone THEN
            file_slots[file] := notavailable
          END;

        END; (* if *)

   END DirectCreate;

   (* -------------------------------------------------- *)
   PROCEDURE DirectOpen( VAR file :DirectFile;
                         name     :ARRAY OF CHAR );

   BEGIN (* DirectOpen *)

        (* find the first open file *)
        file := min_file;
        WHILE ( file <= max_file ) & ( file_slots[file] # available ) DO
           file := file + 1;
        END; (* while *)

        IF file > max_file THEN

           last_file := 0;   status := 1;

           DirFileDone := FALSE;

        ELSE

          last_file := file;

          For$DirOpen( file, name, status );

          DirFileDone := status = 0;

          IF DirFileDone THEN
            file_slots[file] := notavailable;
          END;

        END; (* if *)

   END DirectOpen;

   (* -------------------------------------------------- *)
   PROCEDURE DirectClose( file :DirectFile );

   BEGIN (* DirectClose *)

      last_file := file;

      IF file_slots[file] = notavailable THEN

         For$DirClose( file, status );

         DirFileDone := status = 0;

         IF DirFileDone THEN
           file_slots[file] := available;
         END;

      ELSE
         DirFileDone := FALSE;
         status      := 29;
      END;

   END DirectClose;

   (* -------------------------------------------------- *)
   PROCEDURE DirectWrite( file          :DirectFile;
                          record_number :CARDINAL;
                          record        :ARRAY OF BYTE;
                          n_bytes       :CARDINAL );

   BEGIN (* DirectWrite *)

      last_file := file;

      IF file_slots[file] = notavailable THEN

         For$DirWrite( file, record_number, ADR(record[0]), n_bytes,
                      status );

         DirFileDone := status = 0;

      ELSE
         DirFileDone := FALSE;
         status      := 29;
      END;

   END DirectWrite;

   (* -------------------------------------------------- *)
   PROCEDURE DirectRead( file          :DirectFile;
                         record_number :CARDINAL;
                         VAR record    :ARRAY OF BYTE;
                         n_bytes       :CARDINAL );

   BEGIN (* DirectRead *)

      last_file := file;

      IF file_slots[file] = notavailable THEN

        For$DirRead( file, record_number, ADR(record[0]), n_bytes,
                     status );

        DirFileDone := status = 0;

      ELSE
         DirFileDone := FALSE;
         status      := 29;
      END;

   END DirectRead;

   (* -------------------------------------------------- *)
   PROCEDURE DirFileStatus;
   (* return the status, and the unit number of the last operation *)

   BEGIN (* DirFileStatus *)

      WriteString('File Unit  ='); WriteCard( last_file, 5 );   WriteLn;
      WriteString('I/O Status ='); WriteCard( status, 5 );      WriteLn;

   END DirFileStatus;

BEGIN (* DirectAccessFiles *)

DirFileDone := FALSE;

FOR last_file := min_file TO max_file DO
   file_slots[last_file] := available;
END; (* for *)

last_file := 0;

END DirectAccessFiles.
