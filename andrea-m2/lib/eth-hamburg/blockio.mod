IMPLEMENTATION MODULE BlockIO;

(* J. Andrea, 1991, see the definition file for details *)
(* This code may be freely used and distributed, it may not be sold. *)

IMPORT RMSFiles;

FROM SYSTEM IMPORT BYTE, ADR, SIZE;
FROM RMSDefinitions IMPORT RMS$_EOF,RMS$_NORMAL,RMS$_CUR;
FROM RMS IMPORT FAB, NAM, RAB,
                ORG$SEQ,
                FACset, FOPset, RATset, RFMset, SHRset,
                FACtype, FOPtype, ORGtype, RATtype, RFMtype, SHRtype,
                ROPtype, ROPset;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

CONST
    MaxRecordSize = 512;

TYPE
    RecordSize = [0..MaxRecordSize];

    BlockFile = POINTER TO FileBlock;

    FileBlock = RECORD
                  fab: FAB;
                  nam: NAM;
                  rab: RAB;
                  trn: BOOLEAN;
                  fna: ARRAY [0..127] OF CHAR;
                END;

VAR
  status :CARDINAL;
  lf     :FileBlock;
  op     :ARRAY [0..9] OF CHAR; (* used by ShowStatus *)


(* ------------------------------------------------------------------ *)
PROCEDURE Done(): BOOLEAN;
BEGIN
    RETURN ODD(status);
END Done;


(* ------------------------------------------------------------------ *)
PROCEDURE InitFileBlock( f :BlockFile );
BEGIN
   f^.trn := FALSE;
END InitFileBlock;

(* ------------------------------------------------------------------ *)
PROCEDURE OpenIn( VAR f :BlockFile; filename: ARRAY OF CHAR );
BEGIN

   NEW(f);

   RMSFiles.Open( f^.fab, f^.nam, filename, FACset{FAC$BRO,FAC$GET}, FOPset{},
                  SHRset{}, f^.fna, status);

   IF ODD(status) THEN
     RMSFiles.Connect( f^.fab, f^.rab, ROPset{ROP$BIO}, status );
     InitFileBlock(f);
   END;

   IF NOT ODD(status) THEN
      lf := f^;
      op := "Open";
      DISPOSE(f)
   END;

END OpenIn;

(* ------------------------------------------------------------------ *)
PROCEDURE Close( VAR f :BlockFile);
BEGIN
  IF f <> NIL THEN
    RMSFiles.Disconnect( f^.rab, status );
    RMSFiles.Close( f^.fab, status );
    IF NOT ODD(status) THEN
      lf := f^;
      op := "Close";
    END;
    DISPOSE(f);
  ELSE
    status := RMS$_NORMAL
  END;
END Close;

(* ------------------------------------------------------------------ *)
PROCEDURE OpenOut( VAR f :BlockFile; filename: ARRAY OF CHAR );
          (* create and open a new file *)
VAR
  rfm: RFMtype;
  mrs: CARDINAL;

BEGIN

  rfm := RFM$FIX;
  mrs := MaxRecordSize;

  NEW(f);

  RMSFiles.Create( f^.fab, f^.nam, filename,
                   FACset{FAC$BRO,FAC$PUT,FAC$TRN}, FOPset{},
                   SHRset{}, RATset{}, rfm, ORG$SEQ, mrs, 0, 0,
                   f^.fna, status );

  IF ODD(status) THEN
    RMSFiles.Connect( f^.fab, f^.rab, ROPset{}, status );
    InitFileBlock(f);
  END;

  IF NOT ODD(status) THEN
    lf := f^;
    op := "Create";
    DISPOSE(f)
  END;

END OpenOut;


(* ------------------------------------------------------------------ *)
PROCEDURE Read( VAR f     :BlockFile; count :CARDINAL;
                VAR block :ARRAY OF BYTE; VAR size :CARDINAL );
BEGIN

  size := SIZE( block );

  RMSFiles.Read( f^.rab, count, ADR(block), size, status );

  size := CARDINAL( f^.rab.RSZ );

  IF NOT ODD(status) THEN
    IF status # RMS$_EOF THEN
      RMSFiles.SignalIOStatus( "Read", f^.fna, f^.rab.STS, f^.rab.STV);
    END;
  END;
END Read;

(* ------------------------------------------------------------------ *)
PROCEDURE Write( VAR f :BlockFile; count :CARDINAL;
                 block :ARRAY OF BYTE; size :CARDINAL );
          (* write the count'th block *)
BEGIN

  IF f^.trn THEN

    RMSFiles.Truncate( f^.rab, status );

    IF status = RMS$_CUR THEN (*no current record*)
      RMSFiles.Find( f^.rab,status);
      IF NOT ODD(status) THEN
        IF status = RMS$_EOF THEN
          status := RMS$_NORMAL;
        ELSE
           RMSFiles.SignalIOStatus( "Find",f^.fna,f^.rab.STS,f^.rab.STV);
        END;
      ELSE
        RMSFiles.Truncate(f^.rab, status);
      END;
    END;

    IF NOT ODD(status) THEN
      RMSFiles.SignalIOStatus( "Truncate on Put",f^.fna,f^.rab.STS,f^.rab.STV)
    END;
    f^.trn := FALSE;
  END;

  IF size = 0 THEN
    size := SIZE( block );
  END;

  RMSFiles.Write( f^.rab, count, ADR(block), size, status );
  
  IF NOT ODD(status) THEN
    RMSFiles.SignalIOStatus( "Put", f^.fna, f^.rab.STS, f^.rab.STV )
  END;

END Write;

(* ------------------------------------------------------------------ *)
PROCEDURE Status(): CARDINAL;
BEGIN
    RETURN status;
END Status;


(* ------------------------------------------------------------------ *)
PROCEDURE ShowStatus;
BEGIN
    IF status = lf.fab.STS THEN
      RMSFiles.ShowIOStatus( op, lf.fna, status, lf.fab.STV );
    ELSIF status = lf.rab.STS THEN
      RMSFiles.ShowIOStatus( op, lf.fna, status, lf.rab.STV );
    ELSE
      RMSFiles.ShowIOStatus( "", "", status, 0 );
    END
END ShowStatus;

BEGIN (* BlockIO *)
END BlockIO.
