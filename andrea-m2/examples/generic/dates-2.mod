MODULE TestDates;

FROM InOut IMPORT WriteString, WriteLn, ReadString, WriteCard, ReadCard;
FROM DateOperations IMPORT Date, StringToDate, DateToString,
                      AddDays, SubtractDays;
 
TYPE
  DateProc = PROCEDURE( Date, CARDINAL, VAR Date );
   
   (* --------------------------------- *)
   PROCEDURE Run( sum :CHAR; DayProc :DateProc );
   VAR
   
     string :ARRAY [1..30] OF CHAR;
     date1, date2 :Date;
     x :CARDINAL;
     
   BEGIN
     WriteString( 'date (stop with q) ? ' ); ReadString( string ); WriteLn;
     WHILE CAP( string[1] ) # 'Q' DO
        StringToDate( string, date1 );
        
        WriteString( 'how many days ? ' ); ReadCard( x ); WriteLn;
        DayProc( date1, x, date2 );
        
        DateToString( date1, string );
        WriteString( string ); WriteString( ' ' ); WriteString( sum );
        WriteString( ' ' ); WriteCard( x, 0 ); WriteString( ' days = ' );
        DateToString( date2, string );
        WriteString( string ); WriteLn;
           
        WriteString( 'date (stop with q) ? ' ); ReadString( string ); WriteLn;
     END;
   END Run;
   
BEGIN
  
  WriteString( 'first add some days' ); WriteLn;
  Run( '+', AddDays );

  WriteLn;  
  WriteString( 'now subtract some days' ); WriteLn;
  Run( '-', SubtractDays );
   
END TestDates.
