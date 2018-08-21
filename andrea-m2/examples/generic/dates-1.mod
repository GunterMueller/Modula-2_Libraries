MODULE TestDates;

FROM InOut IMPORT WriteString, WriteLn, ReadString, WriteCard;
FROM DateOperations IMPORT Date, StringToDate, DateToString,
                           PhaseOfMoon, DayOfWeek, Today;
                          
VAR
   string, name :ARRAY [1..30] OF CHAR;
   date :Date;
   
   PROCEDURE Perform( date :Date );
   VAR
     continue :BOOLEAN;

   BEGIN
  
      IF date.true THEN
        WriteString( 'thats a valid date, lets continue' );
        continue := TRUE;
      ELSE
        WriteString( 'thats NOT a valid date !' );
        continue := FALSE;
      END;
      WriteLn;

      WriteString( 'day=' ); WriteCard( date.day, 0 ); WriteLn;
      WriteString( 'month=' ); WriteCard( date.month, 0 ); WriteLn;
      WriteString( 'year=' ); WriteCard( date.year, 0 ); WriteLn;
        
      IF continue THEN
        DateToString( date, string );
        WriteString( 'shows as ' ); WriteString( string ); WriteLn;
  
        PhaseOfMoon( date, name );
        WriteString( 'phase of moon =' ); WriteString( name ); WriteLn;
      
        DayOfWeek( date, name );
        WriteString( 'day of week   =' ); WriteString( name ); WriteLn;
      END;

   END Perform;

BEGIN
  
  Today( date );
  DateToString( date, string );
  WriteString( 'today is ' ); WriteString( string ); WriteLn;
  Perform( date );
  
  WriteLn; WriteLn;
  WriteString( 'now you pick a date !' ); WriteLn;

  WriteString( 'date ? ' ); ReadString( string ); WriteLn;
  WriteString( 'you typed in date=' ); WriteString( string ); WriteLn;
  StringToDate( string, date );
  Perform( date );

END TestDates.
