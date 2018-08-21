IMPLEMENTATION MODULE DateOperations;

(* V1.1, J. Andrea, Sept.8/92 -correct today output if day < 10 *)
(* V1.0, J. Andrea, Aug.16/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM StringOperations IMPORT Length, Assign;
FROM CommonInputOutputProcedures IMPORT LIB$DATE_TIME;

CONST
  min_day_number =  73355; (* Jan.1/1901  *)
  max_day_number = 146038; (* Dec.31/2099 *)
  date_length = 11;    (* size of a vms style date string dd-mmm-yyyy *)
  
VAR
  months :ARRAY [1..12] OF ARRAY [0..2] OF CHAR;
  days   :ARRAY [1..12] OF CARDINAL;
  digits :ARRAY [0..9] OF CHAR;
  false_date :Date;
      
   (* --------------------------------------------------------- *)
   PROCEDURE Today( VAR date :Date );
   VAR
     today :ARRAY [1..date_length] OF CHAR;
     status :CARDINAL;
   BEGIN

     status := LIB$DATE_TIME( today );

     IF today[1] = ' ' THEN
       today[1] := '0';
     END;

     StringToDate( today, date );

   END Today;
   
   (* --------------------------------------------------------- *)
   PROCEDURE MakeDate( day, month, year :CARDINAL; VAR date :Date );
   BEGIN
   
     date.true := TRUE;
     date.day  := day;  date.month := month;  date.year := year;
      
     IF ( date.day = 0 ) OR ( date.day > 31 ) THEN
       date.true := FALSE;
     ELSE
         
       IF date.month > 12 THEN
         date.true := FALSE;
       ELSE
          
         IF ( date.year < 1901 ) OR ( date.year > 2099 ) THEN
           date.true := FALSE;
         ELSE
            
           (* test for day in month *)
           IF date.day > days[date.month] THEN
             IF date.month # 2 THEN
               date.true := FALSE;
             ELSE
               IF NOT LeapYear( date.year ) THEN
                 date.true := FALSE;
               END; 
             END;
           END;
              
           (* precompute some stuff *)
           IF date.true THEN
             date.number := DayNumber( date );
           END;
              
          END;
        END;
      END;
    END MakeDate;
       
   (* --------------------------------------------------------- *)
   PROCEDURE StringToDate( string :ARRAY OF CHAR; VAR date :Date );
   VAR
      i, l, p :CARDINAL;
      mmm     :ARRAY [0..2] OF CHAR;
      ok      :BOOLEAN;
      d, m, y :CARDINAL;
      
      (* --------------------------- *)
      PROCEDURE Digit( c :CHAR ) :CARDINAL;
      BEGIN
         IF ( c >= '1' ) & ( c <= '9' ) THEN
           RETURN ORD( c ) - ORD( '0' );
         ELSE
           RETURN 0;
         END;
      END Digit;
      
      (* ------------------------------------- *)
      PROCEDURE MatchMonth( i :CARDINAL ) :BOOLEAN;
      BEGIN
        IF mmm[0] = months[i][0] THEN
          IF mmm[1] = months[i][1] THEN
            RETURN mmm[2] = months[i][2];
          ELSE
            RETURN FALSE
          END;
        ELSE
          RETURN FALSE
        END;
      END MatchMonth;
      
   BEGIN
      
     date.true := FALSE;
     
     l := Length( string );
     IF l >= date_length - 1 THEN
       (* its got to be at least 10 chars d-mmm-yyy *)
     
       (* parse out the day *)
       
       d := 0;  ok := TRUE;
       p := 0;
       WHILE ( p < l ) & ok DO
         IF ( string[p] >= '0' ) & ( string[p] <= '9' ) THEN
           d := d * 10 + Digit( string[p] );
           p := p + 1;
         ELSE
           ok := FALSE;
         END;
       END;

       (* need a dash here *)
       IF string[p] = '-' THEN
         
         p := p + 1;   (* skip the dash *)
         
         (* take the next 3 chars as the month *)
         
         FOR i := 0 TO 2 DO
           mmm[i] := CAP(string[p]);  p := p + 1;
         END;
        
         (* need a dash here too *)
         IF string[p] = '-' THEN
         
           (* try to match the month name *)
      
           m := 1;
           WHILE ( m <= 12 ) & NOT MatchMonth( m ) DO
             m := m + 1;
           END;
           
           IF m < 13 THEN
           
             p := p + 1; (* skip the dash *)
          
             (* take the rest of the string as a year *)
             ok := TRUE;
             y  := 0;
             WHILE ( p < l ) & ok DO
               IF ( string[p] >= '0' ) & ( string[p] <= '9' ) THEN
                 y := y * 10 + Digit( string[p] );
                 p := p + 1;
               ELSE
                 ok := FALSE;
               END;
             END;
            
             IF ok THEN
               MakeDate( d, m, y, date );
             END;
           END;
         END;
        END;
      END;
    END StringToDate;
    
    (* ------------------------------------------------- *)
    PROCEDURE DateToString( date :Date; VAR string :ARRAY OF CHAR );
    VAR
      d, y :CARDINAL;
      
        (* -------------------------------- *)
        PROCEDURE Digit( x :CARDINAL ) :CHAR;
        BEGIN
           IF x < 10 THEN
             RETURN digits[x];
           ELSE
             RETURN ' ';
           END;
        END Digit;
        
    BEGIN
      IF date.true THEN
      
        (* make sure the string is long enough to hold the date *)
        IF HIGH( string ) + 1 >= date_length THEN
        
          d := date.day DIV 10;
          string[0] := Digit( d );
          
          d := date.day - d * 10;
          string[1] := Digit( d );
          
          string[2] := '-';
          
          string[3] := months[date.month][0];
          string[4] := months[date.month][1];
          string[5] := months[date.month][2];

          string[6] := '-';
          
          y := date.year;
          d := y DIV 1000;
          string[7] := Digit( d );
          y := y - d * 1000;
          d := y DIV 100;
          string[8] := Digit( d );
          y := y - d * 100;
          d := y DIV 10;
          string[9] := Digit( d );
          d := y - d * 10;
          string[10] := Digit( d );  (* offset by zero, so this is 11th char *)
          
          IF HIGH( string ) + 1 > date_length THEN
            string[11] := 0C;  (* string is offset by zero *)
          END;
          
        ELSE
          string[0] := 0C;
        END;
      ELSE
        string[0] := 0C;
      END;
    END DateToString;
   
   (* -------------------------------------------------- *)
   PROCEDURE LeapYear( year :CARDINAL ) :BOOLEAN;
   BEGIN
      IF year MOD 4 = 0 THEN
        IF year MOD 100 = 0 THEN
          RETURN FALSE;
        ELSE
          RETURN TRUE;
        END;
      ELSE
        RETURN FALSE;
      END;
   END LeapYear;
   
   (* -------------------------------------------------- *)
   PROCEDURE DayOfYear( date :Date ) :CARDINAL;
   VAR i, sum :CARDINAL;
   BEGIN
      IF date.true THEN
      
        sum := date.day;
        FOR i := 1 TO date.month - 1 DO
          sum := sum + days[i];
        END;
      
        IF LeapYear( date.year ) THEN
          IF ( date.month > 2 ) OR ( ( date.month = 2 ) & ( date.day = 28 ) ) THEN
            sum := sum + 1;
          END;
        END;
      ELSE
        sum := 0;
      END;
      
      RETURN sum;
   END DayOfYear;
     
   (* -------------------------------------------------- *)
   PROCEDURE DayOfWeek( date :Date; VAR day_name :ARRAY OF CHAR );
   BEGIN
    
    IF date.true THEN
                   
        CASE date.number MOD 7 OF
       0 :Assign( 'Sunday', day_name );
     | 1 :Assign( 'Monday', day_name );
     | 2 :Assign( 'Tuesday', day_name );
     | 3 :Assign( 'Wednesday', day_name );
     | 4 :Assign( 'Thursday', day_name );
     | 5 :Assign( 'Friday', day_name );
     | 6 :Assign( 'Saturday', day_name );
       END; (* case *)
 
     ELSE
       day_name[0] := 0C;
     END;
     
   END DayOfWeek;
   
   (* -------------------------------------------------- *)
   PROCEDURE DayNumber( date :Date ) :INTEGER;
   (* algotithm from Hewlett Packard HP-25 Calculator Applications Programs *)
   (* valid for Mar.1/1700 to Feb.28/2100, but simplified here for
      1-jan-1901 to 31-dec-2099 *)
   VAR
      sum, y, m, d :INTEGER;
   BEGIN
    
    IF date.true THEN
       
       IF date.month <= 2 THEN
         y := date.year - 1;
         m := date.month + 13;
       ELSE
         y := date.year;
         m := date.month + 1;
       END;
       d := date.day;
       
       sum := 365 * y + y DIV 4;     (*  365.25 times year  *)
       
       sum := sum + 30 * m + ( 6 * m ) DIV 10;  (*  30.6 times month  *)
       
       sum := sum + d - 621049;

       RETURN sum;
     ELSE
       RETURN 0;
     END;
     
   END DayNumber;
   
   (* ------------------------------------------------------ *)
   PROCEDURE DaysBetween( date1, date2 :Date ) :INTEGER;
   BEGIN
     IF date1.true & date2.true THEN
       RETURN date1.number - date2.number;
     ELSE
       RETURN 0;
     END;
   END DaysBetween;
   
   (* ------------------------------------------------------ *)
   PROCEDURE AddDays( in_date :Date; delta_days :CARDINAL;
                      VAR out_date :Date );
   VAR
     d, m, y, i :CARDINAL;
     long_days  :INTEGER;
   BEGIN
   
     IF in_date.true THEN
       IF delta_days = 0 THEN
         out_date := in_date;
       ELSE

         long_days := delta_days;       
         IF in_date.number + long_days > max_day_number THEN
           out_date := false_date;
         ELSE
         
           (* do it the hard way, just add a day at a time *)
           d := in_date.day;   m := in_date.month;   y := in_date.year;

           FOR i := 1 TO delta_days DO
             d := d + 1;
             IF d > days[m] THEN
               IF NOT ( LeapYear( y ) & ( m = 2 ) & ( d = 29 ) ) THEN
                 d := 1;
                 m := m + 1;
                 IF m > 12 THEN
                   m := 1;
                   y := y + 1;
                 END;
               END;
             END;
           END;
           
           MakeDate( d, m, y, out_date );
           
         END; 
       END;
     ELSE
       out_date := false_date;
     END;
   END AddDays;
   
   (* ------------------------------------------------------ *)
   PROCEDURE SubtractDays( in_date :Date; delta_days :CARDINAL;
                      VAR out_date :Date );
   VAR
     d, m, y, i :CARDINAL;
     long_days  :INTEGER;
   BEGIN
     IF in_date.true THEN
       IF delta_days = 0 THEN
         out_date := in_date;
       ELSE
       
         long_days := delta_days;
         IF in_date.number - long_days < min_day_number THEN
           out_date := false_date;
         ELSE
         
           (* do it the hard way, just subtract a day at a time *)
           d := in_date.day;   m := in_date.month;   y := in_date.year;

           FOR i := 1 TO delta_days DO
             d := d - 1;
             IF d = 0 THEN
               m := m - 1;
               IF m = 0 THEN
                 m := 12;
                 y := y - 1;
               END;
               IF LeapYear( y ) & ( m = 2 ) THEN
                 d := 29;
               ELSE
                 d := days[m];
               END;
             END;
           END;
           
           MakeDate( d, m, y, out_date );

         END; 
       END;
     ELSE
       out_date := false_date;
     END;
   END SubtractDays;
   
   (* -------------------------------------------------- *)
   PROCEDURE Compare( date1, date2 :Date ) :CHAR;
   VAR c :CHAR;
   BEGIN
      IF date1.true & date2.true THEN
        IF date1.number = date2.number THEN
          c := '=';
        ELSIF date1.number > date2.number THEN
          c := '>';
        ELSE
          c := '<';
        END;
      ELSE
        c := 0C;
      END;
      RETURN c;
   END Compare;
   
   
   (* -------------------------------------------------- *)
   PROCEDURE PhaseOfMoon( date :Date; VAR phase_name :ARRAY OF CHAR );
   (* algorithm from TECO, or so says some people on the net *)
   CONST
     multiplier = 64;
   VAR
     century, golden, epact, period, eighth, sixteenth, phase :CARDINAL;
   BEGIN
    
     IF date.true THEN

       century := date.year DIV 100 + 1;
       golden  := date.year - ( (date.year DIV 19) * 19 ) + 1;
       
       epact   := golden * 11 + 20 + ( (8 * century + 5) DIV 25 - 5) -
                  (3 * century DIV 4 - 12 );
       epact   := epact - ( (epact DIV 30) * 30 );
       IF ( epact = 25 ) & ( golden > 11 ) THEN epact := 26; END;
       IF epact = 24 THEN epact := 25; END;

       period := 29 * multiplier + multiplier DIV 2;
       eighth := period DIV 8;
       sixteenth := eighth DIV 2;
       
       phase := ( DayOfYear( date ) + epact - 1 ) * multiplier + sixteenth;
       phase := phase - ( (phase DIV period) * period);
       phase := phase DIV eighth;
       phase := phase MOD 8;
       
       CASE phase OF
       0 :Assign( 'New', phase_name );
     | 1 :Assign( 'Waxing Crescent', phase_name );
     | 2 :Assign( 'First Quarter', phase_name );
     | 3 :Assign( 'Waxing Gibbous', phase_name );
     | 4 :Assign( 'Full', phase_name );
     | 5 :Assign( 'Waning Gibbous', phase_name );
     | 6 :Assign( 'Last Quarter', phase_name );
     | 7 :Assign( 'Waning Crescent', phase_name );
       ELSE
          phase_name[0] := 0C;
       END; (* case *)
       
     ELSE
       phase_name[0] := 0C;
     END;
     
   END PhaseOfMoon;
 
BEGIN
 
  months[1]  := 'JAN'; months[2]  := 'FEB'; months[3]  := 'MAR';
  months[4]  := 'APR'; months[5]  := 'MAY'; months[6]  := 'JUN';
  months[7]  := 'JUL'; months[8]  := 'AUG'; months[9]  := 'SEP';
  months[10] := 'OCT'; months[11] := 'NOV'; months[12] := 'DEC';
  days[1]  := 31;      days[2]  := 28;      days[3]  := 31;
  days[4]  := 30;      days[5]  := 31;      days[6]  := 30;
  days[7]  := 31;      days[8]  := 31;      days[9]  := 30;
  days[10] := 31;      days[11] := 30;      days[12] := 31;
  
  false_date.true := FALSE;
  false_date.day  := 0;  false_date.month := 0; false_date.year := 0;
  false_date.number := 0;
  
  Assign( '0123456789', digits );

END DateOperations.
