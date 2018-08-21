IMPLEMENTATION MODULE Timer;
(* $S-, $R-, $T- *)
(* get time stats *)

(* John Andrea, Mar.27/92 *)
(* This code may NOT be sold, it may be freely used and distributed *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn;
FROM TimeDate IMPORT Time, GetTime;

CONST
  hours_per_day = 24;
  min_per_day   = hours_per_day * 60;

VAR
  start, now :Time;
  started    :BOOLEAN;
  digits     :ARRAY [0..9] OF CHAR;
  numbers    :ARRAY [0..99] OF ARRAY [0..1] OF CHAR;
  i, j, k    :CARDINAL;

(* -------------------------------------------------------- *)
PROCEDURE StartTimer;
BEGIN
  started := TRUE;
  GetTime( start );
END StartTimer;

(* -------------------------------------------------------- *)
PROCEDURE ShowTimer;

VAR
  dday, dmin, dmil, d, h, m, s, u :CARDINAL;

BEGIN
  GetTime( now );

  IF started THEN

    (* the time is returned from GetTime in this style *)
    (* .day is the day since 1900 *)
    (* .minute is # minutes since midnight *)
    (* .millisec is # millseconds past the minute *)

    dday := now.day - start.day;

    IF dday = 0 THEN
      dmin := now.minute - start.minute;
    ELSE
      dmin := min_per_day + now.minute - start.minute;
    END;

    d := dday;
    h := dmin DIV 60;
    m := dmin - h * 60;

    IF now.millisec >= start.millisec THEN
      dmil := now.millisec - start.millisec;
    ELSE

      (* watch the order of the subtractions, card max is 65534 *)
      dmil := start.millisec - now.millisec;
      dmil := 60000 - dmil;

      (* really one minute less than computed, propagate backwards *)
      IF m > 0 THEN
        m := m - 1;
      ELSE
        IF h > 0 THEN
          h := h - 1;
        ELSE
          d := d - 1;
        END;
      END;

    END;

    s := dmil DIV 1000;
    u := ( dmil - s * 1000 ) DIV 10;

    WriteString( 'elapsed time: ' );
                        WriteString( numbers[d] );
    WriteString( ' ' ); WriteString( numbers[h] );
    WriteString( ':' ); WriteString( numbers[m] );
    WriteString( ':' ); WriteString( numbers[s] );
    WriteString( '.' ); WriteString( numbers[u] );
    WriteLn;

  END;
END ShowTimer;

BEGIN

  started := FALSE;

  digits[0] := '0'; digits[1] := '1'; digits[2] := '2'; digits[3] := '3';
  digits[4] := '4'; digits[5] := '5'; digits[6] := '6'; digits[7] := '7';
  digits[8] := '8'; digits[9] := '9';

  (* generate 2 digit strings for numbers 0 to 99 as a lookup table *)

  k := 0;
  FOR i := 0 TO 9 DO
    FOR j := 0 TO 9 DO
      numbers[k][0] := digits[i];
      numbers[k][1] := digits[j];
       k := k + 1;
    END;
  END;

END Timer.
