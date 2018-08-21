MODULE TestTimer;
    
(* an example of how to time a program *)
(* J. Andrea, Aug.20/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn, ReadCard, ReadLn;

FROM DateTime IMPORT TimeType, Hours, Minutes, Seconds, Hundredths;
FROM Measures IMPORT InitTimer, GetTimer, FreeTimer;

VAR
  elapsed :TimeType;
  cputime, bufio, dirio, pagefaults :CARDINAL;
  hundred :CARDINAL;
  n, i    :CARDINAL;
  x       :REAL;
  
BEGIN

    WriteString( ' how many loops (0 to end) ? ' );
    ReadCard( n ); ReadLn;

    WHILE n # 0 DO

      InitTimer;

      WriteCard( n, 0 ); WriteString( ' loops:' ); WriteLn;

      x := 0.0;
      FOR i := 1 TO n DO
        x := x + 0.5;
        x := x / 12.0;
        x := x + 0.5;
      END;

      GetTimer( elapsed, cputime, bufio, dirio, pagefaults );

      hundred := Hours( elapsed );
      hundred := hundred * 60  + Minutes( elapsed );
      hundred := hundred * 60  + Seconds( elapsed );
      hundred := hundred * 100 + Hundredths( elapsed );

      WriteString( 'took cpu time of '); WriteCard( cputime, 0 );
      WriteString( ' units of 10 milliseconds' ); WriteLn;
      
      WriteString( 'and real time of '); WriteCard( hundred, 0 );
      WriteString( ' hundredths of seconds.' ); WriteLn;

      WriteLn; WriteString( ' how many loops ? ' );
      ReadCard( n ); ReadLn;
   END;

   FreeTimer;

END TestTimer.
