MODULE TestBase3;

(* Test the Base3 module *)
(* John Andrea, Nov.18/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn;
FROM Base3 IMPORT ToBase3, FromBase3;
FROM StringOperations IMPORT Equal;

CONST
  max_known = 42;

VAR
  string3 :ARRAY [0..30] OF CHAR;
  i, j :CARDINAL;
  ok, errors :BOOLEAN;

  known10 :ARRAY [1..max_known] OF CARDINAL;
  known3  :ARRAY [1..max_known] OF ARRAY [0..30] OF CHAR;
  
BEGIN

known10[1] := 0;
known3[1]  := '0';
known10[2] := 1;
known3[2]  := '1';
known10[3] := 2;
known3[3]  := '2';
known10[4] := 3;
known3[4]  := '10';
known10[5] := 4;
known3[5]  := '11';
known10[6] := 8;
known3[6]  := '22';
known10[7] := 9;
known3[7]  := '100';
known10[8] := 10;
known3[8]  := '101';
known10[9] := 11;
known3[9]  := '102';
known10[10] := 17;
known3[10]  := '122';
known10[11] := 18;
known3[11]  := '200';
known10[12] := 19;
known3[12]  := '201';
known10[13] := 27;
known3[13]  := '1000';
known10[14] := 28;
known3[14]  := '1001';
known10[15] := 29;
known3[15]  := '1002';
known10[16] := 47;
known3[16]  := '1202';
known10[17] := 48;
known3[17]  := '1210';
known10[18] := 79;
known3[18]  := '2221';
known10[19] := 80;
known3[19]  := '2222';
known10[20] := 81;
known3[20]  := '10000';
known10[21] := 82;
known3[21]  := '10001';
known10[22] := 99;
known3[22]  := '10200';
known10[23] := 100;
known3[23]  := '10201';
known10[24] := 101;
known3[24]  := '10202';
known10[25] := 126;
known3[25]  := '11200';
known10[26] := 161;
known3[26]  := '12222';
known10[27] := 162;
known3[27]  := '20000';
known10[28] := 163;
known3[28]  := '20001';
known10[29] := 216;
known3[29]  := '22000';
known10[30] := 242;
known3[30]  := '22222';
known10[31] := 243;
known3[31]  := '100000';
known10[32] := 244;
known3[32]  := '100001';
known10[33] := 406;
known3[33]  := '120001';
known10[34] := 485;
known3[34]  := '122222';
known10[35] := 486;
known3[35]  := '200000';
known10[36] := 487;
known3[36]  := '200001';
known10[37] := 999;
known3[37]  := '1101000';
known10[38] := 1000;
known3[38]  := '1101001';
known10[39] := 1458;
known3[39]  := '2000000';
known10[40] := 1459;
known3[40]  := '2000001';
known10[41] := 1958;
known3[41]  := '2200112';
known10[42] := 2000;
known3[42]  := '2202002';

  errors := FALSE;

  FOR i := 1 TO max_known DO

    ToBase3( known10[i], string3, ok );
    IF NOT Equal( string3, known3[i] ) THEN
      errors := TRUE;
      WriteCard( i, 0 );
      WriteString( '   base 3 = ' ); WriteString( string3 );
      WriteString( ' should be = ' ); WriteString( known3[i] );
      WriteLn;
    END;

    FromBase3( known3[i], j, ok );
    IF known10[i] # j THEN
      errors := TRUE;
      WriteString( known3[i] );
      WriteString( '  base 10 = ' ); WriteCard( j, 0 );
      WriteString( ' should be = ' ); WriteCard( known10[i], 0 );
      WriteLn;
    END;
  END;

  IF NOT errors THEN
    WriteString( 'all ok' ); WriteLn;
  END;

END TestBase3.
