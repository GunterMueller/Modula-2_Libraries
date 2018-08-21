MODULE TestBase2;

(* Test the Base2 module *)
(* John Andrea, Nov.18/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn;
FROM Base2 IMPORT ToBase2, FromBase2;
FROM StringOperations IMPORT Equal;

CONST
  max_known = 48;

VAR
  string2 :ARRAY [0..30] OF CHAR;
  i, j :CARDINAL;
  ok, errors :BOOLEAN;

  known10 :ARRAY [1..max_known] OF CARDINAL;
  known2  :ARRAY [1..max_known] OF ARRAY [0..30] OF CHAR;
  
BEGIN

known10[1] := 0;
known2[1]  := '0';
known10[2] := 1;
known2[2]  := '1';
known10[3] := 2;
known2[3]  := '10';
known10[4] := 3;
known2[4]  := '11';
known10[5] := 7;
known2[5]  := '111';
known10[6] := 8;
known2[6]  := '1000';
known10[7] := 9;
known2[7]  := '1001';
known10[8] := 15;
known2[8]  := '1111';
known10[9] := 16;
known2[9]  := '10000';
known10[10] := 27;
known2[10]  := '11011';
known10[11] := 28;
known2[11]  := '11100';
known10[12] := 29;
known2[12]  := '11101';
known10[13] := 30;
known2[13]  := '11110';
known10[14] := 31;
known2[14]  := '11111';
known10[15] := 32;
known2[15]  := '100000';
known10[16] := 63;
known2[16]  := '111111';
known10[17] := 64;
known2[17]  := '1000000';
known10[18] := 65;
known2[18]  := '1000001';
known10[19] := 99;
known2[19]  := '1100011';
known10[20] := 100;
known2[20]  := '1100100';
known10[21] := 101;
known2[21]  := '1100101';
known10[22] := 127;
known2[22]  := '1111111';
known10[23] := 128;
known2[23]  := '10000000';
known10[24] := 129;
known2[24]  := '10000001';
known10[25] := 199;
known2[25]  := '11000111';
known10[26] := 200;
known2[26]  := '11001000';
known10[27] := 201;
known2[27]  := '11001001';
known10[28] := 254;
known2[28]  := '11111110';
known10[29] := 255;
known2[29]  := '11111111';
known10[30] := 256;
known2[30]  := '100000000';
known10[31] := 257;
known2[31]  := '100000001';
known10[32] := 299;
known2[32]  := '100101011';
known10[33] := 300;
known2[33]  := '100101100';
known10[34] := 301;
known2[34]  := '100101101';
known10[35] := 500;
known2[35]  := '111110100';
known10[36] := 501;
known2[36]  := '111110101';
known10[37] := 511;
known2[37]  := '111111111';
known10[38] := 512;
known2[38]  := '1000000000';
known10[39] := 513;
known2[39]  := '1000000001';
known10[40] := 999;
known2[40]  := '1111100111';
known10[41] := 1000;
known2[41]  := '1111101000';
known10[42] := 1001;
known2[42]  := '1111101001';
known10[43] := 1023;
known2[43]  := '1111111111';
known10[44] := 1024;
known2[44]  := '10000000000';
known10[45] := 1025;
known2[45]  := '10000000001';
known10[46] := 1076;
known2[46]  := '10000110100';
known10[47] := 1999;
known2[47]  := '11111001111';
known10[48] := 2000;
known2[48]  := '11111010000';

  errors := FALSE;

  FOR i := 1 TO max_known DO
    ToBase2( known10[i], string2, ok );
    IF NOT Equal( string2, known2[i] ) THEN
      errors := TRUE;
      WriteCard( i, 0 );
      WriteString( '   base 2 = ' ); WriteString( string2 );
      WriteString( ' should be = ' ); WriteString( known2[i] );
      WriteLn;
    END;

    FromBase2( known2[i], j, ok );
    IF known10[i] # j THEN
      errors := TRUE;
      WriteString( known2[i] );
      WriteString( '  base 10 = ' ); WriteCard( j, 0 );
      WriteString( ' should be = ' ); WriteCard( known10[i], 0 );
      WriteLn;
    END;
  END;

  IF NOT errors THEN
    WriteString( 'all ok' ); WriteLn;
  END;

END TestBase2.
