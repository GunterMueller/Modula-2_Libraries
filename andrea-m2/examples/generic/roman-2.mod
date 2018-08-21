MODULE TestRoman;

(* Test the RomanNumerals procedures *)
(* J. Andrea, Jun.5/92 -better looking numerals *)
(* J. Andrea, 1992 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn;
FROM RomanNumerals IMPORT FromRoman, ToRoman;
FROM StringOperations IMPORT Equal;

VAR
   i, x       :CARDINAL;
   string     :ARRAY [0..132] OF CHAR;
   errors     :CARDINAL;
   known      :ARRAY [1..249] OF ARRAY [0..30] OF CHAR;

BEGIN

known[1] := "I";             known[2] := "II";            known[3] := "III";           
known[4] := "IV";            known[5] := "V";             known[6] := "VI";            
known[7] := "VII";           known[8] := "VIII";          known[9] := "IX";           
known[10] := "X";            known[11] := "XI";           known[12] := "XII";          
known[13] := "XIII";         known[14] := "XIV";          known[15] := "XV";           
known[16] := "XVI";          known[17] := "XVII";         known[18] := "XVIII";        
known[19] := "XIX";          known[20] := "XX";           known[21] := "XXI";          
known[22] := "XXII";         known[23] := "XXIII";        known[24] := "XXIV";         
known[25] := "XXV";          known[26] := "XXVI";         known[27] := "XXVII";        
known[28] := "XXVIII";       known[29] := "XXIX";         known[30] := "XXX";          
known[31] := "XXXI";         known[32] := "XXXII";        known[33] := "XXXIII";       
known[34] := "XXXIV";        known[35] := "XXXV";         known[36] := "XXXVI";        
known[37] := "XXXVII";       known[38] := "XXXVIII";      known[39] := "XXXIX";       
known[40] := "XL";           known[41] := "XLI";          known[42] := "XLII";         
known[43] := "XLIII";        known[44] := "XLIV";         known[45] := "XLV";          
known[46] := "XLVI";         known[47] := "XLVII";        known[48] := "XLVIII";       
known[49] := "XLIX";         known[50] := "L";            known[51] := "LI";           
known[52] := "LII";          known[53] := "LIII";         known[54] := "LIV";          
known[55] := "LV";           known[56] := "LVI";          known[57] := "LVII";         
known[58] := "LVIII";        known[59] := "LIX";          known[60] := "LX";           
known[61] := "LXI";          known[62] := "LXII";         known[63] := "LXIII";        
known[64] := "LXIV";         known[65] := "LXV";          known[66] := "LXVI";         
known[67] := "LXVII";        known[68] := "LXVIII";       known[69] := "LXIX";         
known[70] := "LXX";          known[71] := "LXXI";         known[72] := "LXXII";        
known[73] := "LXXIII";       known[74] := "LXXIV";        known[75] := "LXXV";         
known[76] := "LXXVI";        known[77] := "LXXVII";       known[78] := "LXXVIII";      
known[79] := "LXXIX";        known[80] := "LXXX";         known[81] := "LXXXI";        
known[82] := "LXXXII";       known[83] := "LXXXIII";      known[84] := "LXXXIV";       
known[85] := "LXXXV";        known[86] := "LXXXVI";       known[87] := "LXXXVII";      
known[88] := "LXXXVIII";     known[89] := "LXXXIX";       known[90] := "XC";          
known[91] := "XCI";          known[92] := "XCII";         known[93] := "XCIII";       
known[94] := "XCIV";         known[95] := "XCV";          known[96] := "XCVI";        
known[97] := "XCVII";        known[98] := "XCVIII";       known[99] := "XCIX";        
known[100] := "C";           known[101] := "CI";          known[102] := "CII";         
known[103] := "CIII";        known[104] := "CIV";         known[105] := "CV";          
known[106] := "CVI";         known[107] := "CVII";        known[108] := "CVIII";       
known[109] := "CIX";         known[110] := "CX";          known[111] := "CXI";         
known[112] := "CXII";        known[113] := "CXIII";       known[114] := "CXIV";        
known[115] := "CXV";         known[116] := "CXVI";        known[117] := "CXVII";       
known[118] := "CXVIII";      known[119] := "CXIX";        known[120] := "CXX";         
known[121] := "CXXI";        known[122] := "CXXII";       known[123] := "CXXIII";      
known[124] := "CXXIV";       known[125] := "CXXV";        known[126] := "CXXVI";       
known[127] := "CXXVII";      known[128] := "CXXVIII";     known[129] := "CXXIX";       
known[130] := "CXXX";        known[131] := "CXXXI";       known[132] := "CXXXII";      
known[133] := "CXXXIII";     known[134] := "CXXXIV";      known[135] := "CXXXV";       
known[136] := "CXXXVI";      known[137] := "CXXXVII";     known[138] := "CXXXVIII";    
known[139] := "CXXXIX";      known[140] := "CXL";         known[141] := "CXLI";        
known[142] := "CXLII";       known[143] := "CXLIII";      known[144] := "CXLIV";       
known[145] := "CXLV";        known[146] := "CXLVI";       known[147] := "CXLVII";      
known[148] := "CXLVIII";     known[149] := "CXLIX";       known[150] := "CL";          
known[151] := "CLI";         known[152] := "CLII";        known[153] := "CLIII";       
known[154] := "CLIV";        known[155] := "CLV";         known[156] := "CLVI";        
known[157] := "CLVII";       known[158] := "CLVIII";      known[159] := "CLIX";        
known[160] := "CLX";         known[161] := "CLXI";        known[162] := "CLXII";       
known[163] := "CLXIII";      known[164] := "CLXIV";       known[165] := "CLXV";        
known[166] := "CLXVI";       known[167] := "CLXVII";      known[168] := "CLXVIII";     
known[169] := "CLXIX";       known[170] := "CLXX";        known[171] := "CLXXI";       
known[172] := "CLXXII";      known[173] := "CLXXIII";     known[174] := "CLXXIV";      
known[175] := "CLXXV";       known[176] := "CLXXVI";      known[177] := "CLXXVII";     
known[178] := "CLXXVIII";    known[179] := "CLXXIX";      known[180] := "CLXXX";       
known[181] := "CLXXXI";      known[182] := "CLXXXII";     known[183] := "CLXXXIII";    
known[184] := "CLXXXIV";     known[185] := "CLXXXV";      known[186] := "CLXXXVI";     
known[187] := "CLXXXVII";    known[188] := "CLXXXVIII";   known[189] := "CLXXXIX";     
known[190] := "CXC";         known[191] := "CXCI";        known[192] := "CXCII";      
known[193] := "CXCIII";      known[194] := "CXCIV";       known[195] := "CXCV";       
known[196] := "CXCVI";       known[197] := "CXCVII";      known[198] := "CXCVIII";    
known[199] := "CXCIX";       known[200] := "CC";          known[201] := "CCI";         
known[202] := "CCII";        known[203] := "CCIII";       known[204] := "CCIV";        
known[205] := "CCV";         known[206] := "CCVI";        known[207] := "CCVII";       
known[208] := "CCVIII";      known[209] := "CCIX";        known[210] := "CCX";         
known[211] := "CCXI";        known[212] := "CCXII";       known[213] := "CCXIII";      
known[214] := "CCXIV";       known[215] := "CCXV";        known[216] := "CCXVI";       
known[217] := "CCXVII";      known[218] := "CCXVIII";     known[219] := "CCXIX";       
known[220] := "CCXX";        known[221] := "CCXXI";       known[222] := "CCXXII";      
known[223] := "CCXXIII";     known[224] := "CCXXIV";      known[225] := "CCXXV";       
known[226] := "CCXXVI";      known[227] := "CCXXVII";     known[228] := "CCXXVIII";    
known[229] := "CCXXIX";      known[230] := "CCXXX";       known[231] := "CCXXXI";      
known[232] := "CCXXXII";     known[233] := "CCXXXIII";    known[234] := "CCXXXIV";     
known[235] := "CCXXXV";      known[236] := "CCXXXVI";     known[237] := "CCXXXVII";    
known[238] := "CCXXXVIII";   known[239] := "CCXXXIX";     known[240] := "CCXL";        
known[241] := "CCXLI";       known[242] := "CCXLII";      known[243] := "CCXLIII";     
known[244] := "CCXLIV";      known[245] := "CCXLV";       known[246] := "CCXLVI";      
known[247] := "CCXLVII";     known[248] := "CCXLVIII";    known[249] := "CCXLIX";      

   errors := 0;

   FOR i := 1 TO 249 DO

      ToRoman( i, string );
      IF NOT Equal( string, known[i] ) THEN
        errors := errors + 1;
        WriteCard( i, 0 );
        WriteString( ', known=' ); WriteString( known[i] );
        WriteString( ' but conversion=' );
        WriteString( string );
        WriteLn;
      END;

      FromRoman( known[i], x );
      IF i # x THEN
        errors := errors + 1;
        WriteCard( i, 0 ); WriteString( ' expected, found=' );
        WriteCard( x, 0 );
        WriteLn;
      END;

   END;

   WriteLn;
   IF errors = 0 THEN
     WriteString( 'no conversion problems found' );
   ELSE
     WriteCard( errors, 0 ); WriteString( ' conversion errors !' );
   END;
   WriteLn;

END TestRoman.
