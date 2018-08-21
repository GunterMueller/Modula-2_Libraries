MODULE KeyTest;

(*
* This program will show which VT200 key is pressed
* till the user types the sequence "quit" or "exit"
*
* The intended use of this program is to help in the setup of
* PC emulation packages.
* Users can use this to test which keys on the PC map to which VT200 keys.
*)

(* John Andrea, Jun.16/1991 *)

FROM GetCharacter IMPORT Get, GetNoWait,
                         SetPassallCharacterMode, UnSetPassallCharacterMode;

FROM InOut IMPORT WriteString, WriteLn, WriteCard;

VAR
  c                :CHAR;
  v                :CARDINAL;
  quit, qui, qu, q :BOOLEAN;
  exit, exi, ex, e :BOOLEAN;

  esc, esc_openp,
  esc_openp_ABCD,
  esc_openp_16, esc_openp_16_tilde,
  esc_openp_16_09, esc_openp_16_09_tilde,
  esc_O, esc_O_LY                         :BOOLEAN;

  digit_1, digit_2 :CHAR;

  ctrl_names :ARRAY [0..31] OF ARRAY [0..3] OF CHAR;

  (* ------------------------------------------------------------- *)
  PROCEDURE SetupControls;
  (* setup names for the control-characters *)
  BEGIN
      ctrl_names[00] := 'NUL'; (* null, control @ *)
      ctrl_names[01] := 'SOH'; (* control A *)
      ctrl_names[02] := 'STX'; (* control B *)
      ctrl_names[03] := 'ETX'; (* control C *)
      ctrl_names[04] := 'EOT'; (* control D *)
      ctrl_names[05] := 'ENQ'; (* control E *)
      ctrl_names[06] := 'ACK'; (* control F *)
      ctrl_names[07] := 'BEL'; (* bell, control G *)
      ctrl_names[08] := 'BS '; (* backspace, control H *)
      ctrl_names[09] := 'HT '; (* tab, control I *)
      ctrl_names[10] := 'LF '; (* line feed, control J *)
      ctrl_names[11] := 'VT '; (* control K *)
      ctrl_names[12] := 'FF '; (* form feed, control L *)
      ctrl_names[13] := 'CR '; (* carriage return, control M *)
      ctrl_names[14] := 'SO '; (* control N *)
      ctrl_names[15] := 'SI '; (* control O *)
      ctrl_names[16] := 'DLE'; (* control P *)
      ctrl_names[17] := 'DC1'; (* control Q *)
      ctrl_names[18] := 'DC2'; (* control R *)
      ctrl_names[19] := 'DC3'; (* control S *)
      ctrl_names[20] := 'DC4'; (* control T *)
      ctrl_names[21] := 'NAK'; (* control U *)
      ctrl_names[22] := 'SYN'; (* control V *)
      ctrl_names[23] := 'ETB'; (* control W *)
      ctrl_names[24] := 'CAN'; (* control X *)
      ctrl_names[25] := 'EM '; (* control Y *)
      ctrl_names[26] := 'SUB'; (* control Z *)
      ctrl_names[27] := 'ESC'; (* escape, control [ *)
      ctrl_names[28] := 'FS '; (* control \ *)
      ctrl_names[29] := 'GS '; (* control ] *)
      ctrl_names[30] := 'RS '; (* control ^ *)
      ctrl_names[31] := 'US '; (* control _ *)
  END SetupControls;

  (* ------------------------------------------------------------- *)
  PROCEDURE ShowSeq( string :ARRAY OF CHAR );
  BEGIN
     WriteString( 'that sequence corresponded to the ' );
     WriteString( string );
     WriteLn;
  END ShowSeq;

BEGIN (* KeyTest *)

SetupControls;

WriteLn;
WriteString( 'KEYTEST begin, start hitting keys' ); WriteLn;
WriteString( 'the program stops with the sequence' ); WriteLn;
WriteString( 'quit OR exit' ); WriteLn;
WriteLn;

SetPassallCharacterMode;

WHILE NOT ( quit OR exit ) DO

   Get(c);     v := ORD( c );

   WriteString( 'value:' ); WriteCard( v, 4 ); WriteString( ', ' );

   IF ( c > ' ' ) & ( c <= '~' ) THEN
     WriteString( 'the standard character: ' );
     WriteString( c );
   ELSIF c = ' 'THEN
     WriteString( 'the SPACE character' );
   ELSIF v = 127 THEN
     WriteString( 'the DEL character' );
   ELSIF ( v >= 0 ) & ( v <= 31 ) THEN
     WriteString( 'the control character named: ');
     WriteString( ctrl_names[v] );
     WriteString( ' which is Control-' );
     WriteString( CHR(v+64) );
   ELSIF ( v >= 161 ) & ( v <= 253 ) THEN
     WriteString( 'the compose character: ' );
     WriteString( c );
   ELSE
     WriteString( 'unknown character' );
   END;

   WriteLn;

   (* 5 chars *)
   esc_openp_16_09_tilde := esc_openp_16_09 & ( c = '~' );

   (* 4 chars *)
   esc_openp_16_09       := esc_openp_16 & ( ( c >= '0' ) & ( c <= '9' ) );
   esc_openp_16_tilde    := esc_openp_16 & ( c = '~' );

   (* 3 chars *)
   esc_openp_16          := esc_openp & ( ( c >= '1' ) & ( c <= '6' ) );
   esc_openp_ABCD        := esc_openp & ( ( c >= 'A' ) & ( c <= 'D' ) );
   esc_O_LY              := esc_O     & (( CAP(c) >= 'L' ) & ( CAP(c) <= 'Y' ));

   (* 2 chars *)
   esc_O                 := esc & ( c = 'O' );  (* thats an "oh" *)
   esc_openp             := esc & ( c = '[' );

   (* 1 char *)
   esc                   := v = 27;

   IF esc_openp_16 THEN
     digit_1 := c;
   END;

   IF esc_openp_16_09 THEN
     digit_2 := c;
   END;

   IF esc_O_LY THEN
     IF    c = 'p' THEN
       ShowSeq( 'KEYPAD 0' );
     ELSIF c = 'q' THEN
       ShowSeq( 'KEYPAD 1' );
     ELSIF c = 'r' THEN
       ShowSeq( 'KEYPAD 2' );
     ELSIF c = 's' THEN
       ShowSeq( 'KEYPAD 3' );
     ELSIF c = 't' THEN
       ShowSeq( 'KEYPAD 4' );
     ELSIF c = 'u' THEN
       ShowSeq( 'KEYPAD 5' );
     ELSIF c = 'v' THEN
       ShowSeq( 'KEYPAD 6' );
     ELSIF c = 'w' THEN
       ShowSeq( 'KEYPAD 7' );
     ELSIF c = 'x' THEN
       ShowSeq( 'KEYPAD 8' );
     ELSIF c = 'y' THEN
       ShowSeq( 'KEYPAD 9' );
     ELSIF c = 'm' THEN
       ShowSeq( 'KEYPAD MINUS' );
     ELSIF c = 'l' THEN
       ShowSeq( 'KEYPAD COMMA');
     ELSIF c = 'n' THEN
       ShowSeq( 'KEYPAD PERIOD' );
     ELSIF c = 'M' THEN
       ShowSeq( 'KEYPAD ENTER' );
     ELSIF c = 'P' THEN
       ShowSeq( 'PF1' );
     ELSIF c = 'Q' THEN
       ShowSeq( 'PF2' );
     ELSIF c = 'R' THEN
       ShowSeq( 'PF3' );
     ELSIF c = 'S' THEN
       ShowSeq( 'PF4' );
     END;
   END;

   IF esc_openp_16_09_tilde THEN
     IF    digit_1 = '1' THEN
       IF    digit_2 = '7' THEN
         ShowSeq( 'F6 key' );
       ELSIF digit_2 = '8' THEN
         ShowSeq( 'F7 key' );
       ELSIF digit_2 = '9' THEN
         ShowSeq( 'F8 key' );
       END;
     ELSIF digit_1 = '2' THEN
       IF    digit_2 = '0' THEN
         ShowSeq( 'F9 key' );
       ELSIF digit_2 = '1' THEN
         ShowSeq( 'F10 key' );
       ELSIF digit_2 = '3' THEN
         ShowSeq( 'F11 key' );
       ELSIF digit_2 = '4' THEN
         ShowSeq( 'F12 key' );
       ELSIF digit_2 = '5' THEN
         ShowSeq( 'F13 key' );
       ELSIF digit_2 = '6' THEN
         ShowSeq( 'F14 key' );
       ELSIF digit_2 = '8' THEN
         ShowSeq( 'HELP key' );
       ELSIF digit_2 = '9' THEN
         ShowSeq( 'DO key' );
       END;
     ELSIF digit_1 = '3' THEN
       IF    digit_2 = '1' THEN
         ShowSeq( 'F17 key' );
       ELSIF digit_2 = '2' THEN
         ShowSeq( 'F18 key' );
       ELSIF digit_2 = '3' THEN
         ShowSeq( 'F19 key' );
       ELSIF digit_2 = '4' THEN
         ShowSeq( 'F20 key' );
       END;
     END;
   END;

   IF esc_openp_16_tilde THEN
     IF    digit_1 = '1' THEN
        ShowSeq( 'FIND key' );
     ELSIF digit_1 = '2' THEN
        ShowSeq( 'INSERT HERE key' );
     ELSIF digit_1 = '3' THEN
        ShowSeq( 'REMOVE key' );
     ELSIF digit_1 = '4' THEN
        ShowSeq( 'SELECT key' );
     ELSIF digit_1 = '5' THEN
        ShowSeq( 'PREV SCREEN key' );
     ELSIF digit_1 = '6' THEN
        ShowSeq( 'NEXT SCREEN key' );
     END;
   END;

   IF esc_openp_ABCD THEN
     IF    c = 'A' THEN
        ShowSeq( 'UP ARROW' );
     ELSIF c = 'B' THEN
        ShowSeq( 'DOWN ARROW' );
     ELSIF c = 'C' THEN
        ShowSeq( 'RIGHT ARROW' );
     ELSIF c = 'D' THEN
        ShowSeq( 'LEFT ARROW' );
     END;
   END;

   c    := CAP( c );
   quit := qui & ( c = 'T' );
   qui  := qu  & ( c = 'I' );
   qu   := q   & ( c = 'U' );
   q    :=       ( c = 'Q' );
   exit := exi & ( c = 'T' );
   exi  := ex  & ( c = 'I' );
   ex   := e   & ( c = 'X' );
   e    :=       ( c = 'E' );
END; (* while *)

UnSetPassallCharacterMode;

WriteLn; WriteString( 'KEYTEST finished' ); WriteLn;

END KeyTest.
