IMPLEMENTATION MODULE VeryScreen;

(* line and box drawing added, John Andrea, Sept.25/1991 *)
(* John Andrea, Feb.1/1991 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM TTIO IMPORT WriteLn, WriteString;

CONST
   esc = 33C;   (* escape character *)

TYPE
   Number = ARRAY [0..2] OF CHAR;

VAR
  home                  :ARRAY [0..2] OF CHAR;
  erase_screen          :ARRAY [0..2] OF CHAR;
  erase_line            :ARRAY [0..2] OF CHAR;
  cursor_on, cursor_off :ARRAY [0..5] OF CHAR;
  put_cursor            :ARRAY [0..1] OF CHAR;

  cursor_up,   cursor_down  :ARRAY [0..3] OF CHAR;
  cursor_left, cursor_right :ARRAY [0..3] OF CHAR;

  enter_graphics, exit_graphics :ARRAY [0..2] OF CHAR;

  numbers               :ARRAY [0..132] OF Number;

  row_string, col_string :Number;

  i :CARDINAL;

   (* ----------------------------------------------------------------- *)
   PROCEDURE Box( row1, col1, row2, col2 :CARDINAL );
   VAR nr, nc :CARDINAL;
   BEGIN

      (* flip around the rows and cols to find the minimum *)

      IF col1 > col2 THEN
        nc := col2;  col2 := col1;  col1 := nc;
      END;
      nc := col2 - col1;

      IF nc > 0 THEN

        IF row1 > row2 THEN
          nr := row2;  row2 := row1;  row1 := nr;
        END;
        nr := row2 - row1;

        IF nr > 0 THEN

          PutCursor( row1, col1 );

          WriteString( enter_graphics );

          WriteString( 'l' );  (* first corner *)

          FOR i := 1 TO nc - 1 DO
             WriteString( 'q' );
          END;

          WriteString( 'k' );  (* second corner *)
          WriteString( cursor_left ); WriteString( cursor_down );

          FOR i := 1 TO nr - 1 DO
             WriteString( 'x' );
             WriteString( cursor_left ); WriteString( cursor_down );
          END;

          WriteString( 'j' );  (* third corner *)
          WriteString( cursor_left );   WriteString( cursor_left );

          FOR i := 1 TO nc - 1 DO
             WriteString( 'q' );
             WriteString( cursor_left );  WriteString( cursor_left );
          END;

          WriteString( 'm' );  (* fourth corner *)
          WriteString( cursor_left ); WriteString( cursor_up );

          FOR i := 1 TO nr - 1 DO
             WriteString( 'x' );
             WriteString( cursor_left ); WriteString( cursor_up );
          END;

          WriteString( exit_graphics );

        END;

      END;

   END Box;

   (* ----------------------------------------------------------------- *)
   PROCEDURE Line( row1, col1, row2, col2 :CARDINAL );
   VAR n :CARDINAL;
   BEGIN

      (* diagonal lines can't be drawn so simply assume its horizontal if *)
      (* the columns arn't the same, putting the emphasis on horizontal   *)

      IF col1 # col2 THEN

        IF col1 > col2 THEN
          PutCursor( row1, col2 );
          n := col1 - col2;
        ELSE
          PutCursor( row1, col1 );
          n := col2 - col1;
        END;

        IF n > 0 THEN
  
          WriteString( enter_graphics );
          FOR i := 1 TO n DO
             WriteString( 'q' );
          END;
          WriteString( exit_graphics );

        END;

      ELSE

        IF row1 > row2 THEN
          PutCursor( row2, col1 );
          n := row1 - row2;
        ELSE
          PutCursor( row1, col1 );
          n := row2 - row1;
        END;

        IF n > 0 THEN

          WriteString( enter_graphics );
          FOR i := 1 TO n DO
             WriteString( 'x' );
             WriteString( cursor_left ); WriteString( cursor_down );
          END;
          WriteString( exit_graphics );

        END;

      END;

   END Line;

   (* ----------------------------------------------------------------- *)
   PROCEDURE Home;
   BEGIN
      WriteString( home );
   END Home;

   (* ----------------------------------------------------------------- *)
   PROCEDURE EraseScreen;
   BEGIN
      WriteString( erase_screen );
   END EraseScreen;

   (* ----------------------------------------------------------------- *)
   PROCEDURE EraseLine;
   BEGIN
      WriteString( erase_line );
   END EraseLine;

   (* ----------------------------------------------------------------- *)
   PROCEDURE PutString( text :ARRAY OF CHAR; row, col :CARDINAL );
   BEGIN
       PutCursor( row, col );  WriteString( text );
   END PutString;

   (* ----------------------------------------------------------------- *)
   PROCEDURE PutCursor( row, col :CARDINAL );
   BEGIN

      IF ( row < 0 ) OR ( row > 25 ) THEN
        row_string := numbers[0];
      ELSE
        row_string := numbers[ row ];
      END;

      IF ( col < 0 ) OR ( col > 132 ) THEN
        col_string := numbers[0];
      ELSE
        col_string := numbers[ col ];
      END;

      WriteString( put_cursor );
      WriteString( row_string );  WriteString( ';' );
      WriteString( col_string );  WriteString( 'H' );

   END PutCursor;

   (* ----------------------------------------------------------------- *)
   PROCEDURE CursorOn( turn_on :BOOLEAN );
   BEGIN

      IF turn_on THEN
        WriteString( cursor_on );
      ELSE
        WriteString( cursor_off );
      END;

   END CursorOn;
 
BEGIN (* VeryScreen *)

home         := ' [H';    home[0]         := esc;
erase_screen := ' [J';    erase_screen[0] := esc;
erase_line   := ' [K';    erase_line[0]   := esc;

cursor_on  := ' [?25h';  cursor_on[0]  := esc;
cursor_off := ' [?25l';  cursor_off[0] := esc;

cursor_up    := ' [1A'; cursor_up[0]    := esc;
cursor_down  := ' [1B'; cursor_down[0]  := esc;
cursor_right := ' [1C'; cursor_right[0] := esc;
cursor_left  := ' [1D'; cursor_left[0]  := esc;

put_cursor := ' [';      put_cursor[0] := esc;

enter_graphics := ' (0'; enter_graphics[0] := esc;
exit_graphics  := ' (B'; exit_graphics[0] := esc;

(* precompute the strings for numbers in range 0 to 132 *)

numbers[0] := '000';  numbers[1] := '001';  numbers[2] := '002';
numbers[3] := '003';  numbers[4] := '004';  numbers[5] := '005';
numbers[6] := '006';  numbers[7] := '007';  numbers[8] := '008';
numbers[9] := '009';  numbers[10] := '010'; numbers[11] := '011';
numbers[12] := '012'; numbers[13] := '013'; numbers[14] := '014';
numbers[15] := '015'; numbers[16] := '016'; numbers[17] := '017';
numbers[18] := '018'; numbers[19] := '019'; numbers[20] := '020';
numbers[21] := '021'; numbers[22] := '022'; numbers[23] := '023';
numbers[24] := '024'; numbers[25] := '025'; numbers[26] := '026';
numbers[27] := '027'; numbers[28] := '028'; numbers[29] := '029';
numbers[30] := '030'; numbers[31] := '031'; numbers[32] := '032';
numbers[33] := '033'; numbers[34] := '034'; numbers[35] := '035';
numbers[36] := '036'; numbers[37] := '037'; numbers[38] := '038'; 
numbers[39] := '039'; numbers[40] := '040'; numbers[41] := '041';
numbers[42] := '042'; numbers[43] := '043'; numbers[44] := '044';
numbers[45] := '045'; numbers[46] := '046'; numbers[47] := '047';
numbers[48] := '048'; numbers[49] := '049'; numbers[50] := '050';
numbers[51] := '051'; numbers[52] := '052'; numbers[53] := '053';
numbers[54] := '054'; numbers[55] := '055'; numbers[56] := '056';
numbers[57] := '057'; numbers[58] := '058'; numbers[59] := '059';
numbers[60] := '060'; numbers[61] := '061'; numbers[62] := '062';
numbers[63] := '063'; numbers[64] := '064'; numbers[65] := '065';
numbers[66] := '066'; numbers[67] := '067'; numbers[68] := '068';
numbers[69] := '069'; numbers[70] := '070'; numbers[71] := '071';
numbers[72] := '072'; numbers[73] := '073'; numbers[74] := '074';
numbers[75] := '075'; numbers[76] := '076'; numbers[77] := '077'; 
numbers[78] := '078'; numbers[79] := '079'; numbers[80] := '080';
numbers[81] := '081'; numbers[82] := '082'; numbers[83] := '083';
numbers[84] := '084'; numbers[85] := '085'; numbers[86] := '086';
numbers[87] := '087'; numbers[88] := '088'; numbers[89] := '089';
numbers[90] := '090'; numbers[91] := '091'; numbers[92] := '092';
numbers[93] := '093'; numbers[94] := '094'; numbers[95] := '095';
numbers[96] := '096'; numbers[97] := '097'; numbers[98] := '098';
numbers[99] := '099'; numbers[100] := '100'; numbers[101] := '101';
numbers[102] := '102'; numbers[103] := '103'; numbers[104] := '104';
numbers[105] := '105'; numbers[106] := '106'; numbers[107] := '107';
numbers[108] := '108'; numbers[109] := '109'; numbers[110] := '110';
numbers[111] := '111'; numbers[112] := '112'; numbers[113] := '113';
numbers[114] := '114'; numbers[115] := '115'; numbers[116] := '116';
numbers[117] := '117'; numbers[118] := '118'; numbers[119] := '119';
numbers[120] := '120'; numbers[121] := '121'; numbers[122] := '122';
numbers[123] := '123'; numbers[124] := '124'; numbers[125] := '125';
numbers[126] := '126'; numbers[127] := '127'; numbers[128] := '128';
numbers[129] := '129'; numbers[130] := '130'; numbers[131] := '131';
numbers[132] := '132';

END VeryScreen.
