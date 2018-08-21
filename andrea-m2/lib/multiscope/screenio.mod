IMPLEMENTATION MODULE ScreenIO;

(* Do screen io, row/column locations, and color on DOS *)
(* V1.0, J. Andrea, May.11/93 *)

FROM ASCII IMPORT esc, EOL;
FROM Strings IMPORT Length;
FROM Terminal IMPORT Write, Read;

CONST
  ord1 = 49;  (* ORD('1') *)
  f11  = ';'; (* two characters past '9' *)
  f12  = '<'; (* three characters past '9' *)

VAR
  qwerty_row1, qwerty_row2, qwerty_row3 :ARRAY [0..10] OF CHAR;
  char :ARRAY [1..screen_cols] OF ARRAY [1..2] OF CHAR;
  i, k :CARDINAL;

(* -------------- *)
PROCEDURE Bell;
BEGIN
  Write( 7C );
END Bell;


(* ----------------------------------------------------------- *)
PROCEDURE PutString( string :ARRAY OF CHAR; row, col :CARDINAL );
VAR
   i, n :CARDINAL;
BEGIN
   n := Length( string );
   IF n > 0 THEN
     Goto( row, col );
     FOR i := 0 TO n - 1 DO
        Write( string[i] );
     END;
   END;
END PutString;

(* ---------------------------------- *)
PROCEDURE Goto( row, col :CARDINAL );
BEGIN
  IF row = 0 THEN row := 1 END;
  IF col = 0 THEN col := 1 END;
  IF row > screen_rows THEN row := 1 END;
  IF col > screen_cols THEN col := 1 END;

  Write( esc ); Write( '[' );
  Write( char[row][1] ); Write( char[row][2] );
  Write( ';' );
  Write( char[col][1] ); Write( char[col][2] );
  Write( 'H' );
END Goto;

(* ------------------------------------- *)
PROCEDURE GotoRelative( direction :CHAR );
BEGIN
  IF direction = '<' THEN
    Esc; Write( '1' ); Write( 'D' );
  ELSIF direction = '>' THEN
    Esc; Write( '1' ); Write( 'C' );
  ELSIF direction = '^' THEN
    Esc; Write( '1' ); Write( 'A' );
  ELSIF direction = 'v' THEN
    Esc; Write( '1' ); Write( 'B' );
  END;
END GotoRelative;

(* ----------------- *)
PROCEDURE Esc;
BEGIN
  Write( esc ); Write( '[' );
END Esc;

(* -------------------------- *)
PROCEDURE Clear;
BEGIN
  Esc; Write( '2' ); Write( 'J' );
END Clear;
PROCEDURE ClearEOL;
BEGIN
  Esc; Write( 'K' );
END ClearEOL;

(* ------------------------------- *)
PROCEDURE SetColor( c :CHAR );
BEGIN
  Esc; Write( '3' ); Write( c ); Write( 'm' );
END SetColor;

PROCEDURE Red;     BEGIN SetColor( '1' ) END Red;
PROCEDURE Green;   BEGIN SetColor( '2' ) END Green;
PROCEDURE Blue;    BEGIN SetColor( '4' ) END Blue;
PROCEDURE Yellow;  BEGIN SetColor( '3' ) END Yellow;
PROCEDURE Magenta; BEGIN SetColor( '5' ) END Magenta;
PROCEDURE Cyan;    BEGIN SetColor( '6' ) END Cyan;
PROCEDURE White;   BEGIN SetColor( '7' ) END White;

(* ------------------------ *)
PROCEDURE Normal;
BEGIN
  Esc; Write( '0' ); Write( 'm' );
END Normal;
PROCEDURE Bold;
BEGIN
  Esc; Write( '1' ); Write( 'm' );
END Bold;

  (* -------------------------------------------------------- *)
  PROCEDURE ReadKey( VAR ch :CHAR;
                     VAR shift, control, alt, function, arrow, special :BOOLEAN );
  BEGIN

    shift    := FALSE;
    control  := FALSE;
    alt      := FALSE;
    function := FALSE;
    arrow    := FALSE;
    special  := FALSE;

    Read( ch );
    IF ch = 0C THEN
      Read( ch );
      i := ORD( ch );
      CASE i OF
        15 :                                         (* shift tab *)
               control := TRUE;
               shift   := TRUE;
               ch      := 'i';
      | 16..25 :                                     (* alt q..p *)
               alt := TRUE;
               ch  := qwerty_row1[i-16];
      | 30..38 :                                     (* alt a..l *)
               alt := TRUE;
               ch  := qwerty_row2[i-30];
      | 44..50 :                                     (* alt z..m *)
               alt := TRUE;
               ch  := qwerty_row3[i-44];
      | 59..68,133,134 :
               function := TRUE;
               IF i = 133 THEN
                 ch := f11;                          (* f11 *)
               ELSIF i = 134 THEN
                 ch := f12;                          (* f12 *)
               ELSE
                 ch := CHR( ord1 + i - 59 );         (* f1..f10 *)
               END;
      | 71,73,76,79,81..83 :
               special := TRUE;
               CASE i OF
                71 :ch := 'h';                     (* home *)
              | 73 :ch := '^';                     (* page up *)
              | 76 :ch := '5';                     (* keypad 5 *)
              | 79 :ch := 'e';                     (* end *)
              | 81 :ch := 'v';                     (* page down *)
              | 82 :ch := 'i';                     (* insert *)
              | 83 :ch := 'd';                     (* delete *)
               END;
      | 72,75,77,80 :
               arrow := TRUE;
               IF i = 72 THEN
                 ch := '^';                        (* up arrow *)
               ELSIF i = 75 THEN
                 ch := '<';                        (* left arrow *)
               ELSIF i = 77 THEN
                 ch := '>';                        (* right arrow *)
               ELSE
                 ch := 'v';                        (* down arrow *)
               END;
      | 84..93,135,136 :
              shift := TRUE;
              function := TRUE;
              IF i = 135 THEN
                ch := f11;                         (* shift f11 *)
              ELSIF i = 136 THEN
                ch := f12;                         (* shift f12 *)
              ELSE
                ch := CHR( ord1 + i - 84 );        (* shift f1..f10 *)
              END;
      | 94..103,137,138 :
              control := TRUE;
              function := TRUE;
              IF i = 137 THEN
                ch := f11;                         (* control f11 *)
              ELSIF i = 138 THEN
                ch := f12;                         (* control f12 *)
              ELSE
                ch := CHR( ord1 + i - 94 );        (* control f1..f10 *)
              END;
      | 104..113,139,140 :
              alt := TRUE;
              function := TRUE;
              IF i = 139 THEN
                ch := f11;                         (* alt f11 *)
              ELSIF i = 140 THEN
                ch := f12;                         (* alt f12 *)
              ELSE
                ch := CHR( ord1 + i - 104 );       (* alt f1..f10 *)
              END;
      | 115,116,141,145 :
              arrow   := TRUE;
              control := TRUE;
              IF i = 115 THEN
                ch := '<';                          (* contol left arrow *)
              ELSIF i = 116 THEN
                ch := '>';                          (* control right arrow *)
              ELSIF i = 141 THEN
                ch := '^';                          (* control up arrow *)
              ELSE
                ch := 'v';                          (* control down arrow *)
              END;
      | 117..119,132,143,146,147 :
              special := TRUE;
              control := TRUE;
              IF i = 117 THEN
                ch := 'e';                          (* control end *)
              ELSIF i = 118 THEN
                ch := 'v';                          (* control page down *)
              ELSIF i = 119 THEN
                ch := 'h';                          (* control home *)
              ELSIF i = 132 THEN
                ch := '^';                          (* control page up *)
              ELSIF i = 143 THEN
                ch := '5';                          (* control keypad 5 *)
              ELSIF i = 147 THEN
                ch := 'd';                          (* control delete *)
              ELSE
                ch := 'i';                          (* control insert *)
              END;
      | 120..131 :
              alt := TRUE;
              IF ( i >= 120 ) & ( i <= 128 ) THEN    (* alt 1..9 *)
                ch := CHR( ord1 + i - 120 );
              ELSIF i = 129 THEN                     (* alt 0 *)
                ch := '0';
              ELSIF i = 130 THEN                     (* alt - *)
                ch := '-';
              ELSIF i = 131 THEN                     (* alt = *)
                ch := '=';
              END;
      | 148 :
          (* this is a control tab, but it would look just like a
             tab alone, so return it as a shift tab *)
          control := TRUE;
          shift   := TRUE;
          ch := 'i';                           (* control tab, not correct *)
      | 149 :
          control := TRUE;
          alt     := TRUE;
          ch := 'i';                           (* alt tab *)
      ELSE
        ch := 0C;                              (* unknown *)
      END; (* case *)
    ELSE

      IF ch < ' ' THEN
        control := TRUE;
        (* special case for how the lower module handles return *)
        IF ch = EOL THEN
          ch := 'm';
        ELSIF ( ch >= 33C ) & ( ch <= 37C ) THEN
          ch := CHR( ORD( ch ) + 64 );       (* esc and next few *)
        ELSE
          ch := CHR( ORD( ch ) + 96 );
        END;
      ELSIF ( ch >= 'A' ) & ( ch <= 'Z' ) THEN
        shift := TRUE;
        ch    := CHR( ORD( ch ) + 32 );
      END;
    END;

  END ReadKey;

BEGIN

  qwerty_row1 := 'qwertyuiop';
  qwerty_row2 := 'asdfghjkl';
  qwerty_row3 := 'zxcvbnm';

  (* build the table of characters of numbers for lookup *)
  FOR i := 1 TO 9 DO
    char[i][1] := '0';
    char[i][2] := CHR( ORD('0') + i );
  END;
  FOR i := 10 TO screen_cols DO
    k := i DIV 10;
    char[i][1] := CHR( ORD('0') + k );
    k := i - k * 10;
    char[i][2] := CHR( ORD('0') + k );
  END;

END ScreenIO.
