IMPLEMENTATION MODULE FormEdit;

(* Show lines on the screen for user input/editing. *)
(* V1.1, J. Andrea, Jun.9/93 -correct a backspace/delete problem
                             -add first time edit to blank option *)
(* V1.0, J. Andrea, Jun.3/93 *)

FROM ScreenIO IMPORT screen_rows, screen_cols,
                     ReadKey, PutString, Bell,
                     Clear, ClearEOL, Goto,
                     Normal, Green, Yellow;
FROM Strings IMPORT Assign, Length;

CONST
  max_string = 80;

  next = 1;
  last = 2;

TYPE
  LineInfo = RECORD
               on    :BOOLEAN;
               prompt, default, result :ARRAY [1..max_string+1] OF CHAR;
               size, len  :CARDINAL;       (* max_size, and current size *)
               col   :CARDINAL;                 (* prompt column *)
               point :ARRAY [1..2] OF CARDINAL;
               first_edit :BOOLEAN;              (* first time touched *)
             END;

VAR
  lines :ARRAY [1..max_lines] OF LineInfo;
  message :ARRAY [0..80] OF CHAR;
  error   :BOOLEAN;

  ordinary_key, shift_key, control_key, alt_key, function_key, arrow_key,
  special_key :BOOLEAN;
  key_value   :CHAR;

  (* ---------------------------------------- *)
  PROCEDURE Key;
  BEGIN
    ReadKey( key_value,
             shift_key, control_key, alt_key, function_key, arrow_key,
             special_key );
    ordinary_key := NOT ( control_key OR alt_key OR function_key OR
                          arrow_key OR special_key );
  END Key;

  (* ---------------------------------------- *)
  PROCEDURE ShowHelp;
  BEGIN
     Clear;  Normal;
     PutString( 'F1   :show this help', 1, 10 );
     PutString( 'ESC  :redraw editing screen', 2, 10 );

     PutString( 'arrow :move to item to be corrected', 4, 10 );
     PutString( 'Home  :move to beginning of line', 5, 10 );
     PutString( 'End   :move to end of line', 6, 10 );
     PutString( 'Return:same as down arrow', 7, 10 );

     PutString( 'Insert    :open a space', 9, 10 );
     PutString( 'Delete    :delete current character', 10, 10 );
     PutString( 'Backspace :delete previous character', 11, 10 );

     PutString( 'F8 :reset line to original value', 13, 10 );

     PutString( 'PageUp      :move to first line', 15, 10 );
     PutString( 'PageDown    :move to last line', 16, 10 );

     PutString( 'corrections :type to replace current character', 18, 10 );
     PutString( '             typing is always in overwrite mode', 19, 10 );

     PutString( 'F12         :finished editing', 21, 10 );

     PutString( '-- hit any key to continue --', 25, 20 );
     Key;
  END ShowHelp;

  (* ------------------------------------------------------ *)
  PROCEDURE SetDefault( x :CARDINAL );
  BEGIN
    (* save the default as the first value *)
    Assign( lines[x].default, lines[x].result );
    lines[x].len := Length( lines[x].result );
  END SetDefault;

(* ------------------------------------------- *)
PROCEDURE SetLine( line_number :CARDINAL;
                   prompt_string, default_string :ARRAY OF CHAR;
                   number_of_chars :CARDINAL;
                   VAR done :BOOLEAN );
BEGIN
  error := FALSE;
  done  := TRUE;

  IF ( line_number = 0 ) OR ( line_number > max_lines ) THEN
    done := FALSE;
    error := TRUE;
    message := 'Line number out of range';
  ELSE

    IF number_of_chars = 0 THEN
      done    := FALSE;
      error   := TRUE;
      message := 'Size of data item cannot be zero';
    ELSIF number_of_chars >= screen_cols THEN
      done    := FALSE;
      error   := TRUE;
      message := 'Size of data item too large';
    ELSE
      lines[line_number].size := number_of_chars;
    END;

    Assign( prompt_string,  lines[line_number].prompt );
    Assign( default_string, lines[line_number].default );

    SetDefault( line_number );

    lines[line_number].on         := done;
    lines[line_number].first_edit := TRUE;
  END;
END SetLine;

(* ------------------------------------------- *)
PROCEDURE DoEditing( initial_edit, display_column :CARDINAL;
                     first_edit_blank :BOOLEAN );
VAR
  very_first, very_last     :CARDINAL;
  current_line, current_pos :CARDINAL;
  still_editing             :BOOLEAN;
  n_on                      :CARDINAL;

  (* ---------------------------------------- *)
  PROCEDURE Mark( on :BOOLEAN );
  (* Make the current line green, or not *)
  VAR
     x, i, n :CARDINAL;
  BEGIN
    x := current_line;
    IF ( x # 0 ) & lines[x].on THEN
      Goto( x, display_column ); ClearEOL;
      IF on THEN
        Green;
      ELSE
        Yellow;
      END;
      (* show the item *)
      PutString( lines[x].result, x, display_column );
      IF on THEN
        n := lines[x].len + 1;
        i := display_column + n - 1;
        (* and show how big it can be with underlines *)
        WHILE ( n <= lines[x].size ) & ( i < screen_cols ) DO
          PutString( '_', x, i );
          n := n + 1;
          i := i + 1;
        END;
      END;
      PutCursor;
    END;
  END Mark;

  (* -------------------------------------------- *)
  PROCEDURE PutCursor;
  BEGIN
    Goto( current_line, current_pos + display_column - 1 );
  END PutCursor;

  (* --------------------------------------------- *)
  PROCEDURE Display;
  VAR
    i, j, k :CARDINAL;
  BEGIN
    Clear;
    Green;
    PutString( 'F1 :help', screen_rows, 72 );
    Normal;

    FOR i := very_first TO very_last DO
    IF lines[i].on THEN
      k := lines[i].col;
      IF k = 0 THEN
        (* special case for a prompt that is too long,
           show only enough as needed *)
        FOR j := 0 TO display_column - 1 DO
          PutString( lines[i].prompt[j], i, j+1 );
        END;
      ELSE
        PutString( lines[i].prompt, i, k-1 );
      END;
      Yellow;
      PutString( lines[i].result, i, display_column );
      Normal;
    END;
    END;

    Mark( TRUE );
    PutCursor;
  END Display;

  (* ------------------------------------------------------ *)
  PROCEDURE LineMove( forward :BOOLEAN );
  (* Move the next line as defined by the arrow keys *)
  VAR
    direction, x :CARDINAL;
  BEGIN

    (* since we plan to move, turn off the pointer on the current item *)
    Mark( FALSE );

    IF forward THEN
      direction := next;
    ELSE
      direction := last;
    END;

    x := lines[current_line].point[direction];

    (* so set it and mark it *)
    current_line := x;
    Mark( TRUE );
    EditEnd( TRUE );

  END LineMove;
  PROCEDURE LineEnd( bottom :BOOLEAN );
  (* Move to the first or last line *)
  BEGIN
    Mark( FALSE );
    IF bottom THEN
      current_line := very_last;
    ELSE
      current_line := very_first;
    END;
    Mark( TRUE );
    EditEnd( TRUE );
  END LineEnd;

  (* ----------------------------------------------------------- *)
  PROCEDURE EditMove( forward :BOOLEAN );
  (* Move on the current line *)
  BEGIN
    IF forward THEN
      IF current_pos # lines[current_line].size THEN
        current_pos := current_pos + 1;
      END;
    ELSE
      IF current_pos # 1 THEN
        current_pos := current_pos - 1;
      END;
    END;
    PutCursor;
  END EditMove;
  PROCEDURE EditEnd( forward :BOOLEAN );
  (* Move to the beginning or end of the current line *)
  BEGIN
    IF forward THEN
      current_pos := lines[current_line].len;
      IF current_pos = 0 THEN
        (* if the line is blank, then just go to the first position *)
        current_pos := 1;
      END;
    ELSE
      current_pos := 1;
    END;
    PutCursor;
  END EditEnd;

  (* ------------------------------------------------ *)
  PROCEDURE Delete( forward :BOOLEAN );
  (* Delete the current character (FORWARD), or else delete the previous *)
  VAR
    i, k, n, m :CARDINAL;

  BEGIN
    n := lines[current_line].len;
    IF n # 0 THEN
      lines[current_line].first_edit := FALSE;
      IF forward THEN
        m := n;
      ELSE
        (* for back spacing, cursor may be one past last valid character *)
        m := n + 1;
      END;
      IF current_pos <= m THEN
        (* otherwise, cursor is past the end of the string, so just move cursor *)
        IF forward THEN
          (* delete the current item *)
          k := current_pos;
        ELSE
          k := current_pos - 1;
        END;
        IF k # 0 THEN
          (* shrink the current string *)
          FOR i := k TO n - 1 DO
            lines[current_line].result[i] :=
            lines[current_line].result[i+1];
          END;
          lines[current_line].result[n] := 0C;
          lines[current_line].len       := n - 1;
        END;
      END;
      (* move backwards *)
      EditMove( FALSE );
      Mark( TRUE );
      PutCursor;
    END;
  END Delete;

  (* ------------------------------------------------- *)
  PROCEDURE Replace( ch :CHAR );
  (* Replace the current character with whats passed in *)
  VAR
    n, i :CARDINAL;
  BEGIN
    n := lines[current_line].len;
    lines[current_line].first_edit := FALSE;
    IF current_pos > n THEN

      (* its been placed beyond the current end, so fill the rest with spaces *)
      FOR i := n + 1 TO current_pos DO
         lines[current_line].result[i] := ' ';
      END;

      (* save this new position as the length *)
      lines[current_line].len := current_pos;
      (* put a null char after it *)
      lines[current_line].result[current_pos+1] := 0C;
    END;

    (* and put the wanted character there *)
    lines[current_line].result[current_pos] := ch;

    (* skip on to the next character *)
    current_pos := current_pos + 1;
    (* but don't try to go past the end *)
    IF current_pos > lines[current_line].size THEN
      current_pos := current_pos - 1
    END;

    Mark( TRUE );
  END Replace;

  (* ------------------------------------------------------ *)
  PROCEDURE Insert( ch :CHAR );
  (* Insert the given character at the current position, push the other
     characters down one space. *)
  VAR
    i, j, n, x :CARDINAL;
  BEGIN
    x := current_line;
    n := lines[x].len;
    lines[x].first_edit := FALSE;
    IF current_pos > n THEN
      (* can only insert if there's something there *)
      (* otherwise, just put in a space *)
      Replace( ' ' );
      (* but move back to that same position again *)
      EditMove( FALSE );
    ELSE

      (* pull the existing characters after this one back one level *)
      j := n + 1;
      FOR i := current_pos TO n DO
        lines[x].result[j] := lines[x].result[j-1];
        j := j - 1;
      END;

      (* put a new 'space' at the new location *)
      lines[x].result[current_pos] := ' ';

      i := lines[x].size;
      IF n = i THEN
        (* the line was already at its maximum size *)
        n := i;
      ELSE
        (* otherwise, just count up one more character *)
        n := n + 1;
      END;
      lines[x].len         := n;
      lines[x].result[n+1] := 0C;

      Mark( TRUE );
    END;
  END Insert;

  (* ------------------------------------------------------------ *)
  PROCEDURE SetupDisplay;
  (* Determine the loctions of things on the screen, and the pointers
     from one line to the other for easy arrow motion. *)
  VAR
    len, i :CARDINAL;
  BEGIN

    very_first := 0;
    very_last  := 0;
    n_on       := 0;  (* How many are turned on *)

    (* determine the pointers *)
    FOR i := 1 TO max_lines DO
      IF lines[i].on THEN
        n_on := n_on + 1;

        IF very_first = 0 THEN
          (* this is the first line, so save its value *)
          very_first := i;
        END;

        IF very_last # 0 THEN
          (* the one after the last one is this one *)
          lines[very_last].point[next] := i;
        END;

        (* save the value of the previous one, this is ok even if its
           the first, because the first and last will be reset later *)
        lines[i].point[last] := very_last;

        (* save this one as the last one, for the next one it will count
           as the previous one. And it will be the very last at the end of
           the loop *)
        very_last := i;

        (* determine where the columns should go *)
        (* try to show as much as the prompt as possible *)
        len := Length( lines[i].prompt );
        IF len >= display_column THEN
          (* not all of the prompt can be displayed *)
          (* set a special value for the column *)
          lines[i].col := 0;
        ELSE
          (* determine how far back the start should be *)
          lines[i].col := display_column - len;
        END;

      END;
    END;

    (* and finish up the pointers for the first and last so that they wrap *)
    lines[very_first].point[last] := very_last;
    lines[very_last].point[next]  := very_first;
  END SetupDisplay;

BEGIN
  error := FALSE;

  IF display_column = 0 THEN display_column := 1 END;
  IF display_column > screen_cols THEN display_column := screen_cols - 1 END;

  SetupDisplay;

  IF n_on # 0 THEN

    IF initial_edit = 0 THEN initial_edit := very_first END;
    IF initial_edit > very_last THEN initial_edit := very_last END;
    IF NOT lines[initial_edit].on THEN initial_edit := very_first END;

    current_line := initial_edit;
    EditEnd( TRUE );

    Display;

    still_editing := TRUE;

    REPEAT
       Key;

       IF arrow_key THEN
         IF key_value = '^' THEN    (* up *)
           LineMove( FALSE );
         ELSIF key_value = 'v' THEN  (* down *)
           LineMove( TRUE );
         ELSIF key_value = '<' THEN  (* left *)
           EditMove( FALSE );
         ELSE                        (* right *)
           EditMove( TRUE );
         END;

       ELSIF special_key THEN
         IF key_value = 'h' THEN    (* home *)
           EditEnd( FALSE );
         ELSIF key_value = 'e' THEN (* end *)
           EditEnd( TRUE );
         ELSIF key_value = 'd' THEN (* delete *)
           Delete( TRUE );
         ELSIF key_value = 'i' THEN (* insert *)
           Insert( ' ' );
         ELSIF key_value = '^' THEN (* page up *)
           LineEnd( FALSE );
         ELSIF key_value = 'v' THEN (* page down *)
           LineEnd( TRUE );
         END;

       ELSIF function_key THEN
         IF key_value = '<' THEN      (* f12 *)
           still_editing := FALSE;
         ELSIF key_value = '8' THEN   (* f8 *)
           SetDefault( current_line );
           Mark( TRUE );
           EditEnd( TRUE );
         ELSIF key_value = '1' THEN   (* f1 *)
           ShowHelp;
           Display;
         ELSE
           Bell;
         END;

       ELSIF control_key THEN
         IF key_value = '[' THEN       (* esc *)
           Display;
         ELSIF key_value = 'h' THEN    (* backspace *)
           Delete( FALSE );
         ELSIF key_value = 'm' THEN    (* return *)
           LineMove( TRUE );
         ELSE
           Bell;
         END;

       ELSIF ordinary_key THEN
         IF first_edit_blank & lines[current_line].first_edit THEN
           (* set this item to all blank for this first edit *)
           lines[current_line].len := 0;
           current_pos := 1;
         END;
         IF shift_key THEN key_value := CAP( key_value ) END;
         Replace( key_value );

       ELSE
         Bell;
       END;

    UNTIL NOT still_editing;
    Normal; Clear;
  END;
END DoEditing;

(* ------------------------------------------- *)
PROCEDURE GetLine( line_number :CARDINAL;
                   VAR answer :ARRAY OF CHAR;
                   VAR done :BOOLEAN );
(* Return the value for the specified line. *)
BEGIN
  error := FALSE;
  done  := TRUE;

  IF ( line_number = 0 ) OR ( line_number > max_lines ) THEN
    done    := FALSE;
    error   := TRUE;
    message := 'Line number out of range';
  ELSE
    IF lines[line_number].on THEN
      Assign( lines[line_number].result, answer );
    ELSE
      done    := FALSE;
      error   := TRUE;
      message := 'Line not set';
    END;
  END;
END GetLine;

(* ------------------------------------------- *)
PROCEDURE ResetLine( line_number :CARDINAL );
VAR
  i :CARDINAL;
BEGIN
  error := FALSE;
  IF line_number = 0 THEN
    FOR i := 1 TO max_lines DO
       lines[i].on := FALSE;
    END;
  ELSE
    IF ( line_number > 0 ) & ( line_number <= max_lines ) THEN
      lines[line_number].on := FALSE;
    END;
  END;
END ResetLine;

(* ------------------------------------------- *)
PROCEDURE GetMessage( VAR problem :BOOLEAN; VAR string :ARRAY OF CHAR );
BEGIN
  IF error THEN
    Assign( message, string );
  ELSE
    string[0] := 0C;
  END;
END GetMessage;

BEGIN
  error := FALSE;
  ResetLine( 0 );
END FormEdit.
