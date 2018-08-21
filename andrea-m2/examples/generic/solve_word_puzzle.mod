MODULE SolveWordPuzzle;

(*
  Solve word puzzles by brute force as an example of backtracking.
  The puzzles are of the form of sums of words, where each letter
  represents a different character, and the words sum to form the total
  which is the last word. Some examples:

  ONLY   2306       A   1       A    2
  LION   0823     + B   2    +  B    9
  LOIN   0283     ---  --    ----  ---
+ UPON   5723     = C   3    =  CC  11
------  -----
= MENU   9135

  If some smarts were added to this program to try to decide before hand
  some invalid choices ( like humans do ) then it would complete much sooner.
*)
(* J. Andrea, Feb.19/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut    IMPORT WriteString, WriteCard, WriteLn, Read, ReadLn, EOL, Done;

CONST
  max_lines    = 10;
  max_per_line = 10;

TYPE
  String = ARRAY [0..max_per_line] OF CHAR;

VAR
  char_used   :ARRAY ["A".."Z"] OF BOOLEAN;
  char_value  :ARRAY ["A".."Z"] OF CARDINAL;
  digit_taken :ARRAY [0..9] OF BOOLEAN;

  char_list   :ARRAY [1..26] OF CHAR;
  n_char_used :CARDINAL;

  puzzle_line :ARRAY [1..max_lines] OF String;
  line_len    :ARRAY [1..max_lines] OF CARDINAL;
  n_lines     :CARDINAL;

  continue    :BOOLEAN;
  solved      :BOOLEAN;

  attempts    :CARDINAL;

  (* -------------------------------------------------- *)
  PROCEDURE ReadPuzzle;

  VAR
    c :CHAR;
    i :CARDINAL;

  BEGIN (* ReadPuzzle *)

    continue := TRUE;

    WriteString( 'Enter the puzzle, line by line' ); WriteLn;
    WriteString( 'Enter control-Z at end of input' ); WriteLn;
    WriteLn;

    n_lines := 0;

    Read( c );
    WHILE Done AND continue DO

      n_lines := n_lines + 1;

      IF n_lines > max_lines THEN
        WriteLn;
        WriteString('Attempt to add more lines than the program can handle');
        WriteLn;
        WriteString( 'The program will not continue' );
        WriteLn;
        continue := FALSE;
      ELSE

        i := 0;
        WHILE ( c # EOL ) AND continue DO

          IF i > max_per_line THEN
            WriteLn;
            WriteString('Attempt to add a line longer than the ');
            WriteString('program can handle');
            WriteLn;
            WriteString( 'The program will not continue' );
            WriteLn;
            continue := FALSE;
          ELSE

            c := CAP( c );
            IF ( c < 'A' ) OR ( c > 'Z' ) THEN
              WriteLn;
              WriteString('Input characters limited to a-Z' );
              WriteLn;
              WriteString( 'The program will not continue' );
              WriteLn;
              continue := FALSE;
            ELSE

              puzzle_line[n_lines][i] := c;
              Read( c );   i := i + 1;

            END;

          END;

        END;

        Read( c );

      END;

    END;

    IF continue THEN
      IF n_lines < 3 THEN
        WriteLn;
        WriteString( 'Must have 3 lines or more' );
        WriteLn;
        WriteString( 'The program will not continue' );
        WriteLn;
        continue := FALSE;
      END;
    END;

  END ReadPuzzle;

  (* -------------------------------------------------- *)
  PROCEDURE SetupPuzzle;

  VAR
    c       :CHAR;
    i, j, l :CARDINAL;

  BEGIN (* SetupPuzzle *)

     FOR i := 0 TO 9 DO
        digit_taken[i] := FALSE;
     END;

     FOR c := 'A' TO 'Z' DO
        char_used[c]  := FALSE;
        char_value[c] := 0;
     END;

     FOR i := 1 TO n_lines DO

        l := LEN( puzzle_line[i] );
        line_len[i] := l;

        FOR j := 0 TO l-1 DO
           c := puzzle_line[i][j];
           char_used[c] := TRUE;
        END;
     END;

     n_char_used := 0;
     FOR c := 'A' TO 'Z' DO
        IF char_used[c] THEN
          n_char_used            := n_char_used + 1;
          char_list[n_char_used] := c;
        END;
     END;

     IF n_char_used > 10 THEN
       WriteString( 'There cant be more than 10 different characters' );
       WriteLn;
       continue := FALSE;
     END;

  END SetupPuzzle;


  (* -------------------------------------------------- *)
  PROCEDURE PrintAttempts;
  BEGIN (* PrintAttempts *)

    WriteLn;
    WriteString( 'After ' ); WriteCard( attempts, 0 );
    WriteString( ' attempts' );
    WriteLn;

  END PrintAttempts;


  (* -------------------------------------------------- *)
  PROCEDURE TestSolution;
  (* convert each of the lines into a number, add them together *)
  (* and see if they add up to the last line *)

  VAR
     x, sum, total :CARDINAL;
     i             :CARDINAL;
     lines         :ARRAY [1..max_lines] OF CARDINAL;
     c             :CHAR; (* debug *)

    (* -------------------------------------------------- *)
    PROCEDURE LineToValue( string :String; len :CARDINAL ) :CARDINAL;

    (* convert the given line into a value using the current *)
    (* choices of digits for letters *)

    VAR
      i, j, sum :CARDINAL;
      c         :CHAR;

    BEGIN (* LineToValue *)

      sum := 0;
      FOR i := 0 TO len DO
         c   := string[i];
         sum := 10 * sum + char_value[ c ];
      END;

      RETURN sum;

    END LineToValue;

    (* -------------------------------------------------- *)
    PROCEDURE PrintSolution;

    CONST

      space_between = 2;

    VAR
      i, j, line :CARDINAL;
      size       :CARDINAL;

    BEGIN (* PrintSolution *)

      size := 0;
      FOR i := 1 TO n_lines DO
        IF line_len[i] > size THEN size := line_len[i]; END;
      END;
      size := size + 2;

      WriteLn;
      FOR i := 1 TO n_lines DO

        IF i = n_lines THEN     (* before last line output a total bar *)
          FOR j := 1 TO size DO
             WriteString( '-' );
          END;
          FOR j := 1 TO space_between DO
             WriteString( ' ' );
          END;
          FOR j := 1 TO size DO
             WriteString( '-' );
          END;
          WriteLn;
        END;

        (* output enough spaces first to line up this line *)
        FOR j := line_len[i] TO size - 1 DO
           WriteString( ' ' );
        END;

        (* output the original puzzle line *)

        WriteString( puzzle_line[i] );

        FOR j := 1 TO space_between DO
           WriteString( ' ' );
        END;

        (* output enough spaces first to line up this line *)
        FOR j := line_len[i] TO size - 1 DO
           WriteString( ' ' );
        END;

        (* output the result characters for this line *)

        FOR j := 0 TO line_len[i]-1 DO
          WriteCard( char_value[puzzle_line[i][j]], 1 );
        END;

        WriteLn;
      END;

    END PrintSolution;

  BEGIN (* TestSolution *)

     attempts := attempts + 1;

     sum := 0;
     FOR i := 1 TO n_lines -1 DO
        sum := sum + LineToValue( puzzle_line[i], line_len[i]-1 );
     END;
    
     total := LineToValue( puzzle_line[n_lines], line_len[n_lines]-1 );

     IF sum = total THEN
       solved := TRUE;
       PrintSolution;
     END;

  END TestSolution;


  (* -------------------------------------------------- *)
  PROCEDURE SolvePuzzle( next_char :CARDINAL );

  VAR
     d  :CARDINAL;
     c  :CHAR;

  BEGIN (* SolvePuzzle *)

    IF next_char > n_char_used THEN

      TestSolution; (* attempt to solve the puzzle *)

    ELSE

      (* get the next available character *)

      c := char_list[ next_char ];

      (* now try all the available digits for this character *)

      d := 0;
      WHILE ( d < 10 ) & ( NOT solved ) DO 

        IF NOT digit_taken[d] THEN

          digit_taken[d] := TRUE;
          char_value[c]  := d;

          SolvePuzzle( next_char + 1 );

          digit_taken[d] := FALSE;

        END;

        d := d + 1;   (* choose the next digit for this character *)
      END;

    END;

  END SolvePuzzle;


BEGIN (* SolveWordPuzzle *)

  ReadPuzzle;

  IF continue THEN

    SetupPuzzle;

    IF continue THEN

      solved := FALSE;    attempts := 0;

      WriteString( 'Starting to solve the puzzle, this may take some time...' );
      WriteLn;

      SolvePuzzle( 1 );

      IF NOT solved THEN
        WriteString( 'impossible' );  WriteLn;
      END;
   
    END;

    PrintAttempts;

  END;

END SolveWordPuzzle.
