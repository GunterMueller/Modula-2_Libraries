IMPLEMENTATION MODULE LifeModule;

(* objects needed by Life and life Editor programs *)
(* John Andrea, Nov.16/ 1985 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM GetCharacter IMPORT Get, GetNoWait, StartGet, StopGet;

FROM Conversions  IMPORT CardToString;

FROM VeryScreen IMPORT Home, EraseScreen, EraseLine,
                       PutCursor, CursorOn, PutString;

FROM InOut IMPORT WriteString, WriteLn, WriteCard,
                  ReadString,  ReadLn,  ReadCard, Read,
                  OpenInput,   CloseInput, in;

FROM FileSystem IMPORT Eof, File, Done, EOL;

TYPE
  GameType    = (conway,con_mod,fredkin,fred_mod,other);
  Description = ARRAY [0..80] OF CHAR;

VAR
  row, col, population :CARDINAL;
  c                    :CHAR;
  game                 :GameType;
  name                 :Description;
  new, old             :Generation;

    (* ---------------------------------------------------- *)
    PROCEDURE Genesis;

    (* read a set of coordinates (row,col) of positions that are to
       be set to ALIVE positions *)

    VAR
      input_line                         :CARDINAL;
      row_min, row_max, col_min, col_max :CARDINAL;

    BEGIN (* Genesis *)

      row_min := 1; row_max := screen_row_max - screen_row_min + 1;
      col_min := 1; col_max := screen_col_max - screen_col_min + 1;

      population := 0;

      new := all_dead;

      (* read off the file description at line # 1 *)
      Read(c);
      col := 0;
      WHILE (c # EOL) & (col < 80) DO
        name[col] := c;
        col := col + 1;
        Read(c);
      END; (* while *)
      name[col] := 0C;
      ReadLn;

      input_line := 2;

      (* read in the coordinates *)
      ReadCard(row);
      WHILE NOT Eof(in) DO

         ReadCard(col); ReadLn;

         IF (row >= row_min) & (row <= row_max) THEN
            row := row + screen_row_min - 1;
         ELSE
            WriteString('row out of range, input line #');
            WriteCard(input_line,4); WriteLn;
            WriteString('min row = '); WriteCard(row_min,3);
            WriteString('max row = '); WriteCard(row_max,3); WriteLn;
            row := screen_row_min;
         END; (* if *)

         IF (col >= col_min) & (col <= col_max) THEN
            col := col + screen_col_min - 1;
         ELSE
            WriteString('col out of range, input line #');
            WriteCard(input_line,4); WriteLn;
            WriteString('min col = '); WriteCard(col_min,3);
            WriteString('max col = '); WriteCard(col_max,3); WriteLn;
            col := screen_col_min;
         END; (* if *)

         population   := population + 1;
         new[row,col] := alive;

         input_line := input_line + 1;
         ReadCard(row);
      END; (* while *)

    END Genesis;

    (* ---------------------------------------------------- *)
    PROCEDURE Genesis2( VAR new :Generation; VAR population :CARDINAL);

    (* read a set of coordinates (row,col) of positions that are to
       be set to ALIVE positions *)
    (* for MAKELIFE only, no error reporting *)

    VAR
      input_line                         :CARDINAL;
      row_min, row_max, col_min, col_max :CARDINAL;

    BEGIN (* Genesis *)

      row_min := 1; row_max := screen_row_max - screen_row_min + 1;
      col_min := 1; col_max := screen_col_max - screen_col_min + 1;

      population := 0;

      new := all_dead;

      (* read off the file description at line # 1 *)
      Read(c);
      col := 0;
      WHILE (c # EOL) & (col < 80) DO
        name[col] := c;
        col := col + 1;
        Read(c);
      END; (* while *)
      name[col] := 0C;
      ReadLn;

      input_line := 2;
      (* read in the coordinates *)
      ReadCard(row);
      WHILE NOT Eof(in) DO

         ReadCard(col); ReadLn;

         IF (row >= row_min) & (row <= row_max) THEN
            row := row + screen_row_min - 1;
         END; (* if *)

         IF (col >= col_min) & (col <= col_max) THEN
            col := col + screen_col_min - 1;
         END; (* if *)

         population   := population + 1;
         new[row,col] := alive;

         input_line   := input_line + 1;
         ReadCard(row);

      END; (* while *)

    END Genesis2;


    (* ----------------------------------------------------- *)
    PROCEDURE ShowBirths( new, old :Generation; VAR changes :CARDINAL);

    (* show the difference between the two generations *)

    BEGIN (* ShowBirths *)

       changes := 0;

       (* run through the whole field *)

       FOR row := screen_row_min TO screen_row_max DO
          FOR col := screen_col_min TO screen_col_max DO

              (* optimize screen updating, only change a position *)
              (* if it needs changing                             *)

              IF new[row,col] # old[row,col] THEN

                 changes := changes + 1;
                 PutString( ekg[new[row,col]], row, col );

              END; (* if *)

          END; (* for *) (* col *)
       END; (* for *) (* row *)

   END ShowBirths;

    (* -------------------------------------------------- *)
   PROCEDURE StartShow(name : Description);

   (* put up the box and name *)

   BEGIN (* StartShow *)

      (* draw up a box around the playing field *)

      FOR col := screen_col_min -1 TO screen_col_max +1 DO
         PutString( '-', screen_row_min-1, col );
         PutString( '-', screen_row_max+1, col );
      END; (* for *)

      FOR row := screen_row_min -1 TO screen_row_max +1 DO
         PutString( '|', row, screen_col_min-1 );
         PutString( '|', row, screen_col_max+1 );
      END; (* for *)

      (* put up the name *)
      PutString( name, 1, 1 );

   END StartShow;

    (* ----------------------------------------------------- *)
    PROCEDURE ShowGeneration( new :Generation );

    (* put up a boundary and show the generation *)

    VAR
       screen_changes :CARDINAL;

    BEGIN (* ShowGeneration *)

      StartShow(name);

      ShowBirths( new , all_dead , screen_changes );

    END ShowGeneration;


    (* ---------------------------------------------------- *)
    PROCEDURE PlayGod;

    (* keep generating new generations *)

    TYPE
       ContinueModes = ( step , go );

    VAR
      row, col     :CARDINAL;
      god_function :ARRAY [0..17] OF LifeFunction; 
                                               (* what to do with neighbours *)
      diagnosis    :ARRAY [dead..alive] OF CARDINAL; (* convert it to cardinal *)

      c              :CHAR;
      generation     :CARDINAL;
      number         :ARRAY [0..20] OF CHAR;
      screen_changes :CARDINAL;

      finished       :BOOLEAN;
      mode           :ContinueModes;

       (* --------------------------------------------------- *)
       PROCEDURE GiveBirth;

       (* make the next generation using the god_function *)

       VAR
         neighbours, naybor_row, naybor_col :CARDINAL;
         new_population                     :CARDINAL;

       BEGIN (* GiveBirth *)

         new_population := 0;

         (* run through the whole generation *)

         FOR row := screen_row_min TO screen_row_max DO
            FOR col := screen_col_min TO screen_col_max DO

               neighbours := 0;

               IF game = conway THEN

                  FOR naybor_row := row-1 TO row+1 DO
                     FOR naybor_col := col-1 TO col+1 DO
                         IF old[naybor_row,naybor_col] = alive THEN
                            neighbours := neighbours + 1;
                         END; (* if *)
                     END; (* for *) (* col *)
                  END; (* for *) (* row *)

                  IF old[row,col] = alive THEN
                     (* remove itself if necessary *)
                     neighbours := neighbours - 1;
                  END; (* if *)
                  neighbours := neighbours + diagnosis[old[row,col]]*9;

                ELSIF game = con_mod THEN

                  FOR naybor_row := row-1 TO row+1 DO
                     FOR naybor_col := col-1 TO col+1 DO
                         IF old[naybor_row,naybor_col] = alive THEN
                            neighbours := neighbours + 1;
                         END; (* if *)
                     END; (* for *) (* col *)
                  END; (* for *) (* row *)

                  IF old[row,col] = alive THEN
                     (* remove itself if necessary *)
                     neighbours := neighbours - 1;
                  END; (* if *)

                ELSIF game = fredkin THEN

                  IF old[row-1,col] = alive THEN
                     neighbours := neighbours + 1;
                  END; (* if *)

                  IF old[row+1,col] = alive THEN
                     neighbours := neighbours + 1;
                  END; (* if *)

                  IF old[row,col-1] = alive THEN
                     neighbours := neighbours + 1;
                  END; (* if *)

                  IF old[row,col+1] = alive THEN
                     neighbours := neighbours + 1;
                  END; (* if *)

                ELSE (* modified fredkin *)

                  IF old[row-1,col-1] = alive THEN
                     neighbours := neighbours + 1;
                  END; (* if *)

                  IF old[row+1,col-1] = alive THEN
                     neighbours := neighbours + 1;
                  END; (* if *)

                  IF old[row-1,col+1] = alive THEN
                     neighbours := neighbours + 1;
                  END; (* if *)

                  IF old[row+1,col+1] = alive THEN
                     neighbours := neighbours + 1;
                  END; (* if *)

               END; (* if *)

               IF god_function[neighbours] = alive THEN
                  new[row,col]   := alive;
                  new_population := new_population + 1;
               ELSE
                  new[row,col] := dead;
               END; (* if *)

            END; (* for *) (* col *)
         END; (* for *) (* row *)

         population := new_population;

       END GiveBirth;

       (* ----------------------------------------------- *)
       PROCEDURE DrawScreen;
       BEGIN (* DrawScreen *)

          (* initialize the screen *)
          Home; EraseScreen;

          IF game = conway THEN
             PutString('Conways Game Of Life',title_row,title_col);
          ELSIF game = con_mod THEN
             PutString('Modified Conways',title_row,title_col);
          ELSIF game = fredkin THEN
             PutString('Fredkins Game Of Life',title_row,title_col);
          ELSE
             PutString('Modified Fredkins',title_row,title_col);
          END; (* if *)

          StartShow(name);

          PutString('generation:',label_1_row,label_1_col);
          PutString('population:',label_2_row,label_2_col);

          (* option for menu's *)
          PutString('give <CR> for next',menu_row,menu_col);
          PutString('Q to quit',menu_row+1,menu_col);
          PutString('G to not ask again',menu_row+2,menu_col);

       END DrawScreen;


    BEGIN (* PlayGod *)

      WriteLn;
      WriteString('Which type of Life, Conway or Fredkin'); WriteLn;
      WriteString('   or Modified Conway or Modified Fredkin'); WriteLn;
      WriteString('( C or F or D or G ) ? ');
      (* WriteString(' C or F ? '); *)
      ReadString(c); ReadLn; c := CAP(c);
      WHILE (c # 'C') & (c # 'F') & (c # 'D') & (c # 'G') DO
         WriteLn;
         WriteString('( C or F or D or G ) ? ');
         ReadString(c); ReadLn; c := CAP(c);
      END; (* while *)
      WriteLn;

      (* convert dead,alive to a CARDINAL for function arithmetic *)
      diagnosis[dead]  := 0;
      diagnosis[alive] := 1;

      (* define the function to give life or death according to
         the number of neighbours *)
      FOR row := 0 TO 17 DO
         god_function[row] := dead;
      END; (* for *)

      IF c = 'C' THEN
         game             := conway;
         god_function[3]  := alive;
         god_function[11] := alive;
         god_function[12] := alive;
      ELSIF c = 'F' THEN
         game             := fredkin;
         god_function[1]  := alive;
         god_function[3]  := alive;
      ELSIF c = 'G' THEN
         (* modified fredkin rules are the same as fredkin,
            except that it uses the 4 diagonal elements    *)
         game             := fred_mod;
         god_function[1]  := alive;
         god_function[3]  := alive;
      ELSE
         (* modified conway rules are the same as conway
            except that it uses the odd/even rules *)
         game             := con_mod;
         god_function[1]  := alive;
         god_function[3]  := alive;
         god_function[5]  := alive;
         god_function[7]  := alive;
         god_function[9]  := alive;
         god_function[11] := alive;
         god_function[13] := alive;
         god_function[17] := alive;
      END; (* if *)

      DrawScreen;

      CursorOn( FALSE );

      StartGet;

      c              := ' ';
      screen_changes := population;
      generation     := 1;

      (* run the game, until zero-population or user says Quit *)
      mode := step;    finished := FALSE;

      WHILE NOT finished DO

          ShowBirths( new, old, screen_changes );

          finished := ( population = 0 ) OR ( screen_changes = 0 );

          (* generation number *)
          CardToString( generation, 5, number );
          PutString( number, label_1_row, label_1_col+12 );

          (* population number *)
          CardToString( population, 5, number );
          PutString( number, label_2_row, label_2_col+12 );

          IF NOT finished THEN
             old := new;             (* flip generations *)

             GiveBirth;
             generation := generation + 1;

             IF mode = step THEN
               Get(c);
               IF ( c = 'Q' ) OR ( c = 'q' ) THEN
                  finished := TRUE;
               ELSE
                  IF ( c = 'G' ) OR ( c = 'g' ) THEN
                     (* enter GO mode *)
                     (* no more options for menu's *)
                     PutCursor( menu_row,   menu_col ); EraseLine;
                     PutCursor( menu_row+1, menu_col ); EraseLine;
                     PutCursor( menu_row+2, menu_col ); EraseLine;
                     mode := go;
                     PutCursor( q_row, q_col ); EraseLine;
                     PutString( 'press a character to stop', q_row, q_col);
                  END; (* if *)
               END; (* if *)

             ELSE (* already in GO mode *)

               GetNoWait(c);
               IF c # 0C THEN
                  (* return to step mode, and put back the option list *)
                  mode := step;
                  PutString('give <CR> for next',menu_row,menu_col);
                  PutString('Q to quit',menu_row+1,menu_col);
                  PutString('G to not ask again',menu_row+2,menu_col);
                  PutCursor( q_row, q_col ); EraseLine;
               END; (* if *)
             END; (* if *)
          END; (* if *)
      END; (* while *)

      StopGet;

      (* finish up *)
      PutCursor( menu_row,   menu_col );  EraseLine;
      PutCursor( menu_row+1, menu_col );  EraseLine;
      PutCursor( menu_row+2, menu_col );  EraseLine;

      PutCursor( q_row, q_col );  EraseLine;

      IF screen_changes = 0 THEN
         PutString( 'population STABLE', menu_row, menu_col );
      ELSE
         IF population = 0 THEN
            PutString( 'population EXTINCT', menu_row, menu_col );
         ELSE
            PutString( 'QUIT', menu_row, menu_col );
         END; (* if *)
      END; (* if *)

      PutCursor( screen_row_max, 1 );  CursorOn( TRUE );

    END PlayGod;


BEGIN (* LifeModule *)

(* make all of the old one DEAD *)
FOR row := screen_row_min-1 TO screen_row_max+1 DO
   FOR col := screen_col_min-1 TO screen_col_max+1 DO
      all_dead[row,col] := dead;
   END; (* for *) (* col *)
END; (* for *) (* row *)

(* what the dead and alive elements look like *)
ekg[dead]  := ' ';
ekg[alive] := '*';

END LifeModule.
