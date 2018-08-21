MODULE MakeLife;

(* this is a coordinate editor for Life files *)
(* editor idea from Jerry MacGillivray *)

(* John Andrea, Nov.16/1985 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM VeryScreen IMPORT Home, PutCursor, PutString,
                       EraseScreen,  EraseLine, CursorOn;

FROM InOut IMPORT WriteString, WriteLn,
                  ReadString,  ReadLn,  ReadCard, Read,
                  OpenInput,   CloseInput, in ,out;

FROM FileSystem IMPORT Eof, File, Done, EOL;

FROM Conversions IMPORT CardToString;

FROM GetCharacter IMPORT Get, SetNoPassallCharacterMode,
                             UnSetNoPassallCharacterMode;

FROM TextIO IMPORT Write, OpenOutput, Close, WriteCard;

FROM LifeModule IMPORT screen_row_min, screen_row_max,
                       screen_col_min, screen_col_max,
                       title_row,      title_col,
                       label_1_row,    label_1_col,
                       label_2_row,    label_2_col,
                       menu_row,       menu_col,
                       q_row,          q_col, 
                       LifeFunction,   Generation,
                       ekg,            all_dead,
                       Genesis2,       ShowGeneration, ShowBirths;

TYPE
   SelectOption = (quit,exit,continue);
   MotionStatus = (ok,stopped,none);

VAR
  births                 :Generation;
  population             :CARDINAL;
  row, col               :CARDINAL;
  last_row, last_col     :CARDINAL;
  actual_row, actual_col :CARDINAL;
  temp_row,   temp_col   :CARDINAL;
  save_file              :File;
  option                 :SelectOption;
  c, last_dir            :CHAR;
  row_center, col_center :CARDINAL;
  move_status            :MotionStatus;
  changes                :CARDINAL;
   
   (* -------------------------------------------- *)
   PROCEDURE Bell;

   CONST 
      bell = 7C;

   BEGIN (* Bell *)

      Write(out,bell); WriteLn;

   END Bell;


    (* ----------------------------------------------- *)
    PROCEDURE DrawScreen;

    BEGIN (* DrawScreen *)

      Home; EraseScreen;

      ShowGeneration(births);

      (* title *)
      PutString( 'Life Editor',             title_row, title_col );
      PutString( 'use keypad numbers as directions', title_row+2, title_col+1 );
      PutString( '5 ="flip value of cell"', title_row+3, title_col+1 );
      PutString( '0 =move to last pos',     title_row+4, title_col+1 );
      PutString( '. + dir move to border',  title_row+5, title_col+1 );

      (* option for menu's *)
      PutString( 'E to exit',         menu_row,   menu_col );
      PutString( 'Q to quit',         menu_row+1, menu_col );
      PutString( 'W to redraw',       menu_row+2, menu_col );
      PutString( 'L + dir for line',  menu_row+3, menu_col );
      PutString( 'A + dir for alive', menu_row+4, menu_col );
      PutString( 'S + dir to shift',  menu_row+5, menu_col );

    END DrawScreen;

    (* ----------------------------------------------- *)
    PROCEDURE Shift( row_move, col_move :INTEGER );

    VAR
      temp_1, temp_2 :Generation; (* hold the current one *)
      move           :CARDINAL;
      row, col       :CARDINAL;

    BEGIN (* Shift *)

      move := CARDINAL(ABS(row_move));
      IF row_move > 0 THEN

         temp_1 := all_dead;

         (* don't move the columns *)
         FOR col := screen_col_min TO screen_col_max DO
            (* move down *)
            FOR row := screen_row_min TO screen_row_max-move DO
                temp_1[row+move,col] := births[row,col];
            END; (* for *)
         END; (* for *)

      ELSIF row_move < 0 THEN

         temp_1 := all_dead;

         (* don't move the columns *)
         FOR col := screen_col_min TO screen_col_max DO
            (* move up *)
            FOR row := screen_row_min+move TO screen_row_max DO
                temp_1[row-move,col] := births[row,col];
            END; (* for *)
         END; (* for *)

      ELSE
         temp_1 := births;
      END; (* if *)

      move := CARDINAL(ABS(col_move));

      IF col_move > 0 THEN

         temp_2 := all_dead;

         (* don't move the rows *)
         FOR row := screen_row_min TO screen_row_max DO
            (* move right *)
            FOR col := screen_col_min TO screen_col_max-move DO
               temp_2[row,col+move] := temp_1[row,col];
            END; (* for *) (* col *)
         END; (* for *) (* row *)

      ELSIF col_move < 0 THEN

         temp_2 := all_dead;

         (* don't move the rows *)
         FOR row := screen_row_min TO screen_row_max DO
            (* move left *)
            FOR col := screen_col_min+move TO screen_col_max DO
               temp_2[row,col-move] := temp_1[row,col];
            END; (* for *) (* col *)
         END; (* for *) (* row *)

      ELSE
         temp_2 := temp_1;
      END; (* if *)

      ShowBirths( temp_2, births, changes );

      births := temp_2;

    END Shift;

    (* ---------------------------------------- *)
    PROCEDURE Center;

    VAR
      row, col                         :CARDINAL;
      min_row, min_col                 :CARDINAL;
      max_row, max_col                 :CARDINAL;
      temp_row_center, temp_col_center :CARDINAL;
      row_diff, col_diff               :INTEGER;

    BEGIN (* Center *)

      min_col := screen_col_max;
      max_col := screen_col_min;
      FOR row := screen_row_min TO screen_row_max DO

         col := screen_col_min;
         WHILE (col < screen_col_max) & (births[row,col] = dead) DO
           col := col + 1;
         END; (* while *)
         IF col < min_col THEN
            min_col := col;
         END; (* if *)

         col := screen_col_max;
         WHILE (col > screen_col_min) & (births[row,col] = dead) DO
           col := col - 1;
         END; (* while *)
         IF col > max_col THEN
            max_col := col;
         END; (* if *)

      END; (* for *) (* row *)

      min_row := screen_row_max;
      max_row := screen_row_min;
      FOR col := screen_col_min TO screen_col_max DO

         row := screen_row_min;
         WHILE (row < screen_row_max) & (births[row,col] = dead) DO
           row := row + 1;
         END; (* while *)
         IF row < min_row THEN
            min_row := row;
         END; (* if *)

         row := screen_row_max;
         WHILE (row > screen_row_min) & (births[row,col] = dead) DO
           row := row - 1;
         END; (* while *)
         IF row > max_row THEN
            max_row := row;
         END; (* if *)

      END; (* for *) (* col *)

      temp_row_center := ((max_row - min_row) DIV 2) + min_row;
      temp_col_center := ((max_col - min_col) DIV 2) + min_col;

      row_diff := INTEGER(temp_row_center) - INTEGER(row_center);
      col_diff := INTEGER(temp_col_center) - INTEGER(col_center);

      Shift( -row_diff, -col_diff );

    END Center;

    (* ----------------------------------------------------- *)
    PROCEDURE Motion( direction :CHAR );
    BEGIN (* Motion *)

       move_status := ok;

       CASE direction OF
     '1' :
         IF (row >= screen_row_max) OR (col <= screen_col_min) THEN
           Bell;
           move_status := stopped;
         ELSE
           row := row + 1;
           col := col - 1;
         END;
    |'2' :
         IF row >= screen_row_max THEN
            Bell;
            move_status := stopped;
         ELSE
            row := row + 1;
         END;
    |'3' :
         IF (row >= screen_row_max) OR (col >= screen_col_max) THEN
           Bell;
           move_status := stopped;
         ELSE
           row := row + 1;
           col := col + 1;
         END;
    |'4' :
         IF col <= screen_col_min THEN
           Bell;
           move_status := stopped;
         ELSE
           col := col - 1;
         END;
    |'6' :
         IF col >= screen_col_max THEN
           Bell;
           move_status := stopped;
         ELSE
           col := col + 1;
         END;
    |'7' :
         IF (row <= screen_row_min) OR (col <= screen_col_min) THEN
           Bell;
           move_status := stopped;
         ELSE
           row := row - 1;
           col := col - 1;
         END;
    |'8' :
         IF row <= screen_row_min THEN
           Bell;
           move_status := stopped;
         ELSE
           row := row - 1;
         END;
    |'9' :
         IF (row <= screen_row_min) OR (col >= screen_col_max) THEN
           Bell;
           move_status := stopped;
         ELSE
           row := row - 1;
           col := col + 1;
         END;
     ELSE
         Bell;
         move_status := none;
     END; (* case *)

    END Motion;

    (* ------------------------------------------------ *)
    PROCEDURE Flip;

    (* flip the value of the current cell *)

    BEGIN (* Flip *)

       IF births[row,col] = alive THEN
          PutString( ekg[dead], row, col );
          births[row,col] := dead;
          population := population - 1;
       ELSE
          PutString( ekg[alive], row, col );
          births[row,col] := alive;
          population := population + 1;
       END;

    END Flip;


    (* ------------------------------------------------ *)
    PROCEDURE NoFlip;

    (* make the current cell alive *)

    BEGIN (* NoFlip *)

       IF births[row,col] = dead THEN
          PutString( ekg[alive], row, col );
          births[row,col] := alive;
          population := population + 1;
       END;

    END NoFlip;


BEGIN (* MakeLife *)

(* center of the screen *)
row_center := ((screen_row_max - screen_row_min) DIV 2) + screen_row_min;
col_center := ((screen_col_max - screen_col_min) DIV 2) + screen_col_min;

WriteLn;
WriteString('Do you want to edit an existing coordinate file ? ');
Read(c); ReadLn;
IF CAP(c) = 'Y' THEN
  WriteLn;
  WriteString('What is the name of the coordinate file ? ');
  OpenInput('.DAT');
  Genesis2( births, population );
  CloseInput;
END; (* if *)

DrawScreen;

row      := row_center;  col      := col_center;
last_row := row;         last_col := col;
last_dir := ' ';

PutCursor( row, col);

SetNoPassallCharacterMode;

option := continue;

WHILE option = continue DO

  Get(c);

  CASE c OF
'.' :  (* goto border, using next direction *)
     Get(c);

     CASE c OF
     '1' :
          row := screen_row_max;
          col := screen_col_min;
    |'2' :
          row := screen_row_max;
    |'3' :
          row := screen_row_max;
          col := screen_col_max;
    |'4' :
          col := screen_col_min;
    |'5' :
          row := row_center;
          col := col_center;
    |'6' :
          col := screen_col_max;
    |'7' :
          row := screen_row_min;
          col := screen_col_min;
    |'8' :
          row := screen_row_min;
    |'9' :
          row := screen_row_min;
          col := screen_col_max;
     ELSE
       Bell;
     END; (* case *)

|'0' : (* goto last position *)
     temp_row := row;         temp_col := col;
     row := last_row;         col := last_col;
     last_row := temp_row;    last_col := temp_col;

|'1'..'4','6'..'9' :
     last_row := row;         last_col := col;
     last_dir := c;
     Motion(c);

|'5' :
     Flip;
     (* Motion(last_dir); *)

| 'E','e' :     (* exit *)
     option := exit;

| 'Q','q' :     (* quit *)
     option := quit;

| 'W','w' : (* redraw *)
     DrawScreen;

| 'L','l' : (* draw a line *)
     Get(c);
     WHILE move_status = ok DO
       Flip;
       Motion(c);
     END; (* while *)

| 'A','a' : (* draw a line , always alive *)
     Get(c);
     WHILE move_status = ok DO
       NoFlip;
       Motion(c);
     END; (* while *)

| 'S','s' : (* move the whole thing *)
     Get(c);
     CASE c OF
     '1' :
          Shift(1,-1)
    |'2' :
          Shift(1,0)
    |'3' :
          Shift(1,1)
    |'4' :
          Shift(0,-1)
    |'5' : (* center it *)
          Center;
    |'6' :
          Shift(0,1)
    |'7' :
          Shift(-1,-1)
    |'8' :
          Shift(-1,0)
    |'9' :
          Shift(-1,1)
     ELSE
       Bell;
     END; (* case *) (* shift *)

  ELSE (* case *)
     Bell;
     PutCursor( row, col );
  END; (* case *)

  PutCursor( row, col );
  move_status := ok;

END; (* while *)

UnSetNoPassallCharacterMode;

PutCursor( 23, 1 ); WriteLn;

IF option = exit THEN
   WriteLn;
   WriteString('What will be the name of the coordinate file ? ');
   OpenOutput(save_file,'.DAT');

   (* read off the file description at line # 1 *)
   WriteLn; WriteString('input a description'); WriteLn;
   Read(c);
   col := 0;
   WHILE (c # EOL) & (col < 80) DO
      Write(save_file,c);
      col := col + 1;
      Read(c);
   END; (* while *)
   Write(save_file,EOL);
   ReadLn;

   (* put the screen coordinates into the file *)
   FOR row := screen_row_min TO screen_row_max DO
      actual_row := row - screen_row_min + 1;
      FOR col := screen_col_min TO screen_col_max DO
         actual_col := col - screen_col_min + 1;
         IF births[row,col] = alive THEN
            WriteCard(save_file,actual_row,3);
            Write(save_file,' ');
            WriteCard(save_file,actual_col,3);
            Write(save_file,EOL);
         END; (* if *)
      END; (* for *) (* col *)
   END; (* for *) (* row *)
END;

END MakeLife.
