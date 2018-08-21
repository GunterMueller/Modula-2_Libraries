MODULE WaTor;

(* John Andrea, reserected, July 1991 - but no population logging to file *)
(* John Andrea, Dec 1984 *)
(* this third version uses 4 or 8 directions for the neighbourhood *)
(* original idea from Scientific American Mathematical Games 1984 sometime *)
(* This code may be freely used and distributed, it may not be sold. *)

(*
* Gotta shake your tail to make a wave
* fish
* Gotta shake your tail to make a wave
* shark
* Gotta shake your tail to make a wave
* fish
* Gotta shake your tail to make a wave
* shark
* Get hot , red hot
* Everybody create , create your fate
* Everybody create , before its too late
* Live life like a diamond ring
*
*               - The Only Star In Heaven
*                 Frankie Goes To Hollywood
*)

FROM Conversions  IMPORT CardToString;

FROM VeryScreen IMPORT Home, EraseScreen, PutString, PutCursor, CursorOn;

FROM InOut IMPORT WriteString, WriteLn, WriteCard, Done,
                  ReadString,  ReadLn, ReadCard, ReadInt,
                  OpenOutput,  CloseOutput, out;

FROM Randomly IMPORT Choose_0_To_N_Minus_1, Choose_0_To_N, Choose_1_To_N;

FROM FileSystem IMPORT Name;

CONST
    screen_row_min = 2;      (* the coordinates of the playing field *)
    screen_row_max = 23;
    screen_col_min = 2;
    screen_col_max = 50;

    title_row = 4;          (* the title *)
    title_col = 55;

    label_1_row =  8;       (* fish population *)
    label_1_col = 55;
    label_2_row = 10;       (* shark population *)
    label_2_col = 55;
    label_4_row = 12;       (* the generation # *)
    label_4_col = 55;
    label_3_row = 15;       (* parameters *)
    label_3_col = 55;
    label_5_row = 21;       (* logging ? *)
    label_5_col = 55;
    
    row_min = 1;           (* the size of the planet - related to the screen *)
    row_max = screen_row_max - screen_row_min + 1;
    col_min = 1;
    col_max = screen_col_max - screen_col_min + 1;

    max_spots = (row_max - row_min + 1) * (col_max - col_min + 1);
    max_neighbours = 8;

TYPE
    String  = ARRAY [0..80] OF CHAR;

    Species = ( empty, fish, shark );

    Position = RECORD
               row, col :CARDINAL;
               END;

    Chronon   = CARDINAL;

    Directions= ( north, south, east, west,
                 north_east, north_west, south_east, south_west );

    WaTorSpot = RECORD
                next  :ARRAY [north..south_west]  OF   Position;
                life_form                       :Species;
                time_to_breed , time_to_starve  :Chronon;
                END;

    PlanetSurface = ARRAY [row_min..row_max] OF
                    ARRAY [col_min..col_max] OF WaTorSpot;
  
    ScreenMap   = ARRAY [row_min..row_max] OF
                  ARRAY [col_min..col_max] OF CHAR;
  
    NeighbourList = ARRAY [1..max_neighbours] OF Position;


VAR
    dead_planet, old, new :PlanetSurface;
    max_direction         :Directions;

    n_fish, n_sharks, 
    default_n_fish, default_n_sharks :CARDINAL;

    fish_breed, shark_breed, shark_starve,
    default_fish_breed, default_shark_breed, default_shark_starve :Chronon;

    fish_population, shark_population, population :CARDINAL;

    screen    :ScreenMap;
    picture   :ARRAY [empty..shark] OF CHAR;
      
    row, col    :CARDINAL;

    (* ---------------------------------------------------- *)
    PROCEDURE SpawnFish( row, col :CARDINAL);

    BEGIN (* SpawnFish *)

        new[row,col].life_form      := fish;
        new[row,col].time_to_breed  := Choose_0_To_N(fish_breed);
        new[row,col].time_to_starve := 0;
        fish_population             := fish_population + 1;

    END SpawnFish;

    (* ---------------------------------------------------- *)
    PROCEDURE SpawnSharks( row, col :CARDINAL);

    BEGIN (* SpawnSharks *)

        new[row,col].life_form      := shark;
        new[row,col].time_to_breed  := Choose_0_To_N(shark_breed);
        new[row,col].time_to_starve := Choose_0_To_N(shark_starve);
        shark_population            := shark_population + 1;

    END SpawnSharks;

    (* ---------------------------------------------------- *)
    PROCEDURE Initialize;
    (* set up all the parameters for the game, and seed the planet *)

    VAR
      n      :CARDINAL;
      answer :CHAR;
      i, j   :CARDINAL;

        (* ----------------------------------------------------- *)
        PROCEDURE Spawn( animal :Species );

        VAR
          row, col :CARDINAL;

        BEGIN (* Spawn *)

          row := Choose_0_To_N(row_max-row_min) + row_min;
          col := Choose_0_To_N(col_max-col_min) + col_min;

          IF new[row,col].life_form = empty THEN
             IF animal = fish THEN
                SpawnFish( row, col);
             ELSE
                SpawnSharks( row, col);
             END; (* if *)
          END; (* if *)

        END Spawn;

       (* ----------------------------------------------- *)
       PROCEDURE DrawScreen;

       VAR
         number :String;

       BEGIN (* DrawScreen *)

         (* initialize the screen *)
         Home; EraseScreen;

         (* draw up a box around the playing field *)
         FOR col := screen_col_min -1 TO screen_col_max +1 DO
            PutString('-',screen_row_min-1,col);
            PutString('-',screen_row_max+1,col);
         END; (* for *)
         FOR row := screen_row_min -1 TO screen_row_max +1 DO
            PutString('|',row,screen_col_min-1);
            PutString('|',row,screen_col_max+1);
         END; (* for *)

         PutString('Wa Tor',title_row,title_col);
         IF max_direction = west THEN
           PutString('(4 neighbours)',title_row,title_col+8)
         ELSE
           PutString('(8 neighbours)',title_row,title_col+8)
         END; (* if *)

         PutString('fish population:',label_1_row,label_1_col);
         PutString('shark population:',label_2_row,label_2_col);
         PutString('generation:',label_4_row,label_4_col);

         PutString('n_fish: ',label_3_row,label_3_col);
         PutString('n_sharks: ',label_3_row+1,label_3_col);
         PutString('fish breed: ',label_3_row+2,label_3_col);
         PutString('shark breed: ',label_3_row+3,label_3_col);
         PutString('shark starve: ',label_3_row+4,label_3_col);

         CardToString(n_fish,5,number);
         PutString(number,label_3_row,label_3_col+15);

         CardToString(n_sharks,5,number);
         PutString(number,label_3_row+1,label_3_col+15);

         CardToString(fish_breed,5,number);
         PutString(number,label_3_row+2,label_3_col+15);

         CardToString(shark_breed,5,number);
         PutString(number,label_3_row+3,label_3_col+15);

         CardToString(shark_starve,5,number);
         PutString(number,label_3_row+4,label_3_col+15);

       END DrawScreen;


    BEGIN (* Initialize *)

      population       := 0;
      shark_population := 0;
      fish_population  := 0;

      WriteLn;
      WriteString('You must control Y to stop the game'); WriteLn;

      WriteLn;
      WriteString('How many neighbours ( 4 or 8 ) ? ');
      ReadCard(n); ReadLn;
      IF (n # 4) & (n # 8) THEN n := 8; END;
      IF n = 4 THEN
         max_direction := west;
      ELSE
         max_direction := south_west;
      END; (* if *)

      WriteLn;
      WriteString('How many fish ?');
      ReadCard(n_fish);
      IF n_fish = 0 THEN
         n_fish := default_n_fish;
      END; (* if *)
      ReadLn;
      
      WriteLn;
      WriteString('How many sharks ?');
      ReadCard(n_sharks);
      IF n_sharks = 0 THEN
         n_sharks := default_n_sharks;
      END; (* if *)
      ReadLn;
      
      WriteLn;
      WriteString('What is the fish breeding time ?');
      ReadCard(fish_breed);
      IF fish_breed = 0 THEN
         fish_breed := default_fish_breed;
      END; (* if *)
      ReadLn;
      
      WriteLn;
      WriteString('What is the shark breeding time ?');
      ReadCard(shark_breed);
      IF shark_breed = 0 THEN
         shark_breed := default_shark_breed;
      END; (* if *)
      ReadLn;
      
      WriteLn;
      WriteString('What is the shark starving time ?');
      ReadCard(shark_starve);
      IF shark_starve = 0 THEN
         shark_starve := default_shark_starve;
      END; (* if *)
      ReadLn;
      
      (* now allocate all the fishies *)
      IF n_fish > max_spots THEN n_fish := max_spots END;
      FOR n := 1 TO n_fish DO
         Spawn(fish);
      END; (* for *)

      IF n_sharks > max_spots THEN n_sharks := max_spots END;
      FOR n := 1 TO n_sharks DO
         Spawn(shark);
      END; (* for *)

      population := fish_population + shark_population;

      DrawScreen;

    END Initialize;


    (* ---------------------------------------------------- *)
    PROCEDURE Play;

    VAR
      number :String;

      n_neighbours :CARDINAL;

      move_row, move_col    :CARDINAL;
      n_changes, generation :CARDINAL;

       (* ----------------------------------------------- *)
       PROCEDURE ShowChanges;

       VAR
         row, col :CARDINAL;
         c        :CHAR;

        BEGIN (* ShowChanges *)

          n_changes := 0;
          FOR row := row_min TO row_max DO
             FOR col := col_min TO col_max DO
                 c := picture[old[row,col].life_form];
                 IF c # screen[row,col] THEN
                    n_changes := n_changes + 1;
                    screen[row,col] := c;
                    PutString(c,
                       screen_row_min+(row-row_min),
                       screen_col_min+(col-col_min));
                 END; (* if *)
             END; (* for *) (* col *)
          END; (* for *) (* row *)

        END ShowChanges;

       (* --------------------------------------------- *)
       PROCEDURE Kill( row, col :CARDINAL; VAR nuke :PlanetSurface);
       (* make a spot empty *)

       BEGIN (* Kill *)

         nuke[row,col].life_form      := empty;
         nuke[row,col].time_to_breed  := 0;
         nuke[row,col].time_to_starve := 0;

       END Kill;


       (* --------------------------------------------- *)
       PROCEDURE Swim(row,col :CARDINAL; spot :WaTorSpot) :BOOLEAN;
       (* here's a function with side effects *)
       (* if the thing can move, then do it and return true *)

       VAR
         neighbours   :NeighbourList;
         new_position :Position;

          (* --------------------------------------------- *)
          PROCEDURE SearchBoth( catch :Species ) :BOOLEAN;
          (* another function with side-effects *)
          (* look around the neighbourhood for 'catch', and gather them up *)

          VAR
            close :Directions;
            close_row, close_col :CARDINAL;

          BEGIN (* SearchBoth *)

              n_neighbours := 0;
              FOR close := north TO max_direction DO

                 close_row := spot.next[close].row;
                 close_col := spot.next[close].col;
                 (* look in both the old and the new neighbourhoods *)
                 IF (old[close_row,close_col].life_form = catch) &
                    (new[close_row,close_col].life_form = catch) THEN
                    n_neighbours := n_neighbours + 1;
                    neighbours[n_neighbours] := spot.next[close];
                 END; (* if *)

              END; (* for *)

              RETURN (n_neighbours > 0 );

          END SearchBoth;

       BEGIN (* Swim *)

          IF SearchBoth( empty ) THEN

             (* it can be moved *)
             new_position := neighbours[Choose_1_To_N(n_neighbours)];
             move_row := new_position.row;
             move_col := new_position.col;

             (* turn off the old position *)
             Kill(row,col,old);

             (* and move this one *)
             new[move_row,move_col].life_form      := spot.life_form;
             new[move_row,move_col].time_to_breed  := spot.time_to_breed;
             new[move_row,move_col].time_to_starve := spot.time_to_starve;

             RETURN TRUE;

          ELSE
             RETURN FALSE;
          END; (* if *)

       END Swim;

       (* --------------------------------------------- *)
       PROCEDURE FishAction;

       VAR
         this_fish :WaTorSpot;
         row, col  :CARDINAL;

       BEGIN (* FishAction *)

          (* scan the whole planet *)
          FOR row := row_min TO row_max DO
             FOR col := col_min TO col_max DO

                 this_fish := old[row,col];
                 IF this_fish.life_form = fish THEN

                    (* this spot is a fish so do it *)
                    IF this_fish.time_to_breed # 0 THEN
                       this_fish.time_to_breed := this_fish.time_to_breed - 1;
                    END; (* if *)

                    IF Swim(row,col,this_fish) THEN

                       (* is it time for this guy to breed too *)
                       IF this_fish.time_to_breed = 0 THEN
                          (* yes, time to breed *)
                          SpawnFish(row,col);
 
                          (* and the older one must wait again *)
                          new[move_row,move_col].time_to_breed := fish_breed;
                       END; (* if *)

                    ELSE
                       (* can't be moved, just reset it *)
                       new[row,col].life_form      := fish;
                       new[row,col].time_to_breed  := this_fish.time_to_breed;
                       new[row,col].time_to_starve := 0;
                    END; (* if *)

                 END; (* if *)

             END; (* for *) (* col *)
          END; (* for *) (* row *)

       END FishAction;

       (* --------------------------------------------- *)
       PROCEDURE SharkAction;

       VAR
         this_shark :WaTorSpot;
         row, col   :CARDINAL;
         moved      :BOOLEAN;

         (* --------------------------------------------- *)
         PROCEDURE Eat( row, col :CARDINAL; spot :WaTorSpot ) :BOOLEAN;
         (* still another function with side-effects *)

         VAR
           neighbours   :NeighbourList;
           n_neighbours :CARDINAL;
           new_position :Position;

            (* --------------------------------------------- *)
            PROCEDURE Search( catch :Species ) :BOOLEAN;
            (* another function with side-effects *)
            (* look around the neighbourhood for 'catch', and gather them up *)

            VAR
              close :Directions;
              close_row, close_col :CARDINAL;

            BEGIN (* Search *)

                n_neighbours := 0;
                FOR close := north TO max_direction DO

                   close_row := spot.next[close].row;
                   close_col := spot.next[close].col;
                   (* look only in the new neighbourhood *)
                   IF new[close_row,close_col].life_form = catch THEN
                      n_neighbours := n_neighbours + 1;
                      neighbours[n_neighbours] := spot.next[close];
                   END; (* if *)

                END; (* for *)
 
                RETURN (n_neighbours > 0 );

            END Search;

         BEGIN (* Eat *)
             
             IF Search(fish) THEN

                (* it found a victim *)
                new_position := neighbours[Choose_1_To_N(n_neighbours)];
                move_row := new_position.row;
                move_col := new_position.col;

                (* turn off the victim *)
                Kill(move_row,move_col,new);  Kill(move_row,move_col,old);
                fish_population := fish_population - 1;

                (* and move this one *)
                new[move_row,move_col].life_form      := shark;
                new[move_row,move_col].time_to_breed  :=
                                                      this_shark.time_to_breed;
                new[move_row,move_col].time_to_starve := shark_starve;

               RETURN TRUE;

            ELSE

               RETURN FALSE;

            END; (* if *)

         END Eat;


       BEGIN (* SharkAction *)

          (* scan the whole planet *)
          FOR row := row_min TO row_max DO
            FOR col := col_min TO col_max DO

             this_shark := old[row,col];
             IF this_shark.life_form = shark THEN
                (* this spot is a shark so do it *)
                moved := FALSE;

                IF this_shark.time_to_breed # 0 THEN
                   this_shark.time_to_breed := this_shark.time_to_breed - 1;
                END; (* if *)

               IF Eat(row,col,this_shark) THEN
  
                  moved := TRUE;

               ELSE

                  (* found no victims, try to move it *)
                  IF this_shark.time_to_starve # 0 THEN
                     this_shark.time_to_starve := this_shark.time_to_starve-1;
                  END; (* if *)

                  (* unless its time to starve *)
                  IF this_shark.time_to_starve = 0 THEN

                     Kill(row,col,new);
                     shark_population := shark_population - 1;

                  ELSE

                     IF Swim(row,col,this_shark) THEN
                        moved := TRUE;
                     ELSE

                        (* can't be moved, just reset it *)
                        new[row,col].life_form := shark;
                        new[row,col].time_to_breed := this_shark.time_to_breed;
                        new[row,col].time_to_starve :=
                                                     this_shark.time_to_starve;
                     END; (* if *)

                  END; (* if *) (* starved *)
                END; (* if *)

                IF moved THEN
                   (* is it time for this guy to breed too *)
                   IF this_shark.time_to_breed = 0 THEN
                      (* yes, time to breed *)
                      SpawnSharks(row,col);

                      (* and the older one must wait again *)
                      new[move_row,move_col].time_to_breed := shark_breed;
                   END; (* if *)
                END; (* if *)

              END; (* if *)

            END; (* for *) (* col *)
          END; (* for *) (* row *)

       END SharkAction;


    BEGIN (* Play *)

      generation := 1;

      WHILE (fish_population > 0) & (shark_population > 0 ) DO

          FOR row := row_min TO row_max DO
            FOR col := col_min TO col_max DO
               new[row,col] := dead_planet[row,col];
            END; (* col *)
          END; (* row *)

          ShowChanges;

          (* population numbers *)

          CardToString(fish_population,5,number);
          PutString(number,label_1_row,label_1_col+18);

          CardToString(shark_population,5,number);
          PutString(number,label_2_row,label_2_col+18);

          CardToString(generation,5,number);
          PutString(number,label_4_row,label_4_col+18);

          FishAction;

          SharkAction;

          population := fish_population + shark_population;
          generation := generation + 1;

          FOR row := row_min TO row_max DO
            FOR col := col_min TO col_max DO
              old[row,col] := new[row,col];
            END; (* col *)
          END; (* row *)


      END; (* while *)

      (* finish up *)
      IF fish_population = 0 THEN
         PutString('Fish extinct',screen_row_max,1);
      ELSE
         PutString('Sharks extinct',screen_row_max,1);
      END;
      PutCursor(screen_row_max,1);   WriteLn;


    END Play;


    (* ------------------------------------------------------ *)
    PROCEDURE SetNeighbours;
    (* now define the neighbour spots for every spot on the surface *)
    (* remember that the planet is a torus *)


     TYPE
        map_parts = RECORD
                     row, col :INTEGER;
                     END;

     VAR
        wrap_map :ARRAY [north..south_west] OF map_parts;
        compass  :Directions;
        integer_row_min, integer_row_max,
          integer_col_min, integer_col_max :INTEGER;

        (* --------------------------------------------------------- *)
        PROCEDURE SetWrapMap;
        (* these are the values to be added to the given row,col to find
         *  the torus'ly wrapped position on a square grid *)

        BEGIN (* SetWrapMap *)

           wrap_map[north].row      := -1;
           wrap_map[north].col      :=  0;
           wrap_map[south].row      :=  1;
           wrap_map[south].col      :=  0;
           wrap_map[east].row       :=  0;
           wrap_map[east].col       :=  1;
           wrap_map[west].row       :=  0;
           wrap_map[west].col       := -1;
           wrap_map[north_east].row := -1;
           wrap_map[north_east].col :=  1;
           wrap_map[north_west].row := -1;
           wrap_map[north_west].col := -1;
           wrap_map[south_east].row :=  1;
           wrap_map[south_east].col :=  1;
           wrap_map[south_west].row :=  1;
           wrap_map[south_west].col := -1;

           (* and define the corresponding integer values *)
           integer_row_min := INTEGER(row_min);
           integer_row_max := INTEGER(row_max);
           integer_col_min := INTEGER(col_min);
           integer_col_max := INTEGER(col_max);

        END SetWrapMap;

        (* ---------------------------------------------------- *)
        PROCEDURE NewRow( row :CARDINAL; direction :Directions ) :CARDINAL;
        (* define the position of the wrapped row, if it needs to wrap *)

        VAR
          new_row :INTEGER;

        BEGIN (* NewRow *)

            new_row := INTEGER(row) + wrap_map[direction].row;

            IF new_row < integer_row_min THEN
              new_row := integer_row_max;
            ELSIF new_row > integer_row_max THEN
              new_row := integer_row_min;
            END; (* if *)

            RETURN CARDINAL(new_row);

        END NewRow;

        (* ---------------------------------------------------- *)
        PROCEDURE NewCol( col :CARDINAL; direction :Directions ) :CARDINAL;
        (* define the position of the wrapped column, if it needs to wrap *)

        VAR
          new_col :INTEGER;

        BEGIN (* NewCol *)

            new_col := INTEGER(col) + wrap_map[direction].col;

            IF new_col < integer_col_min THEN
              new_col := integer_col_max;
            ELSIF new_col > integer_col_max THEN
              new_col := integer_col_min;
            END; (* if *)

            RETURN CARDINAL(new_col);

        END NewCol;

    BEGIN (* SetNeighbours *)

      (* this algorithm only works if the minimum row or column is not zero,
       *  i.e. the array bounds for the planet cannot start at zero
       *  thats why the following check is included *)
      IF (row_min < 1) OR (col_min < 1) THEN
         (* somebody screwed up when they set up the constants *)
         WriteLn; WriteLn;
         WriteString(' Implementation ERROR '); WriteLn;
         WriteString(' detected in the procedure SetNeighbours'); WriteLn;
         IF row_min < 1 THEN
            WriteString(' the value of row_min cannot be zero'); WriteLn;
         END; (* if *)
         IF col_min < 1 THEN
            WriteString(' the value of col_min cannot be zero'); WriteLn;
         END; (* if *)
         WriteString(' reset the CONST value'); WriteLn;
         WriteLn;
         HALT;
      END; (* if *)


      SetWrapMap;

      (* set up the pseudo-pointers for all the neighbours in the matrix *)
      FOR row := row_min TO row_max DO
         FOR col := col_min TO col_max DO

            (* empty out the arrays too *)
            screen[row,col]                     := picture[empty];
            dead_planet[row,col].life_form      := empty;
            dead_planet[row,col].time_to_breed  := 0;
            dead_planet[row,col].time_to_starve := 0;

            FOR compass := north TO south_west DO
               dead_planet[row,col].next[compass].row := NewRow(row,compass);
               dead_planet[row,col].next[compass].col := NewCol(col,compass);
            END; (* for *) (* compass *)

         END; (* for *) (* col *)
      END; (* for *) (* row *)

    END SetNeighbours;

BEGIN (* WaTor *)

(* what the fish look like *)
picture[empty] := ' ';
picture[fish]  := '.';
picture[shark] := 'o';

(* set the defaults *)
default_n_fish       := 200;
default_n_sharks     := 20;
default_fish_breed   := 3;
default_shark_breed  := 10;
default_shark_starve := 3;

SetNeighbours;

FOR row := row_min TO row_max DO
  FOR col := col_min TO col_max DO
    new[row,col] := dead_planet[row,col];
  END; (* for *) (* col *)
END; (* for *) (* row *)

Initialize;

FOR row := row_min TO row_max DO
  FOR col := col_min TO col_max DO
    old[row,col] := new[row,col];
  END; (* for *) (* col *)
END; (* for *) (* row *)

Play;

END WaTor.
