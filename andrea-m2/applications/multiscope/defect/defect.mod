MODULE Defect;

(* Cellular Automata Defects *)
(* See A.K. Dewdney, Scientific American, Aug.89 "Computer Recreations" *)

(* V4.0, J.Andrea, Jun.22/93 -general fixups *)
(* V3.2, jaa, Jun.13/93 -only 4 neighbours *)
(* V3.0, jaa, May.2/92 - eat neighbours *)
(* This code may be freely used and distributed, it may not be sold *)

FROM Break IMPORT EnableBreak;
FROM Graphics IMPORT gph16CHiRes, ScreenMode, GetScreenMode, Circle;
FROM Terminal IMPORT Read, KeyPressed;
FROM Random IMPORT RandomInt;
FROM InOut IMPORT WriteString, WriteInt, ReadInt, WriteLn;

CONST
  size  = 100;
  min_states = 8; max_states = 15;  (* number useful that is, 0-15 is range *)
  
  first_state    = 'a';
  abs_last_state = 'o';    (* 15 - 1 past 'a' *)

  radius   = 2;
  offset   = 40;
  diameter = 2 * radius;
    
  old = 1;
  new = 2;
  
TYPE
  StateType = CHAR;
  StateRange = [first_state..abs_last_state];  
  Space = ARRAY [1..size] OF ARRAY [1..size] OF StateType;
  
VAR
  space     :ARRAY [old..new] OF Space;
  neighbour :ARRAY [1..4] OF RECORD
                                 row, col :INTEGER;
                              END;

  state_map   :ARRAY StateRange OF INTEGER;  (* clut *)
  state_wrap  :ARRAY StateRange OF StateType;
  s, s_to_eat :StateType;
  
  n_states   :INTEGER;
  last_state :StateType;
  
  i, j, k, l, m :INTEGER;
  
  screen_mode :INTEGER;
  c           :CHAR;
  row, col    :INTEGER;
  changes     :BOOLEAN;
  
BEGIN  EnableBreak;

  WriteString( 'how many states (' );
  WriteInt( min_states, 0 ); WriteString( ' to ' );
  WriteInt( max_states, 0 ); WriteString( ') ? ' );
  ReadInt( n_states );
  WriteLn;
  
  IF n_states < min_states THEN n_states := min_states END;
  IF n_states > max_states THEN n_states := max_states END;
  
  last_state := CHR( ORD( first_state ) + CARDINAL(n_states) - 1 );
  
  WriteInt( n_states, 0 ); WriteString( ' states,' ); WriteLn;
  WriteString( 'in a space of ' ); WriteInt( size, 0 );
  WriteString( ' by ' ); WriteInt( size, 0 ); WriteLn;
  
  (* define the offsets of the neighbours *);
  (* N *)  neighbour[1].row := -1;  neighbour[1].col := +0;
  (* E *)  neighbour[2].row := +0;  neighbour[2].col := +1;
  (* S *)  neighbour[3].row := +1;  neighbour[3].col := +0;
  (* W *)  neighbour[4].row := +0;  neighbour[4].col := -1;
  
  (* initialize the states as the first colours *)
  i := 1;
  FOR c := first_state TO last_state DO
    state_map[c] := i;
    i := i + 1;
  END;
  
  (* and define the wrap of states from one end to another *)
  FOR c := first_state TO last_state DO
     state_wrap[c] := CHR( ORD(c) - 1 );
  END;
  state_wrap[first_state] := last_state;
   
  (* randomize the first space *)
  FOR i := 1 TO size DO
    FOR j := 1 TO size DO
       k := INTEGER( ORD( first_state ) ) + RandomInt( n_states );
       space[old][i,j] := CHR(k);
    END;
  END;
  space[new] := space[old];
  
  WriteLn;
  WriteString( 'hit any key to start, then any key to quit' );
  WriteLn;
  Read( c );
  
  GetScreenMode( screen_mode );
  ScreenMode( gph16CHiRes );
  
  (* draw the initial state *)
  row := radius + offset;
  FOR i := 1 TO size DO
    col := radius;
    FOR j := 1 TO size DO
      
       s := space[old][i,j];
       Circle( row, col, row, col+radius, state_map[s], TRUE );
       
       col := col + diameter;
    END;
    row := row + diameter;
  END;
   
  (* now run the rest *)
  
  WHILE NOT KeyPressed() DO
  
    FOR i := 1 TO size DO
      FOR j := 1 TO size DO
         s_to_eat := state_wrap[ space[old][i,j] ];
         m := 1;
         WHILE m <= 4 DO            
            k := i + neighbour[m].row;
            l := j + neighbour[m].col;
             
            IF k = 0 THEN k := size ELSE IF k > size THEN k := 1 END; END;
            IF l = 0 THEN l := size ELSE IF l > size THEN l := 1 END; END; 
 
            IF space[old][k,l] = s_to_eat THEN
              space[new][i,j] := s_to_eat;
              m := 5;
            ELSE
              m := m + 1;
            END;
            
         END;         
      END;
    END;
    
    (* display the new cells *)
    
    changes := FALSE;
    
    row := radius + offset;
    FOR i := 1 TO size DO
      col := radius;
      FOR j := 1 TO size DO
   
         s := space[new][i,j];
         IF s # space[old][i,j] THEN
           (* there has been a change, so show it, and save it *)
           Circle( row, col, row, col+radius, state_map[s], TRUE );
           space[old][i,j] := s;
           changes := TRUE;
         END;
         
         col := col + diameter;         
      END;
      row := row + diameter;
    END;
 
    (* put up a marker to say 'no changes' *)
    IF NOT changes THEN
      Circle( 10, 200, 10, 200+10, 15, TRUE );
    END;
    
  END;

  Read( c );
  ScreenMode( screen_mode );
 
END Defect.