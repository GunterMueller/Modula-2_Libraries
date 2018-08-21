MODULE Defect;

(* Cellular Automata Defects *)
(* See A.K. Dewdney, Scientific American, Aug.89 "Computer Recreations *)

(* V4.0, jaa, Jun.22/93 -dynamic matricies *)
(* V3.2, jaa, Jun.13/93 -only 4 neighbours *)
(* V3.0, jaa, May.2/92 - eat neighbours *)
(* This code may be freely used and distributed, it may not be sold *)

FROM Break IMPORT EnableBreak;
FROM Graphics IMPORT gph16CHiRes, ScreenMode, GetScreenMode, Circle;
FROM Terminal IMPORT Read, KeyPressed;
FROM Random IMPORT RandomCard;
FROM InOut IMPORT WriteString, WriteInt, ReadInt, WriteLn;
FROM PackMatrix IMPORT Matrix, Build, Get, Put, Duplicate, Destroy;

CONST
  size  = 220;
  min_states = 8; max_states = 15;  (* number useful that is, 0-15 is range *)
  
  radius   = 1;
  offset   = 40;
  diameter = 2 * radius;
    
  old = 1;
  new = 2;
  
TYPE
  StateType = CARDINAL;
  StateRange = [0..max_states];

VAR
  space     :ARRAY [old..new] OF Matrix;
  neighbour :ARRAY [1..4] OF RECORD
                                 row, col :INTEGER;
                              END;

  state_wrap  :ARRAY StateRange OF StateType;
  s, s_to_eat :StateType;
  
  n_states   :INTEGER;
  
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
   
  WriteInt( n_states, 0 ); WriteString( ' states,' ); WriteLn;
  WriteString( 'in a space of ' ); WriteInt( size, 0 );
  WriteString( ' by ' ); WriteInt( size, 0 ); WriteLn;
  
  (* build the matricies *)
  Build( space[old], 0, max_states, size, size );
 
  (* define the offsets of the neighbours *);
  (* N *)  neighbour[1].row := -1;  neighbour[1].col := +0;
  (* E *)  neighbour[2].row := +0;  neighbour[2].col := +1;
  (* S *)  neighbour[3].row := +1;  neighbour[3].col := +0;
  (* W *)  neighbour[4].row := +0;  neighbour[4].col := -1;
  
  (* and define the wrap of states from one end to another *)
  FOR i := 1 TO n_states DO
     state_wrap[i] := i - 1;
  END;
  state_wrap[0]        := n_states;
  state_wrap[n_states] := 0;
   
  (* randomize the first space *)
  FOR i := 1 TO size DO
    FOR j := 1 TO size DO
      Put( space[old], i, j, RandomCard( n_states ) );
    END;
  END;
  Duplicate( space[old], space[new] );
  
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
       Circle( row, col, row, col+radius, Get( space[old], i, j ), TRUE );       
       col := col + diameter;
    END;
    row := row + diameter;
  END;
   
  (* now run the rest *)
  
  WHILE NOT KeyPressed() DO
  
    FOR i := 1 TO size DO
      FOR j := 1 TO size DO
         s_to_eat := state_wrap[ Get( space[old], i, j ) ];
         m := 1;
         WHILE m <= 4 DO            
            k := i + neighbour[m].row;
            l := j + neighbour[m].col;
             
            IF k = 0 THEN k := size ELSE IF k > size THEN k := 1 END; END;
            IF l = 0 THEN l := size ELSE IF l > size THEN l := 1 END; END; 
 
            IF Get( space[old], k, l ) = s_to_eat THEN
              Put( space[new], i, j, s_to_eat );
              m := 5; (* exit loop *)
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
   
         s := Get( space[new], i, j );
         IF s # Get( space[old], i, j ) THEN
           (* there has been a change, so show it, and save it *)
           Circle( row, col, row, col+radius, s, TRUE );
           Put( space[old], i, j, s );
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

  Destroy( space[old] ); Destroy( space[new] );
  
  Read( c );
  ScreenMode( screen_mode );
 
END Defect.