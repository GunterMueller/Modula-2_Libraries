MODULE Screen;

(* test out some simple screen handling *)
(* J. Andrea, 1984 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SimpleScreen IMPORT ErasePage, PutScreen, SetCursor, ScreenFinished;

CONST
  max = 20;

VAR
  row, col :CARDINAL;

BEGIN (* Screen *)

ErasePage(1,1);

FOR row := 1 TO 20 DO
   col := row;
   PutScreen( 'xyzzy', (max-row+1), col );
END; (* for *)

PutScreen('all done',1,70);

SetCursor(23,1);

ScreenFinished;

END Screen.
