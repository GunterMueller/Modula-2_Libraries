MODULE TestVeryScreen;

(* test the very simple screen handling module, and show how its used *)
(* John Andrea, Feb.1/1991 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM VeryScreen IMPORT Home, EraseScreen, PutCursor, CursorOn, PutString;

BEGIN

      Home;   EraseScreen;

      CursorOn( FALSE ); 

      PutString( 'at top of screen', 1, 1 );

      PutString( 'middle of screen', 10, 20 );

      CursorOn( TRUE );

END TestVeryScreen.
