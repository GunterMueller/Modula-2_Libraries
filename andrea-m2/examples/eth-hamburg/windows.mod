MODULE TestWindows;

(* test the simple windows module and show how its used *)
(* J. Andrea, 1984 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SimpleWindows IMPORT
      action_status, PasteBoard, Display,
      CreateDisplay, CreatePasteBoard, DeleteChars,    DeleteDisplay,
      DeleteLine,    DeletePasteBoard, DrawLine,       DrawRectangle,
      EraseChars,    EraseDisplay,     EraseLine,      InsertChars,
      InsertLine,    LabelBorder,      MoveCursor,     MoveDisplay,
      PasteDisplay,  PopDisplay,       PutChars,       PutLine,
      PutWithScroll, RepaintScreen,    RepasteDisplay, RestoreScreen,
      RingBell,      SaveScreen,       SetCursor,      SetPhysicalCursor,
      ScrollDisplay, UnpasteDisplay,
      AddBorder,     RemoveBorder,
      VideoTypes,    VideoAttributes,  ChangeDisplayVideo;

VAR
  a_board :PasteBoard;
  a, b, c :Display;

  attr    :VideoAttributes;

BEGIN (* TestWindows *)

(* create displays *)
CreatePasteBoard( a_board );

CreateDisplay( a, 10, 10, TRUE );
CreateDisplay( b, 10, 20, TRUE );
CreateDisplay( c, 10, 10, TRUE );

(* do actions *)

PutLine( a, 'first line', 1 );
PutLine( b, 'second display', 1 );
PutLine( c, 'next', 1 );

PasteDisplay( a, a_board, 2, 3 );
PasteDisplay( b, a_board, 10, 5 );
PasteDisplay( c, a_board, 16, 20 );

INCL(attr,video_reverse);
ChangeDisplayVideo( b, attr );

PutLine( a, 'first line', 1 );
PutLine( b, 'second display', 1 );
PutLine( c, 'next', 1 );

LabelBorder( b, 'number B' );

RemoveBorder( a );
RemoveBorder( b );
RemoveBorder( c );

AddBorder( a );
AddBorder( b );
AddBorder( c );

SetPhysicalCursor( a_board, 23, 1 );

DeleteDisplay( b );

(* end actions *)

(* get rid of displays *)
DeletePasteBoard( a_board );

END TestWindows.
