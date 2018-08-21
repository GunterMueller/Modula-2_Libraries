IMPLEMENTATION MODULE SimpleWindows;

(* SMG Window management with a simple syntax *)
(* J. Andrea, Aug.12/91 - routines updated *)
(* JAA, Dec 1985, with regards to Gary Doucette *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SYSTEM IMPORT ADDRESS;

FROM SSDefinitions IMPORT   SS$_NORMAL;

FROM SMGDefinitions IMPORT
      (* constants *)
      SMG$C_ASCII,
      SMG$K_TOP,     SMG$M_BORDER,  SMG$V_BORDER,
      SMG$M_UP,
      SMG$M_NORMAL,  SMG$M_BOLD, SMG$M_REVERSE,
      SMG$M_BLINK,   SMG$M_UNDERLINE;

FROM ScreenManagementProcedures IMPORT
      (* procedures  *)
      SMG$CREATE_PASTEBOARD,            SMG$CREATE_VIRTUAL_DISPLAY,
      SMG$DELETE_CHARS,
      SMG$DELETE_LINE,                  SMG$DELETE_PASTEBOARD,
      SMG$DELETE_VIRTUAL_DISPLAY,       
      SMG$DRAW_LINE,                    SMG$DRAW_RECTANGLE,
      SMG$ERASE_CHARS,                  SMG$ERASE_DISPLAY,
      SMG$ERASE_LINE,                   SMG$GET_DISPLAY_ATTR,
      SMG$INSERT_CHARS,                 SMG$INSERT_LINE,
      SMG$LABEL_BORDER,                 SMG$MOVE_VIRTUAL_DISPLAY,
      SMG$PASTE_VIRTUAL_DISPLAY,        SMG$POP_VIRTUAL_DISPLAY,
      SMG$PUT_CHARS,                    SMG$PUT_LINE,
      SMG$PUT_WITH_SCROLL,              
      SMG$REPAINT_SCREEN,
      SMG$REPASTE_VIRTUAL_DISPLAY,      SMG$RESTORE_PHYSICAL_SCREEN,
      SMG$RING_BELL,                    SMG$SAVE_PHYSICAL_SCREEN,
      SMG$SET_CURSOR_ABS,               SMG$SET_CURSOR_REL,
      SMG$SET_PHYSICAL_CURSOR,          SMG$SCROLL_DISPLAY_AREA,
      SMG$UNPASTE_VIRTUAL_DISPLAY,

      SMG$CHANGE_VIRTUAL_DISPLAY;


TYPE 
   PasteBoard = CARDINAL;   (* hidden *)
   Display    = CARDINAL;   (* hidden *)


VAR
   insert_line_clr_flag                       :INTEGER;
   save_screen_row_begin, save_screen_row_end :INTEGER;
   pasteboard_initial_erase                   :INTEGER;
   erase_pasteborard_on_delete                :INTEGER;
   put_line_wrap_flag                         :CARDINAL;
   put_line_direction                         :CARDINAL;
   put_chars_rendition                        :CARDINAL;

   (* ------------------------------------------------------------------ *)
   PROCEDURE CreateDisplay( VAR  display    :Display;
                                 rows, cols :CARDINAL;
                                 border     :BOOLEAN  );

   VAR
      use_border :INTEGER;

   BEGIN (* CreateDisplay *)

      IF border THEN
        use_border := SMG$M_BORDER;
      ELSE
        use_border := SMG$V_BORDER;
      END; (* if *)

      action_status := SS$_NORMAL = 
               SMG$CREATE_VIRTUAL_DISPLAY( rows, cols, display,
                                    BITSET(use_border), BITSET(SMG$M_NORMAL),
                                           SMG$C_ASCII );

   END CreateDisplay;

   (* ------------------------------------------------------------------ *)
   PROCEDURE CreatePasteBoard( VAR  pasteboard :PasteBoard );

   VAR
     rows, cols :CARDINAL;

   BEGIN (* CreatePasteBoard *)

      action_status := SS$_NORMAL =
               SMG$CREATE_PASTEBOARD( pasteboard, 'TT:', rows, cols, 
                       pasteboard_initial_erase );

   END CreatePasteBoard;

   (* ------------------------------------------------------------------ *)
   PROCEDURE DeleteChars( display  :Display;
                          n_chars  :CARDINAL;
                          row, col :CARDINAL );

   BEGIN (* DeleteChars *)

      action_status := SS$_NORMAL =
               SMG$DELETE_CHARS( display, n_chars, row, col );

   END DeleteChars;

   (* ------------------------------------------------------------------ *)
   PROCEDURE DeleteDisplay( display :Display );

   BEGIN (* DeleteDisplay *)

      action_status := SS$_NORMAL = SMG$DELETE_VIRTUAL_DISPLAY( display );

   END DeleteDisplay;

   (* ------------------------------------------------------------------ *)
   PROCEDURE DeleteLine( display    :Display;
                         start_row  :CARDINAL;
                         n_rows     :CARDINAL );

   BEGIN (* DeleteLine *)

      action_status := SS$_NORMAL = 
                       SMG$DELETE_LINE( display, start_row, n_rows );

   END DeleteLine;

   (* ------------------------------------------------------------------ *)
   PROCEDURE DeletePasteBoard( pasteboard :PasteBoard );

   BEGIN (* DeletePasteBoard *)

      action_status := SS$_NORMAL =
                 SMG$DELETE_PASTEBOARD( pasteboard, 
                       BITSET(erase_pasteborard_on_delete) );

   END DeletePasteBoard;

   (* ------------------------------------------------------------------ *)
   PROCEDURE DrawLine( display     :Display;
                       start_row, start_col :CARDINAL;
                       end_row  , end_col   :CARDINAL );

   BEGIN (* DrawLine *)

      action_status := SS$_NORMAL =
                SMG$DRAW_LINE( display, start_row, start_col, end_row, end_col,
                        BITSET(SMG$M_NORMAL), BITSET(SMG$M_NORMAL) );

   END DrawLine;

   (* ------------------------------------------------------------------ *)
   PROCEDURE DrawRectangle( display       :Display;
                            top_left_row  :CARDINAL;
                            top_left_col  :CARDINAL;
                            bot_right_row :CARDINAL;
                            bot_right_col :CARDINAL );

   BEGIN (* DrawRectangle *)

      action_status := SS$_NORMAL =
                    SMG$DRAW_RECTANGLE( display, top_left_row, top_left_col,
                          bot_right_row, bot_right_col,
                          BITSET(SMG$M_NORMAL), BITSET(SMG$M_NORMAL) ); 

   END DrawRectangle;

   (* ------------------------------------------------------------------ *)
   PROCEDURE EraseChars( display  :Display;
                         n_chars  :CARDINAL;
                         row, col :CARDINAL );

   BEGIN (* EraseChars *)

      action_status := SS$_NORMAL = 
                       SMG$ERASE_CHARS( display, n_chars, row, col );

   END EraseChars;

   (* ------------------------------------------------------------------ *)
   PROCEDURE EraseDisplay( display     :Display;
                           start_row, start_col :CARDINAL;
                           end_row  , end_col   :CARDINAL );

   BEGIN (* EraseDisplay *)

      action_status := SS$_NORMAL =
                SMG$ERASE_DISPLAY( display, start_row, start_col,
                                   end_row, end_col );

   END EraseDisplay;

   (* ------------------------------------------------------------------ *)
   PROCEDURE EraseLine( display  :Display;
                        row, col :CARDINAL );

   BEGIN (* EraseLine *)

      action_status := SS$_NORMAL = SMG$ERASE_LINE( display, row, col );

   END EraseLine;

   (* ------------------------------------------------------------------ *)
   PROCEDURE InsertChars( display  :Display;
                          text     :ARRAY OF CHAR;
                          row, col :CARDINAL );

   BEGIN (* InsertChars *)

      action_status := SS$_NORMAL =
               SMG$INSERT_CHARS( display, text, row, col, 
                          BITSET(SMG$M_NORMAL),
                          BITSET(SMG$M_NORMAL),
                                 SMG$C_ASCII );

   END InsertChars;

   (* ------------------------------------------------------------------ *)
   PROCEDURE InsertLine( display  :Display;
                         row      :CARDINAL;
                         text     :ARRAY OF CHAR );

   BEGIN (* InsertLine *)

      action_status := SS$_NORMAL =
                SMG$INSERT_LINE( display, row, text, SMG$M_UP,
                          BITSET(SMG$M_NORMAL), BITSET(SMG$M_NORMAL),
                                 insert_line_clr_flag, SMG$C_ASCII );

   END InsertLine;

   (* ------------------------------------------------------------------ *)
   PROCEDURE LabelBorder( display :Display;
                          text    :ARRAY OF CHAR );

   VAR
       display_attr,
       video_attr, char_set :CARDINAL;
       units, height, width :CARDINAL;

   BEGIN (* LabelBorder *)

      action_status := SS$_NORMAL =
               SMG$GET_DISPLAY_ATTR( display, height, width,
                  BITSET(display_attr), BITSET(video_attr), char_set );

      units := ( width - LEN( text ) ) DIV 2;

      action_status := SS$_NORMAL =
                SMG$LABEL_BORDER( display, text, SMG$K_TOP, units,
                           BITSET(SMG$M_NORMAL), BITSET(SMG$M_NORMAL),
                                  SMG$C_ASCII );

   END LabelBorder;

   (* ------------------------------------------------------------------ *)
   PROCEDURE MoveCursor( display     :Display;
                         delta_row, delta_col :CARDINAL );

   BEGIN (* MoveCursor *)

      action_status := SS$_NORMAL =
                  SMG$SET_CURSOR_REL( display, delta_row, delta_col );

   END MoveCursor;

   (* ------------------------------------------------------------------ *)
   PROCEDURE MoveDisplay( display    :Display;
                          pasteboard :PasteBoard;
                          row, col   :CARDINAL );

   BEGIN (* MoveDisplay *)

      action_status := SS$_NORMAL =
                       SMG$MOVE_VIRTUAL_DISPLAY( display,
                                                 pasteboard,
                                                 row, col,
                                        ADDRESS( display ) );

   END MoveDisplay;

   (* ------------------------------------------------------------------ *)
   PROCEDURE PasteDisplay( display    :Display;
                           pasteboard :PasteBoard;
                           row, col   :CARDINAL );

   BEGIN (* PasteDisplay *)

      action_status := SS$_NORMAL =
               SMG$PASTE_VIRTUAL_DISPLAY( display,
                                          pasteboard,
                                          row, col,
                                 ADDRESS( display ) );

   END PasteDisplay;

   (* ------------------------------------------------------------------ *)
   PROCEDURE PopDisplay( display    :Display;
                         pasteboard :PasteBoard );

   BEGIN (* PopDisplay *)

      action_status := SS$_NORMAL =
                 SMG$POP_VIRTUAL_DISPLAY( display, pasteboard );

   END PopDisplay;

   (* ------------------------------------------------------------------ *)
   PROCEDURE PutChars( display   :Display;
                       text      :ARRAY OF CHAR;
                       row, col  :CARDINAL );

   BEGIN (* PutChars *)

      action_status := SS$_NORMAL = SMG$PUT_CHARS( display, text,
                                                   row, col, 1,
                                            BITSET(put_chars_rendition),
                                            BITSET(put_chars_rendition),
                                                   SMG$C_ASCII );

   END PutChars;

   (* ------------------------------------------------------------------ *)
   PROCEDURE PutLine( display   :Display;
                      text      :ARRAY OF CHAR;
                      line_adv  :CARDINAL );

   BEGIN (* PutLine *)

      action_status := SS$_NORMAL = SMG$PUT_LINE( display, text,
                                         INTEGER( line_adv ),
                                          BITSET( put_chars_rendition ),
                                          BITSET( put_chars_rendition ),
                                          BITSET( put_line_wrap_flag ),
                                                  SMG$C_ASCII,
                                                  SMG$M_UP );

   END PutLine;

   (* ------------------------------------------------------------------ *)
   PROCEDURE PutWithScroll( display :Display;
                            text    :ARRAY OF CHAR );

   BEGIN (* PutWithScroll *)

      action_status := SS$_NORMAL = SMG$PUT_WITH_SCROLL( display, text,
                                          BITSET( put_line_direction ),
                                          BITSET( put_chars_rendition ),
                                          BITSET( put_chars_rendition ),
                                                  put_line_wrap_flag,
                                                  SMG$C_ASCII );

   END PutWithScroll;

   (* ------------------------------------------------------------------ *)
   PROCEDURE RepaintScreen( pasteboard :PasteBoard );

   BEGIN (* RepaintScreen *)

      action_status := SS$_NORMAL = SMG$REPAINT_SCREEN( pasteboard );

   END RepaintScreen;

   (* ------------------------------------------------------------------ *)
   PROCEDURE RepasteDisplay( display    :Display;
                             pasteboard :PasteBoard;
                             row, col   :CARDINAL );

   BEGIN (* RepasteDisplay *)

      action_status := SS$_NORMAL =
                SMG$REPASTE_VIRTUAL_DISPLAY( display, pasteboard,
                                             row, col, display );

   END RepasteDisplay;

   (* ------------------------------------------------------------------ *)
   PROCEDURE RestoreScreen( pasteboard :PasteBoard;
                            display    :Display );
   
   BEGIN (* RestoreScreen *)

      action_status := SS$_NORMAL =
                SMG$RESTORE_PHYSICAL_SCREEN( pasteboard, display );

   END RestoreScreen;

   (* ------------------------------------------------------------------ *)
   PROCEDURE RingBell( display :Display );

   BEGIN (* RingBell *)

      action_status := SS$_NORMAL =
               SMG$RING_BELL( display, 1 );

   END RingBell;

   (* ------------------------------------------------------------------ *)
   PROCEDURE SaveScreen( pasteboard   :PasteBoard;
                         VAR  display :Display );

   BEGIN (* SaveScreen *)

      action_status := SS$_NORMAL =
               SMG$SAVE_PHYSICAL_SCREEN( pasteboard, display,
                      save_screen_row_begin, save_screen_row_end );

   END SaveScreen;

   (* ------------------------------------------------------------------ *)
   PROCEDURE SetCursor( display  :Display;
                        row, col :CARDINAL );

   BEGIN (* SetCursor *)

      action_status := SS$_NORMAL =
                 SMG$SET_CURSOR_ABS( display, row, col );

   END SetCursor;

   (* ------------------------------------------------------------------ *)
   PROCEDURE SetPhysicalCursor( pasteboard :PasteBoard;
                                row, col   :CARDINAL );

   BEGIN (* SetPhysicalCursor *)

      action_status := SS$_NORMAL =
               SMG$SET_PHYSICAL_CURSOR( pasteboard, row, col );

   END SetPhysicalCursor;

   (* ------------------------------------------------------------------ *)
   PROCEDURE ScrollDisplay( display   :Display;
                            direction :CARDINAL;
                            count     :CARDINAL );

   VAR
      display_attr,
      video_attr, char_set,
      height, width        :CARDINAL;

   BEGIN (* ScrollDisplay *)

      action_status := SS$_NORMAL =
                  SMG$GET_DISPLAY_ATTR( display, height, width,
                                BITSET( display_attr ),
                                BITSET( video_attr ), char_set );

      action_status := SS$_NORMAL =
                  SMG$SCROLL_DISPLAY_AREA( display, 1, 1, height, width,
                                   BITSET( direction ), count );

   END ScrollDisplay;

   (* ------------------------------------------------------------------ *)
   PROCEDURE UnpasteDisplay( display    :Display;
                             pasteboard :PasteBoard );

   BEGIN (* UnpasteDisplay *)

      action_status := SS$_NORMAL =
                 SMG$UNPASTE_VIRTUAL_DISPLAY( display, pasteboard );

   END UnpasteDisplay;

   (* ------------------------------------------------------------------ *)
   PROCEDURE AddBorder( display :Display );

   VAR
       display_attributes, video_attributes, char_set :CARDINAL;
       rows, cols    :CARDINAL;

   BEGIN (* AddBorder *)

      action_status := SS$_NORMAL =
               SMG$GET_DISPLAY_ATTR( display, rows, cols,
                             BITSET( display_attributes ),
                             BITSET( video_attributes ),
                                     char_set );

      IF action_status THEN
         action_status := SS$_NORMAL =
                SMG$CHANGE_VIRTUAL_DISPLAY( display, rows, cols,
                                    BITSET( SMG$M_BORDER ),
                                    BITSET( video_attributes ),
                                            char_set );
      END; (* if *)

   END AddBorder;

   (* ------------------------------------------------------------------ *)
   PROCEDURE RemoveBorder( display :Display );

   VAR
       display_attributes, video_attributes, char_set :CARDINAL;
       rows, cols    :CARDINAL;

   BEGIN (* RemoveBorder *)

      action_status := SS$_NORMAL =
               SMG$GET_DISPLAY_ATTR( display, rows, cols,
                             BITSET( display_attributes ),
                             BITSET( video_attributes ),
                                     char_set );

      IF action_status THEN
         action_status := SS$_NORMAL =
                SMG$CHANGE_VIRTUAL_DISPLAY( display, rows, cols,
                                    BITSET( SMG$M_BORDER ),
                                    BITSET( video_attributes ),
                                            char_set );
      END; (* if *)

   END RemoveBorder;

   (* ------------------------------------------------------------------ *)
   PROCEDURE ChangeDisplayVideo( display   :Display;
                                 new_video :VideoAttributes );

   VAR
       display_attributes, video_attributes, char_set :CARDINAL;
       rows, cols    :CARDINAL;

   BEGIN (* ChangeDisplayVideo *)

      action_status := SS$_NORMAL =
               SMG$GET_DISPLAY_ATTR( display, rows, cols,
                             BITSET( display_attributes ),
                             BITSET( video_attributes ),
                                     char_set );

      IF action_status THEN
         (* get the new attributes *)
         video_attributes := SMG$M_NORMAL;
         IF video_bold IN new_video THEN
            video_attributes := video_attributes + SMG$M_BOLD;
         END;
         IF video_reverse IN new_video THEN
            video_attributes := video_attributes + SMG$M_REVERSE;
         END;
         IF video_blink IN new_video THEN
            video_attributes := video_attributes + SMG$M_BLINK;
         END;
         IF video_underline IN new_video THEN
            video_attributes := video_attributes + SMG$M_UNDERLINE;
         END;
         action_status := SS$_NORMAL =
                SMG$CHANGE_VIRTUAL_DISPLAY( display, rows, cols,
                                    BITSET( display_attributes ),
                                    BITSET( video_attributes ),
                                            char_set );
      END; (* if *)

   END ChangeDisplayVideo;

   (* ------------------------------------------------------------------ *)
   PROCEDURE ChangeDefaults;
   BEGIN (* ChangeDefaults *)
   END ChangeDefaults;


BEGIN (* SimpleWindows *)

   action_status := TRUE;

   insert_line_clr_flag := 0;

   save_screen_row_begin := 0;
   save_screen_row_end   := 0;

   pasteboard_initial_erase := 0;      (* yes *)
   erase_pasteborard_on_delete := 0;   (* no  *)

   put_line_wrap_flag := 0;
   put_line_direction := SMG$M_UP;

   put_chars_rendition := SMG$M_NORMAL;

END SimpleWindows.
