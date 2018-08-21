IMPLEMENTATION MODULE SimpleScreen;

(* J. Andrea Dec 1985,  VMS V4 screen manipulation *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SYSTEM IMPORT ADDRESS;

FROM SSDefinitions IMPORT SS$_NORMAL;

FROM SMGDefinitions IMPORT
      SMG$C_ASCII,  SMG$V_BORDER,  SMG$M_NORMAL;

FROM ScreenManagementProcedures IMPORT
      SMG$CREATE_PASTEBOARD,         SMG$CREATE_VIRTUAL_DISPLAY,
      SMG$DELETE_PASTEBOARD,
      SMG$ERASE_DISPLAY,             SMG$ERASE_LINE,
      SMG$PASTE_VIRTUAL_DISPLAY,     SMG$PUT_CHARS,
      SMG$REPAINT_SCREEN,            SMG$SET_CURSOR_ABS;


VAR
   status, paste_board, virtual_display  :CARDINAL;
   display_exists                        :BOOLEAN;
   max_rows, max_cols                    :CARDINAL;


   (* ----------------------------------------------------------------- *)
   PROCEDURE ErasePage( row, col :CARDINAL );

      (* -------------------------------------------------------------- *)
      PROCEDURE BuildDisplay;

      BEGIN (* BuildDisplay *)
         status := SMG$CREATE_PASTEBOARD( paste_board, 'TT:',
                                          max_rows, max_cols, 0 );

         IF status = SS$_NORMAL THEN
            status := SMG$CREATE_VIRTUAL_DISPLAY( max_rows, max_cols,
                                                  virtual_display,
                                           BITSET(SMG$V_BORDER),
                                           BITSET(SMG$M_NORMAL),
                                                  SMG$C_ASCII );

            IF status = SS$_NORMAL THEN
               status := SMG$PASTE_VIRTUAL_DISPLAY( virtual_display,
                                                    paste_board,
                                                    1, 1,
                                           ADDRESS( virtual_display ) );

               IF status = SS$_NORMAL THEN
                  display_exists := TRUE;
               ELSE
                 display_exists := FALSE;
               END; (* if *)

            ELSE
               display_exists := FALSE;
            END; (* if *)

         ELSE
            display_exists := FALSE;
         END; (* if *)

      END BuildDisplay;

   BEGIN

      IF display_exists THEN

         status := SMG$ERASE_DISPLAY( virtual_display,
                   INTEGER(row), INTEGER(col), max_rows, max_cols );

      ELSE
         BuildDisplay;
      END; (* if *)
 
   END ErasePage;

   (* ----------------------------------------------------------------- *)
   PROCEDURE ScreenFinished;
   BEGIN (* ScreenFinished *)

      IF display_exists THEN

         status := SMG$DELETE_PASTEBOARD( paste_board, BITSET(0) );

      END; (* if *)

   END ScreenFinished;

   (* ----------------------------------------------------------------- *)
   PROCEDURE EraseLine( row, col :CARDINAL );
   BEGIN (* EraseLine *)

      status := SMG$ERASE_LINE( virtual_display,
                                INTEGER(row), INTEGER(col) );

   END EraseLine;

   (* ----------------------------------------------------------------- *)
   PROCEDURE PutScreen( text :ARRAY OF CHAR; row, col :CARDINAL);
   BEGIN (* PutScreen *)

      status := SMG$PUT_CHARS( virtual_display, text,
                               INTEGER(row), INTEGER(col), 0,
                        BITSET(SMG$M_NORMAL),
                        BITSET(SMG$M_NORMAL),
                               SMG$C_ASCII );

   END PutScreen;


   (* ----------------------------------------------------------------- *)
   PROCEDURE SetCursor( row, col :CARDINAL);
   BEGIN (* SetCursor *)

      status := SMG$SET_CURSOR_ABS( virtual_display,
                                    INTEGER(row), INTEGER(col) );

   END SetCursor;
 
   (* ------------------------------------ *)
   PROCEDURE ReDrawScreen;

   BEGIN (* ReDrawScreen *)

      status := SMG$REPAINT_SCREEN( paste_board );

   END ReDrawScreen;

BEGIN (* SimpleScreen *)

   display_exists := FALSE;

END SimpleScreen.
