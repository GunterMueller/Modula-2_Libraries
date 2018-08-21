IMPLEMENTATION MODULE VeryScreen;

(* John Andrea, Mar.27/92 *)
(* This code may NOT be sold, it may be freely used and distributed *)

FROM Strings IMPORT Length;
FROM Display IMPORT SetCursorPosition, ClrEOS, ClrEOL, Write;

   (* ----------------------------------------------------------------- *)
   PROCEDURE Home;
   BEGIN
      SetCursorPosition( 1, 1 );
   END Home;

   (* ----------------------------------------------------------------- *)
   PROCEDURE EraseScreen;
   BEGIN
      Home; ClrEOS;
   END EraseScreen;

   (* ----------------------------------------------------------------- *)
   PROCEDURE EraseLine;
   BEGIN
      ClrEOL;
   END EraseLine;

   (* ----------------------------------------------------------------- *)
   PROCEDURE PutString( text :ARRAY OF CHAR; row, col :CARDINAL );
   VAR
     i, max :CARDINAL;
   BEGIN
       PutCursor( row, col );

       max := Length( text );
       IF max > 0 THEN
         max := max - 1;

         FOR i := 0 TO max DO
          Write( text[i] );
         END;

       END;
   END PutString;

   (* ----------------------------------------------------------------- *)
   PROCEDURE PutCursor( row, col :CARDINAL );
   BEGIN
      SetCursorPosition( row, col );
   END PutCursor;

   (* ----------------------------------------------------------------- *)
   PROCEDURE CursorOn( turn_on :BOOLEAN );
   BEGIN
   END CursorOn;
 
BEGIN
END VeryScreen.
