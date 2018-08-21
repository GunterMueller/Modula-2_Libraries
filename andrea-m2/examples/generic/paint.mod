MODULE XPaint;

(* a very simple screen paint program *)
(* J. Andrea, 1992 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM GetCharacter IMPORT StartGet,  StopGet,   Get;
FROM VeryScreen   IMPORT PutString, PutCursor, EraseScreen, Home, EraseLine,
                         Line, Box;
FROM TTIO         IMPORT Write;
FROM Conversions  IMPORT CardToString;
FROM Storage      IMPORT ALLOCATE, DEALLOCATE;
FROM InOut        IMPORT OpenOutput, CloseOutput, OpenInput, CloseInput, Done,
                         WriteCard, WriteString, WriteLn,
                         EOL, Read, ReadCard, ReadLn;

CONST
   control_z = 32C;  esc = 33C;
   max_string = 80;

TYPE
   LineP    = POINTER TO LineData;
   LineData = RECORD
               r1, c1, r2, c2 :CARDINAL;
               next :LineP;
              END;

   BoxP     = POINTER TO BoxData;
   BoxData  = RECORD
               r1, c1, r2, c2 :CARDINAL;
               next :BoxP;
              END;

   TextP    = POINTER TO TextData;
   TextData = RECORD
               r1, c1, r2, c2 :CARDINAL;
               string :ARRAY [0..max_string] OF CHAR;
               next   :TextP;
              END;

VAR
   c        :CHAR;
   row, col :CARDINAL;
   first_row, first_col :CARDINAL;
   begin_r,   end_r     :CARDINAL;
   begin_c,   end_c     :CARDINAL;

   line1, l, pl :LineP;
   box1,  b, pb :BoxP;
   text1, t, pt :TextP;

   (* ------------------------------------------------- *)
   PROCEDURE Beep;
   BEGIN
     Write( 7C );
   END Beep;

   (* ------------------------------------------------- *)
   PROCEDURE Message( message :ARRAY OF CHAR );
   BEGIN
     PutString( message, 24, 1 );  EraseLine;
   END Message;

   (* ------------------------------------------------- *)
   PROCEDURE ShowPos;
   VAR string :ARRAY [1..3] OF CHAR;
   BEGIN
     CardToString( row, 2, string );
     PutString( string, 24, 74 );

     PutString( ',',    24, 76 );

     CardToString( col, 3, string );
     PutString( string, 24, 77 );

     PutCursor( row, col );
   END ShowPos;

   (* ------------------------------------------------- *)
   PROCEDURE ShowMenu;
   BEGIN
     Message( 
     '^Z=exit B=box L=line T=text R=redraw D=delete P=print S=save G=get' );
     ShowPos;
   END ShowMenu;

   (* ------------------------------------------------- *)
   PROCEDURE Redraw;
   VAR i :CARDINAL;
   BEGIN
      Home;  EraseScreen;

      l := line1;
      WHILE l # NIL DO
         Line( l^.r1, l^.c1, l^.r2, l^.c2 );
         l := l^.next;
      END;

      b := box1;
      WHILE b # NIL DO
         Box( b^.r1, b^.c1, b^.r2, b^.c2 );
         b := b^.next;
      END;

      t := text1;
      WHILE t # NIL DO
         PutString( t^.string, t^.r1, t^.c1 );
         t := t^.next;
      END;

      ShowMenu;
   END Redraw;

   (* ------------------------------------------------- *)
   PROCEDURE Move( key :CHAR );
   (* the arrow keys send 3 characters in a row,
      escape, '[', then one of 'A' 'B' 'C' or 'D',
      this procedure is called after the escape and '[' have been
      detected and the movement is to be determined *)
   BEGIN
       IF    key = 'A' THEN
          IF row > 1 THEN row := row - 1; END;
       ELSIF key = 'B' THEN
          IF row < 23 THEN row := row + 1; END;
       ELSIF key = 'C' THEN
          IF col < 80 THEN col := col + 1; END;
       ELSIF key = 'D' THEN
          IF col > 1 THEN col := col - 1; END;
       END;

       ShowPos;
   END Move;

   (* ------------------------------------------------- *)
   PROCEDURE NewBox( row1, col1, row2, col2 :CARDINAL );
   BEGIN

      IF row2 > row1 THEN
        begin_r := row1;
        end_r   := row2;
      ELSE
        begin_r := row2;
        end_r   := row1;
      END;
      IF col2 > col1 THEN
        begin_c := col1;
        end_c   := col2;
      ELSE
        begin_c := col2;
        end_c   := col1;
      END;

       IF box1 = NIL THEN
         NEW( b );
         box1 := b;
       ELSE
         b := box1;
         WHILE b # NIL DO
            pb := b;
            b  := b^.next;
         END;
         NEW( b );
         pb^.next := b;
       END;

       b^.r1   := begin_r;
       b^.c1   := begin_c;
       b^.r2   := end_r;
       b^.c2   := end_c;
       b^.next := NIL;

       Box( begin_r, begin_c, end_r, end_c );
   END NewBox;

   (* ------------------------------------------------- *)
   PROCEDURE NewLine( row1, col1, row2, col2 :CARDINAL );
   BEGIN
      IF col1 # col2 THEN
        begin_r := row1;
        end_r   := row1;
        IF col2 > col1 THEN
          begin_c := col1;
          end_c   := col2;
        ELSE
          begin_c := col2;
          end_c   := col1;
        END;
      ELSE
        begin_c := col1;
        end_c   := col1;
        IF row2 > row1 THEN
          begin_r := row1;
          end_r   := row2;
        ELSE
          begin_r := row2;
          end_r   := row1;
        END;
      END;

      IF line1 = NIL THEN
        NEW( l );
        line1 := l;
      ELSE
        l := line1;
        WHILE l # NIL DO
           pl := l;
           l  := l^.next;
        END;
        NEW( l );
        pl^.next := l;
      END;

      l^.r1   := begin_r;
      l^.c1   := begin_c;
      l^.r2   := end_r;
      l^.c2   := end_c;
      l^.next := NIL;

      Line( begin_r, begin_c, end_r, end_c );
   END NewLine;

   (* ------------------------------------------------- *)
   PROCEDURE NewText( row1, col1 :CARDINAL; string :ARRAY OF CHAR );
   VAR i, j :CARDINAL;
   BEGIN

     IF text1 = NIL THEN
       NEW( t );
       text1 := t;
     ELSE
       t := text1;
       WHILE t # NIL DO
          pt := t;
          t  := t^.next;
       END;
       NEW( t );
       pt^.next := t;
     END;
     t^.r1   := row1;
     t^.c1   := col1;
     t^.r2   := row1;
     t^.c2   := col1 - 1;
     t^.next := NIL;

     i := 0; j := 0;
     WHILE ( j < max_string ) & ( i < LEN( string ) ) DO
        c := string[i];
        IF ( c >= ' ') & ( c <= '~' ) THEN
          t^.string[j] := c;
          j := j + 1;
          t^.c2 := t^.c2 + 1;
        END;
        i := i + 1;
     END;
     t^.string[j] := 0C;

     PutString( t^.string, t^.r1, t^.c1 );
   END NewText;

   (* ------------------------------------------------- *)
   PROCEDURE PaintBox;
   BEGIN
     Message( 'move to other corner, then type B again' );  ShowPos;

     first_row := row; first_col := col;

     Get( c );
     WHILE c = esc DO
        Get( c );
        IF c = '[' THEN
          Get( c ); Move( c ); Get( c );
        END;
     END;

     IF CAP( c ) = 'B' THEN
       NewBox( first_row, first_col, row, col );
     END;

     ShowMenu;
   END PaintBox;

   (* ------------------------------------------------- *)
   PROCEDURE PaintLine;
   BEGIN
     Message( 'move to end of line, then type L again' );  ShowPos;

     first_row := row; first_col := col;

     Get( c );
     WHILE c = esc DO
        Get( c );
        IF c = '[' THEN
          Get( c ); Move( c ); Get( c );
        END;
     END;

     IF CAP( c ) = 'L' THEN
       NewLine( first_row, first_col, row, col );
     END;

     ShowMenu;
   END PaintLine;

   (* ------------------------------------------------- *)
   PROCEDURE PaintText;
   VAR
      i      :CARDINAL;
      string :ARRAY [0..max_string] OF CHAR;
   BEGIN
     Message( 'control-Z to end text' );  ShowPos;

     i := 0;
     Get( c );
     WHILE ( i < max_string ) & ( c # control_z ) DO
        IF ( c >= ' ') & ( c <= '~' ) THEN
          PutString( c, row, col+i );
          string[i] := c;  i := i + 1;
        END;
        Get( c );
     END;
     string[i] := 0C;

     NewText( row, col, string );

     ShowMenu;
   END PaintText;

   (* ------------------------------------------------- *)
   PROCEDURE Delete;
   VAR found, test :BOOLEAN;
   BEGIN
     Message( 'move to an objects corner, type D again, then redraw' );
     ShowPos;

     Get( c );
     WHILE c = esc DO
        Get( c );
        IF c = '[' THEN
          Get( c ); Move( c ); Get( c );
        END;
     END;

     IF CAP( c ) = 'D' THEN

      found := FALSE;

      l := line1;
      WHILE ( NOT found ) & ( l # NIL ) DO
         IF ( ( l^.r1 = row ) & ( l^.c1 = col ) ) OR
            ( ( l^.r2 = row ) & ( l^.c2 = col ) ) THEN
           found := TRUE;
           IF l = line1 THEN
             line1 := l^.next;
           ELSE
             pl^.next := l^.next;
           END;
           (* dispose of l *)
         ELSE
           pl := l;
           l  := l^.next;
         END;
      END;

      IF NOT found THEN

        b := box1;
        WHILE ( NOT found ) & ( b # NIL ) DO
          
          test := ( ( b^.r2 = row ) & ( b^.c1 = col ) );
          IF ( ( b^.r1 = row ) & ( b^.c1 = col ) ) OR
             ( ( b^.r2 = row ) & ( b^.c2 = col ) ) OR
             ( ( b^.r1 = row ) & ( b^.c2 = col ) ) OR test THEN
            found := TRUE;
            IF b = box1 THEN
              box1 := b^.next;
            ELSE
              pb^.next := b^.next;
            END;
            (* dispose of b *)
          ELSE
            pb := b;
            b  := b^.next;
          END;
        END;

        IF NOT found THEN

          t := text1;
          WHILE ( NOT found ) & ( t # NIL ) DO
             found := TRUE;
             IF ( ( t^.r1 = row ) & ( t^.c1 = col ) ) OR
                ( ( t^.r2 = row ) & ( t^.c2 = col ) ) THEN
               IF t = text1 THEN
                 text1 := t^.next;
               ELSE
                 pt^.next := t^.next;
               END;
               (* dispose of t *)
             ELSE
               pt := t;
               t  := t^.next;
             END;
          END;

        END;
      END;
     END;

     ShowMenu;
   END Delete;

   (* ------------------------------------------------- *)
   PROCEDURE Print;
   BEGIN
      Message( 'print file  ' );

      OpenOutput( 'ps' );
      WriteString( '%!PS-Adobe' ); WriteLn;
      WriteString( '/F 9.375 def' ); WriteLn;
      WriteString( '/L { moveto lineto stroke } bind def' ); WriteLn;
      WriteString( '/B { moveto lineto lineto lineto closepath stroke} bind def' );
      WriteLn;
      WriteString( '/T { moveto show } bind def' ); WriteLn;
      WriteString( '/Courier findfont F scalefont setfont' ); WriteLn;
      WriteString( '612 0 translate 90 rotate' ); WriteLn;
      WriteString( '18 200 translate' ); WriteLn;

      l := line1;
      WHILE l # NIL DO
         WriteCard( l^.c2,      4 ); WriteString( ' F mul ' );
         WriteCard( 25 - l^.r2, 4 ); WriteString( ' F mul ' );

         WriteCard( l^.c1,      4 ); WriteString( ' F mul ' );
         WriteCard( 25 - l^.r1, 4 ); WriteString( ' F mul ' );

         WriteString( ' L' ); WriteLn;
         l := l^.next;
      END;

      b := box1;
      WHILE b # NIL DO
         WriteCard( b^.c1,      4 ); WriteString( ' F mul ' );
         WriteCard( 25 - b^.r2, 4 );  WriteString( ' F mul ' );

         WriteCard( b^.c2,      4 ); WriteString( ' F mul ' );
         WriteCard( 25 - b^.r2, 4 ); WriteString( ' F mul ' );

         WriteCard( b^.c2,      4 ); WriteString( ' F mul ' );
         WriteCard( 25 - b^.r1, 4 ); WriteString( ' F mul ' );

         WriteCard( b^.c1,      4 ); WriteString( ' F mul ' );
         WriteCard( 25 - b^.r1, 4 ); WriteString( ' F mul ' );

         WriteString( ' B' ); WriteLn;
         b := b^.next;
      END;

      t := text1;
      WHILE t # NIL DO
         WriteString( '(' );
         WriteString( t^.string );
         WriteString( ') ' );
         WriteCard( t^.c1, 4 );    WriteString( ' F mul ' );
         WriteCard( 25-t^.r1, 4 ); WriteString( ' F mul ' );

         WriteString( ' T' ); WriteLn;
         t := t^.next;
      END;

      WriteString( 'showpage' ); WriteString( 4C );
      CloseOutput;
      Beep;
      Redraw;
   END Print;

   (* ------------------------------------------------- *)
   PROCEDURE SaveFile;
   BEGIN
      Message( 'save as ' );

      OpenOutput( 'dat' );

      l := line1;
      WHILE l # NIL DO
         WriteString( 'L ' );
         WITH l^ DO
            WriteCard( r1, 4 );  WriteCard( c1, 4 );
            WriteCard( r2, 4 );  WriteCard( c2, 4 );
         END;
         WriteLn;
         l := l^.next;
      END;

      b := box1;
      WHILE b # NIL DO
         WriteString( 'B ' );
         WITH b^ DO
            WriteCard( r1, 4 );  WriteCard( c1, 4 );
            WriteCard( r2, 4 );  WriteCard( c2, 4 );
         END;
         WriteLn;
         b := b^.next;
      END;

      t := text1;
      WHILE t # NIL DO
         WriteString( 'T ' );
         WriteCard( t^.r1, 4 );  WriteCard( t^.c1, 4 );
         WriteString( ' ' );
         WriteString( t^.string );
         WriteLn;
         t := t^.next;
      END;

      CloseOutput;
      Beep;
      Redraw;
   END SaveFile;

   (* ------------------------------------------------- *)
   PROCEDURE LoadFile;
   VAR
      r1, r2, c1, c2 :CARDINAL;
      string         :ARRAY [0..max_string] OF CHAR;
      i              :CARDINAL;

   BEGIN
      Message( 'load file ' );

      OpenInput( 'dat' );
      IF Done THEN

        Read( c );
        WHILE Done DO
          c := CAP( c );
          IF c = 'L' THEN
            ReadCard( r1 );  ReadCard( c1 );  ReadCard( r2 );  ReadCard( c2 );
            ReadLn;
            NewLine( r1, c1, r2, c2 );
          ELSIF c = 'B' THEN
            ReadCard( r1 );  ReadCard( c1 );  ReadCard( r2 );  ReadCard( c2 );
            ReadLn;
            NewBox( r1, c1, r2, c2 );
          ELSIF c = 'T' THEN
            ReadCard( r1 );  ReadCard( c1 );
            i := 0;
            Read(c);
            WHILE ( i < max_string ) & ( c # EOL ) DO
              IF ( c >= ' ' ) & ( c <= '~' ) THEN
                string[i] := c;
                i := i + 1;
              END;
              Read(c);
            END;
            string[i] := 0C;
            ReadLn;
            NewText( r1, c1, string );
          ELSE
            ReadLn;
          END;

          Read( c );
        END;

        CloseInput;
      END;

      Redraw;
   END LoadFile;

BEGIN

  row := 1; col := 1;

  line1 := NIL;   box1 := NIL;   text1 := NIL;

  Redraw;

  StartGet;

  Get( c );
  WHILE c # control_z DO

   c := CAP( c );

   IF c = esc THEN
     (* motion consists of three characters, see the Move procedure *)
     Get( c );

     IF c = '[' THEN
       Get( c );
       Move( c );
     END;

   ELSIF c = 'B' THEN
     PaintBox;
   ELSIF c = 'L' THEN
     PaintLine;
   ELSIF c = 'T' THEN
     PaintText;
   ELSIF c = 'R' THEN
     Redraw;
   ELSIF c = 'D' THEN
     Delete;
   ELSIF c = 'P' THEN
     Print;
   ELSIF c = 'S' THEN
     SaveFile;
   ELSIF c = 'G' THEN
     LoadFile;
   ELSE
     Beep;
   END;

    Get( c );
  END;

  PutCursor( 24, 1 );

  StopGet;

END XPaint.
