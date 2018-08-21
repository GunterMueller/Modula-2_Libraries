MODULE Kwic;

(* A keyword-in-context program *)

(* V1.0, J.Andrea, July 85 *)
(* This code may be freely used and distributed, it may not be sold *)

(* 
*  find the input string in a text file and show the line on which
*  it occured in the output file, with the line number --- many times too
*
*  the line consists of a number of characters before and after the string
*    -- this size is input by the user
*
*  the line buffer is implemented with the use of a circular linked list
*   with a pointer to the start, and a pointer to the position at which
*   the string search is being performed
*
*  the input string is converted to uppercase and the search is performed
*   on uppercase only
*
*)

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM InOut   IMPORT OpenInput, OpenOutput, CloseInput, CloseOutput,
                    Read, ReadLn, ReadCard,
                    WriteString, WriteLn, WriteCard,
                    in, out, EOL;

FROM FileSystem IMPORT Eof, Name;


CONST
    null  = 0C;
    blank = ' ';

    abs_max_string_size = 132 DIV 2;
    abs_max_buffer      = 132 - 2;

TYPE
    String = ARRAY [0..abs_max_string_size] OF CHAR;

VAR
    input_line_number : CARDINAL;
    match_count       : CARDINAL;

    string            : String;
    string_size       : CARDINAL;

    buffer_size       : CARDINAL;

    input_file_name   : String;

    i                 : CARDINAL;

   (* --------------------------------------------------------- *)
   PROCEDURE UpperCase( c : CHAR ) : CHAR;
   BEGIN (* *)
     IF ( c >= 'a' ) & ( c <= 'z' ) THEN
        RETURN CAP(c);
     ELSE
        RETURN c;
     END; (* if *)
   END UpperCase;

   (* --------------------------------------------------------- *)
   PROCEDURE InputString;
   (* 
   *  get the string to be search for and make sure that its not too big 
   *)

   VAR
      c       : CHAR;
      p       : CARDINAL;
      too_big : BOOLEAN;

   BEGIN (* InputString *)

      WriteString('Input the string to find ? ');

      too_big := FALSE;
      p := 0;   Read(c);
      WHILE ( c # EOL ) & ( NOT too_big ) DO
         IF p = abs_max_string_size THEN
            too_big := TRUE;
         ELSE
            string[p] := UpperCase(c);
            Read(c);        p := p + 1;
         END; (* if *)
      END; (* while *)
      ReadLn;

      IF too_big THEN
         WriteLn;
         WriteString('The program will only handle strings to a size of');
         WriteCard(abs_max_string_size,10); WriteLn;
         WriteString('so your string was truncated.'); WriteLn;
      END; (* if *)

      string[p]   := null;
      string_size := p;

   END InputString;

   (* --------------------------------------------------------- *)
   PROCEDURE InputBufferSize;
   (*
   *  get the number of characters to be output before and after the
   *  string once it is found 
   *)

   BEGIN (* InputBufferSize *)

      WriteString('What is the number of characters to be buffered');
      WriteLn;
      WriteString(' before and after the search string'); WriteLn;

      (* 
      * make this suggestion about the size if its going on a terminal
      *   or a printer page 
      *)
      WriteString('The suggested sizes are:'); WriteLn;
      IF 80 > string_size THEN
         WriteCard( (80-string_size) DIV 2, 10);
         WriteString(' to fill up an 80 character line.'); WriteLn;
      END; (* if *)
      WriteCard( (132-string_size) DIV 2, 10);
      WriteString(' to fill up a 132 character line.'); WriteLn;

      WriteString(' 1 to'); WriteCard(abs_max_buffer,5);
      WriteString(' ? ');
      ReadCard(buffer_size); ReadLn;

      WHILE ( buffer_size < 1 ) OR ( buffer_size > abs_max_buffer ) DO

         WriteLn;
         WriteString(' -- invalid number, try again --');
         WriteLn;

         WriteString('What is the number of characters to be buffered');
         WriteLn;
         WriteString(' before and after the search string');
         WriteString(' 1 to'); WriteCard(abs_max_buffer,5);
         WriteString(' ? ');
         ReadCard(buffer_size);  ReadLn;

      END; (* while *)
      
   END InputBufferSize;

   (* --------------------------------------------------------- *)
   PROCEDURE ScanText;
   (*
   * search through the input text looking for the input search string 
   *   the search is done a character at a time every time a new character
   *     is input
   *   the beginning of the search position is always kept as a pointer to
   *     the position 'string_position'
   *  the top of the text is the pointer 'first', which moves one position
   *     forward in the ring every time a new character is input
   *  and characters are always input at the end position 'last'
   *
   *  So the ring actually moves backward through the text, with characters
   *    being dropped off at the head 'first', but upon output the head
   *    of the ring is the first character on the line.
   *)

   TYPE
      RingPtr     = POINTER TO RingElement;
      RingElement = RECORD
                      value : CHAR;
                      next  : RingPtr;
                    END; (* record *)


   VAR
      first, last, string_position : RingPtr;
      real_buffer_size             : CARDINAL;

       (* --------------------------------------------------------- *)
       PROCEDURE InitalizeRing;
       (*
       *  build the ring with the given number of characters on each side
       *    of the string using a linked list
       *)

       VAR
          i       : CARDINAL;
          fiddle_dum, fiddle_de : RingPtr;

       BEGIN (* InitalizeRing *)

          (* the the actual size of the linked ring is   *)
          real_buffer_size := 2 * buffer_size + string_size;

          (* build each element in the ring *)
          (* each one points to the next element in the list *)

          (* create the top of the list *)
          NEW(first);
          first^.value := null;

          (* first is the part before the string *)
          NEW(fiddle_dum);
          first^.next := fiddle_dum;

          FOR i := 1 TO buffer_size - 2 DO
             NEW(fiddle_de);
             fiddle_dum^.next := fiddle_de;
             fiddle_dum^.value:= null;
             fiddle_dum       := fiddle_de;
          END; (* for *)

          (* then the string part *)
          NEW(fiddle_de);
          (* make the string point to this place *)
          string_position := fiddle_dum;
          (* and continue *)
          fiddle_dum^.next := fiddle_de;
          fiddle_dum^.value:= null;
          fiddle_dum       := fiddle_de;

          FOR i := 1 TO string_size - 1 DO
             NEW(fiddle_de);
             fiddle_dum^.next := fiddle_de;
             fiddle_dum^.value:= null;
             fiddle_dum       := fiddle_de;
          END; (* for *)

          (* and last is the part after the string *)
          FOR i := 1 TO buffer_size DO
             NEW(fiddle_de);
             fiddle_dum^.next := fiddle_de;
             fiddle_dum^.value:= null;
             fiddle_dum       := fiddle_de;
          END; (* for *)

          (* finally, burn the candle at both ends *)
          fiddle_dum^.next := first;
          fiddle_dum^.value:= null;
          last             := fiddle_dum;

          (* now fill up the ring with the first charcters in the file *)
          i := 0;
          WHILE ( NOT Eof(in) ) & ( i < real_buffer_size ) DO
             ReadNextChar;   UpdateRingPointers;
             i := i + 1;
          END; (* while *)

       END InitalizeRing;

       (* --------------------------------------------------------- *)
       PROCEDURE ReadNextChar;
       (*
       * get the next character in the file, control characters are blanked
       *)

       VAR
          c : CHAR;

       BEGIN (* ReadNextChar *)

          IF NOT Eof(in) THEN

             Read(c);
             IF c = EOL THEN
                (* replace the end of line with a blank *)
                c := blank;
                input_line_number := input_line_number + 1;
             ELSIF c < ' ' THEN
                (* skip control chars *)
                c := blank;
             END; (* if *)

             last := last^.next;
             last^.value := c;

          ELSE
             c := null;
          END; (* if *)
          
       END ReadNextChar;

       (* --------------------------------------------------------- *)
       PROCEDURE UpdateStringPointer;
       (* move the position of the string ahead in the ring *)

       BEGIN (* UpdateStringPointer *)

          string_position := string_position^.next;

       END UpdateStringPointer;

       (* --------------------------------------------------------- *)
       PROCEDURE UpdateRingPointers;
       (* move the position of the first of the ring to the next character *)

       BEGIN (* UpdateRingPointers *)

          first := first^.next;

          UpdateStringPointer;

       END UpdateRingPointers;

       (* --------------------------------------------------------- *)
       PROCEDURE SearchForStringInFirstRing;
       (* look for the string in the first 'buffer' characters' *)

       VAR
          save_position : RingPtr;

       BEGIN (* SearchForStringInFirstRing *)

          (* save the old position *)
          save_position := string_position;

          (* look for the string at the very beginning *)
          string_position := first;

          (*
          * search until we come to the position of the real string pointer
          *  in the normal ring
          *)
          WHILE string_position # save_position DO
             SearchForString;
             UpdateStringPointer;
          END; (* while *)

          (*
          *  reset the old string place, to its normal position
          *  where it will stay for the rest of the program
          *)
          string_position := save_position;

       END SearchForStringInFirstRing;

       (* --------------------------------------------------------- *)
       PROCEDURE SearchForString;
       (* 
       * look for the search string at the current position
       *  if each successive character matches then move the search location
       *  along to the next character in the ring and test again ...
       *)

       VAR
          i            : CARDINAL;
          search_value : RingPtr;
          c            : CHAR;
          char_match   : BOOLEAN;

       BEGIN (* SearchForString *)

          search_value := string_position;

          i := 0;  char_match := TRUE;

          WHILE ( char_match ) & ( i < string_size ) DO
              c            := UpperCase(search_value^.value);
              char_match   := c = string[i];
              i            := i + 1;
              search_value := search_value^.next;
          END; (* while *)

          IF char_match THEN
             (* wow, it must have found the string *)
             match_count := match_count + 1;

             (* output the line number *)
             WriteString('< line');
             WriteCard(input_line_number,10);
             WriteString(' >');WriteLn;

             search_value := first;

             (* output this line *)

             (* do the first character in the ring *)
             WriteString(search_value^.value);
             search_value := search_value^.next;

             (* then continue until we come back to the first again *)
             WHILE search_value # first DO
                WriteString(search_value^.value);
                search_value := search_value^.next;
             END; (* while *)
             WriteLn;  WriteLn;

          END; (* if *)

       END SearchForString;

   BEGIN (* ScanText *)

     match_count := 0;    input_line_number := 1;

     InitalizeRing;

     SearchForStringInFirstRing;

     WHILE NOT Eof(in) DO
        SearchForString;
        ReadNextChar;
        UpdateRingPointers;
     END; (* while *)

     (* keep searching till the end of the current and last ring *)
     FOR i := 1 TO buffer_size + string_size - 1 DO
         UpdateStringPointer;
         SearchForString;
     END; (* for *)

   END ScanText;


BEGIN (* Kwic *)

  WriteLn;
  InputString;

  IF string_size > 0 THEN

     WriteLn;
     InputBufferSize;

     WriteLn;  OpenInput('.TXT');   WriteLn; OpenOutput('.OUT');

     Name(in,input_file_name);
     WriteString('KWIC searching for "'); WriteString(string);
     WriteString('" in '); WriteString(input_file_name);
     WriteLn;
     WriteString('using a'); WriteCard(buffer_size,10);
     WriteString(' character buffer on each side of the string.');
     WriteLn;  WriteLn;

     ScanText;

     CloseInput;  CloseOutput;

     WriteLn;
     WriteCard(match_count,10);       WriteString(' matches found in ');
     WriteCard(input_line_number-1,10); WriteString(' input lines');
     WriteLn;

  ELSE
    WriteString('null string is no good'); WriteLn;
  END; (* if *)

END Kwic.
