MODULE PigLatin;

(* Convert a text file into pig-latin. *)
(* J. Andrea, Mar./91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT ReadString, ReadLn, WriteString, WriteLn, Done,
                  OpenInput, CloseInput, OpenOutput, CloseOutput;

CONST
  nul = 0C;

TYPE
  String = ARRAY [0..132] OF CHAR;

VAR
  in_word, out_word  :String;
  i, j, n, out_len   :CARDINAL;
  first_word_in_line :BOOLEAN;

      (* ----------------------------------------------------- *)
      PROCEDURE Alphabet( c :CHAR ) :BOOLEAN;
      BEGIN
         c := CAP( c );
         RETURN ( c >= 'A' ) & ( c <= 'Z' )
      END Alphabet;

      (* ----------------------------------------------------- *)
      PROCEDURE Vowel( c :CHAR ) :BOOLEAN;
      BEGIN
         c := CAP( c );
         RETURN ( c = 'A' ) OR ( c = 'E' ) OR ( c = 'I' ) OR
                ( c = 'O' ) OR ( c = 'U' )
      END Vowel;

      (* ----------------------------------------------------- *)
      PROCEDURE StartBufferText;
      BEGIN (* StartBufferText *)
        out_len := 0;
        first_word_in_line := TRUE;
      END StartBufferText;

      (* ----------------------------------------------------- *)
      PROCEDURE EndBufferText;
      BEGIN (* EndBufferText *)
        IF out_len > 0 THEN
          WriteLn;
        END;
      END EndBufferText;

      (* ----------------------------------------------------- *)
      PROCEDURE BufferText( string :ARRAY OF CHAR; space :BOOLEAN );
      CONST
        max = 75;
      VAR
        len :CARDINAL;
      BEGIN (* BufferText *)
      
        len := LEN( string );
      
        IF ( len + out_len + 1 > max ) & space THEN
          WriteLn;
          out_len := 0;
          first_word_in_line := TRUE;
        END;

        IF space & NOT first_word_in_line THEN
          WriteString( ' ' );
          out_len := out_len + 1;
        END;
      
        WriteString( string );
        out_len := out_len + len;
      
        first_word_in_line := FALSE;
      
      END BufferText;

   (* ----------------------------------------------------- *)
   PROCEDURE StringToPig( string :ARRAY OF CHAR );
   (* This word may have a dish in it, or punctuation attached to it,
      so break it into any subwords, and deal with them one at a time *)

   VAR
      L, p, q, i, n   :CARDINAL;
      front, no_vowel :BOOLEAN;
      first_letter    :CHAR;
   
   BEGIN (* StringToPig *)
      
     front := TRUE;

     L := LEN( string );
      
     p := 0;
     WHILE p < L DO

       (* find the end of this word *)
       q := p;
       WHILE ( q < L ) & Alphabet( string[q] ) DO
         q := q + 1;
       END;
       q := q - 1;

       first_letter := string[p];

       no_vowel := NOT Vowel( first_letter );

       IF no_vowel THEN p := p + 1; END;

       n := 0;
       FOR i := p TO q DO
         out_word[n] := string[i];
         n := n + 1;
       END;
       out_word[n] := 0C;
      
       BufferText( out_word,  front );     

       IF no_vowel THEN BufferText( first_letter, FALSE ); END;

       BufferText( 'ay', FALSE );

       front := FALSE;

       (* now find any non alphabetic parts and just output them *)

       p := q + 1;

       IF p < L THEN

         q := p;
         WHILE ( q < L ) & NOT Alphabet( string[q] ) DO
           q := q + 1;
         END;
         q := q - 1;

         n := 0;
         FOR i := p TO q DO
           out_word[n] := string[i];
           n := n + 1;
         END;
         out_word[n] := 0C;
      
         BufferText( out_word, FALSE );

         p := q + 1;
       END;

     END;

   END StringToPig;

BEGIN (* PigLatin *)

OpenInput( '.TXT' ); OpenOutput( '.PIG' );

StartBufferText;

ReadString( in_word );
WHILE Done DO

  IF Alphabet( in_word[0] ) THEN

    StringToPig( in_word );

  ELSE
    (* ignore anything that doesn't start with a letter *)
    BufferText( in_word, TRUE );
  END;

  ReadString( in_word );
END;

EndBufferText;

CloseInput; CloseOutput;

END PigLatin. 
