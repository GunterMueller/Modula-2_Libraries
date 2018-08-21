IMPLEMENTATION MODULE WordHash;

(* Reserved Word Hash Table for Modula-2
*  coded by J. Andrea, Jan 1986
*  from
*        Minimal Perfect Hash Functions for Reserved Word Lists
*        R.W. Sebeasta and M.A. Taylor
*        SigPlan Notices, V20, #12, December 1985
* 
*  The function HashValue returns a cardinal which can be compared with the
*    values of the word values, not_a_reserved_word is returned if the input
*    string is not a Modula-2 reserved word
*
*)
(* This code may be freely used and distributed, it may not be sold. *)

FROM StringOperations IMPORT Length;

CONST
   min_char = 'A';    max_char = 'Z';
   min_len  =  2;     max_len  = 14;

TYPE
   LetterInfo = RECORD
                  value    :INTEGER;
                  position :INTEGER;
                END;
   WordInfo   = RECORD
                  word_length :CARDINAL;
                  actual_word :ARRAY [0..max_len] OF CHAR;
                END;

VAR
   letters :ARRAY [min_char .. max_char] OF LetterInfo;
   words   :ARRAY [min_hash .. max_hash] OF WordInfo;
   
   (* --------------------------------------------------------- *)
   PROCEDURE HashValue( string :ARRAY OF CHAR ) :CARDINAL;
   (* compute and return the hash value of a Modula-2 reserved word *)

   VAR
      length                   :CARDINAL;
      first, last, second_last :CHAR;
      value                    :INTEGER;

      (* ------------------------------------------------------ *)
      PROCEDURE SecondCheck() :BOOLEAN;
      (* make sure its the actual word, with a char by char comparison *)

      VAR
         i, m, l :CARDINAL;

      BEGIN (* SecondCheck *)

         m := CARDINAL( value );   l := words[m].word_length;

         i := 0;
         WHILE ( i < l ) & ( string[i] = words[m].actual_word[i] ) DO
            i := i + 1;
         END; (* while *)

         RETURN ( i >= l );

      END SecondCheck;

   BEGIN (* HashValue *)

      length := Length(string);
      IF ( length < min_len ) OR ( length > max_len ) THEN
         RETURN not_a_reserved_word;
      ELSE

         first := string[0];  last := string[length-1];
         second_last := string[length-2];

         IF ( first < min_char ) OR ( first > max_char ) OR
            ( last  < min_char ) OR ( last  > max_char ) OR
            ( second_last < min_char ) OR ( second_last > max_char ) THEN
            RETURN not_a_reserved_word;
         ELSE

            value := letters[first].value + letters[last].value + 
                     INTEGER( length )    + letters[second_last].position;
            IF ( value < min_hash ) OR ( value > max_hash ) THEN
               RETURN not_a_reserved_word;
            ELSE
               IF SecondCheck() THEN
                  RETURN CARDINAL( value );
               ELSE
                  RETURN not_a_reserved_word;
               END; (* if *)
            END; (* if *)

         END; (* if *)

      END; (* if *)

   END HashValue;

BEGIN (* Wordhash *)

letters['A'].value :=  18;    letters['A'].position :=  0;
letters['B'].value :=  16;    letters['B'].position :=  1;
letters['C'].value :=   3;    letters['C'].position :=  2;
letters['D'].value := - 3;    letters['D'].position :=  3;
letters['E'].value := -11;    letters['E'].position :=  4;
letters['F'].value :=   4;    letters['F'].position :=  5;
letters['G'].value := 100;    letters['G'].position :=  6;
letters['H'].value :=  14;    letters['H'].position :=  7;
letters['I'].value :=  15;    letters['I'].position :=  8;
letters['J'].value := 100;    letters['J'].position :=  9;
letters['K'].value := 100;    letters['K'].position := 10;
letters['L'].value :=  15;    letters['L'].position := 11;
letters['M'].value :=   1;    letters['M'].position := 12;
letters['N'].value := - 4;    letters['N'].position := 13;
letters['O'].value :=   7;    letters['O'].position := 14;
letters['P'].value :=   1;    letters['P'].position := 15;
letters['Q'].value :=  22;    letters['Q'].position := 16;
letters['R'].value := - 1;    letters['R'].position := 17;
letters['S'].value :=   3;    letters['S'].position := 18;
letters['T'].value :=   0;    letters['T'].position := 19;
letters['U'].value :=   8;    letters['U'].position := 20;
letters['V'].value :=  22;    letters['V'].position := 21;
letters['W'].value := - 2;    letters['W'].position := 22;
letters['X'].value := 100;    letters['X'].position := 23;
letters['Y'].value :=  14;    letters['Y'].position := 24;
letters['Z'].value := 100;    letters['Z'].position := 25;

words[and].word_length     :=  3;    words[and].actual_word     := 'AND';
words[array].word_length   :=  5;    words[array].actual_word   := 'ARRAY';
words[begin].word_length   :=  5;    words[begin].actual_word   := 'BEGIN';
words[by].word_length      :=  2;    words[by].actual_word      := 'BY';
words[case].word_length    :=  4;    words[case].actual_word    := 'CASE';
words[const].word_length   :=  5;    words[const].actual_word   := 'CONST';
words[definition].word_length := 10;
                           words[definition].actual_word        := 'DEFINITION';
words[div].word_length     :=  3;    words[div].actual_word     := 'DIV';
words[do].word_length      :=  2;    words[do].actual_word      := 'DO';
words[else].word_length    :=  4;    words[else].actual_word    := 'ELSE';
words[elsif].word_length   :=  5;    words[elsif].actual_word   := 'ELSIF';
words[end].word_length     :=  3;    words[end].actual_word     := 'END';
words[exit].word_length    :=  4;    words[exit].actual_word    := 'EXIT';
words[export].word_length  :=  6;    words[export].actual_word  := 'EXPORT';
words[for].word_length     :=  3;    words[for].actual_word     := 'FOR';
words[from].word_length    :=  4;    words[from].actual_word    := 'FROM';
words[if].word_length      :=  2;    words[if].actual_word      := 'IF';
words[implementation].word_length := 14;
                           words[implementation].actual_word    := 'IMPLEMENTATION';
words[import].word_length  :=  6;    words[import].actual_word  := 'IMPORT';
words[in].word_length      :=  2;    words[in].actual_word      := 'IN';
words[loop].word_length    :=  4;    words[loop].actual_word    := 'LOOP';
words[mod].word_length     :=  3;    words[mod].actual_word     := 'MOD';
words[module].word_length  :=  6;    words[module].actual_word  := 'MODULE';
words[not].word_length     :=  3;    words[not].actual_word     := 'NOT';
words[of].word_length      :=  2;    words[of].actual_word      := 'OF';
words[or].word_length      :=  2;    words[or].actual_word      := 'OR';
words[pointer].word_length :=  7;    words[pointer].actual_word := 'POINTER';
words[procedure].word_length := 9;
                           words[procedure].actual_word         := 'PROCEDURE';
words[qualified].word_length := 9;
                           words[qualified].actual_word         := 'QUALIFIED';
words[record].word_length  :=  6;    words[record].actual_word  := 'RECORD';
words[repeat].word_length  :=  6;    words[repeat].actual_word  := 'REPEAT';
words[return].word_length  :=  6;    words[return].actual_word  := 'RETURN';
words[set].word_length     :=  3;    words[set].actual_word     := 'SET';
words[then].word_length    :=  4;    words[then].actual_word    := 'THEN';
words[to].word_length      :=  2;    words[to].actual_word      := 'TO';
words[type].word_length    :=  4;    words[type].actual_word    := 'TYPE';
words[until].word_length   :=  5;    words[until].actual_word   := 'UNTIL';
words[var].word_length     :=  3;    words[var].actual_word     := 'VAR';
words[while].word_length   :=  5;    words[while].actual_word   := 'WHILE';
words[with].word_length    :=  4;    words[with].actual_word    := 'WITH';

END WordHash.
