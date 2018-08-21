MODULE Words;

(* 
 * input a file of characters and break the file into a single list
 * of sorted words to be output to another file
 *
 * words have no punctuation, and case is ignored
 *
 * questions ? should words be broken at underscores
 *           ? should punctuation become separate words
 *
*)

(* V1.0, J.Andrea, June 1985 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut      IMPORT OpenInput, OpenOutput, CloseInput, CloseOutput,
                       Read, ReadString, ReadLn,
                       Write, WriteString, WriteLn, WriteCard,
                       EOL, Done, in, out;

FROM FileSystem IMPORT Eof;

FROM Storage    IMPORT ALLOCATE, DEALLOCATE;

CONST
   null = 0C;

TYPE
   String  = ARRAY [0..132] OF CHAR;

   TriePtr = POINTER TO Trie;
   Trie    = RECORD
               key         : CHAR;
               first_child : TriePtr;
               sibling     : TriePtr;
             END; (* record *)

VAR
   input_word_count, output_word_count : CARDINAL;
   alpha                               : TriePtr; (* the very top of the tree*)


   (* ----------------------------------------------------------- *)
   PROCEDURE Initialize;
   BEGIN (* Initalize *)

     input_word_count  := 0;
     output_word_count := 0;

     NEW(alpha); (* define the very top of the tree *)
     alpha^.key         := '@';  (* it has no value *)
     alpha^.first_child := NIL;   (* no children YET *)
     alpha^.sibling     := NIL;   (* and cannot have any sisters *)

   END Initialize;

   (* ----------------------------------------------------------- *)
   PROCEDURE InputWords;
   (*
    * use ReadString to input each word (separated by blanks) and put
    * each word into a trie-tree character by char
   *)

   VAR
      word : String;
      len  : CARDINAL;

      (* -------------------------------------------------------- *)
      PROCEDURE AddWordToTree;
      (* put this word into the tree *)

      VAR
        p, parent, save : TriePtr;
        i               : CARDINAL;
        c               : CHAR;

      BEGIN (* AddWordToTree *)

        p := alpha;

        FOR i := 0 TO LEN(word) DO

           c := word[i];
           (* make the words case-independent. uppercase them *)
           IF ( c >= 'a' ) & ( c <= 'z' ) THEN c := CAP(c); END;

           parent := p;   p := parent^.first_child;

           (* does this parent have any children *)
           IF p = NIL THEN

              (* no children, this is the first child, so add it *)
              NEW(p);
              parent^.first_child := p;
              p^.key         := c;
              p^.first_child := NIL;
              p^.sibling     := NIL;

           ELSE

              (* check all the children for this character *)

              (* keep it sorted *)
              save := p;
              WHILE ( p # NIL ) & ( p^.key < c ) DO
                save := p;
                p := p^.sibling;
              END; (* while *)

              (* was it found *)
              IF p = NIL THEN
                 (* no, add the new char at the end of the list *)
                 NEW(p);
                 save^.sibling  := p;
                 p^.key         := c;
                 p^.first_child := NIL;
                 p^.sibling     := NIL;
              ELSE
                 IF p^.key # c THEN
                    (* no, insert the char between two existing ones *)
                    IF p = parent^.first_child THEN
                       (* the current char must go before the first *)
                       NEW(p);
                       p^.sibling          := parent^.first_child;
                       parent^.first_child := p;
                       p^.key              := c;
                       p^.first_child      := NIL;
                    ELSE
                       NEW(p);
                       p^.sibling     := save^.sibling;  (* switch points *)
                       save^.sibling  := p;
                       p^.key         := c;
                       p^.first_child := NIL;
                    END; (* if *)
                 ELSE
                 END; (* if *)
              END; (* if *)

           END; (* if *)

        END; (* for *)

      END AddWordToTree;

      (* ------------------------------------------------------- *)
      PROCEDURE ReadProperWord;
      (*
       * words are delemited by control-chars, spaces and punctuation
      *)

      VAR
         c : CHAR;
         i : CARDINAL;

         (* ------------------------------------------------------- *)
         PROCEDURE AlphaNumeric( c : CHAR ) : BOOLEAN;
         VAR cap : CHAR;
         BEGIN (* AlphaNumeric *)
           cap := CAP(c);
           RETURN ( ( cap >= 'A' ) & ( cap <= 'Z' ) )
               OR ( ( c   >= '0' ) & ( c   <= '9' ) )
         END AlphaNumeric;

      BEGIN (* ReadProperWord *)

         (* skip special chars *)
         Read(c);
         WHILE ( NOT Eof(in) ) & ( NOT AlphaNumeric(c) ) DO
            Read(c);
         END; (* while *)

         i := 0;
         WHILE ( NOT Eof(in) ) & ( AlphaNumeric(c) ) DO
            word[i] := c;
            i := i + 1;  Read(c);
         END; (* while *)
         word[i] := null;

      END ReadProperWord;

   BEGIN (* InputWords *)

       ReadProperWord;
       WHILE NOT Eof(in) DO
           input_word_count := input_word_count + 1;
           len := LEN(word);
           IF len > 0 THEN
              AddWordToTree;
           END; (* if *)
           ReadProperWord;
       END; (* while *)

   END InputWords;

   (* ----------------------------------------------------------- *)
   PROCEDURE OutputWords;

   (*
    * do char by char output of the tree into a new file to show
    * each unique word
   *)

    VAR
       output_word : String;

      (* ------------------------------------------------------- *)
      PROCEDURE PrintTree( parent : TriePtr ; level : CARDINAL );
      (* output the contents of the tree *)

      BEGIN (* PrintTree *)

         (* is there a character at this node *)
         IF parent = NIL THEN

            (* no, this must be the bottom of a word *)
            output_word[level] := null;
            WriteString(output_word); WriteLn;
            output_word_count := output_word_count + 1;

         ELSE

            (* do all the children at this same level *)
            WHILE parent # NIL DO

               output_word[level] := parent^.key;

               (* go down another level *)
               PrintTree( parent^.first_child , level+1 );

               (* back at this same level *)
               parent := parent^.sibling;
  
            END; (* while *)

         END; (* if *)

      END PrintTree;

   BEGIN (* OutputWords *)

       PrintTree(alpha^.first_child,0);

   END OutputWords;

BEGIN (* Words *)

   Initialize;

   WriteLn;
   WriteString('For the text of words');
   OpenInput('.TXT');

   InputWords;

   CloseInput;   

   WriteLn;
   WriteString('     There are '); WriteCard(input_word_count,6);
   WriteString(' words in the input file.'); WriteLn;

   WriteLn;
   WriteString('For the single list of words');
   OpenOutput('.OUT');

   OutputWords;

   CloseOutput;

   WriteLn;
   WriteString('     And '); WriteCard(output_word_count,6);
   WriteString(' unique words in the output file.');

   WriteLn;   WriteLn;

END Words.
