MODULE TestHash;

(* test the WordHash module and show how its used *)
(* J. Andrea, 1984 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT Write, WriteLn, WriteString, WriteCard,
                  ReadString,     ReadLn;

FROM WordHash IMPORT not_a_reserved_word, HashValue;

VAR
  string : ARRAY [0..80] OF CHAR;
  x      : CARDINAL;

BEGIN (* TestHash *)

  WriteString(' >');  ReadString(string);  ReadLn;
  WHILE LEN(string) > 0 DO

     x := HashValue( string );
     IF x = not_a_reserved_word THEN
       WriteString(' no ');
     ELSE
       WriteCard( x, 10 );
     END; (* if *)
     WriteLn;

     WriteString(' >');  ReadString(string);  ReadLn;

  END; (* while *)

END TestHash.
