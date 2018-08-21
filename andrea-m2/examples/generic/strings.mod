MODULE TestStrings;

(* test some of the extended string handling procedures, and show
   how they are used *)
(* J. Andrea, Aug.12/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, ReadCard, WriteCard, WriteLn, Read, ReadLn;
FROM StringOperations IMPORT Equal, Compare, Index, Append, Concat, Trim,
                             SubString, Upper, Lower, Insert, Delete, Replace;
FROM MoreStrings IMPORT CaseCompare, WildcardMatch, ReadLine, Locate,
                        EqualSubString, Contains;

VAR
   c                  :CHAR;
   one, two, three    :ARRAY [0..11] OF CHAR;
   result, start, len :CARDINAL;

   (* ----------------------------------------------------------------- *)
   PROCEDURE GetTest( VAR c :CHAR );
   BEGIN

      WriteString( 'a   Equal' ); WriteLn;
      WriteString( 'b   Index' ); WriteLn;
      WriteString( 'c   Concat' ); WriteLn;
      WriteString( 'd   Compare' ); WriteLn;
      WriteString( 'e   WildcardMatch' ); WriteLn;
      WriteString( 'f   Trim' ); WriteLn;
      WriteString( 'g   Append' ); WriteLn;
      WriteString( 'h   Locate' ); WriteLn;
      WriteString( 'i   SubString' ); WriteLn;
      WriteString( 'j   Upper' ); WriteLn;
      WriteString( 'k   Lower' ); WriteLn;
      WriteString( 'l   Delete' ); WriteLn;
      WriteString( 'm   Insert' ); WriteLn;
      WriteString( 'n   EqualSubString' ); WriteLn;
      WriteString( 'o   CaseCompare' ); WriteLn;
      WriteString( 'p   Replace' ); WriteLn;
      WriteString( 'q   Contains' ); WriteLn;
      WriteString( 'X   exit' ); WriteLn;
      WriteString( 'which procedure to test ? ' ); Read( c ); ReadLn;
      WriteLn;

      c := CAP( c );
   END GetTest;

   (* ----------------------------------------------------------------- *)
   PROCEDURE GetString( prompt :ARRAY OF CHAR; VAR string :ARRAY OF CHAR );
   BEGIN

       WriteString( prompt );
       ReadLine( string );

       WriteString( '>' ); WriteString( string ); WriteString( '<' ); WriteLn;
   END GetString;

   (* ----------------------------------------------------------------- *)
   PROCEDURE GetCard( prompt :ARRAY OF CHAR; VAR x :CARDINAL );
   BEGIN

       WriteString( prompt );
       ReadCard( x ); ReadLn;

       WriteCard( x, 0 ); WriteLn;
   END GetCard;

   (* ----------------------------------------------------------------- *)
   PROCEDURE DoCompare( a, b, op :ARRAY OF CHAR );
   BEGIN

       IF Compare( a, op, b ) THEN
         WriteString( 'TRUE with '); WriteString( op );
       ELSE
         WriteString( 'FALSE with '); WriteString( op );
       END;
       WriteLn;

   END DoCompare;

BEGIN (* TestMoreStrings *)

WriteLn;
WriteString( 'the strings used here are all of length ' );
WriteCard( HIGH(one) + 1, 0 );
WriteLn;

GetTest( c );

WHILE c # 'X' DO

IF c = 'A' THEN  (* Equal *)

  GetString( 'enter the first  string ? ', one );
  GetString( 'enter the second string ? ', two );

  IF Equal( one, two ) THEN
    WriteString( 'equal' );
  ELSE
    WriteString( 'NOT equal' );
  END;
  WriteLn;

ELSIF c = 'B' THEN (* Index *)

  GetString( 'enter the long string ? ', one );
  GetString( 'enter the  sub-string ? ', two );

  result := Index( one, two );

  IF result = 0 THEN
    WriteString( 'not found' );
  ELSE
    WriteString( 'position ' ); WriteCard( result, 0 );
  END;
  WriteLn;

ELSIF c = 'C' THEN  (* Concat *)

  GetString( 'enter the first  string ? ', one );
  GetString( 'enter the second string ? ', two );

  Concat( one, two, three );

  WriteString( '>' ); WriteString( three ); WriteString( '<' );
  WriteLn;

ELSIF c = 'D' THEN  (* Compare *)

  GetString( 'enter the first  string ? ', one );
  GetString( 'enter the second string ? ', two );

  DoCompare( one, two, '=' );    DoCompare( one, two, '#' );
  DoCompare( one, two, '>' );    DoCompare( one, two, '<' );
  DoCompare( one, two, '>=' );   DoCompare( one, two, '<=' );

ELSIF c = 'E' THEN  (* WildCardMatch *)

  GetString( 'enter the  string ? ', one );
  GetString( 'enter the pattern ? ', two );

  IF WildcardMatch( one, two ) THEN
    WriteString( 'equal' );
  ELSE
    WriteString( 'NOT equal' );
  END;
  WriteLn;

ELSIF c = 'F' THEN (* Trim *)

  GetString( 'enter the string ? ', one );

  Trim( one );

  WriteString( '>' ); WriteString( one ); WriteString( '<' );
  WriteLn;

ELSIF c = 'G' THEN  (* Append *)

  GetString( 'enter the first  string ? ', one );
  GetString( 'enter the second string ? ', two );

  Append( one, two );

  WriteString( '>' ); WriteString( two ); WriteString( '<' ); WriteLn;

ELSIF c = 'H' THEN  (* Locate *)

  GetString( 'enter the long string ? ', one );
  GetString( 'enter the     pattern ? ', two );
  GetCard( 'start location ? ', start );

  result := Locate( one, two, start );

  IF result = 0 THEN
    WriteString( 'not found' );
  ELSE
    WriteString( 'position ' ); WriteCard( result, 0 );
  END;
  WriteLn;

ELSIF c = 'I' THEN  (* SubString *)

  GetString( 'enter the string ? ', one );
  GetCard( 'start location ? ', start );
  GetCard( 'how many characters ? ', len );

  SubString( one, start, len, two );

  WriteString( '>' ); WriteString( two ); WriteString( '<' ); WriteLn;

ELSIF c = 'J' THEN  (* Upper *)

  GetString( 'enter the string ? ', one );

  Upper( one );

  WriteString( '>' ); WriteString( one ); WriteString( '<' ); WriteLn;

ELSIF c = 'K' THEN  (* Lower *)

  GetString( 'enter the string ? ', one );

  Lower( one );

  WriteString( '>' ); WriteString( one ); WriteString( '<' ); WriteLn;

ELSIF c = 'L' THEN  (* Delete *)

  GetString( 'enter the string ? ', one );
  GetCard( 'start location ? ', start );
  GetCard( 'how many characters ? ', len );

  Delete( one, start, len );

  WriteString( '>' ); WriteString( one ); WriteString( '<' ); WriteLn;

ELSIF c = 'M' THEN  (* Insert *)

  GetString( 'enter the string to insert ? ', one );
  GetString( 'enter the source ? ', two );
  GetCard( 'start location ? ', start );

  Insert( one, start, two );

  WriteString( '>' ); WriteString( two ); WriteString( '<' ); WriteLn;

ELSIF c = 'N' THEN  (* EqualSubString *)

  GetString( 'enter the first  string ? ', one );
  GetString( 'enter the second string ? ', two );
  GetCard( 'start location ? ', start );
  GetCard( 'how many characters ? ', len );

  IF EqualSubString( one, two, start, len ) THEN
    WriteString( 'equal' );
  ELSE
    WriteString( 'NOT equal' );
  END;
  WriteLn;

ELSIF c = 'O' THEN  (* CaseCompare *)

  GetString( 'enter the first  string ? ', one );
  GetString( 'enter the second string ? ', two );

  c := CaseCompare( one, two, TRUE );

  WriteString( 'result with    case-sensitive: ' ); WriteString( c );
  WriteLn;

  c := CaseCompare( one, two, FALSE );

  WriteString( 'result without case-sensitive: ' ); WriteString( c );
  WriteLn;

ELSIF c = 'P' THEN  (* Replace *)

  GetString( 'enter the first       string ? ', one );
  GetString( 'enter the replacement string ? ', two );
  GetCard( 'start location ? ', start );

  Replace( two, start, one );

  WriteString( '>' ); WriteString( one ); WriteString( '<' ); WriteLn;

ELSIF c = 'Q' THEN  (* Contains *)

  GetString( 'enter the  string ? ', one );
  GetString( 'enter the pattern ? ', two );

  IF Contains( one, two ) THEN
    WriteString( 'is contained' );
  ELSE
    WriteString( 'NOT contained' );
  END;
  WriteLn;

END;

WriteLn;
WriteString( 'hit RETURN' ); ReadLn;
WriteLn; WriteLn; WriteLn;

GetTest( c );
END; (* while *)

END TestStrings.
