MODULE Test;

(* Test the FormEdit module *)
(* J. Andrea, Jun.1993 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM FormEdit IMPORT SetLine, GetLine, DoEditing;
FROM InOut IMPORT WriteString, Write, WriteLn;
FROM Break IMPORT EnableBreak;

TYPE
  LongString = ARRAY [1..60] OF CHAR;
  ShortString = ARRAY [1..5] OF CHAR;

VAR
  done :BOOLEAN;

  card_type    :CHAR;
  prof_name,
  out_file     :LongString;
  class_name,
  class_number,
  class_sect,
  card_size,
  n_id, n_answers :ShortString;

BEGIN EnableBreak; (* ------------------------------------------------ *)

    SetLine( 1, 'Test or Evaluation', 'T', 1, done );

    SetLine( 3, 'Prof name', 'no name', 40, done );
    SetLine( 4, 'Class name', 'Chem', 4, done );
    SetLine( 5, 'Class number', '100', 3, done );
    SetLine( 6, 'Class section', '10', 2, done );

    SetLine( 8, 'Card size', '50', 3, done );
    SetLine( 9, 'Id size', '6', 1, done );
    SetLine( 10, 'Number of answers', '50', 3, done );

    SetLine( 12, 'Out file', 'out.out', 12, done );


    DoEditing( 0, 30, FALSE );


    GetLine( 1, card_type, done );

    GetLine( 3, prof_name, done );
    GetLine( 4, class_name, done );
    GetLine( 5, class_number, done );
    GetLine( 6, class_sect, done );

    GetLine( 8, card_size, done );
    GetLine( 9, n_id, done );
    GetLine( 10, n_answers, done );

    GetLine( 12, out_file, done );


    WriteString( card_type );    WriteString( ' !Test or Evaluation' ); WriteLn;
    WriteString( class_name );   WriteString( ' !class name' ); WriteLn;
    WriteString( class_number ); WriteString( ' !class number' ); WriteLn;
    WriteString( class_sect );   WriteString( ' !class section' ); WriteLn;
    WriteString( prof_name );    WriteString( ' !prof name' ); WriteLn;
    WriteString( card_size );    WriteString( ' !card size' ); WriteLn;
    WriteString( n_id );         WriteString( ' !size of id number' ); WriteLn;
    WriteString( n_answers );    WriteString( ' !size of answer data' ); WriteLn;
    WriteString( out_file );     WriteString( ' !out file' ); WriteLn;

END Test.
