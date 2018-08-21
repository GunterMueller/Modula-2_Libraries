IMPLEMENTATION MODULE SayNumbers;

(* Make a cardinal number into a phrase of words of numbers *)
(* V1.0, J. Andrea, Oct.11/92 *)
(* This code may be freely used and distributed, it may not be sold *)

(*
The term "order" here is used as a power of one thousand, i.e.
0 to 999 is order 0, 1000 to 999999 is order 1, 1000000 is order 2, etc.
*)

FROM MoreMath IMPORT SizeCard;
FROM StringOperations IMPORT Assign, Delete, Append, Insert;

CONST
  longest = 10; (* number of decimal digits in the longest number *)
                (* 5 is good for 16 bits, 9 for 32 bits *)
  max_order = 4; (* the power of 1 thousand for the longest number *)
                 (* 1 is good for 16 bits, 2 for 32 bits *)

VAR
  n_order_values :CARDINAL;
  order_values   :ARRAY [0..max_order] OF CARDINAL;

  size_to_order  :ARRAY [0..longest] OF CARDINAL;

  order_name :ARRAY [0..max_order] OF ARRAY [1..11] OF CHAR;

  digit_name :ARRAY [0..9] OF ARRAY [1..5] OF CHAR;
  teen_name  :ARRAY [0..9] OF ARRAY [1..9] OF CHAR;
  tens_name  :ARRAY [1..9] OF ARRAY [1..7] OF CHAR;

  i :CARDINAL;

  (* ---------------------------------------------------- *)
  PROCEDURE SayInt( x :INTEGER; VAR word :ARRAY OF CHAR );
  BEGIN
    IF x = 0 THEN
      Assign( digit_name[0], word );
    ELSE
      SayCard( CARDINAL( ABS( x ) ), word );
      Insert( 'minus ', 1, word );
    END;
  END SayInt;

  (* ---------------------------------------------------- *)
  PROCEDURE SayCard( x :CARDINAL; VAR word :ARRAY OF CHAR );
  VAR
    top, order_size, order_value :CARDINAL;

    (* -------------------------------------------------- *)
    PROCEDURE Say100( x :CARDINAL );
    VAR
      d :CARDINAL;
    BEGIN
      IF x > 99 THEN
        d := x DIV 100;
        Append( ' ', word );
        Append( digit_name[d], word );
        Append( ' hundred', word );
        x := x - d * 100;
      END;

      IF ( x >= 10 ) & ( x <= 19 ) THEN
        Append( ' ', word ); Append( teen_name[x-10], word );
      ELSE

        IF x > 10 THEN
          d := x DIV 10;
          Append( ' ', word ); Append( tens_name[d], word );
          x := x - d * 10;
        END;

        IF x > 0 THEN
          Append( ' ', word ); Append( digit_name[x], word );
        END;
      END;

    END Say100;

  BEGIN

     IF x = 0 THEN
       Assign( digit_name[0], word );
     ELSE

       word[0] := 0C;

       WHILE x > 0 DO
         order_size := size_to_order[ SizeCard( x ) ];


         WHILE order_size > n_order_values DO
           order_value := order_values[n_order_values];
           n_order_values := n_order_values + 1;
           order_values[n_order_values] := order_value * 1000;
         END;

         order_value := order_values[ order_size ];

         (* get the value which is less than one thousand *)
         top := x DIV order_value;

         Say100( top );

         IF order_size > 0 THEN
           (* and now say order too *)
           Append( ' ', word ); Append( order_name[order_size], word );
         END;

         x := x - top * order_value;
       END;

       WHILE word[0] = ' ' DO
          Delete( word, 1, 1 );
       END;

     END;

  END SayCard;

BEGIN

     Assign( "ones",     order_name[0] );
     Assign( "thousand", order_name[1] );
     Assign( "million",  order_name[2] );
     Assign( "billion",  order_name[3] );
     Assign( "trillion", order_name[4] );
(*
     IF max_order > 4 THEN Assign( "quadrillion", order_name[5] ) END;
     IF max_order > 5 THEN Assign( "quintillion", order_name[6] ) END;
     IF max_order > 6 THEN Assign( "sextillion", order_name[7] ) END;
     IF max_order > 7 THEN Assign( "septillion", order_name[8] ) END;
     IF max_order > 8 THEN Assign( "octillion", order_name[9] ) END;
*)

     Assign( "zero",  digit_name[0] );
     Assign( "one",   digit_name[1] );
     Assign( "two",   digit_name[2] );
     Assign( "three", digit_name[3] );
     Assign( "four",  digit_name[4] );
     Assign( "five",  digit_name[5] );
     Assign( "six",   digit_name[6] );
     Assign( "seven", digit_name[7] );
     Assign( "eight", digit_name[8] );
     Assign( "nine",  digit_name[9] );

     Assign( "ten",       teen_name[0] );
     Assign( "eleven",    teen_name[1] );
     Assign( "twelve",    teen_name[2] );
     Assign( "thirteen",  teen_name[3] );
     Assign( "fourteen",  teen_name[4] );
     Assign( "fifteen",   teen_name[5] );
     Assign( "sixteen",   teen_name[6] );
     Assign( "seventeen", teen_name[7] );
     Assign( "eighteen",  teen_name[8] );
     Assign( "nineteen",  teen_name[9] );

     Assign( "ten",     tens_name[1] );
     Assign( "twenty",  tens_name[2] );
     Assign( "thirty",  tens_name[3] );
     Assign( "fourty",  tens_name[4] );
     Assign( "fifty",   tens_name[5] );
     Assign( "sixty",   tens_name[6] );
     Assign( "seventy", tens_name[7] );
     Assign( "eighty",  tens_name[8] );
     Assign( "ninety",  tens_name[9] );

     (* precompute the order for any length of number *)
     size_to_order[0] := 0;
     FOR i := 1 TO longest DO
        size_to_order[i] := ( i - 1 ) DIV 3;
     END;

     (* and precompute some values of the orders *)
     (* if a nigher level value is needed, it will be computed later *)
     (* only having smal ones here also allows code to be moved *)
     (* from platform to platform with no changes *)
     n_order_values := 1;
     order_values[0] := 1;
     order_values[1] := 1000;

END SayNumbers.
