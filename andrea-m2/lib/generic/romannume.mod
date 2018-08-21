IMPLEMENTATION MODULE RomanNumerals;
(* convert to/from strings of Roman Numerals to cardinal values *)

(* J. Andrea, Jun.4/92 - more compact numbers, and more correct *)
(* J. Andrea, Nov.9/91 - better ToRoman *)
(* J. Andrea, Sept.6/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM StringOperations IMPORT Index, Length;

CONST
   nul = 0C;

VAR
   Numerals :ARRAY [1..7] OF CHAR;
   Values   :ARRAY [1..7] OF CARDINAL;
   i, k     :CARDINAL;
   ok       :BOOLEAN;
   len, max_len :CARDINAL;

   (* -------------------------------------------------------------- *)
   PROCEDURE FromRoman( roman :ARRAY OF CHAR; VAR value :CARDINAL );

   CONST
     max_groups = 10;   (* this may not be a valid assumption *)

   VAR
     c, prev :CHAR;
     group   :ARRAY [1..max_groups] OF CARDINAL;
     n_group :CARDINAL;
     prev_group, curr_group :CARDINAL;

   BEGIN

     len := Length( roman );

     IF len = 0 THEN
       value := 0;
     ELSE

       (* in this first section, break the roman numeral into groups *)
       (* of same values *)

       n_group := 0;   prev := nul;

       i := 0;  ok := TRUE;
       WHILE ok & ( i < len ) DO

         c := CAP( roman[i] );

         k := Index( Numerals, c );

         IF k = 0 THEN
           ok := FALSE;
         ELSE

           IF c = prev THEN

             (* same group *)
             group[n_group]  := group[n_group] + Values[k];

           ELSE

             (* new group *)
             n_group := n_group + 1;
             IF n_group > max_groups THEN
               (* algorithm error *)
               ok := FALSE;
             ELSE

               group[n_group]  := Values[k];
               prev            := c;

             END;

           END;

           i := i + 1;
         END;
       END;

       value := 0;

       IF ok THEN

         IF n_group = 1 THEN

           value := group[1];

         ELSE

           (* force the last group to be zero *)
           n_group := n_group + 1;
           group[n_group] := 0;

           (* now add of subtract the groups to form a number *)

           prev_group := group[1];
           i := 2;

           WHILE i <= n_group DO
              curr_group := group[i];

              IF prev_group < curr_group THEN
                prev_group := curr_group - prev_group;
              ELSE
                value      := value + prev_group;
                prev_group := curr_group;
              END;

              i := i + 1;
           END;

         END;

       END;

     END;

   END FromRoman;


   (* -------------------------------------------------------------- *)
   PROCEDURE ToRoman( value :CARDINAL; VAR roman :ARRAY OF CHAR );

   VAR
     s, subtract :CARDINAL;

     (* -------------------------------------------------------------- *)
     PROCEDURE AddChar( c :CHAR );
     BEGIN
       IF len <= max_len THEN
         roman[len] := c; len := len + 1;
       ELSE
         ok := FALSE;
       END;
     END AddChar;
     PROCEDURE InitChar;
     BEGIN
       len     := 0;
       max_len := HIGH( roman );
       ok      := TRUE;
     END InitChar;
     PROCEDURE EndChar;
     BEGIN
       IF len <= max_len THEN  roman[len] := nul; END;
     END EndChar;
   
   BEGIN

     InitChar;

     IF value > 0 THEN

       (* remove any large numbers first *)
       k := 7;
       WHILE ok & ( value >= Values[k] ) DO
         AddChar( Numerals[k] );
         value := value - Values[k];
       END;
         
       (* determine the compound numeral string that represents the value *)

       k := 7;
       WHILE ok & ( value > 0 ) DO

         (* special case for anything less than five *)
                
         IF value < 5 THEN
           IF value = 4 THEN
             AddChar( 'I' ); AddChar( 'V' );
           ELSE
             FOR i := 1 TO value DO
                AddChar( 'I' );
             END;
           END;
           value := 0;
         ELSE
         
           (* find numeral which is less than or equal to the current value *)
           WHILE Values[k] > value DO
             k := k - 1;
           END;
           
           IF value = Values[k] THEN
             (* its an exact match *)
             AddChar( Numerals[k] );
             value := 0;
           ELSE
           
             (* see if this next value can be made up of a roman subtraction *)
             (* keeping in mind that only 1, 10, 100, ... can be subtracted  *)
             (*  from anything, which are the numerals with odd indicies *)
             (* and you can only subtract one level away, eg. not 1 from 100 *)
             
             i := k + 1;  (* the next larger numeral *)
             IF ODD( i ) THEN
               s := i - 2;
             ELSE
               s := i - 1;
             END;
             
             subtract := Values[i] - Values[s];

             IF subtract > value THEN
               (* not a numeral subtract (IX, IV, etc.), so just do a subtract *)
               value := value - Values[k];
               AddChar( Numerals[k] );
             ELSE
               (* it is a subtract, do it *)
               value := value - subtract;
               AddChar( Numerals[s] ); AddChar( Numerals[i] );
             END;
           END;
         END;
       END; (* while *)
         
     END;

     IF ok THEN
       EndChar;
     ELSE
       (* something wrong, return an empty string *)
       roman[0] := nul;
     END;

   END ToRoman;

BEGIN

  Numerals[1] := 'I';    Values[1] := 1;
  Numerals[2] := 'V';    Values[2] := 5;
  Numerals[3] := 'X';    Values[3] := 10;
  Numerals[4] := 'L';    Values[4] := 50;
  Numerals[5] := 'C';    Values[5] := 100;
  Numerals[6] := 'D';    Values[6] := 500;
  Numerals[7] := 'M';    Values[7] := 1000;

END RomanNumerals.
