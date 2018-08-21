IMPLEMENTATION MODULE LewertsScale;

(* Modula-2 implementation by John Andrea, Oct.8/92 *)

FROM MathLib0 IMPORT sqrt, power;
FROM MoreMath IMPORT Log10;

VAR
  n_set    :CARDINAL;
  nice_set :ARRAY [1..4] OF REAL;
 
      (* ---------------------------------------------------------- *)
       PROCEDURE Scale( data_min, data_max :REAL; approx_inc :CARDINAL;
                       VAR scale_min, scale_max, scale_inc :REAL );
      (* Implementation notes:
       * 1) should n_set be large enough to include the 10 or not.
       *    The book is confusing as it is given in C and there is some
       *    comment about including the 10 for a special purpose.
       * 2) there seems to be a problem with the given algorithm in
       *    computing the scale-max, so do a hack here to fix it
       * 3) there seems to be a problem with the given algorithm in
       *    computing the scale-min, so do a hack here to fix it
      *)

      VAR
        actual_inc :INTEGER;

        interval_size :REAL;

        set_index :CARDINAL;
        power_of_ten :REAL;

        nice_num, geo_mean :REAL;

        n_low_mul, n_hi_mul  :INTEGER;

      BEGIN

        IF approx_inc < 2 THEN
          actual_inc := 2;
        ELSE
          actual_inc := INTEGER( approx_inc );
        END;

        IF data_min > data_max THEN
          nice_num := data_min;
          data_min := data_max;
          data_max := nice_num;
        ELSE    
        
          (* compute the smallest potential interval *)
          interval_size := ( data_max - data_min ) / FLOAT( actual_inc );

          (* compute the nice number closest to the approximate potential interval *)
          FirstNiceNum( interval_size, set_index, power_of_ten, nice_num );
 
          geo_mean := sqrt( nice_set[set_index] * nice_set[set_index+1] );

          WHILE geo_mean * power_of_ten < interval_size DO

             NextNiceNum( set_index, power_of_ten, nice_num );

             geo_mean := sqrt( nice_set[set_index] * nice_set[set_index+1] );
          END;

          (* produce the scale using the computed nice number *)
          CalcExtLabel( data_min, data_max, nice_num, n_low_mul, n_hi_mul );

          (* and the scale limits *)
          scale_min  := FLOAT( n_low_mul ) * nice_num;
          scale_max  := FLOAT( n_hi_mul  ) * nice_num;
          actual_inc := n_hi_mul - n_low_mul;

          (* heres the hack to fix the scale_max *)
          scale_inc := ( scale_max - scale_min ) / FLOAT( actual_inc );

          WHILE scale_max < data_max DO
             scale_max := scale_max + nice_num;
          END;

          (* and fix scale_min too *)
          WHILE scale_min > data_min DO
             scale_min := scale_min - nice_num;
          END;

      END;
   END Scale;

       (* ---------------------------------------------------------- *)
      PROCEDURE CalcExtLabel( data_min, data_max, nice_num :REAL;
                              VAR n_low, n_high :INTEGER);
      (* calculate an externally labeled scale *)
      VAR
        check :REAL;
      BEGIN
      
        (* calculate the low multiple *)
        n_low := TRUNC( data_min / nice_num );

        (* double check it *)
        (* make sure that we've got the largest possible minimum *)
        check := FLOAT( n_low + 1 ) * nice_num;
        IF check <= data_min THEN
          n_low := n_low + 1;
        END;

        (* calculate the high multiple *)
        n_high := TRUNC( ( data_max / nice_num ) + 0.5 );

        (* double check it *)
        (* make sure that we've got the smallest possible maximum *)
        check := FLOAT( n_high - 1 ) * nice_num;
        IF check >= data_max THEN
          n_high := n_high - 1;
        END;

    END CalcExtLabel;

       (* ---------------------------------------------------------- *)
    PROCEDURE FirstNiceNum( interval_size :REAL; 
                            VAR set_index :CARDINAL;
                            VAR power_of_ten, nice_num :REAL );

    (* calculate an initial value for the nice-number *)
    (* Implementation notes: *)
    (*   1) can a real be raised to a negative integer power ? *)

    VAR
      exponent :INTEGER;
       
    BEGIN
    
      (* calculate an initial power of ten *)
      exponent := TRUNC( Log10( interval_size ) );
      power_of_ten := power( 10.0, FLOAT( exponent ) );

      (* double check it *)
      IF power_of_ten * 10.0 <= interval_size THEN
        power_of_ten := power_of_ten * 10.0;
      END;

      (* and the index into the set of number is initialy the first number *)
      set_index := 1;

      (* and return the first nice-number *)
      nice_num := power_of_ten;

    END FirstNiceNum;

      (* ---------------------------------------------------------- *)
      PROCEDURE NextNiceNum( VAR set_index :CARDINAL;
                             VAR power_of_ten, nice_num :REAL );

      (* calculate the next value for the nice-number *)
      (* Implementation notes: *)
      (*  1) should the check for the exceeded set size *)
      (*     be ".ge." as it is in the book, or ".gt." *)
      (*     since FORTRAN starts indicies at one rather *)
      (*     than starting at zero as in the book. *)

      BEGIN
      
        (* increment the index *)
        set_index := set_index + 1;

        (* if the maximum has been passed, then reset index to first *)
        (* position, and increase the power of ten *)

        IF set_index > n_set THEN
          set_index    := 1;
          power_of_ten := power_of_ten * 10.0;
        END;

        (* and return the next nice number *)

        nice_num := nice_set[set_index] * power_of_ten;

      END NextNiceNum;

BEGIN

 n_set := 3; (* actual length less one *)

 nice_set[1] := 1.0;  nice_set[2] := 2.0;
 nice_set[3] := 5.0;  nice_set[4] := 10.0;

END LewertsScale.
