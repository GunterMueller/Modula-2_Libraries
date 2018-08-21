IMPLEMENTATION MODULE LargeNumbers;

(* based on ExNumLib in "Modula-2 Library Modules: A Programmers Reference"
   by Robert D. Walker, Tab Books Inc, 1988 *)
(* V3.0, J.Andrea, Nov.14/92 - character operations working *)
(* V2.1, J.Andrea, Nov.13/92 - use lookup table for character operations *)
(* V2.0, J.Andrea, Nov.13/92 - store mantissa as characters not integers *)
(* V1.0, J.Andrea, Nov.13/92 - add tests for non-initialized numbers *)
(* V0.0, implemented and some fixes by J.Andrea, Nov.12/92 *)

FROM StringOperations IMPORT Length, Append;
FROM Conversions IMPORT IntToString;

CONST
   exp_max_len = 7;     (* how many digits could an exponent hold *)
   max_real_digits = 7; (* precision of a REAL type floating point number *)
   an_value = 12321;    (* an unlikely value in garbage memory space *)

VAR
  max_digits :INTEGER;
  i          :INTEGER;
  c, d       :CHAR;

 digit_to_int  :ARRAY [0C..11C] OF INTEGER;  (* range chr(0) to chr(9) *)
 int_to_digit  :ARRAY [0..9] OF CHAR;
 char_to_digit :ARRAY ['0'..'9'] OF CHAR;
 digit_to_char :ARRAY [0C..11C] OF CHAR;
 char_to_int   :ARRAY ['0'..'9'] OF INTEGER;
 mul_digits    :ARRAY [0C..11C] OF ARRAY [0C..11C] OF INTEGER;
 
(* ----------------- *)
PROCEDURE IsAN( a :NumType ) :BOOLEAN;
(* is the large number A Number, has it been initialized ? *)
BEGIN
  RETURN a.an = an_value;
END IsAN;

(* ----------------- *)
PROCEDURE xtoi( x :REAL; k :INTEGER ) :REAL;
(* raise a real number to an integer power *)
VAR
  y :REAL;
  i :INTEGER;
BEGIN
  y := 1.0;  (* initialize, and default for k=0 *)
  IF k > 0 THEN
    FOR i := 1 TO k DO
      y := y * x;
    END;
  ELSE
    IF k < 0 THEN
      FOR i := 1 TO k DO
        y := y / x;
      END;
    END;
  END;
  RETURN y;
END xtoi;

(* -------------------------------------------- *)
PROCEDURE SetDigits( d :INTEGER );
BEGIN
  IF d < 2 THEN
    max_digits := 2;
    status := too_few_digits;
  ELSIF d > max_precision THEN
    max_digits := max_precision;
    status := too_many_digits;
  ELSE
    max_digits := d;
    status := ok;
  END;
END SetDigits;

(* -------------------------------------------- *)
PROCEDURE Add( a, b :NumType; VAR c :NumType );
BEGIN
  IF IsAN( a ) & IsAN( b ) THEN
    IF a.sign = b.sign THEN
      AddUtility( a, b, c );
      IF a.sign = negative THEN
        ChangeSign( c );
      END;
    ELSE
      IF a.sign = positive THEN
        SubtractUtility( a, b, c );
      ELSE
        SubtractUtility( b, a, c );
      END;
    END;
  ELSE
    c := zero;
    status := nan;
  END;
END Add;

(* -------------------------------------------- *)
PROCEDURE Subtract( a, b :NumType; VAR c :NumType );
BEGIN
  IF IsAN( a ) & IsAN( b ) THEN
    IF a.sign = b.sign THEN
      IF a.sign = positive THEN
        SubtractUtility( a, b, c );
      ELSE
        SubtractUtility( b, a, c );
      END;
    ELSE
      AddUtility( a, b, c );
      IF a.sign = negative THEN
        ChangeSign( c );
      END;
    END;
  ELSE
    c := zero;
    status := nan;
  END;
END Subtract;

(* -------------------------------------------- *)
PROCEDURE Multiply( a, b :NumType; VAR c :NumType );
VAR
  i, j, product, carry :INTEGER;
  ex1 :NumType;
BEGIN

  IF IsAN( a ) & IsAN( b ) THEN
    IF ( Compare( a, zero ) = 0 ) OR ( Compare( b, zero ) = 0 ) THEN
      (* multiplication by zero *)
      c := zero;
      status := ok;
    ELSIF Compare( a, one ) = 0 THEN
      (* multiplication by one *)
      c := b;
      status := ok;
    ELSIF Compare( b, one ) = 0 THEN
      (* multiplication by one *)
      c := a;
      status := ok;
    ELSE (* regular multiplication *)

      status := ok;
      c := zero;

      FOR i := max_digits - 1 TO 0 BY -1 DO

         ex1 := zero;
         ex1.exp := a.exp + b.exp - i;
         carry := 0;

         FOR j := max_digits - 1 TO 0 BY -1 DO
            product := mul_digits[a.man[j],b.man[i]] + carry;
            ex1.man[j] := int_to_digit[product MOD 10];
            carry := product DIV 10;
         END;

         (* check for a final carry *)
         IF carry > 0 THEN
           ShiftRight( ex1 );
           ex1.man[0] := int_to_digit[carry];
           Times10( ex1 );
         END;

         (* add up what we've got so far *)
         AddUtility( c, ex1, c );

       END;

       (* adjust product sign *)
       IF a.sign # b.sign THEN
         ChangeSign( c );
       END;

      END;
  ELSE
    c := zero;
    status := nan;
  END;

END Multiply;

(* -------------------------------------------- *)
PROCEDURE Divide( a, b :NumType; VAR c :NumType );
VAR
   i, quotient :INTEGER;
   ex1, ex2 :NumType;
BEGIN

  IF IsAN( a ) & IsAN( b ) THEN
    status := ok;

    IF Compare( b, zero ) = 0 THEN
      status := divide_by_zero;
    ELSIF Compare( a, zero ) = 0 THEN
      (* dividend is zero *)
      c := zero;
    ELSIF Compare( b, one ) = 0 THEN
      (* divide by one *)
      c := a;
    ELSE (* regular division *)

      c := zero;
      c.exp := a.exp - b.exp;

      ex1 := a;        (* let ex1 = ABS( a ) / magnitude( a ) *)
      Abs( ex1 );
      ex1.exp := 0;
      ex2 := b;        (* let ex2 = ABS( b ) / magnitude( b ) *)
      Abs( ex2 );
      ex2.exp := 0;

      FOR i := 0 TO max_digits-1 DO
         quotient := 0;
         WHILE Compare( ex1, ex2 ) = 1 DO     (* while ex1 > ex2 *)
           quotient := quotient + 1;
           SubtractUtility( ex1, ex2, ex1 );
         END;
         c.man[i] := int_to_digit[quotient];
         Divide10( ex2 );
      END;

      (* normalize quotient *)
      Norm( c );

      (* adjust quotient sign *)
      IF a.sign # b.sign THEN
        ChangeSign( c );
      END;

    END;

  ELSE
    c := zero;
    status := nan;
  END;

END Divide;

(* -------------------------------------------- *)
PROCEDURE Abs( VAR a :NumType );
BEGIN
  a.sign := positive;
END Abs;

(* -------------------------------------------- *)
PROCEDURE ChangeSign( VAR a :NumType );
BEGIN
  IF a.sign = positive THEN
    a.sign := negative;
  ELSE
    a.sign := positive;
  END;
END ChangeSign;

(* -------------------------------------------- *)
PROCEDURE Times10( VAR a :NumType );
BEGIN
  IF IsAN( a ) THEN
    a.exp := a.exp + 1;
    IF a.exp > max_exp THEN
      status := overflow;
    ELSE
      status := ok;
    END;
  ELSE
    a := zero;
    status := nan;
  END;
END Times10;

(* -------------------------------------------- *)
PROCEDURE Divide10( VAR a :NumType );
BEGIN
  IF IsAN( a ) THEN
    a.exp := a.exp - 1;
    IF a.exp < min_exp THEN
      status := underflow;
    ELSE
      status := ok;
    END;
  ELSE
    a := zero;
    status := nan;
  END;
END Divide10;

(* -------------------------------------------- *)
PROCEDURE Compare( a, b :NumType ) :INTEGER;
VAR
  done :BOOLEAN;
  i    :INTEGER;
BEGIN
  IF IsAN( a ) & IsAN( b ) THEN
    IF a.sign # b.sign THEN
      IF a.sign = positive THEN
        RETURN 1;      (* a is +, b is -, so a > b *)
      ELSE
        RETURN -1;     (* a is -, b is +, so a < b *)
      END;
    ELSE

      (* a and b have the same sign *)

      IF a.exp # b.exp THEN

        IF a.exp > b.exp THEN
          IF a.sign = positive THEN
            RETURN 1; (* a > b *)
          ELSE
            RETURN -1; (* a < b *)
          END;
        ELSE

          (* a and b have same sign, and a exponent <= b exponent *)
          IF a.sign = positive THEN
            RETURN -1;
          ELSE
            RETURN 1;
          END;

        END;

      ELSE

        (* a and b have the same sign, and the same exponent *)

        done := FALSE;
        i    := 0;

        (* compare each digit till a difference is found, or the end *)
        WHILE ( i <= max_digits - 1 ) & NOT done DO
          IF a.man[i] # b.man[i] THEN
            done := TRUE;
          ELSE
            i := i + 1;
          END;
        END;

        IF i > max_digits - 1 THEN
          (* end reached, so all digits match *)
          RETURN 0;
        ELSE

          (* compare those last digits which are different *)
          IF a.man[i] < b.man[i] THEN
            IF a.sign = positive THEN
              RETURN -1; (* a<b *)
            ELSE
              RETURN 1; (* a>b *)
            END;
          ELSE
            IF a.sign = positive THEN
              RETURN 1; (* a>b *)
            ELSE
              RETURN -1;
            END;
          END;
        END;
      END;

    END;
  ELSE
    RETURN -1; (* this is questionable *)
  END;
  
END Compare;

(* -------------------------------------------- *)
PROCEDURE Norm( VAR a :NumType );
VAR
  value_zero :BOOLEAN;
  i          :INTEGER;
BEGIN
  IF IsAN( a ) THEN
    (* check for a zero *)
    i := 0;
    value_zero := TRUE;
    WHILE ( i <= max_digits - 1 ) & value_zero DO
      IF a.man[i] # int_to_digit[0] THEN
        value_zero := FALSE;
      END;
      i := i + 1;
    END;

    IF value_zero THEN
      (* normalize zero *)
      a.sign := positive;
      a.exp  := 0;
    ELSE
      (* shift mantissa to left until most signifigant digit is non-zero,
         increment exponent with each shift *)
      WHILE a.man[0] = int_to_digit[0] DO
        ShiftLeft( a );
        Divide10( a )
      END;
    END;
  ELSE
    a := zero;
    status := nan;
  END;
END Norm;

(* -------------------------------------------- *)
PROCEDURE StringToNum( s :ARRAY OF CHAR; VAR a :NumType );
VAR
  done, neg_exp :BOOLEAN;
  i, len        :CARDINAL;
  j, exponent   :INTEGER;
  second_digit  :BOOLEAN;

  (* ---------------- *)
  PROCEDURE Numeral( c :CHAR ) :BOOLEAN;
  (* is the character a number *)
  BEGIN
     RETURN ( c >= '0' ) & ( c <= '9' );
  END Numeral;

BEGIN
  status := ok;
  done   := TRUE;

  a := zero;

  exponent := 0;
  neg_exp  := FALSE;

  len := Length( s );

  IF len > 0 THEN
    i := 0;
    (* chek for a negative mantissa *)
    IF s[i] = '-' THEN
      a.sign := negative;
      i := i + 1;
    END;

    (* skip leading zeros *)
    WHILE ( i < len ) & ( s[i] = '0' ) DO
      i := i + 1;
    END;

    IF i < len THEN
      (* more than just zeros in this string *)

      j := 0;

      second_digit := FALSE;
      
      (* handle digits to the left of the decimal point *)
      WHILE ( i < len ) & done DO
        IF Numeral( s[i] ) THEN
          a.man[j] := char_to_digit[s[i]];
          IF second_digit THEN
            Times10( a );
          ELSE
            second_digit := TRUE;
          END;
          j := j + 1;
          i := i + 1;
        ELSE
          done := FALSE;
        END;
      END;      

      IF NOT done THEN
        (* non-numeric was found, could it be something that we want *)
        IF ( s[i] = '.' ) OR ( CAP(s[i]) = 'E' ) THEN

          IF s[i] = '.' THEN
            i := i + 1;
            done := TRUE;
            (* get all the digits to the right of the decimal point *)
            WHILE ( i < len ) & done DO
              IF Numeral( s[i] ) THEN
                a.man[j] := char_to_digit[s[i]];
                j := j + 1;
                i := i + 1;
              ELSE
                done := FALSE;
              END;
            END;
          END;

          IF NOT done THEN
            IF CAP( s[i] ) = 'E' THEN
              i := i + 1;
              neg_exp := s[i] = '-';
              IF ( s[i] = '+' ) OR neg_exp THEN
                i := i + 1; (* skip the sign *)
              END;
              (* handle the exponent *)
              exponent := 0;
              done := TRUE;
              WHILE ( i < len ) & done DO
                IF Numeral( s[i] ) THEN
                  exponent := exponent * 10 + char_to_int[s[i]];
                ELSE
                  done := FALSE;
                END;
              END;

              IF NOT done THEN
                status := invalid_digit;
              ELSE
                IF neg_exp THEN
                  a.exp := a.exp - exponent;
                ELSE
                  a.exp := a.exp + exponent;
                END;
              END;

            ELSE
              status := invalid_digit;
            END;
          END;

        ELSE
          status := invalid_digit;
        END;
        
      END;

    END;
  END;

END StringToNum;

(* -------------------------------------------- *)
PROCEDURE NumToString( a :NumType; VAR s :ARRAY OF CHAR );
VAR
  i, last :INTEGER;
  s_exp :ARRAY [0..exp_max_len] OF CHAR;

BEGIN
  (* create a null string *)
  s[0] := 0C;

  IF IsAN( a ) THEN

    (* the sign *)
    IF a.sign = negative THEN
      Append( '-', s );
    END;

    Append( digit_to_char[a.man[0]], s );

    Append( '.', s );

    (* find the last non-zero digit *)
    last := max_digits - 1;
    WHILE ( last >= 1 ) & ( a.man[last] = 0C ) DO
      last := last - 1;
    END;

    FOR i := 1 TO last DO
      Append( digit_to_char[a.man[i]], s );
    END;

    (* the exponent *)
    Append( 'E', s );

    IntToString( a.exp, 0, s_exp );

    Append( s_exp, s );

  ELSE
    Append( '0', s );
    status := nan;
  END;

END NumToString;

(* -------------------------------------------- *)
PROCEDURE AddUtility( a, b :NumType; VAR c :NumType );
VAR
  i, j, carry, digit, result :INTEGER;
  ex1, ex2 :NumType;
BEGIN
  IF a.exp > b.exp THEN
    ex1 := a;
    ex2 := b;
  ELSE
    ex1 := b;
    ex2 := a;
  END;

  c     := zero;
  c.exp := ex1.exp;

  carry := 0;
  FOR i := max_digits-1 TO 0 BY -1 DO

    j := i + ex2.exp - ex1.exp;        (* j = index to ex2 *)
    (* check that j falls within bounds *)
    IF ( j >= 0 ) & ( j <= max_digits -1 ) THEN
      digit := digit_to_int[ex2.man[j]]; (* get digit from ex2 *)
    ELSE
      digit := 0; (* j is outside, use zero *);
    END;

    (* perform addition with carry *)
    result := digit_to_int[ex1.man[i]] + digit + carry;
    (* check for next carry *)
    IF result >= 10 THEN
      result := result MOD 10;
      carry := 1;
    ELSE
      carry := 0;
    END;
    c.man[i] := int_to_digit[result];

  END;

  (* handle final carry *)
  IF carry > 0 THEN
    ShiftRight( c );        (* shift mantissa to right *)
    c.man[0] := 1C;         (* make first digit = 1 *)
    Times10( c );           (* update exponent *)
  END;

  (* set status *)
  IF c.exp > max_exp THEN
    status := overflow;
  ELSE
    status := ok;
  END;

END AddUtility;

(* -------------------------------------------- *)
PROCEDURE SubtractUtility( a, b:NumType; VAR c :NumType );
VAR
  i, j, borrow, digit, result :INTEGER;
  ex1, ex2  :NumType;
  positive_result :BOOLEAN;
BEGIN
  Abs( a );  Abs( b );
  IF a.exp > b.exp THEN
    ex1 := a;
    ex2 := b;
  ELSE
    ex1 := b;
    ex2 := a;
  END;

  positive_result := Compare( ex1, ex2 ) = 1;

  c := zero;
  c.exp := ex1.exp;

  borrow := 0;

  FOR i := max_digits-1 TO 0 BY -1 DO

     j := i + ex2.exp - ex1.exp;      (* j is index into ex2 *)
     (* check that j falls within bounds *)
     IF ( j >= 0 ) & ( j <= max_digits -1 ) THEN
       digit := digit_to_int[ex2.man[j]]; (* get digit from ex2 *)
     ELSE
       digit := 0; (* j outside array, user zero *)
     END;

     (* perform subtraction with borrowing *)

     result := digit_to_int[ex1.man[i]];
     IF positive_result THEN
       result := result - digit;
     ELSE
       result := digit - result;
     END;
     result := result - borrow;

     (* check for borrow *)
     IF result < 0 THEN
       result := result + 10;
       borrow := 1;
     ELSE
       borrow := 0;
     END;
     c.man[i] := int_to_digit[result];

   END;

   (* normalize *)
   Norm( c );
   
   (* adjust sign *)
   IF Compare( a, b ) = -1 THEN
     ChangeSign( c );
   END;

END SubtractUtility;

(* -------------------------------------------- *)
PROCEDURE ShiftRight( VAR a :NumType );
VAR
  i :INTEGER;
BEGIN
  FOR i := max_digits TO 1 BY -1 DO
    a.man[i] := a.man[i-1];
  END;
  a.man[0] := int_to_digit[0];   (* put zero in most signifigant position *)
END ShiftRight;

(* -------------------------------------------- *)
PROCEDURE ShiftLeft( VAR a :NumType );
VAR
  i :INTEGER;
BEGIN
  FOR i := -1 TO max_digits - 2 DO
    a.man[i] := a.man[i+1];
  END;
  a.man[max_digits-1] := int_to_digit[0];   (* put zero in least signifigant position *)
END ShiftLeft;

BEGIN

  (* precompute conversions *)
  FOR i := 0 TO 9 DO
    digit_to_int[ CHR(i) ] := i;
    int_to_digit[ i ]      := CHR(i);
  END;
  i := 0;
  FOR c := '0' TO '9' DO
    char_to_digit[ c ] := CHR( i );
    digit_to_char[ CHR(i) ] := c;
    char_to_int[ c ] := i;
    i := i + 1;
  END;

  (* precompute tables of adding and multiplying together character digits *)
  FOR c := 0C TO 11C DO
    FOR d := 0C TO 11C DO
       mul_digits[c,d] := digit_to_int[c] * digit_to_int[d];
    END;
  END;

  (* set default for number size *)
  SetDigits( max_precision );

  zero.sign := positive;
  zero.exp  := 0;
  FOR i := -1 TO max_precision DO
    zero.man[i] := int_to_digit[0];
  END;
  zero.an := an_value;

  one := zero;
  one.man[0] := int_to_digit[1];
  
END LargeNumbers.
