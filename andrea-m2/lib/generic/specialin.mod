IMPLEMENTATION MODULE SpecialInOut;

(* procedures to supplement those in InOut and RealInOut *)
(*
  as defined in "Programming and Problem Solving in Modula-2"
  by S. Leestma and L. Nyhoff, MacMillian Publishing Company, 1989
*)
(* some modifications by J. Andrea, Aug.28/91 *)
(* the FWriteReal procedure isn't very good *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT Read, ReadString, Write, WriteString, WriteLn, WriteCard, EOL;
FROM RealInOut IMPORT WriteReal;

CONST
   nul = 0C;

VAR
   digits :ARRAY [0..9] OF CHAR;

(* ------------------------------------------------------------------ *)
PROCEDURE FWriteReal( real_number :REAL;
                      field_width, decimal_places :CARDINAL );
CONST
   significant_digits = 10;

VAR
   r_num, rounding_increment  :REAL;
   left_digits, digit         :CARDINAL;
   num_positions, sign, i     :CARDINAL;

BEGIN

   IF real_number >= 0.0 THEN
     sign := 0;
     r_num := real_number;
   ELSE
     sign := 1;
     r_num := - real_number;
   END;

   (* round the value to 'decimal_places' *)
   rounding_increment := 0.5;
   FOR i := 1 TO decimal_places DO
      rounding_increment := rounding_increment / 10.0;
   END;
   r_num := r_num + rounding_increment;

   (* normalize the number *)
   left_digits := 1;
   WHILE r_num >= 10.0 DO
     left_digits := left_digits + 1;
     r_num       := r_num / 10.0;
   END;

   (* if too many digits, display a message and output the number *)
   num_positions := left_digits + decimal_places;
   IF num_positions > significant_digits THEN
     WriteString( '*** Too many places specified for ' );
     WriteReal( real_number, significant_digits );
   ELSE

     (* print leading blanks so value is right justified *)
     num_positions := num_positions + sign + 1;
     FOR i := num_positions + 1 TO field_width DO
        WriteString( ' ' );
     END;

     IF sign = 1 THEN
       Write( '-' );
     END;

     FOR i := 1 TO left_digits DO
        digit := TRUNC( r_num );
        Write( digits[digit] );
        r_num := 10.0 * ( r_num - FLOAT(digit) );
     END;

     (* decimal point *)

     Write( '.' );

     (* digits to the right of the decimal point *)
     FOR i := 1 TO decimal_places DO
        digit := TRUNC( r_num );
        Write( digits[digit] );
        r_num := 10.0 * ( r_num - FLOAT(digit) );
     END;

   END;
END FWriteReal;


(* ------------------------------------------------------------------ *)
PROCEDURE ReadBoole( VAR boolean_value :BOOLEAN );

VAR
  string :ARRAY [0..5] OF CHAR;
  i      :CARDINAL;

BEGIN

  ReadString( string );

  FOR i := 0 TO 4 DO
     string[i] := CAP( string[i] );
  END;

  IF ( string[0] = 'T' ) & ( string[1] = 'R' ) &
     ( string[2] = 'U' ) & ( string[3] = 'E' ) &
     ( string[4] = nul ) THEN
     boolean_value := TRUE;
  ELSE
     IF ( string[0] = 'F' ) & ( string[1] = 'A' ) &
        ( string[2] = 'L' ) & ( string[3] = 'S' ) &
        ( string[4] = 'E' ) & ( string[5] = nul ) THEN
        boolean_value := FALSE;
     ELSE
       WriteString( "***Bad Input -- not 'TRUE' or 'FALSE' ***" ); WriteLn;
     END;
  END;

END ReadBoole;

(* ------------------------------------------------------------------ *)
PROCEDURE WriteBoole( boolean_value :BOOLEAN );
BEGIN
  IF boolean_value THEN
    WriteString( 'TRUE ' );
  ELSE
    WriteString( 'FALSE ' );
  END;
END WriteBoole;

(* ------------------------------------------------------------------ *)
PROCEDURE ReadAString( VAR string :ARRAY OF CHAR );
VAR
  max_index, i :CARDINAL;
  ch           :CHAR;
BEGIN

  max_index := HIGH( string );

  i := 0;
  Read( ch );
  WHILE ch # EOL DO
    IF i <= max_index THEN
      string[i] := ch;
      i := i + 1;
    END;
    Read( ch );
  END;

  IF i < max_index THEN string[i] := nul; END;

END ReadAString;


BEGIN

  digits[0] := '0';   digits[1] := '1';   digits[2] := '2';
  digits[3] := '3';   digits[4] := '4';   digits[5] := '5';
  digits[6] := '6';   digits[7] := '7';   digits[8] := '8';
  digits[9] := '9';

END SpecialInOut.
