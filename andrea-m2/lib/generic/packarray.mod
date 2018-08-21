IMPLEMENTATION MODULE PackArray;

(* Array of small cardinal values packed into bit items,
   see the corresponding definition *)
(* V1.1, J. Andrea, Jun.22/93 -add Duplicate *)
(* V1.0, J. Andrea, Jun.15/93 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM SYSTEM  IMPORT ADDRESS, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

TYPE
  ImplementationType = BITSET;
  DataType           = CARDINAL;

  Array = POINTER TO RECORD
             length   :CARDINAL;   (* dimensions of the array *)
             start    :ADDRESS;    (* memory address of the first item *)
             min, max :DataType;   (* range of data items *)
             bytes    :CARDINAL;   (* number of bytes in whole array *)
             bits     :CARDINAL;   (* how many bits used to implement values *)
             aligned  :BOOLEAN;    (* bits fit exactly in a word *)
           END;

VAR
  bytes_per_word :CARDINAL;               (* #bytes in a single item *)
  bits_per_word  :CARDINAL;               (* # bits *)
  adra, adrb     :POINTER TO ImplementationType;  (* generic pointers *)

(* -------------------------------------------------------- *)
PROCEDURE InRange( a :Array; element :CARDINAL ) :BOOLEAN;
(* is the specified row/col item in this array ? *)
BEGIN
  RETURN ( element >= 1 ) & ( element <= a^.length );
END InRange;

(* -------------------------------------------------------- *)
PROCEDURE Offsets( a :Array; element :CARDINAL;
                   VAR word, word_bit,
                       bits_in_first, bits_in_second :CARDINAL );
(* The element should be known to be valid at this point. *)
(* Calculate the memory offset to the element item from the first item *)

VAR
  element_first, element_last, word_first, word_last :CARDINAL;
BEGIN

       (* In absolute counts, find the end points of this element *)
       (* These are not zero based *)

  element_last  := a^.bits * element;
  element_first := element_last - a^.bits + 1;

       (* In which word does the current element begin *)
       (* This one is zero based, so the "-1" is needed *)

  word := ( element_first - 1 ) DIV bits_per_word;

       (* In absolute counts, where does the current word begin *)
       (* This is not zero based *)

  word_first := 1 + word * bits_per_word;

       (* The current element's first bit begins somewhere in the word *)
       (* This is also zero based, so no +1 *)

  word_bit := element_first - word_first;

  IF a^.aligned THEN
    bits_in_first  := a^.bits;
    bits_in_second := 0;
  ELSE

    (* Otherwise figure out how much of this element is spread across the
       two contiguous words, if at all *)

       (* In absolute counts, the last bit in the word is *)

    word_last := word_first + bits_per_word - 1;

    IF element_last > word_last THEN

      (* Find the distance from the beginning of the element to the end
         of the word *)

      bits_in_first  := word_last - element_first + 1;
      bits_in_second := a^.bits - bits_in_first;

    ELSE
      bits_in_first  := a^.bits;
      bits_in_second := 0;
    END;
  END;
END Offsets;

(* -------------------------------------------------------- *)
PROCEDURE Build( VAR a :Array; minimum, maximum, n_elements :CARDINAL );
VAR
  total_bits, total_words :CARDINAL;
  i, k, n :CARDINAL;
  temp    :DataType;
BEGIN
  NEW( a );

  IF n_elements = 0 THEN n_elements := 1 END;

  IF minimum > maximum THEN
    temp    := maximum;
    maximum := minimum;
    minimum := temp;
  END;

  a^.min := minimum;
  a^.max := maximum;

  a^.length := n_elements;

  (* here is the range *)
  n := maximum - minimum + 1;

  (* compute the number of bits needed to implement a number of that size *)
  i := 0;
  k := 1;
  WHILE k < n DO
    i := i + 1;
    k := k * 2;
  END;

  a^.bits := i;
  
  a^.aligned := bits_per_word MOD a^.bits = 0;
  
  (* so the total number of bits for the whole array *)
  total_bits := a^.bits * n_elements;

  (* round total number of bits up to fill a whole word *)
  k := total_bits MOD bits_per_word;
  IF k # 0 THEN
    total_bits := total_bits + bits_per_word - k;
  END;

  (* number of words to use *)
  total_words := total_bits DIV bits_per_word;

  (* and how many bytes in total *)
  a^.bytes := total_words * bytes_per_word;

  ALLOCATE( a^.start, a^.bytes );

  (* zero out that piece of memory *)
  k := 0;
  FOR i := 1 TO total_words DO
     adra  := a^.start + k;
     adra^ := {};
     k     := k + bytes_per_word;
  END;

END Build;

(* -------------------------------------------------------- *)
PROCEDURE Destroy( VAR a :Array );
BEGIN
  DEALLOCATE( a^.start, a^.bytes );
  DISPOSE( a );
END Destroy;

(* -------------------------------------------------------- *)
PROCEDURE Put( a :Array; element :CARDINAL; x :DataType );
VAR
  result                :ImplementationType;
  which_word            :CARDINAL;
  element_bit, word_bit :CARDINAL;
  first_set, second_set :CARDINAL;
  location              :CARDINAL;
    
  (* ------------------------------ *)
  PROCEDURE Bits( n :CARDINAL );
  VAR
    i :CARDINAL;
  BEGIN
    FOR i := 1 TO n DO
       IF element_bit IN result THEN
         INCL( adra^, word_bit );
       ELSE
         EXCL( adra^, word_bit );
       END;
       INC( word_bit );
       INC( element_bit );
    END;
   END Bits;
   
BEGIN
  IF InRange( a, element ) THEN

    IF x < a^.min THEN
      x := a^.min;
    ELSIF x > a^.max THEN
      x := a^.max;
    END;
    result := BITSET( x - a^.min );

    Offsets( a, element, which_word, word_bit, first_set, second_set );
    location := which_word * bytes_per_word;
    adra := a^.start + location;

    element_bit := 0;
    Bits( first_set );

    IF second_set # 0 THEN
      (* move to the second word and get the rest of the bits from it *)
      adra     := a^.start + ( location + bytes_per_word );
      word_bit := 0;
      Bits( second_set );
   END;

  END;
END Put;

(* -------------------------------------------------------- *)
PROCEDURE Get( a :Array; element :CARDINAL ) :DataType;
VAR
  result                :ImplementationType;
  which_word            :CARDINAL;
  element_bit, word_bit :CARDINAL;
  first_set, second_set :CARDINAL;
  location              :CARDINAL;
  
  (* ----------------------- *)
  PROCEDURE Bits( n :CARDINAL );
  VAR
    i :CARDINAL;
  BEGIN
    FOR i := 1 TO n DO
      IF word_bit IN adra^ THEN
        INCL( result, element_bit );
      END;
      INC( element_bit );
      INC( word_bit );
    END; 
  END Bits;
  
BEGIN

  IF NOT InRange( a, element ) THEN
    RETURN 0;
  ELSE

    Offsets( a, element, which_word, word_bit, first_set, second_set );
    location := which_word * bytes_per_word;
    adra     := a^.start + location;

    result := {};

    element_bit := 0;
    Bits( first_set );

    IF second_set # 0 THEN
      (* move to the second word and get the rest of the bits from it *)
      adra := a^.start + ( location + bytes_per_word );
      word_bit := 0;
      Bits( second_set );
    END;

    RETURN CARDINAL( result ) + a^.min;
  END;

END Get;

(* -------------------------------------------------------- *)
PROCEDURE Size( a :Array; VAR bits, bytes, elements :CARDINAL );
BEGIN
  bytes    := a^.bytes;
  bits     := a^.bits;
  elements := a^.length;
END Size;

(* -------------------------------------------------------- *)
PROCEDURE Min( a :Array ) :DataType;
VAR
  i         :CARDINAL;
  x, result :DataType;
BEGIN

  result := Get( a, 1 );

  (* Since the absolute minimum is predefined then might as well stop
     if that minimum is reached *)

  i := 2;
  WHILE ( i <= a^.length ) & ( result > a^.min ) DO
    x := Get( a, i );
    IF x < result THEN result := x END;
    i := i + 1;
  END;

  RETURN result;
END Min;

(* -------------------------------------------------------- *)
PROCEDURE Max( a :Array ) :DataType;
VAR
  i         :CARDINAL;
  x, result :DataType;
BEGIN

  result := Get( a, 1 );

  (* Since the absolute maximum is predefined then might as well stop
     if that minimum is reached *)

  i := 2;
  WHILE ( i <= a^.length ) & ( result < a^.min ) DO
    x := Get( a, i );
    IF x > result THEN result := x END;
    i := i + 1;
  END;

  RETURN result;
END Max;

(* -------------------------------------------------------- *)
PROCEDURE Compare( a, b :Array ) :BOOLEAN;
VAR
  same :BOOLEAN;
  i, k :CARDINAL;
  total_words :CARDINAL;
BEGIN

  IF a^.length # b^.length THEN
    same := FALSE;
  ELSE

    same := TRUE;

    total_words := a^.bytes DIV bytes_per_word;
    i := 1;
    k := 0;
    WHILE same & ( i <= total_words ) DO
      adra := a^.start + k;
      adrb := b^.start + k;
      same := adra^ = adrb^;
      i    := i + 1;
      k    := k + bytes_per_word;
    END;

  END;

  RETURN same;

END Compare;

(* -------------------------------------------------------- *)
PROCEDURE Assign( a :Array; x :DataType );
VAR
  i :CARDINAL;
BEGIN
  FOR i := 1 TO a^.length DO
     Put( a, i, x );
  END;
END Assign;

(* -------------------------------------------------------- *)
PROCEDURE Copy( a, b :Array );
VAR
  i, k, total_words :CARDINAL;
BEGIN
  IF ( a^.min = b^.min ) & ( a^.max = b^.max ) & ( a^.length = b^.length ) THEN

    total_words := a^.bytes DIV bytes_per_word;

    k := 0;
    FOR i := 1 TO total_words DO
       adra  := a^.start + k;
       adrb  := b^.start + k;
       adrb^ := adra^;
       k     := k + bytes_per_word;
    END;

  END;
END Copy;

(* -------------------------------------------------------- *)
PROCEDURE Duplicate( a :Array; VAR b :Array );
BEGIN
  Build( b, a^.min, a^.max, a^.length );
  Copy( a, b );
END Duplicate;


BEGIN

  bytes_per_word := TSIZE( ImplementationType );
  bits_per_word  := bytes_per_word * 8;

END PackArray.
