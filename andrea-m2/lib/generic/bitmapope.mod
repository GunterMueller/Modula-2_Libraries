IMPLEMENTATION MODULE BitmapOperations;

(* Known bug - on the compare, all of the last set should not be compared *)

(* Bitmap operations using dynamic memory, see the corresponding definition *)
(* V2.1, J. Andrea, Jun.22/93 - add Duplicate *)
(* V2.0, J. Andrea, Jun.17/93 - get bitmap size at runtime *)
(* V1.0, J. Andrea, Mar.18/92 *)
(* This code may be freely used and distributed, it may not be sold. *)

(*
* A bitmap is implemented as a collection of BITSETS, each bit in the set is
* accessed as required by calculating the memory offset to the bitset containing
* the requested bit, then the offset into that bitset to get the specific bit.
* The global operations (Clear, And, Or, etc.) are performed not one bit at a
* time, but on one BITSETS a time.
* The single bit operations, Put and Get, must be performed on one bit.
*)

FROM SYSTEM IMPORT TSIZE, ADDRESS;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM FileSystem IMPORT Done, Create, Close, WriteChar, File, EOL;
FROM StringOperations IMPORT Length;

TYPE
  Bitmap = POINTER TO RECORD
             rows, cols :CARDINAL;   (* dimensions of the bitmap *)
             start      :ADDRESS;    (* memory address of the first item *)
             size       :CARDINAL;   (* actual size of the bitmap in bytes *)
             in_last    :CARDINAL;   (* number of bits used in last bitset *)
             sets       :CARDINAL;   (* number of bitsets *)
           END;

  DataType = BITSET;

VAR
  bset_bits, bset_bytes :CARDINAL;
  empty, full           :BITSET;
  digits                :ARRAY [0..9] OF CHAR;
  outf                  :File;
  i                     :CARDINAL;


(* ---------------------------------------------------- *)
PROCEDURE WriteLn;
BEGIN
  WriteChar( outf, EOL );
END WriteLn;

(* ---------------------------------------------------- *)
PROCEDURE WriteString( s :ARRAY OF CHAR );
VAR
  i, n :CARDINAL;
BEGIN
  n := Length( s );
  IF n > 0 THEN
    n := n - 1;
    FOR i := 0 TO n DO
      WriteChar( outf, s[i] );
    END;
  END;
END WriteString;

(* ---------------------------------------------------- *)
PROCEDURE WriteCard( x :CARDINAL );
VAR
  y :CARDINAL;
BEGIN
  IF x < 10 THEN
    WriteChar( outf, digits[x] );
  ELSE
    y := x DIV 10;
    WriteCard( y );
    WriteCard( x - y * 10 );
  END;
END WriteCard;

(* -------------------------------------------------------- *)
PROCEDURE InRange( a :Bitmap; row, col :CARDINAL ) :BOOLEAN;
(* is the specified row/col item in this bitmap *)
BEGIN
  RETURN ( row >= 1 ) & ( row <= a^.rows ) & ( col >= 1 ) & ( col <= a^.cols );
END InRange;

(* -------------------------------------------------------- *)
PROCEDURE Offset( a :Bitmap; row, col :CARDINAL; VAR set, bit :CARDINAL );
(* calculate the memory offset to the row/col item from the first item *)
VAR
  which_dot :CARDINAL;
BEGIN

  (* the bitmap is treated as a long string of bits *)
  (* with one row laid after another                *)
  (* calculate the position of this dot             *)
  (* subtract the extra 1 here rather than twice later *)

  which_dot := ( row - 1 ) * a^.cols + col - 1;

  (* the bits are grouped into sets of 32 (imp. dep.) each *)
  (* calculate which set this dot belongs to   *)
  (* - this is an offset from the first, so it should be zero based *)

  set := which_dot DIV bset_bits;

  (* and how far into this set is the dot that we need *)

  bit := which_dot - set * bset_bits;

END Offset;

(* -------------------------------------------------------- *)
PROCEDURE ReverseOffset( a :Bitmap; set, bit :CARDINAL; VAR row, col :CARDINAL );
(* given item position, calculate row and column *)
VAR
  which_dot :CARDINAL;
BEGIN
  which_dot := ( set - 1 ) * bset_bits + bit;
  row       := 1 + ( which_dot DIV a^.cols );
  col       := which_dot - ( ( row - 1 ) * a^.cols );
END ReverseOffset;

(* -------------------------------------------------------- *)
PROCEDURE Build( VAR a :Bitmap; n_rows, n_columns :CARDINAL );
VAR
  n_bits :CARDINAL;
BEGIN
  NEW( a );

  (* protect users from themselves *)
  IF n_rows    = 0 THEN n_rows    := 1 END;
  IF n_columns = 0 THEN n_columns := 1 END;


  a^.rows := n_rows;
  a^.cols := n_columns;

  n_bits  := n_rows * n_columns;

  a^.in_last := n_bits MOD bset_bits;

  IF a^.in_last = 0 THEN
    a^.in_last := bset_bits;       (* last one is filed *)
  ELSE
    (* must be a multiple of bits in a bitset *)
    n_bits := n_bits + bset_bits - a^.in_last;
  END;

  a^.sets := n_bits DIV bset_bits;   (* convert to bitsets *)
  a^.size := a^.sets * bset_bytes;   (* convert to bytes *)

  ALLOCATE( a^.start, a^.size );     (* grab that number of bytes from memory *)

END Build;

(* -------------------------------------------------------- *)
PROCEDURE Destroy( VAR a :Bitmap );
BEGIN
  DEALLOCATE( a^.start, a^.size );
  DISPOSE( a );
END Destroy;

(* -------------------------------------------------------- *)
PROCEDURE Put( a :Bitmap; row, col :CARDINAL; set :CARDINAL );
VAR
  adra                 :POINTER TO DataType;
  which_set, which_bit :CARDINAL;
BEGIN

  IF InRange( a, row, col ) THEN
    Offset( a, row, col, which_set, which_bit );
    adra  := a^.start + which_set * bset_bytes;

    IF set = 0 THEN
      EXCL( adra^, which_bit );
    ELSIF set = 2 THEN
      IF which_bit IN adra^ THEN
        EXCL( adra^, which_bit );
      ELSE
        INCL( adra^, which_bit );
      END;
    ELSE
      INCL( adra^, which_bit );    (* everything else means turn on *)
    END;
  END;
END Put;

(* -------------------------------------------------------- *)
PROCEDURE Get( a :Bitmap; row, col :CARDINAL ) :BOOLEAN;
VAR
  result :BOOLEAN;
  adra   :POINTER TO DataType;
  which_set, which_bit :CARDINAL;
BEGIN
  IF InRange( a, row, col ) THEN
    Offset( a, row, col, which_set, which_bit );
    adra   := a^.start + which_set * bset_bytes;
    result := which_bit IN adra^;
  ELSE
    result := FALSE;
  END;
  RETURN result;
END Get;

(* -------------------------------------------------------- *)
PROCEDURE Size( a :Bitmap; VAR n_rows, n_cols :CARDINAL );
BEGIN
  n_rows := a^.rows;
  n_cols := a^.cols;
END Size;

(* -------------------------------------------------------- *)
PROCEDURE Compare( a, b :Bitmap ) :BOOLEAN;
VAR
  result     :BOOLEAN;
  adra, adrb :POINTER TO DataType;
  i, k       :CARDINAL;
BEGIN

  result := TRUE;

  IF ( a^.rows = b^.rows ) & ( a^.cols = b^.cols ) THEN

    i := 1;
    k := 0;
    WHILE result & ( i <= a^.sets ) DO
       adra   := a^.start + k;
       adrb   := b^.start + k;
       result := adra^ = adrb^;
       i      := i + 1;
       k      := k + bset_bytes;
    END;

  ELSE
    result := FALSE;
  END;

  RETURN result;

END Compare;

(* -------------------------------------------------------- *)
PROCEDURE Clear( a :Bitmap );
VAR
  adra :POINTER TO DataType;
  i, k :CARDINAL;
BEGIN

  k := 0;
  FOR i := 1 TO a^.sets DO
     adra  := a^.start + k;
     adra^ := empty;
     k     := k + bset_bytes;
  END;

END Clear;

(* -------------------------------------------------------- *)
PROCEDURE Copy( a, b :Bitmap );
VAR
  adra, adrb :POINTER TO DataType;
  i, k       :CARDINAL;
BEGIN
  IF ( a^.rows = b^.rows ) & ( a^.cols = b^.cols ) THEN

    k := 0;
    FOR i := 1 TO a^.sets DO
       adra  := a^.start + k;
       adrb  := b^.start + k;
       adrb^ := adra^;
       k     := k + bset_bytes;
    END;

  END;
END Copy;

(* -------------------------------------------------------- *)
PROCEDURE Duplicate( a :Bitmap; VAR b :Bitmap );
BEGIN
  Build( b, a^.rows, a^.cols );
  Copy( a, b );
END Duplicate;

(* -------------------------------------------------------- *)
PROCEDURE Not( a :Bitmap; VAR b :Bitmap );
VAR
  adra, adrb :POINTER TO DataType;
  i, k       :CARDINAL;
BEGIN

  Build( b, a^.rows, a^.cols );

  k := 0;
  FOR i := 1 TO a^.sets DO
     adra  := a^.start + k;
     adrb  := b^.start + k;
     adrb^ := adra^ / full;        (* a Xor with <all bits> *)
     k     := k + bset_bytes;
  END;

END Not;

(* -------------------------------------------------------- *)
PROCEDURE And( a, b :Bitmap; VAR c :Bitmap; VAR ok :BOOLEAN );
VAR
  adra, adrb, adrc :POINTER TO DataType;
  i, k       :CARDINAL;
BEGIN

  IF ( a^.rows # b^.rows ) OR ( a^.cols # b^.cols ) THEN
    ok := FALSE;
  ELSE
    ok := TRUE;

    Build( c, a^.rows, a^.cols );

    k := 0;
    FOR i := 1 TO a^.sets DO
       adra  := a^.start + k;
       adrb  := b^.start + k;
       adrc  := c^.start + k;
       adrc^ := adra^ * adrb^;       (* set intersection *)
       k     := k + bset_bytes;
    END;

  END;

END And;

(* -------------------------------------------------------- *)
PROCEDURE Or( a, b :Bitmap; VAR c :Bitmap; VAR ok :BOOLEAN );
VAR
  adra, adrb, adrc :POINTER TO DataType;
  i, k             :CARDINAL;
BEGIN

  IF ( a^.rows # b^.rows ) OR ( a^.cols # b^.cols ) THEN
    ok := FALSE;
  ELSE
    ok := TRUE;

    Build( c, a^.rows, a^.cols );

    k := 0;
    FOR i := 1 TO a^.sets DO
       adra  := a^.start + k;
       adrb  := b^.start + k;
       adrc  := c^.start + k;
       adrc^ := adra^ + adrb^;    (* set union *)
       k     := k + bset_bytes;
    END;

  END;

END Or;

(* -------------------------------------------------------- *)
PROCEDURE Xor( a, b :Bitmap; VAR c :Bitmap; VAR ok :BOOLEAN );
VAR
  adra, adrb, adrc :POINTER TO DataType;
  i, k             :CARDINAL;
BEGIN

  IF ( a^.rows # b^.rows ) OR ( a^.cols # b^.cols ) THEN
    ok := FALSE;
  ELSE
    ok := TRUE;

    Build( c, a^.rows, a^.cols );

    k := 0;
    FOR i := 1 TO a^.sets DO
       adra  := a^.start + k;
       adrb  := b^.start + k;
       adrc  := c^.start + k;
       adrc^ := adra^ / adrb^;   (* symmetric set difference *)
       k     := k + bset_bytes;
    END;

  END;

END Xor;

(* -------------------------------------------------------- *)
PROCEDURE Line( a :Bitmap; row1, col1, row2, col2 :CARDINAL; set :CARDINAL );

VAR
  dr, dc, length, x, y :REAL;
  i, n                 :CARDINAL;

BEGIN
   IF InRange( a, row1, col1 ) & InRange( a, row2, col2 ) THEN

     dr := FLOAT( row2 ) - FLOAT( row1 );
     dc := FLOAT( col2 ) - FLOAT( col1 );

     IF ABS( dr ) > ABS( dc ) THEN
       length := ABS( dr );
     ELSE
       length := ABS( dc );
     END;

     n := TRUNC( length );
     IF n > 0 THEN

       dr := dr / length;
       dc := dc / length;

       x := FLOAT( col1 ) + 0.5;
       y := FLOAT( row1 ) + 0.5;
       
       FOR i := 1 TO n DO
          Put( a, TRUNC( y ), TRUNC( x ), set );
          x := x + dc;
          y := y + dr;
       END;

     END;

   END;
END Line;

(* -------------------------------------------------------- *)
PROCEDURE PrintText( a :Bitmap; filename :ARRAY OF CHAR; VAR ok :BOOLEAN );
VAR
   r, c, i, j, n :CARDINAL;
   adra          :POINTER TO DataType;
BEGIN

  Create( outf, filename, TRUE, TRUE );

  IF Done() THEN
    ok := TRUE;

    n := a^.cols;
    IF n > 132 THEN n := 132 END;      (* no wider than 132 characters *)

    i := 0;
    j := bset_bits;

    r := 1;
    WHILE r <= a^.rows DO

       c := 1;
       WHILE c <= a^.cols DO

          IF j >= bset_bits THEN
            (* jump to the next bitset *)
            i := i + 1;  j := 0;
            adra := a^.start + ( i - 1 ) * bset_bytes;
          END;

          IF c <= n THEN

            IF j IN adra^ THEN
              WriteChar( outf, '*' );
            ELSE
              WriteChar( outf, '.' );
            END;

          END;

          c := c + 1;
          j := j + 1;
       END;
       WriteLn;

       r := r + 1;
    END;

    Close( outf );

  ELSE
    ok := FALSE;
  END;

END PrintText;

(* -------------------------------------------------------- *)
PROCEDURE PrintSixel( a :Bitmap; filename :ARRAY OF CHAR; VAR ok :BOOLEAN );

CONST
   esc     = 33C;
   max_out = 800;
   blank   = 63;

VAR
   out_line           :ARRAY [1..max_out] OF CARDINAL;
   r1, r2, r, c, f, x :CARDINAL;
   i, j, k, n         :CARDINAL;

BEGIN
  Create( outf, filename, TRUE, FALSE );

  IF Done() THEN
    ok := TRUE;

    n  := a^.cols;
    IF n > max_out THEN n := max_out; END;

    WriteChar( outf, esc ); WriteString( 'P0q' ); (* into Sixel graphics mode *)

    r1 := 1;
    WHILE r1 <= a^.rows DO

      (* work in sets of 6 rows at a time *)
      r2 := r1 + 5;        IF r2 > a^.rows THEN r2 := a^.rows; END;

      FOR c := 1 TO n DO
         k := blank;
         f := 1;
         FOR r := r1 TO r2 DO
            IF Get( a, r, c ) THEN
              k := k + f;
            END;
            f := f * 2;
         END;
         out_line[c] := k;
      END;

      (* truncate trailing blanks *)
      j := n;
      WHILE ( j > 1 ) & ( out_line[j] = blank ) DO
        j := j - 1;
      END;

      WriteString( '??' );  (* spaces *)

      (* try to use repeat counts to reduce file space and print time *)

      c := 1;
      WHILE c <= j DO

         k := 0;
         x := out_line[c];
         i := c + 1;

         WHILE ( i <= j ) & ( x = out_line[i] ) DO
            k := k + 1;
            i := i + 1;
         END;

         k := k + 1;   (* first + number of repeats *)

         (* determine if its repeated enough *)

         IF k > 3 THEN
           WriteChar( outf, '!' );
           WriteCard( k );
           WriteChar( outf, CHR( x ) );
         ELSE
           FOR i := 1 TO k DO
             WriteChar( outf, CHR( x ) );
           END;
         END;

         c := c + k;   (* jump ahead *)
      END;

      WriteString( '-' );  (* new graphics line *)

      r1 := r2 + 1;
    END;


    WriteChar( outf, esc ); WriteString( '\' );   (* out of Sixel mode *)

    Close( outf );
  ELSE
    ok := FALSE;
  END;

END PrintSixel;

(* -------------------------------------------------------- *)
PROCEDURE PrintPS( a :Bitmap; filename :ARRAY OF CHAR; size :CARDINAL;
                   VAR ok :BOOLEAN );

CONST
   max_rows = 750;
   max_cols = 560;
   side     = 25;

VAR
   r, c    :CARDINAL;
   i, j, k :CARDINAL;
   adra    :POINTER TO DataType;

   (* ------------- *)
   PROCEDURE WriteDot;
   BEGIN
      IF ( r * size < max_rows ) & ( c * size < max_cols ) THEN
        WriteCard( c ); WriteString( ' ' );
        WriteCard( r ); WriteString( ' D' ); WriteLn;
      END;
   END WriteDot;

BEGIN

  Create( outf, filename, TRUE, TRUE );

  IF Done() THEN
    ok := TRUE;

    IF size < 1 THEN
      size := 1;
    ELSE
      IF size > 100 THEN
        size := 100;
      END;
    END;

    WriteString( '%!PS-Adobe' ); WriteLn;
    WriteString( '/D { moveto ' );
    WriteString( ' 1 0 rlineto 0 1 rlineto -1 0 rlineto closepath fill' );
    WriteString( ' } bind def' ); WriteLn;

    WriteCard( side ); WriteString( ' ' ); WriteCard( max_rows );
    WriteString( ' translate 0 0 moveto' ); WriteLn;
    WriteString( '.2 setgray' ); WriteLn;
    WriteCard( size ); WriteString( ' dup neg scale' ); WriteLn;

    k := 0;
    FOR i := 1 TO a^.sets - 1 DO
       adra := a^.start + k;
       IF adra^ # empty THEN
         FOR j := 0 TO bset_bits DO
            IF j IN adra^ THEN
              ReverseOffset( a, i, j, r, c );
              WriteDot;
            END;
         END;
       END;
       k := k + bset_bytes;
    END;

    adra := a^.start + k;
    ReverseOffset( a, a^.sets, 0, r, c );
    FOR j := 0 TO a^.in_last - 1 DO
       IF j IN adra^ THEN
         WriteDot;
       END;
       c := c + 1;
    END;

    WriteString( 'showpage' ); WriteLn;
    WriteString( '%eof' );

    Close( outf );
  ELSE
    ok := FALSE;
  END;

END PrintPS;

BEGIN

  bset_bytes := TSIZE( DataType );
  bset_bits  := bset_bytes * 8;

  empty := {};

  full := {};
  FOR i := 0 TO bset_bits - 1 DO
     INCL( full, i );
  END;

  FOR i := 0 TO 9 DO
     digits[i] := CHR( i + 48 );
  END;

END BitmapOperations.
