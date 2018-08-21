MODULE Symmetry;

(* Symmetric Chaos: four fold *)
(* See "Mathematical Recreations", Scientific American, December 1992 *)

(* J. Andrea, Dec.1992 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM Randomly IMPORT RandomReal;
FROM VGAGraphics IMPORT BeginVGA, EndVGA, EraseScreen, Move, Dot, Text;
FROM Terminal IMPORT Read, ReadString, WriteString, WriteLn;
FROM RealConversions IMPORT StringToReal;
FROM RealInOut IMPORT WriteReal;
FROM Break IMPORT EnableBreak;

CONST
  max_iter = 32000;    (* maximum size for DOS *)
  max_loop = 1;        (* so do it again *)
  
  nx = 640; ny = 480;
  
VAR
  a, b, c, d :REAL;
  x, x2, x3, x4, y, y2, y3, y4 :REAL;
  u, v, f :REAL;
  x_min, x_max, y_min, y_max, x_offset, y_offset, x_scale, y_scale :REAL;
  i, j, k, iter, loop :CARDINAL;
  ch :CHAR;
  
  PROCEDURE Function;
  BEGIN
    x2 := x * x;  x3 := x2 * x;  x4 := x3 * x;
    y2 := y * y;  y3 := y2 * y;  y4 := y3 * y;
    f := a + b * (x2 + y2);
    u := f * x + c * (x4 - 6.0 * x2 * y2 + y4) + d * (x3 - 3.0 * x * y2);
    v := f * y + c * (4.0 * x3 * y - 4.0 * x * y3) + d * ( y3 - 3.0 * x2 * y);
    x := u;
    y := v;
  END Function;
  
  PROCEDURE GetValue( name, default :ARRAY OF CHAR; VAR out :REAL );
  VAR
    value :ARRAY [0..20] OF CHAR;
    ok    :BOOLEAN;
    longout :LONGREAL;
  BEGIN
    WriteString( name ); WriteString( ' (default = ' );
    WriteString( default ); WriteString( ') ? ' );
    ReadString( value ); WriteLn;
    IF value[0] = 0C THEN
      StringToReal( default, longout, ok );
    ELSE
      StringToReal( value, longout, ok );
    END;
    IF ok THEN
      out := longout;
    ELSE
      out := 0.0;
    END;
  END GetValue;
  
BEGIN EnableBreak;

  GetValue( 'a', '2.5', a );
  GetValue( 'b', '-2.5', b );
  GetValue( 'c', '0.0', c );
  GetValue( 'd', '-0.55', d );
  
  WriteReal( a, 10 ); WriteString( '  ' ); WriteReal( b, 10 ); WriteString( ' ' );
  WriteReal( c, 10 ); WriteString( '  ' ); WriteReal( d, 10 ); WriteLn;

  WriteString( 'wait...' ); WriteLn;

  x := RandomReal( 0.0, 1.0 );
  y := RandomReal( 0.0, 1.0 );

  x_min := x; x_max := x;
  y_min := y; y_max := y;
  FOR i := 1 TO 5000 DO
    Function;
    IF x < x_min THEN
      x_min := x;
    ELSIF x > x_max THEN
      x_max := x;
    END;
    IF y < y_min THEN
      y_min := y;
    ELSIF y > y_max THEN
      y_max := y;
    END;
  END;
  
  (* change 5% *)
  x_min := x_min - x_min / 20.0;
  x_max := x_max + x_max / 20.0;
  y_min := y_min - y_min / 20.0;
  y_max := y_max + y_max / 20.0;
  
  x_offset := x_min;
  y_offset := y_min;
  
  x_scale := FLOAT( nx ) / ( x_max - x_min );
  y_scale := FLOAT( ny ) / ( y_max - y_min );

  BeginVGA; EraseScreen;
  
  FOR loop := 1 TO max_loop DO
  FOR iter := 1 TO max_iter DO
    Function;

    i := TRUNC( ( x - x_offset ) * x_scale );
    j := TRUNC( ( y - y_offset ) * y_scale );

    IF ( i > 0 ) & ( i <= nx ) & ( j > 0 ) & ( j <= ny ) THEN
      Move( i, j ); Dot;
    END;

  END; END;

  Move( 600, 10 ); Text( 'done' );
  Read( ch );
  EndVGA;
  
END Symmetry.