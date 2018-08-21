MODULE ShowBits;

(* test the bitwise logical functions *)
(* J. Andrea, 1984 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteLn, WriteCard;

FROM BitsetFunctions IMPORT Or, Nor, Xor, And, Nand, Xand, Not,
                            ShiftRight, ShiftLeft;

VAR
  a, b, c :BITSET;

   (* ------------------------------------------ *)
   PROCEDURE StartResults;
   (* begin a truth table *)
   BEGIN (* StartResults *)
       WriteLn;
       WriteString('operation 0  1  value'); WriteLn;
       WriteString('         -----       '); WriteLn;
   END StartResults;

   (* ------------------------------------------ *)
   PROCEDURE ShowResults( type : ARRAY OF CHAR);

   BEGIN (* ShowResults *)
      WriteString(type); WriteString(' 0    ');
      IF 0 IN c THEN
        WriteString('1  ');
      ELSE 
        WriteString('0  ');
      END;
      IF 1 IN c THEN
        WriteString('1');
      ELSE 
        WriteString('0');
      END;

      (* show the value of the result too,
         but if its a negation type function then subtract
         all those other bits that were negated too *)
      IF type[0] # 'N' THEN
         WriteCard(CARDINAL(c),6);
      ELSE
         WriteCard(CARDINAL(c)-4294967280,6);
      END; (* if *)
      WriteLn;
         
      WriteString('     1    ');
      IF 2 IN c THEN
        WriteString('1  ');
      ELSE 
        WriteString('0  ');
      END;
      IF 3 IN c THEN
        WriteString('1');
      ELSE 
        WriteString('0');
      END;
      WriteLn;
      WriteString('         -----       '); WriteLn;
   END ShowResults;

BEGIN (* ShowBits *)

(* initialize a and b for testing *)
a := {3,2};  (* 1100 *)
b := {3,1};  (* 1010 *)

WriteLn;
WriteString('a=1100,'); WriteCard( CARDINAL(a), 5 ); WriteLn;
WriteString('b=1010,'); WriteCard( CARDINAL(b), 5 ); WriteLn;
WriteLn;
StartResults;

c := Or(a,b);   ShowResults('OR  ');
c := Nor(a,b);  ShowResults('NOR ');
c := Xor(a,b);  ShowResults('XOR ');
c := And(a,b);  ShowResults('AND ');
c := Nand(a,b); ShowResults('NAND');
c := Xand(a,b); ShowResults('XAND');
c := Not(a);    ShowResults('NOT ');

(* now test the shift functions *)

WriteLn;
WriteString( '12 shifted right twice should give 3 right !' ); WriteLn;
WriteCard( CARDINAL( ShiftRight(a,2) ), 10 );
WriteString('    right ?'); WriteLn;

WriteString( 'and 12 shifted left twice should give 48 right !' ); WriteLn;
WriteCard( CARDINAL( ShiftLeft(a,2) ), 10);
WriteString('    right ?'); WriteLn;
WriteLn;

WriteString( '1 shifted right once should give 2^31 !' ); WriteLn;
WriteCard( CARDINAL( ShiftRight(BITSET(1),1) ), 10 ); WriteLn;

WriteString( 'and 2^31 shifted left once should give 1 !' ); WriteLn;
WriteCard( CARDINAL( ShiftLeft({31},1) ), 10 ); WriteLn;

WriteLn;
END ShowBits.
