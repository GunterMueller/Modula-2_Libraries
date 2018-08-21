MODULE ShowMem;

(* Show the usage of dynamic memory *)
(* V1.0, J. Andrea, Sept.4/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn;
FROM MemoryStatus IMPORT Used, Available;
FROM Storage IMPORT ALLOCATE;
FROM SYSTEM IMPORT TSIZE;

TYPE
   Big = RECORD
           value :ARRAY [1..25000] OF CARDINAL;     (* this is a big record *)
         END;

VAR
   big     :POINTER TO Big;
   i, size :CARDINAL;
   u, a, t :CARDINAL;

   (* ------------------------------------- *)
   PROCEDURE Show;
   BEGIN
     WriteString( 'used =' ); WriteCard( Used(), 9 );  WriteString( ' ' );
     WriteString( 'avail=' ); WriteCard( Available(), 9 );
   END Show;

BEGIN

size := TSIZE( Big );

u := Used();   a := Available();
Show; WriteString( ' initial' ); WriteLn;

t := 0;
FOR i := 1 TO 9 DO
   NEW( big );
   t := t + size;

   Show;
   WriteString( ' grabbed ' ); WriteCard( size, 0 ); WriteString( ' bytes' );
   WriteLn;
END;

WriteLn;

u := Used() - u;
WriteString( 'difference between initial used and now is ' );
WriteCard( u, 0 ); WriteLn;

a := a - Available();
WriteString( 'difference between initial available and now is ' );
WriteCard( a, 0 ); WriteLn;

WriteString( 'difference those two is ' ); WriteCard( u - a, 0 ); WriteLn;

WriteString( 'amount grabbed is ' ); WriteCard( t, 0 ); WriteLn;

WriteString( 'difference between taken total and report is ' );
WriteCard( a - t, 0 ); WriteString( ', must be overhead.' ); WriteLn;

END ShowMem.
