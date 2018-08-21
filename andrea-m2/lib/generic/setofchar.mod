IMPLEMENTATION MODULE SetOfChar;

(* V1.1, John Andrea, Jun.22/93 -add Duplicate *)
(* V1.0, John Andrea, Apr.5/92 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM Storage IMPORT ALLOCATE, DEALLOCATE;
IMPORT LargeSets;

                                    (* Some compilers don't allow the direct *)
                                    (* type CharSet = LargeSets.LargeSet     *)
                                    (* So build a second layer here          *)
TYPE
   CharSet  = POINTER TO CharData;
   CharData = RECORD
                 set :LargeSets.LargeSet;
              END;

PROCEDURE Build( VAR a :CharSet );
BEGIN
  NEW( a );
  LargeSets.Build( a^.set, 0, 255 );
END Build;

PROCEDURE Destroy( VAR a :CharSet );
BEGIN
  LargeSets.Destroy( a^.set );
  DISPOSE( a );
END Destroy;

PROCEDURE Empty( a :CharSet );
BEGIN
  LargeSets.Empty( a^.set );
END Empty;

PROCEDURE Fill( a :CharSet );
BEGIN
  LargeSets.Fill( a^.set );
END Fill;

PROCEDURE Incl( a :CharSet; c :CHAR );
BEGIN
  LargeSets.Incl( a^.set, ORD( c ) );
END Incl;

PROCEDURE Excl( a :CharSet; c :CHAR );
BEGIN
  LargeSets.Excl( a^.set, ORD( c ) );
END Excl;

PROCEDURE In( a :CharSet; c :CHAR ) :BOOLEAN;
BEGIN
  RETURN LargeSets.In( a^.set, ORD( c ) );
END In;

PROCEDURE Not( a :CharSet );
BEGIN
  LargeSets.Not( a^.set );
END Not;

PROCEDURE Equal( a, b :CharSet ) :BOOLEAN;
BEGIN
   RETURN LargeSets.Equal( a^.set, b^.set );
END Equal;

PROCEDURE Copy( a, b :CharSet );
BEGIN
   LargeSets.Copy( a^.set, b^.set );
END Copy;

PROCEDURE Duplicate( a :CharSet; VAR b :CharSet );
BEGIN
   Build( b );
   Copy( a, b );
END Duplicate;

BEGIN
END SetOfChar.
