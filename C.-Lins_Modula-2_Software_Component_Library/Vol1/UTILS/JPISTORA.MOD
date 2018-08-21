IMPLEMENTATION MODULE JPIStorage; 
(*====================================================================
    Version  : 1.0	Sat, Mar 4, 1989	C. Lins
    Compiler : JPI TopSpeed Modula-2
	Code Size: R-  bytes
    Component: Tool - JPI M2SCL Storage Management Utility

    INTRODUCTION
    This module provides facilities for allocation and deallocation of
	dynamic memory.  The routines are compatible with the JPI Storage
	module. Allocate differs from ALLOCATE in returning NIL
	if unsuccessful instead of HALTing. Deallocate does nothing if
	the given pointer is NIL, otherwise NIL is returned.

    REVISION HISTORY
    v1.0	Sat, Mar 4, 1989	C. Lins
      Initial implementation.

	PROPRIETARY NOTICES
    JPIStorage.MOD Copyright (C) 1989 Charles A. Lins.
====================================================================*)

FROM SYSTEM IMPORT
    (*--Type*) ADDRESS;

FROM Storage IMPORT
    (*--Proc*) ALLOCATE, DEALLOCATE, Available;

    (*-----------------------*)
    (*     CONSTRUCTORS      *)

PROCEDURE Allocate   (VAR thePointer : ADDRESS  (*--out  *);
                          theSize    : CARDINAL (*--in   *));
BEGIN
  IF Available(theSize) THEN
    ALLOCATE(thePointer, theSize);
  ELSE
  	thePointer := NIL;
  END (*--if*);
END Allocate;
(*--------------------*)

PROCEDURE Deallocate (VAR thePointer : ADDRESS  (*--inout*);
                          theSize    : CARDINAL (*--in   *));
BEGIN
  IF (thePointer # NIL) THEN
    DEALLOCATE(thePointer, theSize);
	thePointer := NIL;
  END (*--if*);
END Deallocate;
(*--------------------*)

END JPIStorage.