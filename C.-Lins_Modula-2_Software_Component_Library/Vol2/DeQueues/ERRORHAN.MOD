IMPLEMENTATION MODULE ErrorHandling;
(*====================================================================
    Version  : 1.0  Sun, Mar 5, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Code Size: R-  bytes
    Data Size: 10 bytes
    Component: Standard Error Handling Support Utility

    INTRODUCTION
    This module provides a general support mechanism for the exceptions
    declared by the various ADT component modules.

    REVISION HISTORY
    v1.0  Sun, Mar 5, 1989  C. Lins
      Initial JPI implementation.

    PROPRIETARY NOTICES
    ErrorHandling.MOD Copyright (C) 1989 Charles A. Lins.
====================================================================*)

FROM SYSTEM IMPORT
    (*--Type*) BYTE;


PROCEDURE ExitOnError	 (    moduleID	 : CARDINAL    (*-- in	  *);
			      operation  : BYTE        (*-- in	  *);
			      exception  : BYTE        (*-- in	  *));
BEGIN
  HALT;
END ExitOnError;
(*----------------------------*)

PROCEDURE Raise 	 (    moduleID	 : CARDINAL    (*-- in	  *);
			      operation  : BYTE        (*-- in	  *);
			      exception  : BYTE        (*-- in	  *);
			      theHandler : HandlerProc (*-- in	  *));
BEGIN
  IF (theHandler # NullHandler) THEN
    theHandler(moduleID, operation, exception);
  END (*--if*);
END Raise;
(*----------------------------*)

END ErrorHandling.
