(*$Copyright 1988 by Olsen & Associates (O&A), Zurich, Switzerland.

                       All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies, and
that both that copyright notice and this permission notice appear in
supporting documentation, and that all modifications of this software
or its documentation not made by O&A or its agents are accompanied
by a prominent notice stating who made the modifications and the date
of the modifications.

O&A DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE AND ITS
DOCUMENTATION, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS.  IN NO EVENT SHALL O&A BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES, ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE OR ITS DOCUMENTATION.
******************)

IMPLEMENTATION MODULE HackTextIO;
(*
 * Sample module for aiding in porting the preprocessor.  This should
 * run on most byte stream file oriented systems with few modifications.
 * This version has been tested on a Sun-3 running Sun 3.2 and with
 * Sun's M2 compiler.
 * 
 * We do buffered IO for efficiency, but it really isn't necessary.
 *)
IMPORT 
    ASCII,
    IOConsts,
    ProgErrOutput,
    SYSTEM,
    UnixError,
    UnixCsys_file;

TYPE
    Object = CARDINAL;

VAR
    inputBuffer : ARRAY [ 0 .. 511 ] OF CHAR;
    inputIndex  : INTEGER;	(* Must be int to deal with unix *)
    inputLength : INTEGER;	(* ditto *)

PROCEDURE ReadChars(
        file      : Object;          (* in "ok" state and readable *)
        buf       : SYSTEM.ADDRESS;  (* location where read data is put *)
        bufSize   : CARDINAL;        (* maximum number of chars to read *)
    VAR charsRead : CARDINAL         (* actual number of chars read *)
    )             : States;          (* see States *)
    VAR
	cp : POINTER TO ARRAY [ 0 .. 1001 ] OF CHAR;
	ch : CHAR;
    BEGIN (* ReadChars *)
	cp := buf;
	charsRead := 0;
	LOOP
	    IF inputIndex >= inputLength THEN
		inputLength := UnixCsys_file.read( file, 
		    SYSTEM.ADR( inputBuffer ), HIGH( inputBuffer ) + 1 );
		IF inputLength <= 0 THEN
		    IF inputLength = 0 THEN
			RETURN endOfFile;
		    END;
		    RETURN error;
		END;
		inputIndex := 0;
	    END;

	    ch := inputBuffer[ inputIndex ];
	    INC( inputIndex );
	    IF ch = ASCII.lf THEN
		(* Throws away the lf *)
		RETURN endOfLine;
	    END;
	    cp^[ charsRead ] := ch;
	    INC( charsRead );
	    IF charsRead >= bufSize THEN
		RETURN ok;
	    END;
        END; (* LOOP *)

    END ReadChars;


PROCEDURE WriteChars(
        file    : Object;          (* not in "error" state and writable *)
        buf     : SYSTEM.ADDRESS;  (* contains data to be written *)
        bufSize : CARDINAL         (* number of chars to be written *)
    )           : States;          (* success => "ok" *)
    VAR
	count : INTEGER;
    BEGIN (* WriteChars *)

	REPEAT
	    count := UnixCsys_file.write( file, buf, bufSize );
	    IF count <= 0 THEN
		RETURN error;
	    END;
	    DEC( bufSize, CARDINAL( count ) );
	    INC( buf, CARDINAL( count ) );
	UNTIL bufSize = 0;

	RETURN ok;

    END WriteChars;

VAR
    newLine : ARRAY [ 0 .. 0 ] OF CHAR;
PROCEDURE WriteLn(
    file : Object   (*  in "ok" state and writable *)
    )    : States;  (* success => "ok" *)
    BEGIN (* WriteLn *)
	RETURN WriteChars( file, SYSTEM.ADR( newLine ), HIGH( newLine ) + 1 );
    END WriteLn;


PROCEDURE PrintErrorMessage(
    file : Object;
    msg  : ARRAY OF CHAR
    );
    VAR
	path : IOConsts.Path;
    BEGIN (* PrintErrorMessage *)
	GetOpenPath( file, path );
	UnixError.PrintMessage( UnixError.Get(), path );
    END PrintErrorMessage;


PROCEDURE GetOpenPath(
        file : Object;        (* must be valid *)
    VAR path : IOConsts.Path  (* path of external file, or empty *)
    );
    BEGIN (* GetOpenPath *)
	IF file = GetInput() THEN
	    path := "standard input";
	ELSE
	    path := "standard output";
	END;
    END GetOpenPath;


PROCEDURE GetInput(
    ) : Object;  (* is "readOnly" *)
    BEGIN (* GetInput *)
	RETURN UnixCsys_file.stdInputHandle;
    END GetInput;


PROCEDURE GetOutput(
    ) : Object;  (* is "appendOnly" *)
    BEGIN (* GetOutput *)
	RETURN UnixCsys_file.stdOutputHandle;
    END GetOutput;


BEGIN (* HackText *)

    (* Set up the read buffer *)
    inputIndex := 0;
    inputLength := 0;

    newLine[ 0 ] := ASCII.lf;

END HackTextIO.
