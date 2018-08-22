IMPLEMENTATION MODULE PPTextIO;

	(********************************************************)
	(*							*)
	(*	Extra file operations for preprocessor.		*)
	(*	This module is needed only when using the	*)
	(*	FST compiler - it adds some procedures which	*)
	(*	are missing from the FST library.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	16 August 1994			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

IMPORT FileSystem, ASCII;

(************************************************************************)

PROCEDURE WriteString (VAR (*INOUT*) f: File;  str: ARRAY OF CHAR);

    (* Sends the string to file f. *)

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	WHILE (j <= HIGH(str)) AND (str[j] <> CHR(0)) DO
	    FileSystem.WriteChar (f, str[j]);
	    INC (j);
	END (*WHILE*);
    END WriteString;

(************************************************************************)

PROCEDURE WriteLn (VAR (*INOUT*) f: File);

    (* Writes an end-of-line marker to file f. *)

    BEGIN
	FileSystem.WriteChar (f, ASCII.EOL);
    END WriteLn;

(************************************************************************)

PROCEDURE ReadString (VAR (*INOUT*) f: File;
				VAR (*OUT*) str: ARRAY OF CHAR);

    (* Reads from file f until end-of-line.  The line-terminating	*)
    (* characters are not stored, we simply skip over them.  If the	*)
    (* whole line won't fit in str, we read only what will fit.		*)

    VAR j: CARDINAL;  ch: CHAR;

    BEGIN
	j := 0;
	LOOP
            IF f.eof OR (j > HIGH(str)) THEN EXIT(*LOOP*) END(*IF*);
	    FileSystem.ReadChar(f, ch);
	    IF (ch = ASCII.CtrlZ) OR (ch = ASCII.EOL) THEN
		EXIT(*LOOP*)
	    END(*IF*);
	    str[j] := ch;  INC(j);
	END (*LOOP*);
	IF j <= HIGH(str) THEN
	    str[j] := CHR(0);
	END (*IF*);
    END ReadString;

(************************************************************************)

END PPTextIO.
