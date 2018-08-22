IMPLEMENTATION MODULE PPMisc;

	(********************************************************)
	(*							*)
	(*	Miscellaneous procedures for preprocessor.	*)
	(*							*)
	(*	The purpose of this module is to collect	*)
	(*	together the compiler-dependent of the		*)
	(*	preprocessor.  The module consists mostly	*)
	(*	of file and string operations.			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	15 March 1994			*)
	(*  Status:		Working with FST, TopSpeed 1.17	*)
	(*			   and TopSpeed 3.10		*)
	(*							*)
	(*  Rowley version is untested and probably wrong.	*)
	(*							*)
	(********************************************************)

(*<FST
IMPORT System, InOut, FileSystem, Directories, PPTextIO, Strings;
>*)

(*<Rowley
IMPORT Environment, InOut, FileSystem, Directories, TextIO, Strings;
>*)

(*<TopSpeed*)
IMPORT SYSTEM, FIO, IO, Lib, Str;
(*>*)

(************************************************************************)

(*<TopSpeed*)

(* Warning: the code used for TopSpeed allocates file buffers on the	*)
(* assumption that we never have more than one input file and one	*)
(* output file open at any one time.					*)

VAR InFileBuffer, OutFileBuffer: ARRAY [1..2048+FIO.BufferOverhead] OF BYTE;
(*>*)

(************************************************************************)
(*			   STRING OPERATIONS				*)
(************************************************************************)

PROCEDURE Length (s: ARRAY OF CHAR): CARDINAL;

    (* Returns the length of string s. *)

    BEGIN
	RETURN (*<TopSpeed*) Str.Length(s); (*>*)
	       (*<FST|Rowley Strings.Length(s); >*)
    END Length;

(************************************************************************)

PROCEDURE StringMatch (s1, s2: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff the two strings are equal. *)

    BEGIN
	(*<FST RETURN (Strings.CompareStr (s1, s2) = 0); >*)
	(*<TopSpeed*) RETURN (Str.Compare (s1, s2) = 0); (*>*)
	(*<Rowley RETURN (Strings.Compare (s1, s2) = 0); >*)
    END StringMatch;

(************************************************************************)

PROCEDURE CopyString (src: ARRAY OF CHAR; VAR (*OUT*) dst: ARRAY OF CHAR);

    (* Copies src to dst. *)

    BEGIN
	(*<FST|Rowley Strings.Assign (src, dst); >*)
	(*<TopSpeed*) Str.Copy (dst, src); (*>*)
    END CopyString;

(************************************************************************)

PROCEDURE Pos (pattern, string: ARRAY OF CHAR): CARDINAL;

    (* If pattern is a substring of string, returns the index in	*)
    (* string of the start of the first occurrence of pattern.  If	*)
    (* no match then the value returned is beyond the end of string.	*)

    BEGIN
	(*<FST|Rowley RETURN Strings.Pos (pattern, string); >*)
	(*<TopSpeed*)
	(* A trap for the unwary: note the order of the parameters. *)
	RETURN Str.Pos (string, pattern);
	(*>*)
    END Pos;

(************************************************************************)
(*				TERMINAL I/O				*)
(************************************************************************)

PROCEDURE Message (mess: ARRAY OF CHAR);

    (* Writes a string to the screen. *)

    BEGIN
	(*<FST|Rowley InOut.WriteString (mess); >*)
	(*<TopSpeed*) IO.WrStr (mess); (*>*)
    END Message;

(************************************************************************)

PROCEDURE EndOfMessage;

    (* Goes to the next line on the screen. *)

    BEGIN
	(*<FST|Rowley InOut.WriteLn; >*)
	(*<TopSpeed*) IO.WrLn; (*>*)
    END EndOfMessage;

(************************************************************************)

PROCEDURE CommandLine (argNr: CARDINAL;  VAR (*OUT*) strg : ARRAY OF CHAR;
					VAR (*OUT*) arglen : CARDINAL);

    (* Picks up argument number argNr from the command line. *)

(*<FST
    BEGIN
	System.GetArg (strg, arglen);
    END CommandLine;
>*)

(*<TopSpeed*)
    BEGIN
	Lib.ParamStr (strg, argNr);
	arglen := Str.Length (strg);
    END CommandLine;
(*>*)

(*<Rowley
    VAR OK : BOOLEAN;
    BEGIN
	Environment.GetArg (argNr, strg, OK);
	IF NOT OK THEN
	    arglen := 0;
	ELSE
	    arglen := Strings.Length (strg);
	END;
    END CommandLine;
>*)

(************************************************************************)
(*			     FILE OPERATIONS				*)
(************************************************************************)

PROCEDURE OpenFile (VAR (*OUT*) f: File;  filename: ARRAY OF CHAR;
						create: BOOLEAN): BOOLEAN;

    (* Opens the file specified as "filename".  We open it for input if	*)
    (* create=FALSE, or create a new file for output if create=TRUE.	*)
    (* The function result indicates success.				*)

    (*<TopSpeed*) VAR temp: BOOLEAN; (*>*)

    BEGIN
(*<TopSpeed*)
	temp := FIO.IOcheck;  FIO.IOcheck := FALSE;
	IF create THEN
	    f := FIO.Create (filename);
	ELSE
	    f := FIO.Open (filename);
	END (*IF*);
	FIO.IOcheck := temp;
	IF f < MAX(CARDINAL) THEN
	    IF create THEN
		FIO.AssignBuffer (f, OutFileBuffer);
	    ELSE
		FIO.AssignBuffer (f, InFileBuffer);
	    END (*IF*);
	    RETURN TRUE;
	ELSE
	    RETURN FALSE;
	END (*IF*);
(*>*)
(*<FST|Rowley
	FileSystem.Lookup (f, filename, create);
	RETURN (f.res = FileSystem.done);
>*)
    END OpenFile;

(************************************************************************)

PROCEDURE CloseFile (VAR (*INOUT*) f: File);

    (* Closes file f. *)

    BEGIN
	(*<FST|Rowley FileSystem.Close(f); >*)
	(*<TopSpeed*) FIO.Close(f); (*>*)
    END CloseFile;

(************************************************************************)

PROCEDURE EndOfFile (VAR (*INOUT*) f: File): BOOLEAN;

    (* Returns TRUE iff the end of file f has been reached.		*)
    (* Remark: in the TopSpeed case the library procedure FIO.EOF is	*)
    (* inadequate for this purpose, because (a) it is only updated	*)
    (* after a read, and (b) there is no feedback about WHICH file has	*)
    (* reached its end.							*)

    BEGIN
	(*<FST|Rowley RETURN f.eof; >*)
	(*<TopSpeed*) RETURN FIO.GetPos(f) >= FIO.Size(f); (*>*)
    END EndOfFile;

(************************************************************************)

PROCEDURE WriteToFile (VAR (*INOUT*) f: File;
				VAR (*IN*) str: ARRAY OF CHAR);

    (* Writes a text string to file f. *)

    BEGIN
	(*<FST PPTextIO.WriteString (f, str); >*)
	(*<Rowley TextIO.WriteString (f, str); >*)
	(*<TopSpeed*) FIO.WrStr (f, str); (*>*)
    END WriteToFile;

(************************************************************************)

PROCEDURE TerminateLine (VAR (*INOUT*) f: File);

    (* Writes an end-of-line to file f. *)

    BEGIN
	(*<FST PPTextIO.WriteLn (f); >*)
	(*<Rowley TextIO.WriteLn (f); >*)
	(*<TopSpeed*) FIO.WrLn (f); (*>*)
    END TerminateLine;

(************************************************************************)

PROCEDURE ReadLine (VAR (*INOUT*) f: File;  VAR (*OUT*) strg: ARRAY OF CHAR);

    (* Reads a line of text from file f. *)

    BEGIN
	(*<FST PPTextIO.ReadString (f,strg); >*)
	(*<Rowley TextIO.ReadString (f,strg);  TextIO.ReadLn (f); >*)
	(*<TopSpeed*) FIO.RdStr (f,strg); (*>*)
    END ReadLine;

(************************************************************************)

PROCEDURE DeleteFile (name: ARRAY OF CHAR);

    (* Deletes a file, if it exists.  The file must not be open. *)

    BEGIN
	(*<FST|Rowley
	Directories.Delete(name);
	>*)
	(*<TopSpeed*)
	IF FIO.Exists(name) THEN FIO.Erase(name); END (*IF*);
	(*>*)
    END DeleteFile;

(************************************************************************)

PROCEDURE RenameFile (originalname, newname: ARRAY OF CHAR);

    (* Renames an existing file.  The file should not be open. *)

    BEGIN
	(*<FST|Rowley Directories.Rename(originalname, newname); >*)
	(*<TopSpeed*) FIO.Rename(originalname, newname); (*>*)
    END RenameFile;

(************************************************************************)

END PPMisc.
