MODULE PP;

	(********************************************************)
	(*							*)
	(*		Version extraction program		*)
	(*							*)
	(*	Originally derived from a program given to	*)
	(*	me by Deane Blackman of Monash University,	*)
	(*	using a source syntax similar to that used	*)
	(*	by a utility in Rowley Modula-2.		*)
	(*							*)
	(*  Compilers currently supported:			*)
	(*	FST, Rowley, TopSpeed 1.17, TopSpeed 3.10	*)
	(*	The Rowley version is untested and may have	*)
	(*	errors.						*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	24 February 1995		*)
	(*  Status:		Working				*)
	(*							*)
	(*  Minor shortcoming: with the "S" command-line	*)
	(*	option, we can get blank lines in the output	*)
	(*	file resulting from the fact that we've		*)
	(*	deleted everything except some space and tab	*)
	(*	characters.  Fixing this would require a	*)
	(*	change in strategy - we'd need to avoid		*)
	(*	writing a partial line, saving it instead	*)
	(*	so that we can look at the whole line before	*)
	(*	deciding whether it consists entirely of	*)
	(*	white space.  For now I don't consider this	*)
	(*	important enough to justify the effort of	*)
	(*	redesigning for that case.			*)
	(*							*)
	(*  Remark: in case you're wondering why all the File	*)
	(*	parameters in procedures are VAR parameters:	*)
	(*	we have to do it this way because some		*)
	(*	compilers (e.g. FST) implement a File variable	*)
	(*	as a record, rather than as some sort of	*)
	(*	handle which points to the file descriptor.	*)
	(*	If we used value parameters then we'd have	*)
	(*	multiple copies of file descriptors in various	*)
	(*	procedures, and they wouldn't be consistent	*)
	(*	with one another in terms of things like the	*)
	(*	end-of-file flag.				*)
	(*							*)
	(********************************************************)

(************************************************************************)
(*									*)
(* Selection by (*<xxx where  xxx  is a selecting string.  The		*)
(* terminator for this region is >*).  The text between the (*< and	*)
(* >*) will be activated iff the  xxx  evaluates to TRUE using the	*)
(* assignments in the PP.CFG file.					*)
(*									*)
(* If the S (for suppress) option is specified, all inactive text and	*)
(* all selection delimiters are deleted.  If the S option is absent	*)
(* then all original text is retained but the part that is supposed to	*)
(* be inactive is commented out.					*)
(*									*)
(************************************************************************)

FROM PPExpr IMPORT
    (* proc *)	DefineSymbol, Id, Expr, StringExpr, DumpSymbolTable;

FROM PPMisc IMPORT
    (* type *)	File,
    (* proc *)	Length, CopyString, Pos,
		OpenFile, CloseFile, WriteToFile, TerminateLine,
		ReadLine, EndOfFile, DeleteFile, RenameFile,
		Message, EndOfMessage, CommandLine;

(************************************************************************)
(*			   GLOBAL VARIABLES				*)
(************************************************************************)

TYPE
    BufferSubscript = [0..255];
    FileNumber = [1..255];
    SpecialSymbol = (EofSymbol, StartVersion, EndVersion, CommentedEndVersion);
    CharSet = SET OF CHAR;

CONST
    AlphaNumerics = CharSet {"A".."Z", "a".."z", "0".."9"};

VAR
    (* Buffer holds the current source line being processed.  We make	*)
    (* it global since it is accessed by a number of procedures.	*)
    (* LinePlace is the location in Buffer that holds the next		*)
    (* character to be looked at.					*)

    Buffer: ARRAY BufferSubscript OF CHAR;
    LinePlace: CARDINAL;

    (* The following flag is set if we have read an empty line from	*)
    (* the input file.  We use it to distinguish between lines that	*)
    (* were empty originally, and those we have made empty by deletions.*)
    (* This allows us to avoid generating excess blank lines in the	*)
    (* output, while still retaining those blank lines that the author	*)
    (* intended to keep.						*)

    TerminateEmptyLine: BOOLEAN;

    (* The next flag says whether there is anything in Buffer.  This	*)
    (* is a guard against reading rubbish or writing the same line	*)
    (* twice.  (There's probably a better way to achieve this, but so	*)
    (* far I haven't worked out a simpler way to handle the end-of-file	*)
    (* condition.)							*)

    BufferLoaded: BOOLEAN;

    (* SourceLength is either MAX(CARDINAL) or the length of the string	*)
    (* in Buffer.  Most of the time we leave it equal to MAX(CARDINAL)	*)
    (* and recompute only as needed.					*)

    SourceLength: CARDINAL;

    (* FileName is a list of files that have to be processed. *)

    FileName: ARRAY FileNumber OF ARRAY [0..127] OF CHAR;

    (* There are LastFileNumber+1 files in total. *)

    LastFileNumber: [0..MAX(FileNumber)];

(************************************************************************)
(*		INSERTIONS AND DELETIONS IN THE LINE BUFFER		*)
(************************************************************************)

PROCEDURE InsertString (offset: INTEGER;  str: ARRAY OF CHAR);

    (* Inserts str at Buffer[LinePlace+offset].  If offset is zero or	*)
    (* negative then LinePlace is adjusted.				*)

    VAR j, amount, base: CARDINAL;

    BEGIN
	IF SourceLength > HIGH(Buffer) THEN
	    SourceLength := Length(Buffer);
	END (*IF*);
	amount := Length(str);
	base := INTEGER(LinePlace) + offset;
	FOR j := SourceLength+amount-1 TO base+amount BY -1 DO
	    Buffer[j] := Buffer[j-amount];
	END (*FOR*);
	FOR j := 0 TO amount-1 DO
	    Buffer[base+j] := str[j];
	END (*FOR*);
	IF offset <= 0 THEN
	    LinePlace := LinePlace + amount;
	END (*IF*);
	INC (SourceLength, amount);
	IF SourceLength <= HIGH(Buffer) THEN
	    Buffer[SourceLength] := CHR(0);
	END (*IF*);
    END InsertString;

(************************************************************************)

PROCEDURE DeleteString (offset: INTEGER;  length: CARDINAL);

    (* Deletes the substring of length "length" starting at		*)
    (* Buffer[LinePlace+offset].  If offset is negative then LinePlace	*)
    (* is adjusted.							*)

    VAR j, base: CARDINAL;

    BEGIN
	IF SourceLength > HIGH(Buffer) THEN
	    SourceLength := Length(Buffer);
	END (*IF*);
	base := INTEGER(LinePlace) + offset;
	FOR j := base TO SourceLength-length DO
	    Buffer[j] := Buffer[j+length];
	END (*FOR*);
	Buffer[SourceLength-length+1] := CHR(0);
	IF offset < 0 THEN
	    IF INTEGER(length) + offset > 0 THEN LinePlace := base;
	    ELSE DEC(LinePlace, length);
	    END (*IF*);
	END (*IF*);
	DEC (SourceLength, length);
    END DeleteString;

(************************************************************************)

PROCEDURE WritePartialLine (VAR (*INOUT*) f: File;  pos: CARDINAL);

    (* Writes Buffer[0..pos-1] to file f, and then deletes the written	*)
    (* data from Buffer.						*)

    VAR ch: CHAR;

    BEGIN
	ch := Buffer[pos];  Buffer[pos] := CHR(0);
	WriteToFile (f, Buffer);
	Buffer[pos] := ch;
	DeleteString (-INTEGER(pos), pos);

	(* This next assignment is to ensure that the output line	*)
	(* will be terminated even if the rest of it is deleted.	*)

	TerminateEmptyLine := TRUE;

    END WritePartialLine;

(************************************************************************)
(*			COPYING "ORDINARY" TEXT				*)
(************************************************************************)

PROCEDURE EndOfLine(): BOOLEAN;

    (* Returns TRUE iff we are at the end of the line buffer. *)

    BEGIN
	RETURN (LinePlace > HIGH(Buffer)) OR (Buffer[LinePlace] = CHR(0));
    END EndOfLine;

(************************************************************************)

PROCEDURE WriteCurrentLine (VAR (*INOUT*) f: File);

    (* Writes out the current buffer contents. *)

    BEGIN
	(* Blank line suppression test. *)
	IF Buffer[0] = CHR(0) THEN
	    IF TerminateEmptyLine THEN
		TerminateLine (f);
	    END (*IF*);
	ELSE
	    WriteToFile (f, Buffer);
	    TerminateLine (f);
	END (*IF*);
	BufferLoaded := FALSE;  TerminateEmptyLine := FALSE;
    END WriteCurrentLine;

(************************************************************************)

PROCEDURE GetNextLine (VAR (*INOUT*) fin, fout: File;  SendToOutput: BOOLEAN);

    (* Writes out the current line to fout if SendToOutput is TRUE,	*)
    (* and then re-fills the line buffer from fin.			*)

    BEGIN
	IF SendToOutput AND BufferLoaded THEN
	    WriteCurrentLine (fout);
	END (*IF*);
	IF NOT EndOfFile(fin) THEN
	    ReadLine (fin, Buffer);  LinePlace := 0;
	    TerminateEmptyLine := Buffer[0] = CHR(0);
	    SourceLength := MAX(CARDINAL);

	    (* The next test is to guard against the possibility that  *)
	    (* all we have read is the terminating end-of-file char.   *)

	    BufferLoaded := NOT (TerminateEmptyLine AND EndOfFile(fin));

	END (*IF*);

    END GetNextLine;

(************************************************************************)

PROCEDURE EndCommentMarkerPresent(): BOOLEAN;

    (* If an "end comment" delimiter is found in the source then we	*)
    (* move past it and return TRUE.  Otherwise the result is FALSE.	*)

    BEGIN
	IF (Buffer[LinePlace] = "*") AND (Buffer[LinePlace+1] = ")") THEN
	    INC (LinePlace, 2);
	    RETURN TRUE;
	END (*IF*);
	RETURN FALSE;
    END EndCommentMarkerPresent;

(************************************************************************)

PROCEDURE SkipComment (VAR (*INOUT*) fin, fout: File;  SendToOutput: BOOLEAN);

    (* On entry we have just passed a "start comment" delimiter in the	*)
    (* source.  We copy this over, taking nested comments into account,	*)
    (* and return with Buffer[LinePlace] hold the character just beyond	*)
    (* the corresponding "end comment" delimiter.			*)

    BEGIN
	LOOP
	    IF EndOfLine() THEN
		IF EndOfFile (fin) THEN
		    RETURN;
		END (*IF*);
		GetNextLine (fin, fout, SendToOutput);
		IF NOT BufferLoaded THEN
		    RETURN;
		END (*IF*);
	    ELSIF EndCommentMarkerPresent() THEN
		RETURN;
	    ELSIF (Buffer[LinePlace] = "(") AND (Buffer[LinePlace+1] = "*") THEN
		INC (LinePlace, 2);
		SkipComment (fin, fout, SendToOutput);
	    ELSE
		INC (LinePlace);
	    END (*IF*);
	END (*LOOP*);
    END SkipComment;

(************************************************************************)

PROCEDURE CopyOver (VAR (*INOUT*) fin, fout: File;
				SendToOutput: BOOLEAN): SpecialSymbol;

    (* This procedure simply reads the input file fin, copying it to	*)
    (* fout if SendToOutput is TRUE, and returns on end of file or	*)
    (* when an opening or closing version delimiter is detected.  Note	*)
    (* that version delimiters inside comments, or outside comments but	*)
    (* inside string delimiters, are treated as ordinary text.		*)
    (* The function result is the symbol that caused this procedure to	*)
    (* return.  On return we have already read past that symbol.	*)

    (* Note: on return there might still be some unprocessed text in	*)
    (* Buffer.  This is indicated by the global variable BufferLoaded.	*)

    VAR ch: CHAR;

    BEGIN
	LOOP
	    WHILE EndOfLine() DO
		IF EndOfFile (fin) THEN
		    IF SendToOutput AND BufferLoaded THEN
			WriteCurrentLine (fout);
		    END (*IF*);
		    RETURN EofSymbol;
		END (*IF*);
		GetNextLine (fin, fout, SendToOutput);
	    END (*WHILE*);

	    (* Pick up next source character. *)

	    ch := Buffer[LinePlace];  INC(LinePlace);

	    IF (ch = "'") OR (ch = '"') THEN

		(* Special case: character string. *)

		WHILE NOT EndOfLine() AND (Buffer[LinePlace] <> ch) DO
		    INC(LinePlace);
		END (*WHILE*);
		IF EndOfLine() THEN
		    EndOfMessage;
		    Message ("Error: Unterminated character string");
		    EndOfMessage;
		    Message (Buffer);
		ELSE
		    INC(LinePlace);
		END (*IF*);

	    ELSIF (ch = "(") AND (Buffer[LinePlace] = "*") THEN

		(* Special case: start of comment.  This could be an	*)
		(* ordinary comment, a "start version" delimiter, or a	*)
		(* commented-out "end version" delimiter.		*)

		INC (LinePlace);
		IF Buffer[LinePlace] = "<" THEN
		    INC (LinePlace);
		    RETURN StartVersion;
		ELSIF Buffer[LinePlace] = ">" THEN
		    INC (LinePlace);
		    IF EndCommentMarkerPresent() THEN
			RETURN CommentedEndVersion;
		    END (*IF*);
		ELSE
		    SkipComment (fin, fout, SendToOutput);
		END (*IF*);

	    ELSIF ch = ">" THEN

		(* This could be an "end version" delimiter - look in	*)
		(* more detail.						*)

		IF EndCommentMarkerPresent() THEN
		    RETURN EndVersion;
		END (*IF*);

	    END (*IF*);

	END (*LOOP*);

    END CopyOver;

(************************************************************************)
(*			THE MAIN CONVERSION ROUTINES			*)
(************************************************************************)

PROCEDURE Filter (VAR (*INOUT*) fin, fout: File;
			KeepText, Active: BOOLEAN;
			VAR (*INOUT*) ChangeMade: BOOLEAN): SpecialSymbol;

    (* Copies from fin to fout, altering the version delimiters as	*)
    (* appropriate to comment and uncomment text.  This procedure	*)
    (* returns after reaching the end of fin, or finding the		*)
    (* first unmatched "close version" delimiter, whichever comes	*)
    (* first.  The reason for defining the return condition in this	*)
    (* way is that we use recursive calls to handle nested conditions.	*)

    (* KeepText specifies whether the unselected versions should be	*)
    (* copied over (commented out); if KeepText is false then the	*)
    (* unselected code is omitted from the output file.  The "Active"	*)
    (* parameter specifies whether we are currently in a section that	*)
    (* should be retained without being commented out - it should be	*)
    (* TRUE on the first call, but may be false on recursive calls.	*)
    (* The function result indicates which special symbol terminated	*)
    (* the processing.							*)

    VAR code: SpecialSymbol;  include, endcomment: BOOLEAN;
	markerstart, length: CARDINAL;

    BEGIN
	LOOP
	    code := CopyOver (fin, fout, KeepText OR Active);
	    IF NOT KeepText AND NOT Active THEN
		ChangeMade := TRUE;
	    END (*IF*);

	    (* The result of CopyOver can be one of:			*)
	    (*	EofSymbol:	means processing is complete		*)
	    (*	StartVersion:	means that we have to conditionally	*)
	    (*			copy a version				*)
	    (*	EndVersion, CommentedEndVersion: these terminate a	*)
	    (*			recursive call				*)

	    IF code <> StartVersion THEN EXIT(*LOOP*) END(*IF*);

	    (* We have reached a "start version" delimiter; so we have	*)
	    (* to decide whether to retain or remove the following	*)
	    (* section of source.					*)

	    markerstart := LinePlace - 3;
	    include := Expr(Buffer, LinePlace) AND Active;
	    endcomment := EndCommentMarkerPresent();

	    IF include THEN

		(* The code in this section should be included.  If	*)
		(* KeepText is TRUE we should put an "end comment"	*)
		(* delimiter after this "start version" marker, unless	*)
		(* it's already there.  If KeepText is FALSE then we	*)
		(* should delete the entire version selector, including	*)
		(* the "end comment" delimiter if present.		*)

		IF KeepText THEN
		    IF NOT endcomment THEN
			InsertString (0, "*)");  ChangeMade := TRUE;
		    END (*IF*);
		ELSE
		    length := LinePlace - markerstart;
		    DeleteString (-INTEGER(length), length);
		    ChangeMade := TRUE;
		END (*IF*);
		code := Filter (fin, fout, KeepText, TRUE, ChangeMade);
		IF code = EndVersion THEN

		    (* Delete the "end version" delimiter, or comment	*)
		    (* it out, depending on the value of KeepText.	*)

		    IF KeepText THEN
			InsertString (-3, "(*");
		    ELSE
			DeleteString (-3, 3);
		    END (*IF*);
		    ChangeMade := TRUE;

		ELSIF (code = CommentedEndVersion) AND NOT KeepText THEN
		    (* Delete a commented "end version" delimiter if	*)
		    (* KeepText is FALSE.				*)
		    DeleteString (-5, 5);
		    ChangeMade := TRUE;
		END (*IF*);

	    ELSE

		(* The code in this section should be suppressed.  If	*)
		(* KeepText is FALSE we may need to write out the part	*)
		(* of the input line before the suppressed text.  If	*)
		(* KeepText is TRUE we merely remove any "end comment"	*)
		(* delimiter after this "start version" marker.		*)

		IF KeepText THEN
		    IF endcomment THEN
			DeleteString (-2, 2);
			IF Buffer[LinePlace] IN AlphaNumerics THEN
			    InsertString (0, " ");
			END (*IF*);
			ChangeMade := TRUE;
		    END (*IF*);
		ELSIF Active THEN
		    WritePartialLine (fout, markerstart);
		END (*IF*);
		code := Filter (fin, fout, KeepText, FALSE, ChangeMade);
		IF KeepText THEN
		    IF code = CommentedEndVersion THEN
			(* Change the CommentedEndVersion to an EndVersion. *)
			DeleteString (-5, 2);
			ChangeMade := TRUE;
		    END (*IF*);
		ELSE
		    (* Delete that part of the line already processed. *)
		    DeleteString (-INTEGER(LinePlace), LinePlace);
		    ChangeMade := TRUE;
		END (*IF*);

	    END (*IF include*);

	    (* In all cases, the special symbol just read should have	*)
	    (* been some variety of "end version" marker.		*)

	    IF code = EofSymbol THEN
		EndOfMessage;
		Message ("Error: section not terminated before end of file.");
		EXIT (*LOOP*);
	    END (*IF*);

	END (*LOOP*);

	RETURN code;

    END Filter;

(************************************************************************)

PROCEDURE ConvertFile (VAR (*INOUT*) fin, fout: File;
		KeepText: BOOLEAN;  VAR (*OUT*) changed: BOOLEAN): BOOLEAN;

    (* Copies fin to fout, converting as directed by the embedded	*)
    (* version control delimiters.  Returns FALSE if the operation	*)
    (* stopped before end-of-file.					*)

    BEGIN
	ReadLine (fin, Buffer);  LinePlace := 0;
	changed := FALSE;
	TerminateEmptyLine := Buffer[0] = CHR(0);
	SourceLength := MAX(CARDINAL);
	BufferLoaded := TRUE;
	IF Filter (fin, fout, KeepText, TRUE, changed) <> EofSymbol THEN
	    EndOfMessage;
	    Message ("Error: unmatched delimiter, entire file not read.");
	    EndOfMessage;
	    RETURN FALSE;
	END (*IF*);
	RETURN TRUE;
    END ConvertFile;

(************************************************************************)

PROCEDURE ConvertAllFiles (KeepText: BOOLEAN);

    (* Converts all files named in array FileName.  The original copies	*)
    (* are renamed with a .BAK extension.				*)

    VAR j: FileNumber;  success, changed: BOOLEAN;
	tmpname, BAKname: ARRAY [0..127] OF CHAR;
	dotplace: CARDINAL;
	fin, fout: File;

    BEGIN
	IF LastFileNumber = 0 THEN RETURN END(*IF*);

	FOR j := 1 TO LastFileNumber DO

	    Message (FileName[j]);
	    Message (": ");

	    (* From the file name, obtain two derived names. *)

	    CopyString (FileName[j], BAKname);
	    dotplace := Pos (".", BAKname);
	    IF dotplace > Length(BAKname) THEN
		dotplace := Length(BAKname);
		BAKname[dotplace] := ".";
	    END (*IF*);
	    BAKname[dotplace+1] := "B";
	    BAKname[dotplace+2] := "A";
	    BAKname[dotplace+3] := "K";
	    BAKname[dotplace+4] := CHR(0);
	    CopyString (BAKname, tmpname);
	    tmpname[dotplace+1] := "$";
	    tmpname[dotplace+2] := "$";
	    tmpname[dotplace+3] := "$";

	    (* Open the source file, and open a temporary file for	*)
	    (* output.							*)

	    success := TRUE;
	    IF OpenFile (fin, FileName[j], FALSE) THEN
		IF NOT OpenFile (fout, tmpname, TRUE) THEN
		    Message ("can't create output file.");
		    CloseFile (fin);
		    success := FALSE;
		END (*IF*);
	    ELSE
		Message ("missing input file ");
		Message (FileName[j]);
		success := FALSE;
	    END (*IF*);

	    IF success THEN

		(* Perform the conversion. *)

		success := ConvertFile (fin, fout, KeepText, changed);
		CloseFile (fin);  CloseFile (fout);

		IF success THEN

		    IF changed THEN

			(* Delete any existing .BAK file, and rename	*)
			(* the input and output files.			*)

			DeleteFile (BAKname);
			RenameFile (FileName[j], BAKname);
			RenameFile (tmpname, FileName[j]);
			Message ("done");

		    ELSE

			(* File has not changed, so throw away the	*)
			(* output file.					*)

			DeleteFile (tmpname);
			Message ("unchanged");

		    END (*IF*);

		END (*IF*);

	    END (*IF*);
	    EndOfMessage;

	END (*FOR*);

    END ConvertAllFiles;

(************************************************************************)
(*			LOADING THE SYMBOL TABLE			*)
(************************************************************************)

PROCEDURE SkipBlanks;

    (* Moves LinePlace past any space or tab characters in Buffer. *)

    CONST space = " ";  tab = CHR(9);

    BEGIN
	WHILE (LinePlace < HIGH(Buffer)) AND
		((Buffer[LinePlace] = space) OR (Buffer[LinePlace] = tab)) DO
	    INC (LinePlace);
	END (*WHILE*);
    END SkipBlanks;

(************************************************************************)

PROCEDURE ProcessAssignment;

    (* Handles an assignment in the line buffer.  The assignment can be	*)
    (* a single symbol (in which it is given the value "TRUE"), or it	*)
    (* can be of the form    <symbol> := <expression>			*)

    VAR symbol, result: ARRAY [0..31] OF CHAR;

    BEGIN
	Id (Buffer, LinePlace, symbol);
	SkipBlanks;
	IF (Buffer[LinePlace] = ":") AND (Buffer[LinePlace+1] = "=") THEN
	    INC (LinePlace, 2);
	    SkipBlanks;
	    StringExpr (Buffer, LinePlace, result);
	ELSE
	    CopyString ("TRUE", result);
	END (*IF*);
	DefineSymbol (symbol, result);
    END ProcessAssignment;

(************************************************************************)

PROCEDURE ProcessConfigurationFile(): BOOLEAN;

    (* Processes the assignments in the configuration file PP.CFG, and	*)
    (* loads the results into the symbol table.  Returns TRUE to	*)
    (* indicate success.						*)

    VAR f: File;

    BEGIN
	IF NOT OpenFile (f, "PP.CFG", FALSE) THEN
	    Message ("Error: Can't find file PP.CFG");
	    EndOfMessage;
	    RETURN FALSE;
	END (*IF*);

	(* We process one line of input on each pass through this loop.	*)

	LOOP
	    IF EndOfFile (f) THEN
		CloseFile (f);  RETURN TRUE;
	    END (*IF*);
	    ReadLine (f, Buffer);  LinePlace := 0;

	    (* Leading white space is legal. *)

	    SkipBlanks;

	    (* A legal assignment should start with an alphanumeric. *)

	    IF Buffer[LinePlace] IN AlphaNumerics THEN
		ProcessAssignment;
	    END (*IF*);

	    (* Trailing white space is also legal. *)

	    SkipBlanks;

	    (* The only other thing that should be on the line is an	*)
	    (* optional comment.  Comments start with "--".		*)

	    IF NOT EndOfLine() AND ((Buffer[LinePlace] <> "-")
					OR (Buffer[LinePlace+1] <> "-")) THEN
		Message ("Error: illegal expression in PP.CFG");
		EndOfMessage;
		Message (Buffer);
		EndOfMessage;
		CloseFile (f);
		RETURN FALSE;
	    END (*IF*);

	END (*LOOP*);

    END ProcessConfigurationFile;

(************************************************************************)
(*		     LOADING THE LIST OF FILE NAMES			*)
(************************************************************************)

PROCEDURE GetFileNames(): BOOLEAN;

    (* Reads a list of file names from the file PP.FIL.  Returns FALSE	*)
    (* if the operation failed.						*)

    VAR f: File;

    BEGIN
	IF NOT OpenFile (f, "PP.FIL", FALSE) THEN
	    Message ("Error: Can't find file PP.FIL");
	    EndOfMessage;
	    RETURN FALSE;
	END (*IF*);
	LastFileNumber := 0;
	
	(* We process one line of input on each pass through this loop.	*)

	LOOP
	    IF EndOfFile (f) THEN
		CloseFile (f);
		IF LastFileNumber = 0 THEN
		    Message ("Warning: no files to process.");
		    EndOfMessage;
		    RETURN FALSE;
		ELSE
		    RETURN TRUE;
		END (*IF*);
	    END (*IF*);

	    INC (LastFileNumber);
	    ReadLine (f, FileName[LastFileNumber]);

	    (* Watch out for blank lines and comment lines. *)

	    IF (FileName[LastFileNumber][0] = CHR(0))
			OR ((FileName[LastFileNumber][0] = "-")
			    AND (FileName[LastFileNumber][1] = "-")) THEN
		DEC (LastFileNumber);
	    END (*IF*);

	END (*LOOP*);

    END GetFileNames;

(************************************************************************)
(*			PARSING THE COMMAND LINE			*)
(************************************************************************)

PROCEDURE ParseCommandLine (VAR (*OUT*) KeepText: BOOLEAN): BOOLEAN;

    (* Reads the command line and checks for the "suppress unused	*)
    (* versions" flag.  Returns FALSE if there is something wrong with	*)
    (* the command line.						*)

    VAR str: ARRAY [0..31] OF CHAR;
	arglen: CARDINAL;

    BEGIN
	CommandLine (1, str, arglen);
	IF arglen = 0 THEN
	    KeepText := TRUE;  RETURN TRUE;
	END (*IF*);

	(* An argument was present.  Only "S" or "s" are legal. *)

	KeepText := (arglen <> 1) OR (CAP(str[0]) <> "S");

	(* Check for two or more arguments. *)

	CommandLine (2, str, arglen);
	IF (arglen > 0) OR KeepText THEN
	    Message ("Usage: PP [S]");
	    EndOfMessage;
	    RETURN FALSE;
	END (*IF*);
	RETURN TRUE;

    END ParseCommandLine;

(************************************************************************)
(*			    MAIN PROGRAM				*)
(************************************************************************)

VAR KeepText: BOOLEAN;

BEGIN
    IF ParseCommandLine (KeepText) AND GetFileNames()
		AND ProcessConfigurationFile() THEN
	DumpSymbolTable;
	ConvertAllFiles (KeepText);
    END (*IF*);
END PP.
