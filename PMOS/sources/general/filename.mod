IMPLEMENTATION MODULE FileNames;

	(********************************************************)
	(*							*)
	(*		File name parsing			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:		Just started			*)
	(*	Apparently complete but untested.		*)
	(*							*)
	(*	This module, which is part of the file system	*)
	(*	and is not intended for end-user use, looks	*)
	(*	after translating file name strings into a	*)
	(*	more convenient internal form.			*)
	(*							*)
	(*	It is still questionable whether the syntax	*)
	(*	imposed by this module is the best one.		*)
	(*	Need to check the rules for DOS file names	*)
	(*	more carefully.  Should perhaps also report	*)
	(*	an incompletely consumed name string as an	*)
	(*	error.						*)
	(*							*)
	(********************************************************)

FROM Windows IMPORT	(* for testing	*)
    (* type *)	Window,
    (* proc *)	OpenSimpleWindow, WriteString, WriteChar, WriteLn;

FROM Devices IMPORT
    (* proc *)	NullDevice, IdentifyDevice;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST Nul = CHR(0);

(************************************************************************)
(*				FOR TESTING				*)
(************************************************************************)

CONST testing = FALSE;

VAR debug: Window;		(* used only for testing *)

(************************************************************************)
(*			   PARSING A FILE NAME				*)
(************************************************************************)

PROCEDURE WriteFileName (w: Window;  name: FileName);

    (* For testing: writes name in window w.	*)

    BEGIN
	WriteString (w, name.fname);  WriteChar (w, ".");
	WriteString (w, name.fext);
    END WriteFileName;

(************************************************************************)

PROCEDURE SimpleScan (VAR (*IN*) string: ARRAY OF CHAR;
		VAR (*OUT*) substring: ARRAY OF CHAR;
		VAR (*OUT*) separator: CHAR;
		VAR (*INOUT*) point: CARDINAL);

    (* Extracts a substring of "string", starting at string[point]	*)
    (* and continuing until one of the characters ".", ":", "\", " ",	*)
    (* or Nul is found.  The terminating character is returned as	*)
    (* "separator".  (If we reach the end of the string, Nul is		*)
    (* returned as the separator.)  On return, point is updated so that	*)
    (* string[point] is the first character after the separator, except	*)
    (* in the end-of-string case where the value of point is irrelevant.*)
    (* Alphabetic characters are converted to upper case in substring.	*)
    (* If array substring is longer than the substring extracted by	*)
    (* this procedure, the trailing character positions are not		*)
    (* disturbed - it is up to the caller to supply defaults.		*)

    CONST SeparatorSet = CharSet {Nul, " ", ".", ":", "\"};

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	LOOP
	    IF point > HIGH(string) THEN EXIT (*LOOP*)
	    ELSIF string[point] IN SeparatorSet THEN EXIT(*LOOP*)
	    ELSIF j <= HIGH(substring) THEN
		substring[j] := CAP(string[point]);  INC(j);
	    END (*IF*);
	    INC (point);
	END (*LOOP*);
	IF point <= HIGH(string) THEN
	    separator := string[point];  INC(point);
	ELSE
	    separator := Nul;
	END (*IF*);
    END SimpleScan;

(************************************************************************)

PROCEDURE Scan (VAR (*IN*) string: ARRAY OF CHAR;
		VAR (*OUT*) name: FileName;
		VAR (*OUT*) separator: CHAR;
		VAR (*INOUT*) point: CARDINAL);

    (* Extracts a file name from "string", starting at string[point].	*)
    (* The terminating character is returned as "separator".  (If we	*)
    (* reach the end of the string, Nul is returned as the separator).	*)
    (* On return, point is updated so that string[point] is the first	*)
    (* character after the separator, except in the end-of-string case	*)
    (* where the value of point is irrelevant.				*)

    BEGIN
	name.fname := "        ";  name.fext := "   ";
	SimpleScan (string, name.fname, separator, point);
	IF separator = "." THEN
	    SimpleScan (string, name.fext, separator, point);
	END (*IF*);
    END Scan;

(************************************************************************)
(*		   THE EXTERNALLY CALLABLE PROCEDURES			*)
(************************************************************************)

PROCEDURE Parse (name: ARRAY OF CHAR;  VAR (*OUT*) device: Device;
		VAR (*OUT*) unit: CARDINAL;  VAR (*OUT*) result: NameList;
		VAR (*OUT*) StartAtRoot: BOOLEAN);

    (* Translates a text string "name" into NameList form.  If the	*)
    (* string includes a device specification this is returned as	*)
    (* (device, unit); otherwise we return with device = NullDevice.	*)
    (* Output parameter StartAtRoot is TRUE iff the filename string	*)
    (* started with a '\'.						*)

    CONST Terminators = CharSet {Nul, " ", ".", ":"};

    VAR separator: CHAR;
	place: CARDINAL;
	devicestring: EightChar;
	previous, current: NameList;

    BEGIN
	place := 0;  devicestring := "        ";  result := NIL;

	(* Extract the device name, if present.	*)

	SimpleScan (name, devicestring, separator, place);
	IF separator = ":" THEN
	    IdentifyDevice (devicestring, device, unit);
	ELSE
	    devicestring := "        ";
	    device := NullDevice();  place := 0;
	END (*IF*);

	IF testing THEN
	    WriteString (debug, "The device name is ");
	    WriteString (debug, devicestring);  WriteLn (debug);
	END (*IF*);

	(* A leading "\" indicates the root directory.	*)

	StartAtRoot := name[place] = "\";
	IF StartAtRoot THEN
	    INC (place);
	END (*IF*);

	(* Check whether this is all that was supplied.	*)

	IF (place > HIGH(name)) OR (name[place] IN Terminators) THEN
	    RETURN;
	END (*IF*);

	(* Now fill in all the details of the path.	*)

	current := NIL;
	REPEAT
	    previous := current;
	    NEW (current);
	    IF previous = NIL THEN result := current;
	    ELSE previous^.child := current;
	    END (*IF*);
	    WITH current^ DO
		Scan (name, string, separator, place);
		child := NIL;
	    END (*WITH*);
	UNTIL separator <> '\';

    END Parse;

(************************************************************************)

PROCEDURE DiscardNameList (VAR (*INOUT*) path: NameList);

    (* Disposes of the storage occupied by a NameList.  Returns NIL.	*)

    VAR current: NameList;

    BEGIN
	WHILE path <> NIL DO
	    current := path;  path := current^.child;
	    DISPOSE (current);
	END (*WHILE*);
    END DiscardNameList;

(************************************************************************)

BEGIN
    IF testing THEN
	OpenSimpleWindow (debug, 0, 4, 0, 79);
    END (*IF*);
END FileNames.
