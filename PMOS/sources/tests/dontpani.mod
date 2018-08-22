MODULE DontPanic;

	(********************************************************)
	(*							*)
	(*		Simple test of screen output		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	20 January 1993			*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

FROM Windows IMPORT
    (* type *)	Window, RowRange, ColumnRange, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteLn;

(************************************************************************)

TYPE DisplayString = ARRAY ColumnRange OF CHAR;

(************************************************************************)

PROCEDURE WriteTheMessage;

    CONST blob = CHR(219);

    VAR w: Window;  j, k: CARDINAL;
	line: ARRAY RowRange OF DisplayString;

    BEGIN
	OpenWindow (w, red, green, 0, 24, 0, 79, noframe, nodivider);
	FOR j := 0 TO MAX(RowRange) DO
	    line[j] := "";
	END (*FOR*);

	line[3] :=
"           ******        ****      **     **    **   ************";
	line[4] :=
"           **   **      *    *     ***    **     *        **     ";
	line[5] :=
"           **    **    **    **    ****   **              **     ";
	line[6] :=
"           **    **   **      **   ** **  **              **     ";
	line[7] :=
"           **    **   **      **   **  ** **              **     ";
	line[8] :=
"           **    **    **    **    **   ****              **     ";
	line[9] :=
"           **   **      *    *     **    ***              **     ";
	line[10] :=
"           ******        ****      **     **              **     ";

	line[13] :=
"          ******         *        **     **    ****      ******* ";
	line[14] :=
"          **   **       ***       ***    **     **      *      **";
	line[15] :=
"          **    **     ** **      ****   **     **     **        ";
	line[16] :=
"          **   **     **   **     ** **  **     **     **        ";
	line[17] :=
"          ******     **     **    **  ** **     **     **        ";
	line[18] :=
"          **        ***********   **   ****     **     **        ";
	line[19] :=
"          **       **         **  **    ***     **      *      **";
	line[20] :=
"          **      **           ** **     **    ****      ******* ";

	FOR j := 0 TO MAX(RowRange) DO
	    FOR k := 0 TO MAX(ColumnRange) DO
		IF line[j][k] = "*" THEN
		    line[j][k] := blob;
		END (*IF*);
	    END (*FOR*);
	END (*FOR*);

	WriteString (w, line[0]);
	FOR j := 1 TO MAX(RowRange) DO
	    WriteLn (w); WriteString(w, line[j]);
	END (*FOR*);

	FOR j := 0 TO 32767 DO
	    FOR k := 0 TO 1000 DO
		(* Nothing *)
	    END (*FOR*);
	END (*FOR*);

	CloseWindow(w);
    END WriteTheMessage;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    WriteTheMessage;
END DontPanic.
