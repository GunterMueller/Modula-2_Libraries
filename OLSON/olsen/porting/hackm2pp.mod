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

MODULE hackm2pp;
(*
 * Hack front end of M2PParser and M2PScanner for porting purposes.
 *
 * True variables are hard-coded so you don't need ProgArgs or
 * ProgEnviron.  The input and output are defined by the module
 * HackTextIO which is a hacked up version of the normal TextIO.
 *)
IMPORT
    M2PParser,
    M2PScanner,
    HackNameLists,
    HackTextIO;

PROCEDURE Process( 
    input      : HackTextIO.Object;
    output     : HackTextIO.Object;
    variables  : HackNameLists.Object 
    )          : BOOLEAN;

    VAR
	result    : BOOLEAN;
	processedObject: M2PScanner.Object;

    BEGIN (* Process *)

        M2PScanner.Open( processedObject, input, output, 
			 M2PScanner.stripOff );

	result := M2PParser.Parse( processedObject, variables );
 
	result := M2PScanner.Close( processedObject ) AND result;

	RETURN result;

    END Process;

VAR
    variables : HackNameLists.Object;
BEGIN (* hackm2pp *)

    HackNameLists.Create( variables, HackNameLists.forwardSorted,
		      HackNameLists.caseSensitive );
    HackNameLists.Insert( variables, "Assert", HackNameLists.DONTCARE );
    HackNameLists.Insert( variables, "Debug", HackNameLists.DONTCARE );
    HackNameLists.Insert( variables, "SysTypesCheck", HackNameLists.DONTCARE );
    HackNameLists.Insert( variables, "SunOS", HackNameLists.DONTCARE );
    HackNameLists.Insert( variables, "SunM2", HackNameLists.DONTCARE );
    HackNameLists.Insert( variables, "SunArch", HackNameLists.DONTCARE );
    HackNameLists.Insert( variables, "BigEndian", HackNameLists.DONTCARE );

    IF NOT Process( HackTextIO.GetInput(), HackTextIO.GetOutput(), variables ) 
      THEN HALT;
    END;

END hackm2pp.
