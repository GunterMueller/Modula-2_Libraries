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

IMPLEMENTATION MODULE HackNameLists;
(*
 * THIS MODULE CONTAINS AS MUCH IMPLEMENTATION AS NECESSARY TO SUPPORT
 * THE PROGRAM hackm2pp.   IT SHOULD NOT BE USED FOR ANYTHING ELSE.
 *)
IMPORT
    GenConsts,
    SafeStorage,
    Strings,
    SYSTEM,
    SysTypes;

TYPE
    Element = POINTER TO ElementRec;
    Object = POINTER TO ObjectRec;
    ObjectRec = RECORD
	head : Element;
    END;

    ElementRec = RECORD
	key    : GenConsts.Identifier;
	impObj : SysTypes.ANYPTR;
	next   : Element;
    END;

PROCEDURE Create(
    VAR list          : Object;
        howToTraverse : Orderings;
        howToFind     : FindCases
    );
    BEGIN (* Create *)
	IF ( howToTraverse # forwardSorted ) OR
	   ( howToFind # caseSensitive ) THEN
	    HALT;
	END;
	SafeStorage.ALLOCATE( list, SYSTEM.TSIZE( ObjectRec ) );
	WITH list^ DO
	    head := NIL;
	END;
    END Create;


PROCEDURE Insert(
    list           : Object;
    name           : ARRAY OF CHAR;
    importerObject : SysTypes.ANYPTR
    );
    VAR
	element : Element;
    BEGIN (* Insert *)
	SafeStorage.ALLOCATE( element, SYSTEM.TSIZE( ElementRec ) );
	WITH element^ DO
	    Strings.Assign( name, key );
	    impObj := importerObject;
	    next := list^.head;
	    list^.head := element;
	END;
    END Insert;

PROCEDURE Equal(
    left  : ARRAY OF CHAR;
    right : ARRAY OF CHAR
    )     : BOOLEAN;
    VAR
	i    : SysTypes.Card;
	high : SysTypes.Card;
    BEGIN (* Equal *)
	IF HIGH( left ) > HIGH( right ) THEN
	    high := HIGH( right );
	ELSE
	    high := HIGH( left );
	END;

	FOR i := 0 TO high DO
	    IF left[ i ] # right[ i ] THEN
		RETURN FALSE;
	    END;
	    IF left[ i ] = 0C THEN	(* Same and both null! *)
		RETURN TRUE;
	    END;
	END;

	IF HIGH( left ) = HIGH( right ) THEN
	    (* All values equal and high's equal *)
	    RETURN TRUE;
	ELSIF HIGH( left ) > HIGH( right ) THEN
	    (* Left is statically larger.  If 0C at just past right's high.. *)
	    RETURN left[ HIGH( right ) + 1 ] = 0C;
	END;
	(* Right is statically larger.  If 0C at just past left's high.. *)
	RETURN right[ HIGH( left ) + 1 ] = 0C;
    END Equal;

PROCEDURE Find(
        list           : Object;       
        name           : ARRAY OF CHAR;
    VAR importerObject : SysTypes.ANYPTR
    )                  : BOOLEAN;     
    VAR
	element : Element;
    BEGIN (* Find *)
	element := list^.head;
	WHILE element # NIL DO
	    WITH element^ DO
		IF Equal( name, key ) THEN
		    importerObject := impObj;
		    RETURN TRUE;
		END;
		element := next; 
		(* WITH INVALID *)
	    END;
	END;
	importerObject := NIL; (* Cause an error in case there are bugs *)
	RETURN FALSE;
    END Find;

END HackNameLists.
