IMPLEMENTATION MODULE MaintenancePages;

	(********************************************************)
	(*							*)
	(*	Support for "maintenance page" screen output	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	3 July 1993			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM MultiScreen IMPORT
    (* type *)	ScreenGroup, VirtualScreen,
    (* proc *)	CreateScreenGroup, CreateVirtualScreen,
		RemoveVirtualScreen, MapToVirtualScreen;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, WriteString, PutOnPage, SetCursor;

(************************************************************************)

TYPE MaintenancePage = VirtualScreen;

VAR
    (* All maintenance pages are collected together as a single group.	*)

    MPGroup: ScreenGroup;

    (* The prompt window appears on every maintenance page.	*)

    prompt: Window;

(************************************************************************)
(*			EXTERNALLY CALLABLE PROCEDURES			*)
(************************************************************************)

PROCEDURE CreateMaintenancePage (VAR (*OUT*) page: MaintenancePage);

    (* Creates a new maintenance page.	*)

    BEGIN
	page := CreateVirtualScreen (MPGroup);
    END CreateMaintenancePage;

(************************************************************************)

PROCEDURE RemoveMaintenancePage (VAR (*INOUT*) page: MaintenancePage);

    (* Destroys all associations between the given page and its screen	*)
    (* windows (but does not close the windows), and permanently	*)
    (* removes this page from the collection of maintenance pages.	*)

    BEGIN
	RemoveVirtualScreen (page);
    END RemoveMaintenancePage;

(************************************************************************)

PROCEDURE Associate (w: Window;  page: MaintenancePage);

    (* Before calling this procedure, both w and page must have been	*)
    (* created.  This procedure ensures that window w is visible on	*)
    (* the screen only when the given maintenance page is active.	*)

    BEGIN
	MapToVirtualScreen (w, page);
    END Associate;

(************************************************************************)
(*			   MODULE INITIALISATION			*)
(************************************************************************)

BEGIN
    MPGroup := CreateScreenGroup (1);
    OpenWindow (prompt, blue, cyan, 24, 24, 0, 79, noframe, nodivider);
    PutOnPage (prompt, 1);
    WriteString (prompt, "  F6 cycle");
    SetCursor (prompt, 0, 51);
    WriteString (prompt, "^P maintenance pages on/off");
END MaintenancePages.
