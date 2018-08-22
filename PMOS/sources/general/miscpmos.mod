IMPLEMENTATION MODULE MiscPMOS;

	(************************************************)
	(*						*)
	(*	Miscellaneous PMOS procedures		*)
	(*						*)
	(*  Programmer:         P. Moylan		*)
	(*  Last edited:        24 August 1994		*)
	(*  Status:             OK			*)
	(*	The FST code is incompletely tested.	*)
	(*						*)
	(************************************************)

FROM SYSTEM IMPORT BYTE (*<FST , ASSEMBLER >*);

(*<TopSpeed*)
FROM SYSTEM IMPORT
    (* proc *)  DI, SetFlags, GetFlags;

FROM Lib IMPORT
    (* proc *)  Intr;
(*>*)

(*<FST IMPORT System; >*)

FROM LowLevel IMPORT
    (* proc *)	LowByte, InByte, OutByte;

(************************************************************************)
(*				STRING COPY				*)
(************************************************************************)

PROCEDURE CopyString (source: ARRAY OF CHAR;  VAR (*OUT*) dest: ARRAY OF CHAR);

    (* Copies a string, with truncation or null termination as needed.	*)
    (* This function is provided in order to help software portability,	*)
    (* i.e. to avoid having to rewrite code for no reason other than	*)
    (* a change of compilers.						*)

    VAR j, last: CARDINAL;  AddNull: BOOLEAN;

    BEGIN
	last := HIGH(dest);
	AddNull := HIGH(source) < last;
	IF AddNull THEN last := HIGH(source) END (*IF*);
	FOR j := 0 TO last DO dest[j] := source[j] END (*FOR*);
	IF AddNull THEN dest[last+1] := CHR(0) END (*IF*);
    END CopyString;

(************************************************************************)
(*                      PROCEDURES TO ACCESS CMOS                       *)
(************************************************************************)

CONST
    CMOSAddressPort = 70H;
    CMOSDataPort = CMOSAddressPort + 1;

(************************************************************************)

PROCEDURE ReadCMOS (location: CMOSaddress): BYTE;

    (* Returns the value at the given CMOS location.    *)

    BEGIN
	OutByte (CMOSAddressPort, LowByte(location));
	RETURN InByte (CMOSDataPort);
    END ReadCMOS;

(************************************************************************)

PROCEDURE WriteCMOS (location: CMOSaddress;  value: BYTE);

    (* Stores a value at the given CMOS location.       *)

    BEGIN
	OutByte (CMOSAddressPort, LowByte(location));
	OutByte (CMOSDataPort, value);
    END WriteCMOS;

(************************************************************************)
(*                        BIOS/MS-DOS CALLS                             *)
(************************************************************************)

PROCEDURE BIOS (InterruptNumber: CARDINAL;
			VAR (*INOUT*) Registers: RegisterPacket);

    (* Performs a software interrupt, with the given interrupt number,  *)
    (* after loading the components of variable "Registers" into the    *)
    (* machine registers.  After the handler returns, the updated       *)
    (* register values are put back into variable "Registers".          *)

    BEGIN
	(*<TopSpeed*) Intr (Registers, CARDINAL(InterruptNumber)); (*>*)
	(*<FST
	System.AX := Registers.AX;
	System.BX := Registers.BX;
	System.CX := Registers.CX;
	System.DX := Registers.DX;
	System.BP := Registers.BP;
	System.SI := Registers.SI;
	System.DI := Registers.DI;
	System.DS := Registers.DS;
	System.ES := Registers.ES;
	System.XTrap (InterruptNumber);
	Registers.AX := System.AX;
	Registers.BX := System.BX;
	Registers.CX := System.CX;
	Registers.DX := System.DX;
	Registers.BP := System.BP;
	Registers.SI := System.SI;
	Registers.DI := System.DI;
	Registers.DS := System.DS;
	Registers.ES := System.ES;
	>*)
    END BIOS;

(************************************************************************)
(*                   MISCELLANEOUS LOW-LEVEL OPERATIONS                 *)
(************************************************************************)

PROCEDURE EnterCriticalSection (): CARDINAL;

    (* Saves the processor flags word, including the current "interrupt	*)
    (* enable" status, on the caller's stack, and returns with		*)
    (* interrupts disabled.   NOTE: this procedure and the following    *)
    (* one should be used as a matched pair.                            *)

    VAR SavedProcessorStatus: CARDINAL;

    BEGIN

	(*<TopSpeed*)
	SavedProcessorStatus := GetFlags ();
	DI();
	(*>*)

	(*<FST
	ASM
	    PUSHF
	    POP AX
	    MOV SavedProcessorStatus, AX
	    CLI
	END (*ASM*);
	>*)

	RETURN SavedProcessorStatus;

    END EnterCriticalSection;

(************************************************************************)

PROCEDURE LeaveCriticalSection (SavedProcessorStatus: CARDINAL);

    (* Restores the processor flags word, including the "interrupt	*)
    (* enable" status, from the stack.  NOTE: this procedure and the	*)
    (* one above should be used as a matched pair.                      *)

    BEGIN

	(*<TopSpeed*)
	SetFlags (SavedProcessorStatus);
	(*>*)

	(*<FST
	ASM
	    MOV AX, SavedProcessorStatus
	    PUSH AX
	    POPF
	END (*ASM*);
	>*)

    END LeaveCriticalSection;

(************************************************************************)

PROCEDURE ProcessorStatus(): CARDINAL;

    (* Returns the current value of the processor flags word.   *)

    VAR result: CARDINAL;

    BEGIN
	(*<TopSpeed*)
	RETURN GetFlags();
	(*>*)

	(*<FST
	ASM
	    PUSHF
	    POP AX
	    MOV result, AX
	END (*ASM*);
	RETURN result;
	>*)
    END ProcessorStatus;

(************************************************************************)

PROCEDURE ShortDelay (amount: CARDINAL);

    (* Provides a time delay for those cases where the required delay   *)
    (* is not long enough to justify a Sleep() operation.               *)
    (* The present version is not entirely satisfactory - needs to be   *)
    (* re-tuned for different compiler options, different processor     *)
    (* models, etc.  This should be seen as an interim solution only.   *)

    VAR j: CARDINAL;

    BEGIN
	FOR j := 1 TO amount DO
	END (*FOR*);
    END ShortDelay;

(************************************************************************)

END MiscPMOS.
