IMPLEMENTATION MODULE DMA;

	(********************************************************)
	(*							*)
	(*	Procedures to deal with Direct Memory Access	*)
	(*		    input and output.			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

(************************************************************************)
(*									*)
(* HARDWARE ASSUMPTIONS:						*)
(*  DMA transfers to/from main memory are performed by the 8237A DMA	*)
(*  controller.  The DMA controller is at ports 000H-01FH.		*)
(*									*)
(*  A complicating factor is that the DMA controller expects physical	*)
(*  addresses (i.e. it knows nothing about memory segmentation), and	*)
(*  furthermore it can only handle 16-bit addresses.  To get around	*)
(*  this limitation, port 081H is a latch for the higher-order bits of	*)
(*  the address.  Unfortunately, this arrangement means that, when the	*)
(*  DMA controller increments the address during a transfer, any carry	*)
(*  from the low-order 16 bits is not propagated upwards.  The		*)
(*  practical consequence is that the user's data buffer must not cross	*)
(*  a 64 Kbyte boundary in main memory.					*)
(*									*)
(*  Added later: according to information I have just received, each	*)
(*  DMA channel has its own port for holding the high-order part of the	*)
(*  address.  This information has been incorporated into the current	*)
(*  version of the software, but has not been tested (14/2/91).		*)
(*									*)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE;

FROM LowLevel IMPORT
    (* proc *)	OutByte, Physical, LowByte, HighByte, LowWord, HighWord;

FROM MiscPMOS IMPORT
    (* proc *)	EnterCriticalSection, LeaveCriticalSection;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE CARD9 = ARRAY [0..8] OF CARDINAL;

CONST
    DMAchip = 0;
    LatchPort = CARD9 (87H, 83H, 81H, 82H, 00H, 8BH, 89H, 8AH, 8FH);

    (* Remark: note that channel 4 does not appear to have a latch	*)
    (* port.  Note also that I've added a latch port for "channel 8",	*)
    (* which is not in fact a DMA channel so far as I know but has been	*)
    (* given to me as the "refresh" port, and I've put it into this	*)
    (* list so that I won't forget the number in case I ever need it.	*)
    (* Note in any case that the present version of this module only	*)
    (* covers channels 0..3, so that the other numbers are here only as	*)
    (* a reminder for future enhancements to the module.		*)

(************************************************************************)
(*		      DEALING WITH THE 64K LIMITATION			*)
(************************************************************************)

PROCEDURE CheckDMAAddress (Address: ADDRESS;  count: CARDINAL): BOOLEAN;

    (* Returns TRUE iff the given address and count values are		*)
    (* suitable for a DMA transfer.  An unsuitable pair is one where	*)
    (* the transfer would cross a 64K boundary in memory - a case which	*)
    (* the DMA hardware cannot handle.					*)

    VAR first: LONGCARD;

    BEGIN
	first := Physical (Address);
	RETURN HighWord(first) = HighWord(first+LONGCARD(count-1));
    END CheckDMAAddress;

(************************************************************************)

PROCEDURE AllocateDMABuffer (VAR (*OUT*) Address: ADDRESS;  size: CARDINAL);

    (* Like the standard ALLOCATE procedure, but ensures that the	*)
    (* allocated memory does not straddle a 64K boundary.		*)

    VAR NotOK, temp: POINTER TO ADDRESS;

    BEGIN
	NotOK := NIL;  ALLOCATE (Address, size);
	WHILE (Address <> NIL) AND NOT CheckDMAAddress(Address, size) DO

	    (* This one didn't work out, put it on a list of buffers	*)
	    (* to be discarded later, and try again.			*)

	    temp := Address;  temp^ := NotOK;  NotOK := temp;
	    ALLOCATE (Address, size);

	END (*LOOP*);

	(* Deallocate all the buffers that didn't work out. *);

	WHILE NotOK <> NIL DO
	    temp := NotOK;  DEALLOCATE (NotOK, size);
	    NotOK := temp;
	END (*WHILE*);

    END AllocateDMABuffer;

(************************************************************************)
(*			SETTING UP THE DMA CONTROLLER			*)
(************************************************************************)

PROCEDURE LoadDMAparameters (channel, operation: CARDINAL;
				Address: ADDRESS; count: CARDINAL);

    (* Loads the DMA controller with the address and count values	*)
    (* as a preliminary to starting a disk transfer.			*)
    (* The code for "operation" is 0 for verify, 1 for read (i.e. from	*)
    (* device to memory), and 2 for write (from main memory to device).	*)
    (* Other values are illegal.					*)

    (* Note that this procedure requires critical section protection	*)
    (* because, even though the four DMA channels can operate in	*)
    (* parallel, there is only a single mode register and a single	*)
    (* "byte pointer" flipflop.  Semaphores are inappropriate for this	*)
    (* application because the procedure is sometimes called from	*)
    (* inside an interrupt handler.					*)

    CONST
	ChannelEnable = DMAchip + 0AH;
	ModeRegister = DMAchip + 0BH;
	ClearByteFlipFlop = DMAchip + 0CH;

    VAR PhysicalAddress: LONGCARD;
	bottom16bits: CARDINAL;
	AddressRegister, CountRegister: CARDINAL;
	savedPSW: WORD;
	
    BEGIN
	AddressRegister := DMAchip + 2*channel;
	CountRegister := AddressRegister + 1;

	savedPSW := EnterCriticalSection();

	(* Specify the operation to be performed.  The constant 40H	*)
	(* encodes the options: single transfer mode, autoincrementing	*)
	(* address, and no autoinitialisation.				*)

	OutByte (ModeRegister, BYTE(40H + 4*operation + channel));

	(* Clear the "byte pointer" flipflop in the DMA chip, to ensure	*)
	(* that the subsequent writes to the address and count ports	*)
	(* can be sent in a known order.  The actual value written to	*)
	(* this port is irrelevant.					*)

	OutByte (ClearByteFlipFlop, 0);

	(* Load the starting address as a physical address.  Only 16	*)
	(* bits of the physical address will fit in the DMA controller.	*)
	(* The high-order 4 bits must be loaded into a separate latch.	*)
	(* Error to watch out for: can't cross a 64KB boundary.  In the	*)
	(* present version we treat that as the caller's problem.	*)

	PhysicalAddress := Physical (Address);
	bottom16bits := LowWord (PhysicalAddress);

	OutByte (AddressRegister, LowByte (bottom16bits));
	OutByte (AddressRegister, HighByte (bottom16bits));
	OutByte (LatchPort[channel], LowByte (HighWord(PhysicalAddress)));

	(* Load the count register.  The value loaded is one less than	*)
	(* the actual desired count.					*)

	DEC (count);
	OutByte (CountRegister, LowByte(count));
	OutByte (CountRegister, HighByte(count));

	(* Ensure that this DMA channel is enabled, by clearing a bit	*)
	(* in the mask register.					*)

	OutByte (ChannelEnable, BYTE(channel));

	LeaveCriticalSection (savedPSW);

    END LoadDMAparameters;

(************************************************************************)

END DMA.
