	.MODEL large

	PUBLIC InnerKernel$EnterKernel
	PUBLIC InnerKernel$LeaveKernel
	PUBLIC InnerKernel$TaskInit
	PUBLIC InnerKernel$InitMainTask
	PUBLIC InnerKernel$Transfer
	PUBLIC InnerKernel$IOTransfer
	PUBLIC InnerKernel$StartInterruptTask
	PUBLIC InnerKernel$DisconnectFromInterrupt
	PUBLIC InnerKernel$MakeFloatSaveSelector
	PUBLIC InnerKernel$NPXsave
	PUBLIC InnerKernel$NPXrestore
	PUBLIC InnerKernel$PrepareForShutdown

	;****************************************************************)
	;*								*)
	;*			INNERKER.ASM				*)
	;*	This version is for use with compilers that pass	*)
	;*	parameter values on the stack.  We assume that		*)
	;*	the called procedure has the responsibility of		*)
	;*	removing parameters from the stack, and that DX:AX	*)
	;*	is used to return a two-word function result.		*)
	;*	Further assumption: the parameters are pushed onto	*)
	;*	the stack in such a way that the first parameter	*)
	;*	ends up deepest in the stack.				*)
	;*								*)
	;*	The above assumptions should be satisfied with the	*)
	;*	following compilers:					*)
	;*		FST (?) (untested)				*)
	;*		TopSpeed 1.17 (tested)				*)
	;*		TopSpeed 3.10 (tested) - but only when we use	*)
	;*			the stack-passing convention rather	*)
	;*			than the compiler's default method.	*)
	;*								*)
	;*	Note: this version sets up task stacks in such a way	*)
	;*	that SS:0000 is the first address in the stack segment.	*)
	;*	This is not the most logical way to allocate a stack	*)
	;*	segment - I would have preferred to allocate an		*)
	;*	"expand-down" segment ending at SS:FFFF - but it turns	*)
	;*	out to be necessary for TopSpeed 3.10 because that	*)
	;*	compiler's libraries assume that they are allowed to	*)
	;*	write into some "reserved" bytes near the beginning	*)
	;*	of the stack segment.  I'm not yet sure whether this	*)
	;*	is also a good choice with other compilers.  The	*)
	;*	answer depends on whether the compiler or run-time	*)
	;*	system make assumptions about the use of fixed		*)
	;*	locations in the stack segment, and since this is	*)
	;*	usually an undocumented feature the only way to check	*)
	;*	is to watch out for intermittent bugs that could be	*)
	;*	caused by writes to nonexistent parts of the stack	*)
	;*	segment.						*)
	;*								*)
	;*	This is the nonportable part of the PMOS kernel.	*)
	;*	It contains procedures whose implementation depends	*)
	;*	not only on the processor, but also on compiler		*)
	;*	conventions (which registers are saved, etc.).		*)
	;*								*)
	;*	Programmer:	P. Moylan				*)
	;*	Last edited:	22 March 1995				*)
	;*	Status:		Working					*)
	;*								*)
	;****************************************************************)

;************************************************************************)
;*				MODULE DATA				*)
;************************************************************************)

;* Offset in stack segment of the control block for an interrupt task.	*)
;* This is compiler-dependent, since some compilers assume that they	*)
;* have some "reserved" locations at fixed locations in the stack	*)
;* segment.  The choice IntBlock=6 appears to be suitable for TopSpeed	*)
;* version 3, and my tests so far suggest that it's also OK for		*)
;* TopSpeed version 1.17.  I'm not sure about other compilers.		*)

	IntBlock	= 6

;********* Constants related to the 8259A interrupt controllers *********)

	EOI		= 20H		;* code for "end of interrupt"	*)
	Master8259	= 20H		;* port numbers for master and	*)
	Slave8259	= 0A0H		;*  slave interrupt controllers	*)

;*********************** The default data segment ***********************)

_DATA	SEGMENT 'DATA'

    ;* BackgroundSelector is a selector for the interrupted task, in	*)
    ;* the case where an interrupt task is running.  Task switching to	*)
    ;* and from interrupt tasks is done directly by this module, not by	*)
    ;* the higher-level part of the kernel.  Interrupt tasks are always	*)
    ;* allowed to "run to completion" - that is, to their next call to	*)
    ;* IOTransfer - and nested interrupt handlers are not supported.	*)
    ;* (To support them, we would have to make BackgroundSelector a	*)
    ;* stack rather than a simple variable, and this would impair the	*)
    ;* efficiency of interrupt handlers).  To ensure this, interrupt	*)
    ;* tasks must never enable interrupts.				*)
    ;* Convention: if no interrupt task is running, we set the second	*)
    ;* word (the "segment" part) of BackgroundSelector to zero.		*)

    BackgroundSelector	DW	0, 0

_DATA	ENDS

;**************** Start of code segment for this module *****************)

InnerKernel_TEXT	SEGMENT 'CODE'

	ASSUME CS: InnerKernel_TEXT

;************************************************************************)
;* PROCEDURE EnterKernel (): CARDINAL;					*)
;*									*)
;* Saves the processor flags word, including the current "interrupt	*)
;* enable" status, and returns with interrupts disabled.		*)
;* NOTE: this procedure and the following one should be used as a	*)
;* matched pair.							*)
;************************************************************************)

InnerKernel$EnterKernel	PROC FAR

	pushf
	pop ax
	cli
	ret

InnerKernel$EnterKernel	ENDP

;************************************************************************)
;* PROCEDURE LeaveKernel (PSW: CARDINAL);				*)
;*									*)
;* Restores the processor flags word, including the "interrupt enable"	*)
;* status.  NOTE: this procedure and the one above should be used as a	*)
;* matched pair.							*)
;************************************************************************)

InnerKernel$LeaveKernel	PROC FAR

	push bp
	mov bp, sp
	push  ax
	mov ax, 6[bp]		; pick up the parameter value
	push ax
	popf			; transfer it to flags
	pop ax
	pop bp
	ret 2

InnerKernel$LeaveKernel	ENDP

;************************************************************************)
;*			CREATING A NEW TASK				*)
;************************************************************************)

;************************************************************************)
;* PROCEDURE TaskInit (StackBase: ADDRESS;  StackSize: CARDINAL;	*)
;*			EnableInterrupts: BOOLEAN;			*)
;*			TaskExit, StartAddress: PROC): TaskSelector;	*)
;*									*)
;* Initialises the stack for a new task.  Parameter StackBase		*)
;* points to a block of memory which can be used to hold the stack	*)
;* (note that this is a pointer to the start of the memory block,	*)
;* not to the bottom of the stack); and StackSize is the size of	*)
;* this block.  The next parameter specifies whether processor		*)
;* interrupts should be enabled when the task is started.  StartAddress	*)
;* and TaskExit are the start address of the task code and the start	*)
;* address of the code to execute when the task terminates.  The	*)
;* value returned is a selector for the new task.			*)
;*									*)
;* Note that the new task will commence execution as the result of a	*)
;* call to procedure StackSwap by some other task; therefore we must	*)
;* set up the stack to make it look as if the new task had just called	*)
;* StackSwap in the middle of Transfer; and the return address		*)
;* seen by Transfer must be the address at which we wish execution to	*)
;* commence.  This means that the initial stack must contain 21 words:	*)
;*									*)
;*	SS:SP->	address TaskStart within procedure Transfer (1 word)	*)
;*		initial DS (1 word)					*)
;*		other dummy saved registers (9 words)			*)
;*		processor flags (1 word)				*)
;*		initial BP (1 word)					*)
;*		StartAddress (2 words)					*)
;*		dummy TaskStart parameters (4 words)			*)
;*		TaskExit (2 words)					*)
;*									*)
;* The point of putting the address of TaskExit at the bottom of the	*)
;* stack is to act as a return address if the new task falls out the	*)
;* bottom of its code.							*)
;*									*)
;* In earlier versions of this module, the stack was set up so that	*)
;* the bottom byte in the stack was at address SS:FFFF, a choice which	*)
;* would have simplified porting this code to a Protected Mode version	*)
;* using "expand-down" stack segments.  Unfortunately this decision	*)
;* turned out to be incompatible with the TopSpeed floating point	*)
;* library; some of the routines in that library store information in	*)
;* locations SS:0001 to SS:0004 inclusive, so we have to make sure	*)
;* that those locations are in a task's stack space.  For this reason,	*)
;* we set up the stack so that its lowest address is SS:0000, and we	*)
;* initially clear the following locations:				*)
;*									*)
;*	SS:0000		unused (1 byte)					*)
;*	SS:0001		reserved for floating point library (4 bytes)	*)
;*	SS:0005		unused (1 byte)					*)
;*									*)
;* With compilers other than TopSpeed the above is redundant, but we	*)
;* reserve those locations anyway for the sake of having a single	*)
;* version of this module for multiple compilers.  The redundant part	*)
;* should be harmless.							*)
;*									*)
;* For interrupt tasks, some extra information must be stored, and	*)
;* view of the above design decision the most convenient place to store	*)
;* it is in a record starting at location SS:0006.  See procedure	*)
;* StartInterruptTask for the details of this control block.		*)
;*									*)
;* For non-interrupt tasks, a buffer at SS:000E is used for saving the	*)
;* task's floating point state.  This is compatible with TopSpeed	*)
;* version 3; I'm not yet sure about other compilers.			*)
;*									*)
;* Parameters:	StackBase:	doubleword at 18[bp]			*)
;*		StackSize:	word at 16[bp]				*)
;*		EnableInterrupts: byte at 14[bp]			*)
;*		TaskExit:	doubleword at 10[bp]			*)
;*		StartAddress:	doubleword at 6[bp]			*)
;*		result to be returned in DX:AX				*)
;*									*)
;************************************************************************)

InnerKernel$TaskInit	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp
        push di			;* save some registers		*)
        push si
        push ds
        push es
        push bx
        push cx
        mov ax, 18[bp]		; pick up StackBase
        mov bx, 20[bp]		;  (two words)
        mov cx, 16[bp]		; pick up StackSize (one word)

	;* Round up the stack address so that we start at a segment	*)
	;* boundary, and make sure that the last word in the stack has	*)
	;* an even address.  This could lose us up to 16 bytes of	*)
	;* memory, but that is a small price to pay if StackSize is not	*)
	;* too small.							*)

	mov si, 15		;* a useful constant		*)
	add ax, si		;* add enough to the stack	*)
	jnc Init10		;*  address to make it a	*)
	add bx, 1000H		;*   multiple of 16		*)
Init10:	sub cx, si		;* reduce the stack size by the	*)
	and si, ax		;*  same amount			*)
	add si, cx		;* SI now holds stack size	*)
	mov cx, 4
	shr ax, cl		;* divide offset by 16, and add	*)
	add ax, bx		;*  that to the segment value	*)
	mov bx, 0FFFEH
	and bx, si		;* make stack size even		*)
	sub bx, 42		;* allow for initial contents	*)
	mov es, ax		;* new stack pointer is ES:BX	*)

	;* Clear the three reserved words at the start of the stack	*)
	;* segment, in case we are using the TopSpeed floating point	*)
	;* library.							*)

	pushf			;* save direction flag		*)
	cld			;* set direction = forwards	*)
	xor ax, ax
	mov di, ax		;* start at location ES:0000	*)
	mov cx, 3
	rep stosw		;* clear three words		*)

	;* Now load the initial stack contents, using ES:DI temporarily	*)
	;* as a stack pointer.  A copy of the stack pointer is in BX.	*)

	mov di, bx		;* initial stack pointer	*)
	mov ax, OFFSET TaskStart ;* task starting point		*)
	stosw
	mov ax, _DATA		;* initial DS for task		*)
	stosw
	sub ax, ax
	mov cx, 9		;* nine dummy "saved registers"	*)
	rep stosw		;* (we arbitrarily make them 0)	*)
	or al, 14[bp]		;* set the initial flags	*)
	jz Init20		;*   to an all-zero word,	*)
	mov ax, 200H		;*   except for "interrupt	*)
Init20:	stosw			;*   enable" where specified	*)
	sub ax, ax		;* dummy "saved BP"		*)
	stosw
	mov ax, ss		;* make DS:SI point to other	*)
	mov ds, ax		;*   parameters			*)
	lea si, 6[bp]
	movsw			;* StartAddress (2 words)	*)
	movsw
	sub ax, ax
	mov cx, 4
	rep stosw		;* dummy parameters (4 words)	*)
	movsw			;* TaskExit (2 words)		*)
	movsw
	popf			;* restore direction flag	*)
		
	;* All done.  The stack pointer for this newly created	*)
	;* stack is now in ES:BX, and this is what we return to	*)
	;* the caller as the selector value.			*)

	mov dx, es		;* result to DX:AX		*)
	mov ax, bx	
	pop cx
	pop bx
	pop es			;* restore saved registers	*)
	pop ds
	pop si
	pop di
	pop bp
	ret 16

InnerKernel$TaskInit	ENDP
	
;************************************************************************)
;* PROCEDURE InitMainTask (): TaskSelector;				*)
;*									*)
;* Like TaskInit, but for the special case of the original task which	*)
;* is running at program startup.  The function of this procedure is	*)
;* simply to ensure that the main stack layout is consistent with what	*)
;* we do for all other tasks.  A selector is returned in DX:AX.		*)
;************************************************************************)

InnerKernel$InitMainTask	PROC	FAR

	sub ax, ax
	mov dx, ss		;* return SS:0000 as the result selector *)
	ret 0

InnerKernel$InitMainTask	ENDP
	
;************************************************************************)
;*			THE TASK SWITCH OPERATION			*)
;************************************************************************)

StackSwap	PROC NEAR

	;* Local procedure to switch to a new stack.  Because we return	*)
	;* using a return address taken from the NEW stack, this	*)
	;* procedure does not directly return to the caller.  However,	*)
	;* some future call to this procedure will cause a switch back	*)
	;* to the original caller, so from the caller's point of view	*)
	;* this just looks like a procedure which takes an unusually	*)
	;* long time to execute.  The intervening execution of other	*)
	;* tasks does, however, mean that the caller must assume that	*)
	;* all registers have been corrupted.				*)
	
	;* Enter with interrupts disabled, with BX:AX holding the	*)
	;* desired new SS:SP, and with DS:SI pointing to the doubleword	*)
	;* where we should save the caller's SS:SP.			*)

	mov [si], sp		;* save the current SS:SP	*)
	mov [si][2], ss
	mov ss, bx		;* switch to new stack		*)
	mov sp, ax
	ret 0			;* return to destination task	*)

StackSwap	ENDP
	
;************************************************************************)
;* PROCEDURE Transfer (VAR (*OUT*) source: TaskSelector;		*)
;*					destination: TaskSelector);	*)
;*									*)
;* Performs a task switch to the destination task, at the same time	*)
;* saving a selector for the outgoing task in variable "source".	*)
;* This allows a subsequent call to Transfer to resume the		*)
;* original task.  By the time this procedure has returned to the	*)
;* caller, then, we are again executing the calling task.		*)
;*									*)
;* Special case: if this procedure is called by an interrupt task, the	*)
;* call is interpreted as a requiring a task switch from the		*)
;* interrupted task - i.e. the source parameter must specify the	*)
;* interrupted task - to the destination task.  In this case the actual	*)
;* switch to the destination task does not happen until the interrupt	*)
;* task makes its next call to IOTransfer.  The reason for this		*)
;* interpretation is that task switching to and from interrupt tasks is	*)
;* managed internally by this module, because the occurrence of an	*)
;* interrupt is not something that can be controlled by the caller.	*)
;************************************************************************)

InnerKernel$Transfer	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp

	; Parameter addresses (all offsets expressed in decimal):
	;	ADR(source)	doubleword	at 10[bp]
	;	destination	doubleword	at 6[bp]

	pushf			;* save flags			*)
	db 60H	;* pusha *)	;* save all registers		*)
	push es
	push ds
	mov si, 10[bp]		; pick up address of source
	mov ds, 12[bp]
	mov ax, 6[bp]		; pick up destination selector
	mov bx, 8[bp]
	mov cx, _DATA
	mov es, cx		;* point to BackgroundSelector	*)
	mov di, OFFSET BackgroundSelector+2
	mov cx, es:[di]		;* a nonzero second word	*)
	test cx, cx		;*   means that an interrupt	*)
	jnz Tran20		;*   task is running		*)
	call StackSwap		;* do the task switch		*)
	
	;* By the time we get back here, there will have been	*)
	;* two or more task switches, and the original task	*)
	;* will again be running.				*)
	;* This is also the point at which new tasks start.	*)
	
TaskStart:	
	pop ds
	pop es
	db 61H	;* popa *)
	popf
	pop bp
	ret 8

	;* If an interrupt task is running, we defer the task	*)
	;* switch, but update BackgroundSelector so that the	*)
	;* task switch will happen the next time IOTransfer is	*)
	;* called.  Note that in this situation the source	*)
	;* parameter in DS:SI points to the selector for the	*)
	;* interrupted task, not the interrupt task.		*)

Tran20:
	mov [si][2], cx		;* let BackgroundSelector	*)
	mov es:[di], bx		;*   select the target task,	*)
	dec di
	dec di			;*   and store the old value of	*)
	mov cx, es:[di]		;*   BackgroundSelector as the	*)
	mov [si], cx		;*   new source selector	*)
	mov es:[di], ax
	jmp TaskStart

InnerKernel$Transfer	ENDP

;************************************************************************)
;* PROCEDURE IOTransfer;						*)
;*									*)
;* May be called only from an interrupt task.  Performs a task		*)
;* switch from the current interrupt task to the task identified	*)
;* by BackgroundSelector.  Unlike Transfer, no parameters are		*)
;* required because (a) the selector for the destination task is	*)
;* already in BackgroundSelector, having been put there at the time	*)
;* the interrupt occurred; and (b) selectors for interrupt tasks are	*)
;* maintained directly by this module rather than by TaskControl.	*)
;************************************************************************)

InnerKernel$IOTransfer	PROC FAR

	pushf			;* save flags			*)
	db 60H	;* pusha *)	;* save all registers		*)
	push es
	push ds
	mov ax, _DATA		;* make DS:SI a pointer		*)
	mov ds, ax		;*    to BackgroundSelector	*)
	mov si, OFFSET BackgroundSelector
	cld
	lodsw			;* fetch the value of		*)
	mov bx, [si]		;*    BackgroundSelector	*)
	mov WORD PTR [si], 0	;* clear BackgroundSelector	*)
	mov cx, ss		;* make DS:SI a pointer to the	*)
	mov ds, cx		;*    selector location in our	*)
	mov si, IntBlock+0AH	;*    stack segment		*)
	call StackSwap		;* do the task switch		*)

	;* We reach here on the next interrupt.  For interrupt	*)
	;* requests which are mapped through the 8259A		*)
	;* controllers, we need to send a "nonspecific EOI"	*)
	;* code to the master and/or slave controllers.		*)

	mov ah, EOI		;* this is the EOI code		*)
	mov al, ss:[IntBlock]	;* check the interrupt number	*)
	cmp al, 8		;* for interrupt numbers in	*)
	jb IOT20		;*   the range 8..15 inclusive,	*)
	cmp al, 0FH		;*   we must send the EOI to	*)
	jbe IOT10		;*   the master 8259A		*)
	cmp al, 70H		;* for interrupt numbers in	*)
	jb IOT20		;*   the range 70H..77H,	*)
	cmp al, 77H		;*   we must kick first the	*)
	ja IOT20		;*   slave and then the master	*)
	mov al, ah
	out Slave8259, al	;* EOI to slave controller	*)
	mov al, 11
	out Slave8259, al	;* read the in-service register	*)
	in al, Slave8259	;*   of the slave controller	*)
	test al, al		;* skip the EOI to master if	*)
	jnz IOT20		;*   nonzero value in ISR	*)
IOT10:	mov al, ah
	out Master8259, al	;* EOI to master controller	*)

	;* Return to the main body of the interrupt task code.	*)

IOT20:	pop ds
	pop es
	db 61H	;* popa *)
	popf
	ret 0

InnerKernel$IOTransfer	ENDP

;************************************************************************)
;*		INSTALLING AND DE-INSTALLING INTERRUPT TASKS		*)
;************************************************************************)

;************************************************************************)
;* PROCEDURE StartInterruptTask (TS: TaskSelector;			*)
;*					InterruptNumber: CARDINAL);	*)
;*									*)
;* Starts an interrupt task by running its initialisation section	*)
;* - i.e. everything up to the first IOTransfer - and arranging that	*)
;* from then on it will be activated by the given interrupt.		*)
;*									*)
;* This module stores what it needs to know about an interrupt task	*)
;* in a small control block which is kept near the beginning of that	*)
;* task's stack segment.  The control block contents are as follows.	*)
;*									*)
;*	SS:IntBlock	interrupt number (1 byte)			*)
;*	SS:IntBlock+1	a far CALL to procedure IntHandler (5 bytes)	*)
;*	SS:IntBlock+6	saved interrupt vector (2 words)		*)
;*	SS:IntBlock+0AH	selector: a saved SS:SP (2 words)		*)
;*									*)
;* The interrupt vector for an interrupt task is set to point to	*)
;* the CALL at location SS:IntBlock+1.  This allows IntHandler to	*)
;* determine which stack segment to switch to, and hence implicitly	*)
;* which interrupt task to switch to on the occurrence of an interrupt.	*)
;* The saved SS in this data block looks redundant, but it is there for	*)
;* consistency with the selector format used for ordinary tasks.	*)
;*									*)
;************************************************************************)

InnerKernel$StartInterruptTask	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp

	; Parameter addresses (all offsets expressed in decimal):
	;	TS		doubleword	at 8[bp]
	;	InterruptNumber	word		at 6[bp]

	pushf			;* save all registers and flags		*)
	db 60H	;* pusha *)	;* (We need to save everything, since	*)
	push es			;*  the task switch below potentially	*)
	push ds			;*  corrupts all register contents)	*)

	mov ax, 8[bp]		; pick up parameter values
	mov bx, 10[bp]
	mov cx, 6[bp]

	push cx			;* save interrupt number		*)
	push bx			;* save a copy of task's stack segment	*)
	mov dx, _DATA		;* make DS:SI a pointer			*)
	mov ds, dx		;*   to BackgroundSelector		*)
	mov si, OFFSET BackgroundSelector
	call StackSwap		;* run the interrupt task		*)

	;* When we get back to here, the interrupt task has run to its	*)
	;* first call to IOTransfer.  We now set up the control block	*)
	;* in the interrupt task's stack segment.  Note: procedure	*)
	;* IOTransfer has already saved a selector in that block.	*)

	cli			;* disable interrupts		*)
	cld			;* set direction = forwards	*)
	pop es			;* select task's stack segment	*)
	mov di, IntBlock
	pop ax			;* restore interrupt number	*)
	mov si, ax		;* save a copy of it in si	*)
	mov ah, 9AH		;* this is a "far call" opcode	*)
	stosw			;* store these in control block	*)
	mov ax, OFFSET IntHandler ;* address field for the	*)
	stosw			;*    call instruction		*)
	mov ax, cs		;*    (two words)		*)
	stosw
	shl si, 1		;* calculate 4 times interrupt	*)
	shl si, 1		;*   number			*)
	xor ax, ax		;* let DS:SI point to the	*)
	mov ds, ax		;*   interrupt vector		*)
	movsw			;* copy interrupt vector (2	*)
	mov ax, [si]		;*   words) into save area in	*)
	stosw			;*   interrupt task's stack	*)
	mov [si], es		;* reload interrupt vector to	*)
	dec si
	dec si			;*   point to CALL instruction	*)
	mov WORD PTR [si],IntBlock+1  ;*   in the control block	*)

	pop ds			;* restore all saved registers	*)
	pop es
	db 61H	;* popa *)
	popf
	pop bp
	ret 6

InnerKernel$StartInterruptTask	ENDP
	
;************************************************************************)
;* PROCEDURE DisconnectFromInterrupt (TS: TaskSelector);		*)
;*									*)
;* Restores the interrupt vector to which TS was connected to its	*)
;* state before TS was established as an interrupt task.  (N.B. The	*)
;* result could be chaotic if there was no previous call to		*)
;* ConnectToInterrupt.)							*)
;* Remark: only the "segment" part of TS is used by this procedure.	*)
;************************************************************************)

InnerKernel$DisconnectFromInterrupt	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp
	push ax
	push ds
	push es			;* save some registers			*)
	push si
	push di

	mov ds, 8[bp]		;* select task's stack segment		*)
	mov al, DS:[IntBlock]	;* get interrupt number			*)
	sub ah, ah
	mov si, IntBlock+6	;* DS:SI points to saved int vector	*)
	shl ax, 1
	shl ax, 1		;* calculate 4 times interrupt number	*)
	mov di, ax		;* let ES:DI point to actual		*)
	xor ax, ax		;*    interrupt vector			*)
	mov es, ax
	pushf			;* save processor flags			*)
	cli			;* disable interrupts			*)
	cld			;* direction = forwards			*)
	movsw			;* restore original value of interrupt	*)
	movsw			;*   vector from save area in interrupt	*)
				;*   task's stack			*)
	popf			;* restore processor flags		*)
	pop di
	pop si			;* restore saved registers		*)
	pop es
	pop ds
	pop ax
	pop bp
	ret 4

InnerKernel$DisconnectFromInterrupt	ENDP
	
;************************************************************************)
;*			THE GENERIC INTERRUPT HANDLER			*)
;************************************************************************)

IntHandler	PROC

	;* This is the interrupt routine for any interrupt task.  On an	*)
	;* interrupt, the first instruction executed is a far CALL to	*)
	;* this procedure from a location in the interrupt task's stack	*)
	;* segment.  By looking at the return address, we can work out	*)
	;* which stack segment to use, and therefore which task to	*)
	;* switch to.  This sneaky trick does unfortunately mean that	*)
	;* the code in this procedure is a little obscure.		*)

	db 60H	;* pusha *)		;* save AX..DI			*)
	mov bp, sp			;* we save DS, and later ES,	*)
	mov [bp][10H], ds		;* UNDER the other registers	*)
	mov bx, es			;* save old ES in stack by	*)
	xchg bx, [bp][12H]		;*   swapping it with segment	*)
					;*   part of the return address	*)
	mov ds, bx			;* load the target stack	*)
	mov ax, DS:[IntBlock+0AH]	;*   pointer into BX:AX		*)
	mov cx, _DATA			;* make DS:SI a pointer		*)
	mov ds, cx			;*   to BackgroundSelector	*)
	mov si, OFFSET BackgroundSelector
	call StackSwap			;* switch to interrupt task	*)

	;* On return from the above call, the interrupt task has	*)
	;* completed its operation, and we are back in the interrupted	*)
	;* task.  If there appears to be a mismatch between the way we	*)
	;* now restore registers and the way we originally saved them,	*)
	;* it is because we have discarded the return address which was	*)
	;* saved by the CALL in the interrupt task's stack segment.	*)
	
	db 61H	;* popa *)
	pop ds
	pop es
	iret

IntHandler	ENDP

;************************************************************************)
;*			FLOATING POINT SAVE/RESTORE			*)
;************************************************************************)

;************************************************************************)
;* PROCEDURE MakeFloatSaveSelector (selector: TaskSelector)		*)
;*						: FloatSaveSelector;	*)
;*									*)
;* Creates the special form of selector which must subsequently be used	*)
;* in calls to save and restore the floating point state.  The		*)
;* parameter supplied must be the value of the task selector as created	*)
;* by TaskInit.								*)
;************************************************************************)

InnerKernel$MakeFloatSaveSelector	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp

	; Parameter addresses (all offsets expressed in decimal):
	;	selector	doubleword	at 6[bp]

	mov ax, 8[bp]		;* return segment part of argument
	pop bp			;*    as the result
	ret 4

InnerKernel$MakeFloatSaveSelector	ENDP

;************************************************************************)
;* PROCEDURE NPXsave (selector: FloatSaveSelector);			*)
;*									*)
;* Saves the state of the Numeric Processor Extension coprocessor.	*)
;* (Has no effect, apart from a short time delay, if there is no	*)
;* coprocessor present.)						*)
;************************************************************************)

InnerKernel$NPXsave	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp
	push ds
	push si

	; Parameter addresses (all offsets expressed in decimal):
	;	selector	word	at 6[bp]
	; We use this selector as a segment selector, and save the
	; floating point state at location 14 of that (stack) segment.

	mov si, 14		; put buffer address
	mov ds, 6[bp]		;   into DS:SI
	fnsave [si]		; save floating point state
	wait			; wait for completion
	pop si
	pop ds
	pop bp
	ret 2

InnerKernel$NPXsave	ENDP

;************************************************************************)
;* PROCEDURE NPXrestore (selector: FloatSaveSelector);			*)
;*									*)
;* The operation complementary to NPXsave.  Restores the previously	*)
;* saved state of the floating point coprocessor.			*)
;************************************************************************)

InnerKernel$NPXrestore	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp
	push ds
	push si

	; Parameter addresses (all offsets expressed in decimal):
	;	selector	word	at 6[bp]

	mov si, 14		; put buffer address
	mov ds, 6[bp]		;   into DS:SI
	wait			; wait for FPU ready
	frstor [si]		; save floating point state
	pop si
	pop ds
	pop bp
	ret 2

InnerKernel$NPXrestore	ENDP

;************************************************************************)
;* PROCEDURE PrepareForShutdown;					*)
;*									*)
;* This procedure is called just before executing the "exit from	*)
;* program" operation, and is provided to get around a problem with	*)
;* some memory models in TopSpeed version 3.  The present version is	*)
;* a dummy.								*)
;************************************************************************)

InnerKernel$PrepareForShutdown	PROC FAR

	ret 0

InnerKernel$PrepareForShutdown	ENDP

;************************************************************************)
;*			MODULE INITIALISATION				*)
;************************************************************************)

; This is a problem area, since most assemblers don't support the concept
; of a "module initialisation" section.  What the initialisation code
; SHOULD do is set the second word of variable BackgroundSelector to zero.
; What we do in this version is to set up BackgroundSelector as
; initialised data - this should serve the purpose, because it means that
; that word will hold zero when the program is first loaded.

;************************************************************************)

InnerKernel_TEXT	ENDS

END
