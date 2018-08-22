	.MODEL large

	PUBLIC InnerKernel$EnterKernel
	PUBLIC InnerKernel$LeaveKernel
	PUBLIC InnerKernel$TaskInit
	PUBLIC InnerKernel$Transfer
	PUBLIC InnerKernel$IOTransfer
	PUBLIC InnerKernel$StartInterruptTask
	PUBLIC InnerKernel$DisconnectFromInterrupt
	PUBLIC InnerKernel$NPXsave
	PUBLIC InnerKernel$NPXrestore

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
	;*	ends up closest to the top.  (There's another version	*)
	;*	of this module that makes the opposite assumption.)	*)
	;*								*)
	;*	This is the nonportable part of the PMOS kernel.	*)
	;*	It contains procedures whose implementation depends	*)
	;*	not only on the processor, but also on compiler		*)
	;*	conventions (which registers are saved, etc.).		*)
	;*								*)
	;*	Programmer:	P. Moylan				*)
	;*	Last edited:	19 August 1994				*)
	;*								*)
	;*	Status:		Not working - it turns out that		*)
	;*		TopSpeed v1.17 does not satisfy the above	*)
	;*		assumptions re parameter passing.  I'm now	*)
	;*		switched to another copy of this module that	*)
	;*		makes different assumptions.			*)
	;*								*)
	;*		THIS VERSION IS PROVIDED SOLELY AS A		*)
	;*		GUIDE FOR PEOPLE WHO WISH TO PORT PMOS		*)
	;*		TO ANOTHER SYSTEM.  IT HAS NOT BEEN TESTED.	*)
	;*								*)
	;****************************************************************)

;************************************************************************)
;*				MODULE DATA				*)
;************************************************************************)

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
;*			ENTERING AND LEAVING THE KERNEL			*)
;************************************************************************)

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
;* commence.  This means that the initial stack must look as follows:	*)
;*									*)
;*	SS:SP->	address TaskStart within procedure Transfer		*)
;*		initial DS						*)
;*		other dummy saved registers (9 words)			*)
;*		processor flags						*)
;*		StartAddress						*)
;*		TaskExit						*)
;*									*)
;* The point of putting the address of TaskExit at the bottom of the	*)
;* stack is to act as a return address if the new task falls out the	*)
;* bottom of its code.							*)
;*									*)
;* For interrupt tasks, some extra information must be stored, and the	*)
;* most convenient place to store it is at the very bottom of the stack	*)
;* (i.e. underneath the address of TaskExit).  To keep life simple, we	*)
;* choose to store this information for all tasks - even though it is	*)
;* unused by ordinary tasks - because the overhead is low.  This extra	*)
;* information has the form:						*)
;*									*)
;*	SS:FFF2		interrupt number (1 byte)			*)
;*	SS:FFF3		a far CALL to procedure IntHandler (5 bytes)	*)
;*	SS:FFF8		saved interrupt vector (2 words)		*)
;*	SS:FFFC		selector: a saved SS:SP (2 words)		*)
;*									*)
;* The interrupt vector for an interrupt task will be set to point to	*)
;* the CALL at the bottom of the stack.  This allows IntHandler to	*)
;* determine which stack segment to switch to, and hence implicitly	*)
;* which interrupt task to switch to on the occurrence of an interrupt.	*)
;* The saved SS at the very end of the stack segment looks redundant,	*)
;* but it is there for consistency with the selector format used for	*)
;* ordinary tasks.							*)
;*									*)
;* Note: the selector at the end of the stack segment is used only for	*)
;* interrupt tasks.  For an ordinary task its value is meaningless.	*)
;* In fact for an ordinary task, especially one which makes DOS calls,	*)
;* it would be unsafe to assume that there was anything special at the	*)
;* end of the stack segment.						*)
;*									*)
;************************************************************************)

InnerKernel$TaskInit	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp

	; Parameter addresses (all offsets expressed in decimal):
	;	StackBase: ADDRESS	doubleword	at 6[bp]
	;	StackSize: CARDINAL	word		at 10[bp]
	;	EnableInterrupts: BOOLEAN byte		at 12[bp]
	;	TaskExit: PROC		doubleword	at 14[bp]
	;	StartAddress: PROC	doubleword	at 18[bp]

        push di			;* save some registers		*)
        push si
        push ds
        push es
        push bx
        push cx
        mov ax, 6[bp]		; pick up StackBase
        mov bx, 8[bp]		;  (two words)
        mov cx, 10[bp]		; pick up StackSize (one word)
        mov dx, 12[bp]		; pick up EnableInterrupts (one byte,
        			;   plus one byte of padding)

	;* Fiddle the address and size for the new stack in such a way	*)
	;* that the last word in the stack has address seg:0FFFEH	*)
	;* (while keeping the physical address invariant).  This could	*)
	;* lose us up to 15 bytes of memory, but that is a small price	*)
	;* to pay if StackSize is not too small.  This "normalisation"	*)
	;* simplifies our treatment of interrupt tasks, in that		*)
	;* information we store at the bottom of the stack is at a	*)
	;* known offset, independent of stack size, relative to the	*)
	;* start of the stack segment.  It also has the advantages of	*)
	;* (a) ensuring that the stack pointer is even, which aids	*)
	;* execution efficiency; (b) simplifying the porting of PMOS to	*)
	;* other versions, such as a Protected Mode version.		*)
	
	add ax, cx		;* StackBase.offset + StackSize	*)
	mov cl, 4
	shr ax, cl		;* divide this by 16		*)
	add ax, bx		;* add in StackBase.segment	*)
	sub ax, 1000H		;* final result for segment	*)
	mov es, ax		;* keep new stack pointer	*)
	mov di, 0FFD2H		;*    in ES:DI for now		*)
	mov bx, di		;* save a copy in BX		*)

	;* Now load the initial stack contents, using ES:DI as	*)
	;* the temporary stack pointer.				*)

	pushf			;* save direction flag		*)
	cld			;* set direction = forwards	*)
	mov ax, OFFSET TaskStart ;* task starting point		*)
	stosw
	mov ax, _DATA		;* initial DS for task		*)
	stosw
	sub ax, ax
	mov cx, 9		;* nine dummy "saved registers"	*)
	rep stosw		;* (we arbitrarily make them 0)	*)
	or al, dl		;* set the initial flags	*)
	jz Init20		;*   to an all-zero word,	*)
	mov ax, 200H		;*   except for "interrupt	*)
Init20:	stosw			;*   enable" where specified	*)
	mov ax, ss		;* make DS:SI point to other	*)
	mov ds, ax		;*   parameters			*)
	lea si, [bp][14]
	mov cx, 4
	rep movsw		;* push StartAddress, TaskExit	*)

	;* Now the stuff relevant to interrupt handling.	*)
	
	mov ax, 9A00H		;* interrupt number = 0, and	*)
	stosw			;*    the "far call" opcode	*)
	mov ax, OFFSET IntHandler ;* address field for the	*)
	stosw			;*    call instruction		*)
	mov ax, cs		;*    (two words)		*)
	stosw
	sub ax, ax
	stosw			;* dummy saved interrupt vector	*)
	stosw
	mov ax, bx
	stosw			;* saved SP			*)
	mov dx, es
	mov es:[di], dx		;* saved SS			*)
	popf			;* restore direction flag	*)
		
	;* All done.  The stack pointer for this newly created	*)
	;* stack is now in DX:AX, and this is what we return to	*)
	;* the caller as the selector value.			*)
	
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
	;	ADR(source)	doubleword	at 6[bp]
	;	destination	doubleword	at 10[bp]

	pushf			;* save flags			*)
	db 60H	;* pusha *)	;* save all registers		*)
	push es
	push ds
	mov si, 6[bp]		; pick up address of source
	mov ds, 8[bp]
	mov ax, 10[bp]		; pick up destination selector
	mov bx, 12[bp]
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
	mov ds, cx		;*    selector location at the	*)
	mov si, 0FFFCH		;*    the bottom of our stack	*)
	call StackSwap		;* do the task switch		*)

	;* We reach here on the next interrupt.  For interrupt	*)
	;* requests which are mapped through the 8259A		*)
	;* controllers, we need to send a "nonspecific EOI"	*)
	;* code to the master controller, and possibly also to	*)
	;* the slave controller.				*)

	mov ah, EOI		;* this is the EOI code		*)
	mov al, ss:[0FFF2H]	;* check the interrupt number	*)
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
;************************************************************************)

InnerKernel$StartInterruptTask	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp

	; Parameter addresses (all offsets expressed in decimal):
	;	TS		doubleword	at 6[bp]
	;	InterruptNumber	word		at 10[bp]

	pushf			;* save all registers and flags		*)
	db 60H	;* pusha *)	;* (We need to save everything, since	*)
	push es			;*  the task switch below potentially	*)
	push ds			;*  corrupts all register contents)	*)

	mov ax, 6[bp]		; pick up parameter values
	mov bx, 8[bp]
	mov cx, 10[bp]

	push cx			;* save interrupt number		*)
	push bx			;* save a copy of task's stack segment	*)
	mov dx, _DATA		;* make DS:SI a pointer			*)
	mov ds, dx		;*   to BackgroundSelector		*)
	mov si, OFFSET BackgroundSelector
	call StackSwap		;* run the interrupt task		*)

	;* When we get back to here, the interrupt task has run to its	*)
	;* first call to IOTransfer.					*)

	pop es			;* select task's stack segment		*)
	mov di, 0FFF2H
	pop ax			;* restore interrupt number		*)
	mov es:[di], al		;* save interrupt no. in stack segment	*)
	add di, 6		;* point to where old interrupt		*)
	shl ax, 1		;*    vector will be saved		*)
	shl ax, 1		;* calculate 4 times interrupt number	*)
	mov si, ax		;* let DS:SI point to interrupt vector	*)
	xor ax, ax
	mov ds, ax
	cli			;* disable interrupts			*)
	cld			;* direction = forwards			*)
	movsw			;* copy interrupt vector (2 words)	*)
	mov ax, [si]		;*   into save area in interrupt	*)
	stosw			;*   task's stack			*)
	mov [si], es		;* reload interrupt vector to point	*)
	dec si
	dec si			;*   to CALL instruction in		*)
	mov WORD PTR [si], 0FFF3H ;*   interrupt task's stack		*)

	pop ds			;* restore all saved registers		*)
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
;************************************************************************)

InnerKernel$DisconnectFromInterrupt	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp
	push ax
	push bx

	; Parameter addresses (all offsets expressed in decimal):
	;	TS		doubleword	at 6[bp]

	push ds
	push es			;* save some registers			*)
	push si
	push di

	mov ax, 6[bp]		; pick up parameter value
	mov bx, 8[bp]

	mov ds, bx		;* select task's stack segment		*)
	mov al, DS:[0FFF2H]	;* get interrupt number		*)
	sub ah, ah
	mov si, 0FFF8H		;* DS:SI points to saved int vector	*)
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
	pop bx
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
	mov ax, DS:[0FFFCH]		;*   pointer into BX:AX		*)
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
;* PROCEDURE NPXsave (VAR (*OUT*) Buffer: NPXSaveArea);			*)
;*									*)
;* Saves the state of the Numeric Processor Extension coprocessor.	*)
;* (Has no effect, apart from a short time delay, if there is no	*)
;* coprocessor present.)						*)
;************************************************************************)

InnerKernel$NPXsave	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp
	push ax
	push bx
	push si

	; Parameter addresses (all offsets expressed in decimal):
	;	ADR(Buffer)	doubleword	at 6[bp]

	mov si, 6[bp]		; put buffer address
	mov ax, 8[bp]		;   into DS:SI
	mov ds, ax
	fnsave [si]		; save floating point state
	wait			; wait for completion
	pop si
	pop bx
	pop ax
	pop bp
	ret 4

InnerKernel$NPXsave	ENDP

;************************************************************************)
;* PROCEDURE NPXrestore (VAR (*IN*) Buffer: NPXSaveArea);		*)
;*									*)
;* The operation complementary to NPXsave.  Restores the previously	*)
;* saved state of the floating point coprocessor.			*)
;************************************************************************)

InnerKernel$NPXrestore	PROC FAR

	push bp			;* set up a stack frame		*)
	mov bp, sp
	push ax
	push bx
	push si

	; Parameter addresses (all offsets expressed in decimal):
	;	ADR(Buffer)	doubleword	at 6[bp]

	mov si, 6[bp]		; put buffer address
	mov ax, 8[bp]		;   into DS:SI
	mov ds, ax
	wait			; wait for FPU ready
	frstor [si]		; save floating point state
	pop si
	pop bx
	pop ax
	pop bp
	ret 4

InnerKernel$NPXrestore	ENDP

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