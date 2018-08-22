	PUBLIC PlayBuff$BufferAddress
	PUBLIC PlayBuff$SetCycleCount
	PUBLIC PlayBuff$StartPlaying
	PUBLIC PlayBuff$Synch0
	PUBLIC PlayBuff$Synch1
	PUBLIC PlayBuff$StopPlaying

	;(***************************************************************)
	;(*								*)
	;(*			   PLAYBUFF.ASM				*)
	;(*		This version is for use with compilers		*)
	;(*		that pass parameter values on the stack.	*)
	;(*		We assume that the called procedure has		*)
	;(*		the responsibility of removing parameters	*)
	;(*		from the stack, and that DX:AX is used to	*)
	;(*		return a two-word function result.		*)
	;(*								*)
	;(*	This is a very low-level module, which takes data	*)
	;(*	from a buffer and uses it directly as timer "count"	*)
	;(*	values to control the speaker.  The caller has the	*)
	;(*	responsibility to do all pre-processing necessary	*)
	;(*	to compute the counts.					*)
	;(*								*)
	;(*	This version uses two timers: timer 0 to control the	*)
	;(*	sampling rate, and timer 2 to produce the PWM output	*)
	;(*	to the speaker.						*)
	;(*								*)
	;(*	Programmer:	P. Moylan, T. Channon			*)
	;(*			(see below for further credits)		*)
	;(*	Last edited:	1 March 1995				*)
	;(*	Status:		Working with TopSpeed 1.17		*)
	;(*								*)
	;(*	HISTORY AND CREDITS:					*)
	;(*								*)
	;(*	The first version of my 3-part music software was	*)
	;(*	written many years ago on a homebrew 8080A system.	*)
	;(*	(It had a crude D/A converter wired across the front	*)
	;(*	panel lights, and that more than compensated for the	*)
	;(*	slowness of the processor.)  The original idea		*)
	;(*	probably came from something like a Dr. Dobbs		*)
	;(*	article, but at this distance in time I no longer	*)
	;(*	remember the details.					*)
	;(*								*)
	;(*	I wrote the PC version at about the beginning of May	*)
	;(*	1994, using direct control of the "speaker enable" bit	*)
	;(*	to implement a PWM technique.  It ran well enough, but	*)
	;(*	had the shortcoming that it had to be re-tuned when	*)
	;(*	ported to a processor with a different clock speed.	*)
	;(*								*)
	;(*	The suggestion for using a timer to make the code	*)
	;(*	independent of processor speed came from Tim Channon	*)
	;(*	(tchannon@black.demon.co.uk).  More importantly, Tim	*)
	;(*	came up with some major improvements to the coding,	*)
	;(*	and managed to turn a buzzy sound into something you	*)
	;(*	could listen to.  His modifications to cut down the	*)
	;(*	size of the interrupt routine turned out to be crucial.	*)
	;(*								*)
	;(*	The idea of using mode 0 for timer 2 came from a hint	*)
	;(*	by Mark Feldman (myndale@cairo.anu.edu.au), the author	*)
	;(*	of the Games Programmers' Encyclopaedia.  That led to	*)
	;(*	a further speed-up of the software, to the point where	*)
	;(*	there was some time left over for higher-level modules	*)
	;(*	to do some real-time waveform processing.		*)
	;(*								*)
	;(***************************************************************)

;(***********************************************************************)
;(*			SOME TECHNICAL DETAILS				*)
;(*									*)
;(* The speaker is controlled by the two low-order bits of port 87.	*)
;(* Bit 0 is the GATE input to timer 2 - this controls whether timer 2	*)
;(* is running - and bit 1 turns the speaker on.  The actual signal to	*)
;(* the speaker is a logical AND of bit 1 and the output of timer 2.	*)
;(* This module leaves both of these bits turned on all the time, so	*)
;(* that the speaker is driven directly by timer 2.			*)
;(*									*)
;(* We're using a pulse width modulation technique, where the waveform	*)
;(* sent to the speaker has a fixed frequency and the desired signal	*)
;(* is obtained by modulating the duty cycle.  Timer 0 is used to set	*)
;(* the basic sampling frequency, and the interrupt routine for timer 0	*)
;(* is used to update the "count" values sent to timer 2.		*)
;(*									*)
;(* The data buffer from which the interrupt routine is taking its	*)
;(* "count" values must be filled by a higher-level module.  The	*)
;(* procedures Synch0 and Synch1 are supplied to let that higher-level	*)
;(* module avoid overwriting parts of the buffer which the interrupt	*)
;(* routine has not yet reached.					*)
;(*									*)
;(***********************************************************************)

;(***********************************************************************)
;(*			SOME IMPORTANT CONSTANTS			*)
;(***********************************************************************)

    ;(* OutputBufferSize is the size of a half-buffer.  (The overall	*)
    ;(* size of the circular buffer is twice this.)  Warning: make sure	*)
    ;(* that this declaration is consistent with the one in the		*)
    ;(* definition module, since I haven't yet found a way to make	*)
    ;(* this assembler import a constant from the definition module.	*)

    OutputBufferSize = 128

    ;(* Definitions of port numbers. *)

    Timer0 = 0
    Timer2 = 2
    CountPort0 = 40H + Timer0
    CountPort2 = 40H + Timer2
    TimerControlPort = 43H
    SpeakerPort = 61H

    ;(* The codes we have to write to the timer control ports to set	*)
    ;(* the modes.  Note in particular that we're using timer 2 in its	*)
    ;(* "interrupt on terminal count" mode.  This doesn't actually cause*)
    ;(* an interrupt - the original PC designers apparently didn't	*)
    ;(* envisage that any programmers would have liked to have the timer*)
    ;(* output connected to the interrupt controller - but that mode	*)
    ;(* does have the useful property that its output changes (rather	*)
    ;(* than just pulsing) on the expiration of the count.		*)

    LoadOption = 1		;(* to use one-byte counts	*)
    Timer0Mode = 2
    Timer2Mode = 0
    OurMode0 = 64*Timer0 + 16*LoadOption + 2*Timer0Mode
    OurMode2 = 64*Timer2 + 16*LoadOption + 2*Timer2Mode

    NormalMode = 36H		;(* DOS uses mode 3 = square wave *)

    ;(* The number we have to write to the speaker port to enable the	*)
    ;(* speaker and turn on the timer 2 gate.  Bits 2 and 3 have to be	*)
    ;(* set for reasons unrelated to the speaker, bit 1 has to be set to*)
    ;(* enable the speaker, and bit 0 has to be set to allow timer 2	*)
    ;(* to run.								*)

    EnableSpeaker = 0FH

;(***********************************************************************)
;(*				MODULE DATA				*)
;(***********************************************************************)

PlayBuff_DATA	SEGMENT 'DATA'

    ;(* CycleCount controls the sampling frequency.  Giving it too low	*)
    ;(* a value will put an impossible load on the processor, and the	*)
    ;(* software will be unable to keep up.  Giving it too high a value	*)
    ;(* will cause a whistle because of aliasing effects.  A compromise	*)
    ;(* has to be found between these extremes.				*)

    ;(* Note that a change to CycleCount requires a re-working of	*)
    ;(* the way the caller calculates frequencies.			*)

    CycleCount	 DB	64

    ;(* Saved copy of the original INT 8 interrupt vector.	*)

    SavedVector  DW	0, 0

    ;(* Save copies of the interrupt masks. *)

    Mask1        DB	0
    Mask2        DB	0

PlayBuff_DATA	ENDS

;(***********************************************************************)
;(************    Start of code segment for this module    **************)
;(***********************************************************************)

	;(*******************************************************)
	;(*							*)
	;(*	 WARNING: IN ORDER TO KEEP THE INTERRUPT	*)
	;(*	 ROUTINE SHORT, WE KEEP SOME OF OUR DATA	*)
	;(*	  IN THIS CODE SEGMENT.  THIS IS NOT A		*)
	;(*	VERY CLEAN PROGRAMMING STYLE, BUT IT DOES	*)
	;(*	SAVE US HAVING TO LOAD A SEGMENT REGISTER.	*)
	;(*							*)
	;(*******************************************************)

PlayBuff_TEXT	SEGMENT PARA 'CODE'

    ;(* A circular buffer to hold a queue of data for the interrupt	*)
    ;(* routine.  Each queue entry is a count to be loaded into timer 2.*)
    ;(* IMPORTANT: This must come at the beginning of the segment.	*)

    CircBuffer	DB	2*OutputBufferSize DUP (0)

    ;(* The circular buffer pointer used by the interrupt routine.	*)

    OutPtr	DW	CircBuffer

	ASSUME CS: PlayBuff_TEXT

;(***********************************************************************)
;(* PROCEDURE InterruptHandler;						*)
;(*									*)
;(* This is the INT 8 interrupt handler.  It is triggered by timer 0,	*)
;(* which is running at a constant rate.  It has been totally reworked	*)
;(* by TC to speed it up.						*)
;(***********************************************************************)

InterruptHandler PROC

	push bx
	push ax
	mov bx, cs:[OutPtr]	;(* load DS and buffer pointer	*)
	mov al, cs:[bx]		;(* fetch the next count	*)
	out CountPort2, al	;(* send new count to timer #2	*)
	inc bl			;(* store back updated		*)
	mov BYTE PTR cs:[OutPtr], bl	;(*   buffer pointer	*)
	mov al, 20H
	out 20H, al		;(* send the EOI		*)
	pop ax
	pop bx
	iret

InterruptHandler ENDP

;(***********************************************************************)
;(* PROCEDURE StartPlaying;						*)
;(*									*)
;(* Call this just after the first half-buffer has been filled.  It	*)
;(* initialises OutPtr, sets up InterruptHandler as an interrupt routine*)
;(* for INT 8, and puts timers 0 and 2 into the modes we want to use.	*)
;(***********************************************************************)

PlayBuff$StartPlaying	PROC	FAR

	push ds			;(* save registers		*)
	push es
	push ax
	push si
	push di
	pushf
	mov ax, 0
	mov BYTE PTR cs:[OutPtr], al ;(* initialise buffer pointer *)
	mov ds, ax		;(* make DS:SI point to		*)
	mov si, 32		;(*   INT 8 vector		*)

	ASSUME DS: 0, ES: PlayBuff_DATA

	mov ax, PlayBuff_DATA	;(* make ES:DI point to the	*)
	mov es, ax		;(*   place to save the vector	*)
	mov di, OFFSET SavedVector
	cli			;(* disable interrupts		*)
	cld			;(* direction := forwards	*)
	lodsw
	stosw			;(* save the original contents	*)
	mov ax, [si]		;(*   of the interrupt vector	*)
	stosw
	mov WORD PTR [si], SEG InterruptHandler
	dec si			;(* load new interrupt vector	*)
	dec si
	mov WORD PTR [si], OFFSET InterruptHandler

	in al, 021H		;(* save interrupt mask		*)
	mov es:[Mask1], al	;(*   in Mask1, Mask2		*)
	in al, 0A1H
	mov es:[Mask2], al
	mov al, 0FCH		;(* disable all interrupts	*)
	out 021H, al		;(*   except keyboard and	*)
	mov al, 0FFH		;(*     timer			*)
	out 0A1H, al

	mov al, OurMode2	;(* set mode for timer 2	*)
	out TimerControlPort, al
	mov al, EnableSpeaker	;(* enable speaker and turn on	*)
	out SpeakerPort, al	;(*   timer 2 gate		*)
	mov al, OurMode0	;(* set mode for timer 0	*)
	out TimerControlPort, al
	mov al, es:[CycleCount]	;(* specify sampling rate in	*)
	out CountPort0, al	;(*   timer 0			*)
	popf
	pop di
	pop si
	pop ax
	pop es
	pop ds
	ret

PlayBuff$StartPlaying	ENDP

;(***********************************************************************)
;(* PROCEDURE StopPlaying;						*)
;(*									*)
;(* Stops the music and cleans up.  This procedure restores the original*)
;(* INT 8 interrupt handler, and puts the timer 0 mode back to what DOS	*)
;(* expects.								*)
;(***********************************************************************)

PlayBuff$StopPlaying	PROC	FAR

	push ds
	push es
	push ax
	push si
	push di
	pushf
	mov ax, PlayBuff_DATA	;(* select our data segment	*)
	mov ds, ax

	ASSUME DS: PlayBuff_DATA, ES: 0

	mov ax, 0
	mov es, ax		;(* make ES:DI point to		*)
	mov di, 32		;(*   INT 8 vector		*)
	mov si, OFFSET SavedVector
	cli			;(* disable interrupts		*)
	cld
	lodsw
	stosw			;(* copy the contents of	*)
	lodsw			;(*   SavedVector (2 words) to	*)
	stosw			;(*     the INT 8 vector	*)
	mov al, [Mask1]		;(* restore the original	*)
	out 021H, al		;(*   interrupt masks		*)
	mov al, [Mask2]
	out 0A1H, al
	mov al, NormalMode	;(* reset the timer mode	*)
	out TimerControlPort, al
	sub al, al
	out CountPort0, al	;(* load an all-zero timer count*)
	out CountPort0, al	;(*  (which is what DOS expects)*)
	popf
	pop di
	pop si
	pop ax
	pop es
	pop ds
	ret

PlayBuff$StopPlaying	ENDP

;(***********************************************************************)
;(* PROCEDURE BufferAddress (): ADDRESS;				*)
;(*									*)
;(* Returns the address of the first buffer in DX:AX.			*)
;(***********************************************************************)

PlayBuff$BufferAddress	PROC FAR

	sub ax, ax
	mov dx, cs
	ret

PlayBuff$BufferAddress	ENDP

;(***********************************************************************)
;(* PROCEDURE Synch0;							*)
;(*									*)
;(* Call this when you've just finished the second half of the buffer	*)
;(* and want to move back to the first half.  On return from this	*)
;(* procedure, the caller is permitted to start filling the first half.	*)
;(***********************************************************************)

PlayBuff$Synch0	PROC FAR

	;(* This code is for the case OutputBufferSize = 128. *)

loop0:	test BYTE PTR cs:[OutPtr], 80H
	jz loop0
	ret

	;(* Alternative: the following code is for the case where	*)
	;(* OutputBufferSize = 256.					*)
;(*
;loop0:	cmp byte cs:[OutPtr], 0H
;	jnz loop0
;	mov byte cs:[OutPtr+1], 1
;	ret far 0
;*)

PlayBuff$Synch0	ENDP

;(***********************************************************************)
;(* PROCEDURE Synch1;							*)
;(*									*)
;(* Call this when you've just finished the first half of the buffer	*)
;(* and want to move on to the second half.  On return from this	*)
;(* procedure, the caller is permitted to start filling the second half.*)
;(***********************************************************************)

PlayBuff$Synch1	PROC FAR

	;(* This code is for the case OutputBufferSize = 128. *)

loop1:	test BYTE PTR cs:[OutPtr], 80H
	jnz loop1
	ret

	;(* Alternative: the following code is for the case where	*)
	;(* OutputBufferSize = 256.					*)
;(*
;loop1:	cmp byte cs:[OutPtr], 0H
;	jnz loop1
;	mov byte cs:[OutPtr+1], 0
;	ret far 0
;*)

PlayBuff$Synch1	ENDP

;(***********************************************************************)
;(* PROCEDURE SetCycleCount (count: BYTE);				*)
;(*									*)
;(* Sets the cycle count which determines the sampling frequency.	*)
;(* The default value is 64.						*)
;(***********************************************************************)

PlayBuff$SetCycleCount	PROC FAR

	push bp
	mov bp, sp
	push ds
	push ax
	mov ax, PlayBuff_DATA	;(* select our data segment *)
	mov ds, ax

	ASSUME DS: PlayBuff_DATA

	mov al, 6[bp]
	mov [CycleCount],al	;(* store the new value *)
	pop ax
	pop ds
	pop bp
	ret 2

PlayBuff$SetCycleCount	ENDP

;(***********************************************************************)

PlayBuff_TEXT	ENDS

END
