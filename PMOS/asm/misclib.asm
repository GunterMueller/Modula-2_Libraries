	.MODEL large
	.186

	PUBLIC MiscLib$InStringWord
	PUBLIC MiscLib$OutStringWord

	;(***************************************************************)
	;(*								*)
	;(*			   MISCLIB.ASM				*)
	;(*		Miscellaneous library functions for use		*)
	;(*		with TopSpeed version 1.17.			*)
	;(*								*)
	;(*	Programmer:	P. Moylan				*)
	;(*	Last edited:	23 August 1994				*)
	;(*	Status:		Working					*)
	;(*								*)
	;(***************************************************************)

MiscLib_TEXT	SEGMENT 'CODE'

;(***********************************************************************)
;(* PROCEDURE InStringWord (port: CARDINAL;  BufferAddress: ADDRESS;	*)
;(*						count: CARDINAL);	*)
;(*									*)
;(* Reads count words from an input port.				*)
;(***********************************************************************)

MiscLib$InStringWord	PROC FAR

	push bp		;(* create a stack frame		*)
	mov bp, sp
	push es		;(* save some registers		*)
	push di
	push cx
	push dx
	pushf		;(* save processor status	*)
	cli		;(* disable interrupts		*)
	cld		;(* set direction := forwards	*)
	mov cx, 6[bp]	;(* load the word count		*)
	les di, 8[bp]	;(* put buffer address in ES:DI	*)
	mov dx, 12[bp]	;(* get port number		*)
	rep insw	;(* do the string input		*)
	popf		;(* restore processor status	*)
	pop dx
	pop cx
	pop di
	pop es
	pop bp
	ret 8

MiscLib$InStringWord	ENDP

;(***********************************************************************)
;(* PROCEDURE OutStringWord (port: CARDINAL;  BufferAddress: ADDRESS;	*)
;(*						count: CARDINAL);	*)
;(*									*)
;(* Writes count words to an output port.				*)
;(***********************************************************************)

MiscLib$OutStringWord	PROC FAR

	push bp		;(* create a stack frame		*)
	mov bp, sp
	push ds		;(* save some registers		*)
	push si
	push cx
	push dx
	pushf		;(* save processor status	*)
	cli		;(* disable interrupts		*)
	cld		;(* set direction := forwards	*)
	mov cx, 6[bp]	;(* load the word count		*)
	lds si, 8[bp]	;(* put buffer address in DS:SI	*)
	mov dx, 12[bp]	;(* get port number		*)
	rep outsw	;(* do the string output	*)
	popf		;(* restore processor status	*)
	pop dx
	pop cx
	pop si
	pop ds
	pop bp
	ret 8

MiscLib$OutStringWord	ENDP

;(***********************************************************************)

MiscLib_TEXT	ENDS

END
