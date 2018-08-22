	.MODEL large

	PUBLIC ParTest$TestProc

	;(***************************************************************)
	;(*								*)
	;(*		Test of parameter passing			*)
	;(*								*)
	;(*	Programmer:	P. Moylan				*)
	;(*	Last edited:	28 February 1985			*)
	;(*	Status:		Working					*)
	;(*								*)
	;(*	The only function of this module is to let me check,	*)
	;(*	via the debugger, the order in which parameters are	*)
	;(*	passed on the stack.					*)
	;(*								*)
	;(***************************************************************)

ParTest_TEXT	SEGMENT 'CODE'

;(***********************************************************************)
;(* PROCEDURE TestProc (VAR a, b: CARDINAL);				*)
;(***********************************************************************)

ParTest$TestProc	PROC	FAR

	push bp
	mov bp, sp
	push ds			;(* save registers		*)
	push si
	lds si, 6[bp]		; get first address
	mov [si], 1		; return a result
	lds si, 10[bp]		; get first address
	mov [si], 2		; return a result
	pop si
	pop ds
	pop bp
	ret 8

ParTest$TestProc	ENDP

;(***********************************************************************)

ParTest_TEXT	ENDS

END
