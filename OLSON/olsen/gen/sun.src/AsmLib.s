| Copyright 1988 by Olsen & Associates (O&A), Zurich, Switzerland.
| 
|                       All Rights Reserved
|
| Permission to use, copy, modify, and distribute this software and its
| documentation for any purpose and without fee is hereby granted,
| provided that the above copyright notice appear in all copies, and
| that both that copyright notice and this permission notice appear in
| supporting documentation, and that all modifications of this software
| or its documentation not made by O&A or its agents are accompanied
| by a prominent notice stating who made the modifications and the date
| of the modifications.
| 
| O&A DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE AND ITS
| DOCUMENTATION, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
| FITNESS.  IN NO EVENT SHALL O&A BE LIABLE FOR ANY SPECIAL, INDIRECT OR
| CONSEQUENTIAL DAMAGES, ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
| USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
| OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
| PERFORMANCE OF THIS SOFTWARE OR ITS DOCUMENTATION.
|******************
|
| Interfaces to assembler and certain C functions for general
| part of Olsen Portable Modula-2 Library, Sun 68000 version.
|
|
| To assembly this file, you will need to run it through "cpp"
| with the appropriate flags set.

#ifdef MC68881Arch
|
| Manipulate the contents of the 68881 floating point control register.
|
	.even
        .globl  _Get68881ControlRegister
_Get68881ControlRegister:
	fmovel	fpcr,d0		| return the contents
        rts

| Set the contents.  We assume that d1 and d0 are available to
| play and that sp@(4) is the value we are to set.  Clearly, this
| is tightly coupled with the modula/c run-time.
	.even
        .globl  _Set68881ControlRegister
_Set68881ControlRegister:
	fmovel	fpcr,d0		| return the old contents.
	fmovel	sp@(4),fpcr	| C always passes longs so this isn't a problem.
        rts
#endif

|
| The following section of code handles math errors in a more general
| way than the math library does.   We establish a procedure variable
| in place of the hardwired procedure name "matherr" (see the man page).
|
	.globl	_mathErrorHandler
	.globl	_DefaultMathErrorHandler
	.globl	_matherr

	.data
	.even
| Procedure variable which is compatible with "matherr"
_mathErrorHandler:
	.long	_DefaultMathErrorHandler
	.text

	.even
| Gets called by the C math library when an error occurs.
_matherr:
	| This procedure is implemented in a rather tricky way, because
	| we don't want to fuss with registers and passing parameters
	| and such.  We push the address we want to "jump" to on the
	| stack and then return to it.  The 68K doesn't allow memory
	| deferred addressing (near as I can tell) for "jmp" instructions.
	movl	_mathErrorHandler,sp@-	| Load procedure variable.
	rts				| jmp _MathErrorHandler@

	.even
| Handler called when nothing is registered.
_DefaultMathErrorHandler:
	clrl	d0		| return 0 means not "handled" to mathlib
	rts

#ifdef Tasks
| Assembler code for Tasks and Interrupts.  A task switch consists for
| three parts: saving current context, switching tasks, and restoring
| new context.   The middle part is managed by TasksPRIVATE while
| this module performs the context saves/restores (except for the
| the preemptionLevel).  The division of labor has been defined by
| the ability to implement assembler in Modula-2.
|
| NOTE: TasksPRIVATE *must* register for taskSwitcher or this
|       code will not work.
|
| Exports
	.globl  _taskSwitcher
	.globl	_tasksSavedPC
	.globl	_tasksSavedPSL
	.globl	_tasksSavedMask
	.globl	_TasksSwitchRunning
	.globl	_InterruptsSwitchRunning
	    
| Imports
	.globl	_errno

	.data
_taskSwitcher:
	.long	 0x0	| Procedure to call for task switch.
			| PROCEDURE( SYSTEM.ADDRESS ) : SYSTEM.ADDRESS;
_tasksSavedPC:   
	.long	 0x0	| Set before InterruptsSwitchRunning called
_tasksSavedPSL:
	.word	 0x0	| ditto
_tasksSavedMask:
	.long	 0x0	| Set before either SwitchRunning called


	.text
	.proc
_TasksSwitchRunning:
	| The return address has been pushed on the stack, so we must 
	| push the PSL and jump to SharedSwitchRunning.
	clrw	sp@-		| push on a dummy status word doesn't matter
	bra	SwitchRunning	| do the context switch
    
	.proc
_InterruptsSwitchRunning:
	| We get here after a return from a signal handler.  The savedPC
	| and savedPSL must be pushed on the stack and then we go to the
	| shared code.
	movl	_tasksSavedPC, sp@-	| save the PC which was interrupted.
	movw	_tasksSavedPSL, sp@-	| save the CCR which was interrupted.
	bra	SwitchRunning		| do the context switch

SwitchRunning:
	| The meat of the context switching.   The two other switch running
	| procedures make sure the stack have the PC and the PSL on pushed
	| on already before this code is "jumped" to.
	moveml	#0xfffe, sp@-		| save the registers
	| floating point should be saved here...
	movl	_errno, sp@-		| save the current errno
	movl	_tasksSavedMask, sp@-	| save the mask

	| Do the Modula-2 level task switching stuff.
	movl	sp, sp@-		| push the stack pointer as an arg
	movl	_taskSwitcher, a0	| Call registered procedure.
	jsr	a0@

	| Don't need to pop the argument, because we are going to switch stacks.
	movl	d0, sp			| set the new stack pointer (return)

	| The saved mask is on top of the stack so is arg to sigsetmask.
	bsr	_sigsetmask		| set the new mask
	addql	#4, sp			| pop the mask

	movl	sp@+, _errno		| restore the errno (after sigset op).
	| Restore floating point here....
	moveml	sp@+, #0x7fff		| restore the registers
	rtr				| return and set CCR
#endif
