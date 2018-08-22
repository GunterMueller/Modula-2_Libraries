IMPLEMENTATION MODULE LowLevel;

	(********************************************************)
	(*							*)
	(*	   Miscellaneous low-level procedures		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	28 February 1995		*)
	(*  Status:		Working				*)
	(*	Exception: the string I/O is not working in	*)
	(*	the FST case, and for now I don't know how	*)
	(*	to fix it.  The offending lines are		*)
	(*	commented out.					*)
	(*							*)
	(*	Note that the implementation of this module	*)
	(*	is heavily compiler-dependent.  This version	*)
	(*	is a "semi-portable" version, which contains	*)
	(*	preprocessor directives to choose which code	*)
	(*	to activate.  Use the PP preprocessor to	*)
	(*	select the correct version.			*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT BYTE, WORD, ADDRESS (*<FST ,ASSEMBLER >*);
(*<TopSpeed*) IMPORT SYSTEM, Lib; (*>*)
(*<TopSpeed1 IMPORT MiscLib; >*)
(*<FST IMPORT System; >*)

TYPE
    t02 = [0..2];  t03 = [0..3];

    Table = ARRAY [0..15] OF CARDINAL;

    Word =  RECORD
		CASE :t03 OF
		  | 0:	bits: BITSET;
		  | 1:	low, high: BYTE;
		  | 2:	w: WORD;
		  | 3:	c: CARDINAL;
		END (*CASE*);
	    END (*RECORD*);

    Double = RECORD
		CASE :t02 OF
		  | 0:	low, high: WORD;
		  | 1:	lw: LONGCARD;
		  | 2:	a: FarPointer;
		END (*CASE*);
	     END (*RECORD*);

(*<TopSpeed*)
CONST power2 = Table (1, 2, 4, 8, 16, 32, 64, 128, 256, 512,
			1024, 2048, 4096, 8192, 16384, 32768);
(*>*)

(*<FST
VAR power2: Table;
>*)

(************************************************************************)
(*			    BITWISE LOGIC				*)
(************************************************************************)

(*<~TopSpeed3

PROCEDURE IAND (first, second: WORD): WORD;

    (* Bit-by-bit logical AND.	*)

    VAR a, b, result: Word;

    BEGIN
	a.w := first;  b.w := second;
	result.bits := a.bits * b.bits;
	RETURN result.w;
    END IAND;

(************************************************************************)

PROCEDURE IANDB (first, second: BYTE): BYTE;

    (* Bit-by-bit logical AND for bytes.	*)

    BEGIN
	RETURN LowByte(IAND(ORD(first), ORD(second)));
    END IANDB;

(************************************************************************)

PROCEDURE IOR (first, second: WORD): WORD;

    (* Bit-by-bit inclusive OR.	*)

    VAR a, b, result: Word;

    BEGIN
	a.w := first;  b.w := second;
	result.bits := a.bits + b.bits;
	RETURN result.w;
    END IOR;

(************************************************************************)

PROCEDURE IORB (first, second: BYTE): BYTE;

    (* Bit-by-bit inclusive OR.	*)

    BEGIN
	RETURN LowByte(IOR(ORD(first), ORD(second)));
    END IORB;

(************************************************************************)

PROCEDURE IXOR (first, second: WORD): WORD;

    (* Bit-by-bit exclusive OR.	*)

    VAR a, b, result: Word;

    BEGIN
	a.w := first;  b.w := second;
	result.bits := a.bits / b.bits;
	RETURN result.w;
    END IXOR;

(************************************************************************)

PROCEDURE IXORB (first, second: BYTE): BYTE;

    (* Bit-by-bit exclusive OR.	*)

    BEGIN
	RETURN LowByte(IXOR(ORD(first), ORD(second)));
    END IXORB;

(************************************************************************)

PROCEDURE INOT (value: WORD): WORD;

    (* Bit-by-bit Boolean complement.	*)

    BEGIN
	RETURN WORD(0FFFFH-ORD(value));
    END INOT;

(************************************************************************)

PROCEDURE INOTB (value: BYTE): BYTE;

    (* Bit-by-bit Boolean complement.	*)

    BEGIN
	RETURN LowByte(0FFH-ORD(value));
    END INOTB;

(************************************************************************)

PROCEDURE ROL (value: WORD;  count: CARDINAL): WORD;

    (* Left rotation of "value" by "count" bit positions.	*)

    BEGIN
	count := count MOD 16;
	RETURN WORD(ORD(LS(value, count)) + ORD(RS(value, 16-count)));
    END ROL;

(************************************************************************)

PROCEDURE ROLB (value: BYTE;  count: CARDINAL): BYTE;

    (* Left rotation of "value" by "count" bit positions.	*)

    BEGIN
	count := count MOD 8;
	RETURN LowByte(ORD(LSB(value, count)) + ORD(RSB(value, 8-count)));
    END ROLB;

(************************************************************************)

PROCEDURE LS (value: WORD;  count: CARDINAL): WORD;

    (* Left shift of "value" by "count" bit positions, with zero fill.	*)

    BEGIN
	IF count > 15 THEN RETURN WORD(0)
	ELSIF count = 0 THEN RETURN value
	ELSE
	    value := IAND (value, WORD(power2[16-count]-1));
	    RETURN WORD(ORD(value) * power2[count]);
	END (*IF*);
    END LS;

(************************************************************************)

PROCEDURE LSB (value: BYTE;  count: CARDINAL): BYTE;

    (* Left shift of "value" by "count" bit positions, with zero fill.	*)

    BEGIN
	RETURN LowByte (LS(MakeWord(LowByte(0),value), count));
    END LSB;

(************************************************************************)

PROCEDURE ROR (value: WORD;  count: CARDINAL): WORD;

    (* Right rotation of "value" by "count" bit positions.	*)

    BEGIN
	count := count MOD 16;
	RETURN WORD(ORD(RS(value, count)) + ORD(LS(value, 16-count)));
    END ROR;

(************************************************************************)

PROCEDURE RORB (value: BYTE;  count: CARDINAL): BYTE;

    (* Right rotation of "value" by "count" bit positions.	*)

    BEGIN
	count := count MOD 8;
	RETURN LowByte(ORD(RSB(value, count)) + ORD(LSB(value, 8-count)));
    END RORB;

(************************************************************************)

PROCEDURE RS (value: WORD;  count: CARDINAL): WORD;

    (* Right shift of "value" by "count" bit positions, with zero fill.	*)

    BEGIN
	(*<TopSpeed (*# save, check(overflow => off) *) >*)
	IF count > 15 THEN RETURN WORD(0)
	ELSE RETURN WORD(ORD(value) DIV power2[count]);
	END (*IF*);
	(*<TopSpeed (*# restore *) >*)
    END RS;

(************************************************************************)

PROCEDURE RSB (value: BYTE;  count: CARDINAL): BYTE;

    (* Right shift of "value" by "count" bit positions, with zero fill.	*)

    BEGIN
	RETURN LowByte (RS(ORD(value), count));
    END RSB;

(************************************************************************)
(*			    POINTER OPERATIONS				*)
(************************************************************************)

PROCEDURE Far (A: ADDRESS): FarPointer;

    (* Converts a pointer to a far pointer. *)

    BEGIN
	RETURN A;
    END Far;

(************************************************************************)

PROCEDURE MakePointer (segment, offset: WORD): FarPointer;

    (* Creates a pointer, given the segment and offset within segment.	*)

    VAR value: Double;

    BEGIN
	value.low := offset;  value.high := segment;
	RETURN value.a;
    END MakePointer;

(************************************************************************)

PROCEDURE SEGMENT (A: ADDRESS): WORD;

    (* Returns the segment part of an address.	*)

    VAR value: Double;

    BEGIN
	value.a := A;
	RETURN value.high;
    END SEGMENT;

(************************************************************************)

PROCEDURE FarSEGMENT (A: FarPointer): WORD;

    (* Returns the segment part of an address.	*)

    VAR value: Double;

    BEGIN
	value.a := A;
	RETURN value.high;
    END FarSEGMENT;

(************************************************************************)

PROCEDURE OFFSET (A: ADDRESS): WORD;

    (* Returns the offset part of an address.	*)

    VAR value: Double;

    BEGIN
	value.a := A;
	RETURN value.low;
    END OFFSET;

>*)

(************************************************************************)

PROCEDURE Virtual (PA: LONGCARD): FarPointer;

    (* Converts a physical address to a virtual address, if possible.	*)
    (* There are no guarantees in the case where there is no such	*)
    (* virtual address.							*)

    CONST Sixteen = (*<FST 16L >*)  (*<~FST*) 16 (*>*);

    VAR value: Double;

    BEGIN
	value.low := LowWord(PA MOD Sixteen);
	value.high := LowWord(PA DIV Sixteen);
	RETURN value.a;
    END Virtual;

(************************************************************************)

PROCEDURE Physical (A: ADDRESS): LONGCARD;

    (* Converts a virtual address to a physical address.  Use with care!*)

    VAR value: Double;

    BEGIN
	value.a := A;
	(*<TopSpeed*)
	RETURN 16*VAL(LONGCARD,value.high) + VAL(LONGCARD,value.low);
	(*>*)
	(*<FST
	RETURN 16L*LONG(ORD(value.high)) + LONG(ORD(value.low));
	>*)
    END Physical;

(************************************************************************)

(*<~TopSpeed3

PROCEDURE AddOffset (A: ADDRESS;  increment: CARDINAL): ADDRESS;

    (* Returns a pointer to the memory location whose physical address	*)
    (* is Physical(A)+increment.  In the present version, it is assumed	*)
    (* that the caller will never try to run off the end of a segment.	*)

    VAR value: Double;

    BEGIN
	(*<TopSpeed
	value.a := A;  INC(value.low, increment);
	RETURN value.a;
	>*)
	(*<FST RETURN A+increment; >*)
    END AddOffset;

(************************************************************************)

PROCEDURE SubtractOffset (A: ADDRESS;  decrement: CARDINAL): ADDRESS;

    (* Like AddOffset, except that we go backwards in memory.  Running	*)
    (* off the beginning of the segment is an undetected error.		*)

    VAR value: Double;

    BEGIN
	(*<TopSpeed
	value.a := A;  DEC(value.low, decrement);
	RETURN value.a;
	>*)
	(*<FST RETURN A-decrement; >*)
    END SubtractOffset;

(************************************************************************)

PROCEDURE FarAddOffset (A: FarPointer;  increment: CARDINAL): FarPointer;

    (* Like AddOffset, except for the parameter types. *)

    VAR value: Double;

    BEGIN
	(*<TopSpeed
	value.a := A;  INC(value.low, increment);
	RETURN value.a;
	>*)
	(*<FST RETURN A+increment; >*)
    END FarAddOffset;

(************************************************************************)

PROCEDURE FarSubtractOffset (A: FarPointer; decrement: CARDINAL): FarPointer;

    (* Like SubtractOffset, except for the parameter types. *)

    VAR value: Double;

    BEGIN
	(*<TopSpeed
	value.a := A;  DEC(value.low, decrement);
	RETURN value.a;
	>*)
	(*<FST RETURN A-decrement; >*)
    END FarSubtractOffset;

(************************************************************************)
(*			BYTE/WORD/LONGCARD CONVERSIONS			*)
(************************************************************************)

PROCEDURE LowByte (w: WORD): BYTE;

    (* Returns the low-order byte of its argument.	*)

    VAR value: Word;

    BEGIN
	value.w := w;
	RETURN value.low;
    END LowByte;

(************************************************************************)

PROCEDURE HighByte (w: WORD): BYTE;

    (* Returns the high-order byte of its argument.	*)

    VAR value: Word;

    BEGIN
	value.w := w;
	RETURN value.high;
    END HighByte;

(************************************************************************)

PROCEDURE MakeWord (high, low: BYTE): WORD;

    (* Combines two bytes into a word.  The first argument becomes the	*)
    (* most significant byte of the result.				*)

    VAR value: Word;

    BEGIN
	value.low := low;
	value.high := high;
	RETURN value.w;
    END MakeWord;

(************************************************************************)

PROCEDURE SignExtend (val: BYTE): INTEGER;

    (* Converts a signed 8-bit number to signed integer. *)

    VAR result: INTEGER;

    BEGIN
	result := ORD(val);
	IF result > 127 THEN
	    DEC (result, 256);
	END (*IF*);
	RETURN result;
    END SignExtend;

(************************************************************************)

PROCEDURE LowWord (w: LONGCARD): WORD;

    (* Returns the low-order word of its argument.	*)

    VAR value: Double;

    BEGIN
	value.lw := w;
	RETURN value.low;
    END LowWord;

(************************************************************************)

PROCEDURE HighWord (w: LONGCARD): WORD;

    (* Returns the high-order word of its argument.	*)

    VAR value: Double;

    BEGIN
	value.lw := w;
	RETURN value.high;
    END HighWord;

(************************************************************************)

PROCEDURE MakeLongword (high, low: WORD): LONGCARD;

    (* Combines two words into a longword.  The first argument becomes	*)
    (* the most significant word of the result.				*)

    VAR value: Double;

    BEGIN
	value.low := low;
	value.high := high;
	RETURN value.lw;
    END MakeLongword;

(************************************************************************)
(*			MISCELLANEOUS ARITHMETIC			*)
(************************************************************************)

PROCEDURE INCV (VAR (*INOUT*) dest: CARDINAL;  src: CARDINAL): BOOLEAN;

    (* Computes dest := dest + src, and returns TRUE iff the addition	*)
    (* produced a carry.						*)

    BEGIN
	IF dest > MAX(CARDINAL) - src THEN
	    DEC (dest, MAX(CARDINAL) - src + 1);
	    RETURN TRUE;
	ELSE
	    INC (dest, src);
	    RETURN FALSE;
	END (*IF*);
    END INCV;

(************************************************************************)

PROCEDURE INCVB (VAR (*INOUT*) dest: BYTE;  src: BYTE): BOOLEAN;

    (* Computes dest := dest + src, and returns TRUE iff the addition	*)
    (* produced a carry.						*)

    BEGIN
	IF ORD(dest) > 255 - ORD(src) THEN
	    dest := LowByte(ORD(dest) - (256 - ORD(src)));
	    RETURN TRUE;
	ELSE
	    dest := LowByte(ORD(dest) + ORD(src));
	    RETURN FALSE;
	END (*IF*);
    END INCVB;

(************************************************************************)

PROCEDURE DECV (VAR (*INOUT*) dest: CARDINAL;  src: CARDINAL): BOOLEAN;

    (* Computes dest := dest - src, and returns TRUE iff the		*)
    (* subtraction produced a borrow.					*)

    BEGIN
	IF dest < src THEN
	    INC (dest, MAX(CARDINAL) - src + 1);  RETURN TRUE;
	ELSE
	    DEC (dest, src);  RETURN FALSE;
	END (*IF*);
    END DECV;

(************************************************************************)

PROCEDURE DECVB (VAR (*INOUT*) dest: BYTE;  src: BYTE): BOOLEAN;

    (* Computes dest := dest - src, and returns TRUE iff the		*)
    (* subtraction produced a borrow.					*)

    BEGIN
	IF dest < src THEN
	    dest := LowByte(ORD(dest) + (256 - ORD(src)));  RETURN TRUE;
	ELSE
	    dest := LowByte(ORD(dest) - ORD(src));
	    RETURN FALSE;
	END (*IF*);
    END DECVB;

(************************************************************************)

PROCEDURE Mul (A, B: CARDINAL): LONGCARD;

    (* Same as A*B, except for the type of the result.  We provide this	*)
    (* as a general-purpose function since this combination of operands	*)
    (* is often precisely what is wanted.				*)

    BEGIN
	RETURN VAL(LONGCARD,A) * VAL(LONGCARD,B);
    END Mul;

(************************************************************************)

PROCEDURE MulB (A, B: BYTE): CARDINAL;

    (* Same as A*B, except for the type of the result.  We provide this	*)
    (* as a general-purpose function since this combination of operands	*)
    (* is often precisely what is wanted.				*)

    BEGIN
	RETURN ORD(MakeWord(LowByte(0),A)) * ORD(MakeWord(LowByte(0),B));
    END MulB;

(************************************************************************)

PROCEDURE IMul (A, B: INTEGER): LONGINT;

    (* Like Mul, but signed. *)

    BEGIN
	RETURN VAL(LONGINT,A) * VAL(LONGINT,B);
    END IMul;

(************************************************************************)

PROCEDURE IMulB (A, B: BYTE): INTEGER;

    (* Like MulB, but signed. *)

    BEGIN
	RETURN SignExtend(A) * SignExtend(B);
    END IMulB;

(************************************************************************)

PROCEDURE DivB (A: CARDINAL;  B: BYTE): BYTE;

    (* Same as A DIV B, except for the type of A.  We provide this as	*)
    (* a general-purpose function since this combination of operands	*)
    (* is often precisely what is wanted.				*)

    BEGIN
	RETURN LowByte(A DIV ORD(MakeWord(LowByte(0),B)));
    END DivB;

(************************************************************************)

PROCEDURE Div (A: LONGCARD;  B: CARDINAL): CARDINAL;

    (* Same as A DIV B, except for the type of A.  We provide this as	*)
    (* a general-purpose function since this combination of operands	*)
    (* is often precisely what is wanted.				*)

    BEGIN
	RETURN VAL(CARDINAL, A DIV VAL(LONGCARD,B));
    END Div;

(************************************************************************)
(*			     BLOCK MOVES				*)
(************************************************************************)

PROCEDURE Copy (source, destination: ADDRESS;  bytecount: CARDINAL);

    (* Copies an array of bytes from the source address to the		*)
    (* destination address.  In the case where the two arrays overlap,	*)
    (* the destination address should be lower in physical memory than	*)
    (* the source address.						*)

    BEGIN
	(*<TopSpeed Lib.Move (source, destination, bytecount); >*)
	(*<FST System.Move (source, destination, bytecount); >*)
    END Copy;

(************************************************************************)

PROCEDURE FarCopy (source, destination: FarPointer;  bytecount: CARDINAL);

    (* Copies an array of bytes from the source address to the		*)
    (* destination address.  In the case where the two arrays overlap,	*)
    (* the destination address should be lower in physical memory than	*)
    (* the source address.						*)

    BEGIN
	(*<TopSpeed Lib.Move (source, destination, bytecount); >*)
	(*<FST System.Move (source, destination, bytecount); >*)
    END FarCopy;

>*)

(************************************************************************)

PROCEDURE CopyUp (source, destination: FarPointer;  bytecount: CARDINAL);

    (* A variant of Copy which does the move backwards, in order	*)
    (* to handle the case where the destination address is inside the	*)
    (* source array.  In this special case Copy cannot be used,		*)
    (* because it would overwrite data it was about to copy.		*)

    BEGIN
	(*<TopSpeed*) Lib.Move (source, destination, bytecount); (*>*)
	(*<FST System.Move (source, destination, bytecount); >*)
    END CopyUp;

(************************************************************************)

(*<~TopSpeed3

PROCEDURE BlockFill (destination: FarPointer;
				bytecount: CARDINAL;  value: BYTE);

    (* Fills the destination array with the given value.	*)

    (*<FST VAR p: POINTER TO BYTE;  j: CARDINAL; >*)

    BEGIN
	(*<TopSpeed Lib.Fill (destination, bytecount, value); >*)
	(*<FST
	p := destination;
	FOR j := 1 TO bytecount DO
	    p^ := value;  p := AddOffset(p,1);
	END (*FOR*);
	>*)
    END BlockFill;

(************************************************************************)

PROCEDURE BlockFillWord (destination: FarPointer;  wordcount: CARDINAL;
							value: WORD);

    (* Fills the destination array with the given value.	*)

    (*<FST VAR p: POINTER TO WORD;  j: CARDINAL; >*)

    BEGIN
	(*<TopSpeed Lib.WordFill (destination, wordcount, value); >*)
	(*<FST
	p := destination;
	FOR j := 1 TO wordcount DO
	    p^ := value;  p := AddOffset(p, 2);
	END (*FOR*);
	>*)
    END BlockFillWord;

(************************************************************************)
(*			    INPUT AND OUTPUT				*)
(************************************************************************)

PROCEDURE OutByte (port: CARDINAL; value: BYTE);

    (* Puts the value out to an output port.	*)

    BEGIN
	(*<TopSpeed SYSTEM.Out (port, value); >*)
	(*<FST
	ASM
	    MOV DX, port
	    MOV AL, value
	    OUT DX, AL
	END (*ASM*);
	>*)
    END OutByte;

(************************************************************************)

PROCEDURE InByte (port: CARDINAL): BYTE;

    (* Reads a byte from an input port.	*)

    (*<FST VAR result: BYTE; >*)

    BEGIN
	(*<TopSpeed RETURN SYSTEM.In (port); >*)
	(*<FST
	ASM
	    MOV DX, port
	    IN AL, DX
	    MOV result, AL
	END (*ASM*);
	RETURN result;
	>*)
    END InByte;

>*)

(************************************************************************)

PROCEDURE InStringWord (port: CARDINAL;  BufferAddress: ADDRESS;
						count: CARDINAL);

    (* Reads count words from an input port.	*)

    VAR j: CARDINAL;  p: POINTER TO WORD;
	lobyte, hibyte: BYTE;

    BEGIN
	(*<FST
	ASM
	    PUSHF
	    CLI
	    CLD
	    MOV CX, count
	    LES DI, BufferAddress
	    MOV DX, port
	    (*REP INSW*)
	    POPF
	END (*ASM*);
	>*)

	(*<TopSpeed1
	MiscLib.InStringWord (port, BufferAddress, count);
	>*)

	(*<TopSpeed3*)
	p := BufferAddress;
	FOR j := 1 TO count DO
	    p^ := SYSTEM.InW (port);
	    Lib.IncAddr (p, 2);
	END (*FOR*);
	(*>*)
    END InStringWord;

(************************************************************************)

PROCEDURE OutStringWord (port: CARDINAL;  BufferAddress: ADDRESS;
						count: CARDINAL);

    (* Writes count words to an output port.	*)

    VAR j: CARDINAL;  p: POINTER TO WORD;

    BEGIN
	(*<FST
	ASM
	    PUSHF
	    CLI
	    CLD
	    MOV CX, count
	    LDS SI, BufferAddress
	    MOV DX, port
	    (*REP OUTSW*)
	    POPF
	END (*ASM*);
	>*)

	(*<TopSpeed1
	MiscLib.OutStringWord (port, BufferAddress, count);
	>*)

	(*<TopSpeed3*)
	p := BufferAddress;
	FOR j := 1 TO count DO
	    SYSTEM.OutW (port, p^);
	    Lib.IncAddr (p, 2);
	END (*FOR*);
	(*>*)
    END OutStringWord;

(************************************************************************)

(*<FST
BEGIN
    power2[0]  := 1;
    power2[1]  := 2;
    power2[2]  := 4;
    power2[3]  := 8;
    power2[4]  := 16;
    power2[5]  := 32;
    power2[6]  := 64;
    power2[7]  := 128;
    power2[8]  := 256;
    power2[9]  := 512;
    power2[10] := 1024;
    power2[11] := 2048;
    power2[12] := 4096;
    power2[13] := 8192;
    power2[14] := 16384;
    power2[15] := 32768;
>*)
END LowLevel.
