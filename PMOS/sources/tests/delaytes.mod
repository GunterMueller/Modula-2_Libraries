MODULE DelayTest;

	(********************************************************)
	(*							*)
	(*	Test of how well we can read back the count	*)
	(*	of the timer that normally drives the speaker.	*)
	(*							*)
	(*	This program doesn't produce any output, I	*)
	(*	rely on the debugger to look at the results.	*)
	(*							*)
	(*	Programmer:	P. Moylan			*)
	(*	Last edited:	5 March 1995			*)
	(*	Status:		Working				*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* proc *)	BYTE, ADR;

FROM LowLevel IMPORT
    (* proc *)	InByte, OutByte, LowByte, HighByte, MakeWord, IANDB, IORB;

(************************************************************************)

VAR
    Sample: ARRAY [0..1000] OF BYTE;

(************************************************************************)

PROCEDURE SpeakerOff;

    (* Turns off both the timer gate and the speaker.	*)

    BEGIN
	OutByte (97, IANDB(InByte (97), 0FCH));
    END SpeakerOff;

(************************************************************************)

PROCEDURE SpeakerOn;

    (* Turns on the speaker and enables the timer gate.	*)

    BEGIN
	OutByte (97, IORB(InByte (97), 3));
    END SpeakerOn;

(************************************************************************)

PROCEDURE EnableTimerGate;

    (* Enables the timer gate without turning on the speaker.	*)

    BEGIN
	OutByte (97, IORB(IANDB(InByte (97), 0FCH), 1));
    END EnableTimerGate;

(************************************************************************)

PROCEDURE RunTheTest;

    CONST TimerControlPort = 67;  channel2 = 66;

    VAR j: CARDINAL;  CycleCount: BYTE;

    BEGIN
	CycleCount := 64;

	(*SpeakerOn;*)	(* As a check that the count's not too high	*)
	EnableTimerGate;

	(* The control byte sent to the timer control port specifies:	*)
	(* timer #2, one count bytes to follow, and mode 2.  CycleCount	*)
	(* specifies the frequency, via the formula			*)
	(* freq = 1193180/CycleCount.  This gives a frequency range of	*)
	(* 18.2 Hz (count=65535) to 1.193 MHz.				*)
	(* The bit assignments in the control byte are as follows:	*)
	(*	7-6	timer # (0, 1, or 2) (3 is for readback)	*)
	(*	5-4	00=different command, 01=low count byte to	*)
	(*		follow, 10=high count byte to follow, 11=two	*)
	(*		count bytes to follow (low order first)		*)
	(*	3-1	mode						*)
	(*	0	0=binary, 1=BCD					*)
	(* The results for different modes are:				*)
	(*	0,1	counts down to 0, then continues counting	*)
	(*		down from 65535					*)
	(*	2	counts down to 0, then restarts cycle with	*)
	(*		original count.					*)
	(*	3	(square wave) counts down to zero by twos,	*)
	(*		flips output and counts down to zero again,	*)
	(*		then restarts the cycle with original count.	*)
	(*	4	much the same as mode 1.  The difference is	*)
	(*		in hardware triggering, but for our present	*)
	(*		purposes that difference is irrelevant.		*)
	(*	5	counts down from some random number, ignores	*)
	(*		initial count.  I guess this is waiting for	*)
	(*		an external trigger.				*)
	(*	6	same as mode 2.					*)
	(*	7	same as mode 3.					*)

	OutByte (TimerControlPort, 094H);
	OutByte (channel2, CycleCount);

	(* Now read back the count, as fast as we can go. *)

	FOR j := 0 TO HIGH(Sample) DO

	    (* The "counter latch" command, to take a copy of the	*)
	    (* current count in timer #2, would be an 80H sent to the	*)
	    (* control port.  Since the count is always a one-byte	*)
	    (* quantity in our tests, we can save some time by skipping	*)
	    (* the "latch" step.					*)

	    (*OutByte (TimerControlPort, 80H);*)
	    Sample[j] := InByte (channel2);

	END (*FOR*);

	(* Result of test (obtained by looking with VID): on my DX/2	*)
	(* and using mode 3, each sample differed by 14, i.e. the	*)
	(* count changed by 14 in the time it took to get around the	*)
	(* loop.  The jitter was about plus or minus 2.			*)
	(* Removing run-time checks had negligible effect.		*)
	(* Going to mode 2, using a one-byte counter, and setting all	*)
	(* options to "fastest possible" got me to the point where	*)
	(* the decrement was usually 2 and sometimes 3.  I don't think	*)
	(* I can go any faster except by going to assembly language	*)
	(* - and even then it won't be very much faster.		*)
	(* Somewhat surprisingly, turning turbo mode off doesn't seem	*)
	(* to affect the results very much - I don't understand this.	*)

    END RunTheTest;

(************************************************************************)
(*			MODULE INITIALISATION				*)
(************************************************************************)

BEGIN
    RunTheTest;
    SpeakerOff;
END DelayTest.
