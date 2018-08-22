IMPLEMENTATION MODULE Explode;
(*****************************************************************************)
(**  Procedures to produce sound effects.                                   **)
(*****************************************************************************)

FROM Music IMPORT SetNoteDuration,SetOctave,PlayMusic,WaitForMusicFinished;


PROCEDURE ExplodeSound(Explosion:CARDINAL);

BEGIN
    SetNoteDuration(5);
    SetOctave(2);
    CASE Explosion OF
         1:     SetNoteDuration(5);
                SetOctave(2);
                PlayMusic("CC#uCC#dCC#EE");  (* Hit Dirt with missile *)
         |
         2:       (*  tank explode *)
                SetOctave(5);
                SetNoteDuration(10);
                PlayMusic("CCDCDEdCDECDDCDFdCFDEFDECFDdCFDEFCDABCDdCDEFEFEDdCDFEEFDEDED");


         |
         3:     (* Vertical Guidence hit dirt*)
                SetOctave(4);
                SetNoteDuration(5);
                PlayMusic("GuGuGCdCdC");
         |
         4:     (* Dirt Blaster *)
                SetNoteDuration(30);
                SetOctave(2);
                PlayMusic("E#EE#EE#CC#GGuG");
         END; (*CASE*)


END ExplodeSound;

END Explode.    	   	
    	