(*****************************************************************************)
(*  TANKBLAST SVGA  A PMOS DEMONSTRATION PROGRAM                             *)
(*  AUTHOR : JASON HARPER                                                    *)
(*           ELEC 460   1993                                                 *)
(*									*)
(*	Modifications by P. Moylan					*)
(*	Last edited:	23 February 1994				*)
(*	Status:		A long way to go!				*)
(*		My original assumption was that this could be made to	*)
(*		work with relatively little work.  It is now evident	*)
(*		that he has been using his own custom modifications to	*)
(*		various PMOS modules, and I don't have the source for	*)
(*		those modifications so I have to re-create alternatives.*)
(*									*)
(*                                                                           *)
(*  THIS PROGRAM WAS WRITTEN TO DEMONSTRATE EXISTING AND NEW FEATURES OF     *)
(*  PMOS WITH EMPHASIS ON GRAPHICS.                                          *)
(*                                                                           *)
(*  STATUS:  WORKING OK EXCEPT FOR :                                         *)
(*           :OCCASIONALY THE KEYS 'LOCK UP'.  THIS CAN BE FIXED BY PRESSING *)
(*            THE Ctrl KEY OR ARROW KEYS UNTIL NORMAL CONTROL IS REGAINED.   *)
(*           :OCCASIONALY A THE PROGRAM CRASHES WHEN A VARIABLE GOES OUT OF  *)
(*            RANGE. COULD BE TRACKED DOWN BUT IT ONLY HAPPENS RARELY.       *)
(*                                                                           *)
(*  DEMONSTRATION FEATURES:                                                  *)
(*    :  PLOTTING OF VOLATILE LINES AND POINTS IN GRAPHICS WINDOWS.          *)
(*    :  PLOTTING TEXT IN GRAPHICS WINDOWS.                                  *)
(*    :  THE USE OF 256 COLOUR PALLETTE, AND CHANGING THE PALLETTE.          *)
(*    :  USING AND WRITING TO TEXT WINDOWS.                                  *)
(*    :  USING MENUS.                                                        *)
(*    :  USING RANDOM FUNCTIONS.                                             *)
(*    :  USING SOUND.                                                        *)
(*    :  AND MUCH MORE ...                                                   *)
(*                                                                           *)
(*****************************************************************************)



MODULE BlastWin;

FROM Conversions IMPORT
    (* proc *)	CardinalToString;

FROM Graphics IMPORT
    (* type *)	ColourType;

FROM GWindows IMPORT OpenWindow,SetColour,Window,single,double,SetCursor,
     ClearWindow,WriteString,WriteLn,CloseWindow,PutPixel2C,Line2C,
     InitGraphics;

FROM Random IMPORT RANDOM,Randomize;
FROM IO IMPORT WrChar,WrReal,WrLn,WrInt;
FROM Graphics IMPORT SetDefaultMode,SetMode,SetPaletteColour,PlotDot,Fill,PlotLine,GetScreenShape;
FROM Timer IMPORT Sleep;
FROM ScreenGeometry IMPORT Point;
FROM MATHLIB IMPORT Sin,Cos,Sqrt;
FROM Keyboard IMPORT InKey;
FROM Music IMPORT PlayMusic,SetNoteDuration,WaitForMusicFinished(*,SetOctave*);
FROM Menu2 IMPORT initvalu,BlastMenu,playertype;
FROM KBdriver IMPORT GetScanCode;
FROM Tank IMPORT MainTank;
FROM Explode IMPORT ExplodeSound;
VAR j,l:LONGCARD;
    x,y,down1,up2,down2,up1,k,bombsize,
    music,screensize,damage1,damage2,MaxGround,
    RightBorder,TopBorder,stars,Weapon:INTEGER;
    a,b,Vel1,Vel2,wind,Wind,velocity:REAL;
    angle1,angle2:LONGREAL;
    GroundLine:ARRAY [-10..1280] OF INTEGER;
    DirtFall:ARRAY [1..1280] OF INTEGER;
    Weapons1: ARRAY [0..3] OF INTEGER;
    Weapons2: ARRAY [0..3] OF INTEGER;
    BkgrndColr:ARRAY [0..63] OF ColourType;
    GroundColour:ARRAY [0..63] OF ColourType;
    w1,w2,wmain,wblast,wquit,wback:Window;
    Tank1,Tank2,explode,CompExplode:Point;
    colr1,colr2,MaxColour:ColourType;
    key2,PlayAgain:CHAR;
    L,B,T,R,left,bottom,right,top,count,XMAX,YMAX:CARDINAL;
    sunset,Sound,Trace,CtrlPress,Tank1Explode,Tank2Explode,HitToLeft,HitToRight,
    MorAng,SmartCom,Human,AnglChanged,VelChanged,initialised:BOOLEAN;
    init:initvalu;
    ScanCode:BYTE;

CONST pi          = 3.141592654;
      player1     = 1;
      player2     = 2;
      quitnow     = -99.0;
      CtrlCode    = BYTE(1DH);
      CtrlRelease = BYTE(9DH);
      Up          = BYTE(48H);
      Down        = BYTE(50H);
      Left        = BYTE(4BH);
      Right       = BYTE(4DH);
      Enter       = BYTE(9CH);
      Space       = BYTE(185);
      Alt         = BYTE(184);
      F           = BYTE(161);
      W           = BYTE(91H);
      Esc         = BYTE(129);
      RANDNUM     = 923925;

(************************************************************************)
(*			SOME BASIC WRITE OPERATIONS			*)
(************************************************************************)

PROCEDURE WriteCard (w: Window;  number: CARDINAL;  colour: ColourType);

    VAR buffer: ARRAY [0..5] OF CHAR;

    BEGIN
	SetColour (w, colour);
	CardinalToString (number, buffer, 5);
	WriteString (w, buffer);
    END WriteCard;

(************************************************************************)

PROCEDURE WriteStringC (w: Window;  str: ARRAY OF CHAR;  colour: ColourType);

    BEGIN
	SetColour (w, colour);
	WriteString (w, str);
    END WriteStringC;

(************************************************************************)
PROCEDURE Fire (w:Window;Velh,Vely,Gravity:REAL;
		wind,x0,y0,player:INTEGER;colr:ColourType); FORWARD;
PROCEDURE PlotCircle2(w1:Window;x,y,radius:INTEGER;  colr: ColourType); FORWARD;



(*************************************************************************
    A very crude way of delting a number of characters.  I'm sure there
    must be an easier way but I couldn't find it.   CHR(219) doesnt seem
    to work in graphics mode.
**************************************************************************)
PROCEDURE DelChar (w:Window;row,column,numchars:CARDINAL);
VAR i,r,c:CARDINAL;
BEGIN
    r:=row;
    c:=column;
    FOR i:=1 TO numchars DO
        SetCursor(w,r,c);
        WriteStringC(w,CHR(7),0);
        SetCursor(w,r,c);
        WriteStringC(w,CHR(8),0);
        INC(c);
        END;
        SetCursor(w,row,column);
END DelChar;

(*************************************************************************)
(*  WriteWin :   writes   fireing specifications as they are changed     *)
(*************************************************************************)
PROCEDURE WriteWin(w:Window;Weapon,WeaponsLeft,damage,screen,
	wind,player,changed:INTEGER;colr:ColourType;angle:LONGREAL;Vel:REAL);
BEGIN
        CASE changed OF
        1:
            DelChar(w,0,17,3);
            WriteCard(w,CARDINAL(0.5+angle*180.0/pi),2);
        |
        2:
            DelChar(w,0,30,4);
            WriteCard(w,CARDINAL(10.0*Vel),2);
        |
        3:
            DelChar(w,4,9,20);
            WriteCard(w,CARDINAL(WeaponsLeft),2);
            CASE Weapon OF
              0:     WriteStringC(w,"  Missile",2);
              |
              1:     WriteStringC(w,"  Super Missile",2);
              |
              2:     IF screen = 1 THEN
                        WriteStringC(w,"  Vertical Guidance",2);
                     ELSE WriteStringC(w,"  Vertical Guide",2);
                     END;
              |

              3:     WriteStringC(w,"  Dirt Blaster",2);

              END (* CASE*);
         END (*CASE*);
END WriteWin;

(*************************************************************************
** Sets up display of Player, and initial fireing specs                 **
*************************************************************************)
PROCEDURE SetWin(w:Window;wind,damage,player:INTEGER;colr:ColourType);
BEGIN
       IF player = 1 THEN WriteStringC(w,"PLAYER 1 ",colr);
            ELSE               WriteStringC(w,"PLAYER 2 ",colr);
            END;

            WriteStringC(w,"ANGLE      VELOCITY",colr);
            SetCursor(w,2,0);
            WriteStringC(w,"WIND ",colr);
            SetCursor(w,2,5);
            WriteCard(w,CARDINAL(ABS(10*wind)),2);
            IF wind < 0 THEN WriteStringC(w," <-- ",colr); END;
            IF wind >0  THEN WriteStringC(w," --> ",colr); END;

            SetCursor(w,2,29);
            WriteCard(w,CARDINAL(damage),2);
            SetCursor(w,2,21);
            WriteStringC(w,"DAMAGE:   %",colr);
            SetCursor(w,4,0);

         WriteStringC(w,"WEAPON: ",colr);

END SetWin;

(***************************************************************************
**  Returns a background colour at a particular height                    **
***************************************************************************)
PROCEDURE ColourY(y,colr :INTEGER):ColourType;
BEGIN
    IF colr <> -1 THEN RETURN VAL(ColourType,colr)
      ELSE RETURN BkgrndColr[INTEGER(REAL(y)/(REAL(TopBorder)/63.0))];
    END;
END ColourY;

(***************************************************************************
**  Produces stars in the background.                                     **
***************************************************************************)
PROCEDURE StaryNight(w:Window);
VAR a,b:REAL;
    y1,y2,x1,x2,stars,stars2:CARDINAL;
    BEGIN
        FOR stars := 1 TO 5 DO
        a:=RANDOM();
        b:=RANDOM();
        y1:=CARDINAL(a*REAL(TopBorder+10));
        y2:=CARDINAL(b*REAL(TopBorder+10));
        x1:=CARDINAL(a*REAL(RightBorder-2))+1;
        x2:=CARDINAL(b*REAL(RightBorder-2))+1;

        IF (stars <3) AND(y2 > CARDINAL(GroundLine[x1])+30)AND(y2<CARDINAL(TopBorder)-5) THEN
           PutPixel2C(w,x1,y2,15); END;
        IF (x2>5) AND (x2 <CARDINAL(RightBorder-5)) AND
           (y1 > CARDINAL(GroundLine[x2])+20) AND
           (y1 > CARDINAL(GroundLine[x2+5])+20) AND
           (y1 > CARDINAL(GroundLine[x2-5])+20)
        THEN
           PlotCircle2(w,x2,y1,9,0);END;
        END;
    END StaryNight;





(*************************************************************************)
(*  Aim :   Accept user input to aim the tanks fire.                     *)
(*************************************************************************)
PROCEDURE Aim(Weapons:ARRAY OF INTEGER; damage,screen,player,x,y:INTEGER;
	angle:LONGREAL;colr:ColourType;Vel,wind:REAL;w,wmain:Window):LONGREAL;
VAR key:CHAR;
    x1,y1,wind2,changed:INTEGER;
    ang:CHAR;
    scan:BYTE;
    CtrlPress:BOOLEAN;
    Velh,vel2:REAL;

BEGIN
    CtrlPress:=FALSE;
    x1:=x;
    y1:=y;
    wind2:=INTEGER(wind);
    Weapon:=0;     (* missile*)
    SetWin(w,wind2,damage,player,colr);
    FOR changed:=1 TO 3 DO
        WriteWin(w,Weapon,Weapons[Weapon],damage,screen,wind2,player,changed,colr,angle,Vel);
        END; (*FOR*)
    changed:=6;

    SetNoteDuration(3);
    (*SetOctave(0);*)

    LOOP
       IF (stars=1) THEN StaryNight(wmain); END;
       WriteWin(w,Weapon,Weapons[Weapon],damage,screen,wind2,player,changed,colr,angle,Vel);
       Line2C(wmain,x,y,x1,y1,ColourY(y1,-1));
       Line2C(wmain,x+1,y,x1+1,y1,ColourY(y1,-1));
       x1:= x+INTEGER(8.0*Cos(angle));
       y1:= y+VAL(INTEGER,8.0*Sin(angle));
       Line2C(wmain,x,y,x1,y1,colr);
       Line2C(wmain,x+1,y,x1+1,y1,colr);
       IF (NOT Human) AND (player = 2) THEN EXIT; END;
       scan:=GetScanCode();
       (* decrease velocity: down arrow *)
       IF (scan=Down) AND (Vel >0.0) THEN
           IF Sound THEN PlayMusic("D"); END;
           IF CtrlPress THEN
              Vel :=Vel -1.0;
              ELSE Vel:=Vel -10.0;
              END;
           IF Vel < 0.0 THEN Vel := 0.0; END;
           changed:=2;
           a:=RANDOM();
           (* increase  angle: left arrow *)
       ELSIF (scan=Left) AND (angle <pi) THEN
           IF CtrlPress THEN
              angle := angle +1.0 *pi/180.0;
              ELSE angle := angle +5.0 *pi/180.0;
              END;
           IF Sound THEN PlayMusic("C");END;
           IF angle >180.0 THEN angle :=180.0; END;
           changed:=1;
           (* decrease angle: right arrow *)
       ELSIF (scan=Right) AND (angle >0.0) THEN
           IF CtrlPress THEN
              angle := angle -1.0*pi/180.0;
              ELSE angle := angle -5.0*pi/180.0;
              END;
           IF Sound THEN PlayMusic("C"); END;
           IF angle < 0.0 THEN angle :=0.0; END;
           changed:=1;
           (* increase velocity: up arrow *)
       ELSIF (scan=Up) AND (Vel <200.0) THEN
           changed:=2;
           IF Sound THEN PlayMusic("D");END;
           IF CtrlPress THEN
              Vel := Vel +1.0;
              ELSE Vel := Vel +10.0;
              END;
           IF Vel >200.0 THEN Vel := 200.0; END;
       ELSIF(scan = Esc) THEN RETURN quitnow;
       ELSIF (scan = Enter)OR(scan=Space) THEN EXIT;
       ELSIF (scan = CtrlCode ) THEN CtrlPress:=TRUE;
       ELSIF (scan=CtrlRelease) THEN CtrlPress:=FALSE;
       ELSIF (scan=W)OR(scan=Alt) THEN
             REPEAT
                   IF Sound THEN PlayMusic("uu**E//dd"); END;
                   IF Weapon = 3 THEN Weapon :=0;
                   ELSE INC(Weapon);
                   END (*IF*);
             UNTIL Weapons[Weapon]  <> 0;
             changed:=3;
       END (*IF*);

       END;(*LOOP*)
       IF (NOT Human) AND (player = 2) AND (NOT SmartCom) THEN
        IF VelChanged THEN
             VelChanged:=FALSE;
             AnglChanged:=TRUE;
             IF (CompExplode.x < Tank1.x) THEN
               angle := angle -5.0*pi/180.0;
               ELSE angle := angle +5.0 *pi/180.0;
               END;
             IF angle >3.0*pi/4.0 THEN angle:=3.0*pi/4.0; END;
             WriteWin(w,Weapon,Weapons[Weapon],damage,screen,wind2,player,1,colr,angle,Vel);

          ELSE
             AnglChanged:=FALSE;
             VelChanged:=TRUE;
             IF CompExplode.x < Tank1.x THEN
              Vel:=Vel-10.0;
              ELSE Vel:=Vel+10.0;
              END;
             IF Vel < 40.0 THEN Vel:=40.0; END;
             WriteWin(w,Weapon,Weapons[Weapon],damage,screen,wind2,player,2,colr,angle,Vel);

         END;


       ELSIF (SmartCom) AND (player = 2) THEN
               IF (MorAng) AND(CompExplode.x >Tank1.x) THEN
                angle := angle -5.0*pi/180.0;
                IF angle >3.0*pi/4.0 THEN angle:=3.0*pi/4.0; END;


               ELSE
                IF ((HitToLeft AND (CompExplode.x > Tank1.x)) OR
                   (HitToRight AND (CompExplode.x < Tank1.x))) AND
                   (CompExplode.x <> 9999) THEN
                   velocity:=velocity/2.0;
                  END;

                IF CompExplode.x < Tank1.x THEN
                  HitToLeft:=TRUE;
                  HitToRight:=FALSE;
                  Vel:=Vel-velocity;
                 ELSE Vel:=Vel+velocity;
                  HitToRight:=TRUE;
                 END;

               END;
               WriteWin(w,Weapon,Weapons[Weapon],damage,screen,wind2,player,2,colr,angle,Vel);

        END;


       IF player = 1 THEN
          DEC(Weapons1[Weapon]);
          Vel1:=Vel;
       ELSE
          DEC(Weapons2[Weapon]);
          Vel2 :=Vel;
       END;

        Velh := Vel*VAL(REAL,Cos(angle));
        IF (SmartCom) AND (player = 2) THEN Velh:=Velh-REAL(wind2);END;

       Fire(wmain,Velh,Vel*VAL(REAL,Sin(angle)),9.8,wind2,x1,y1,player,colr);
       RETURN angle;
   END Aim;


(*************************************************************************)
(*  TankDraw : Draw a Tank.                                              *)
(*************************************************************************)
PROCEDURE TankDraw (wmain:Window; x,y,colr:INTEGER);

BEGIN


    Line2C(wmain,x-4,y+1,x+4,y+1,ColourY(y+1,colr));
    Line2C(wmain,x-5,y+2,x+5,y+2,ColourY(y+2,colr));
    IF colr <>-1 THEN
        PutPixel2C(wmain,x-2,y+2,0);
        PutPixel2C(wmain,x,y+2,0);
        PutPixel2C(wmain,x+2,y+2,0);
        END (*IF*);
    Line2C(wmain,x-6,y+3,x+6,y+3,ColourY(y+3,colr));
    Line2C(wmain,x-5,y+4,x+5,y+4,ColourY(y+4,colr));
    Line2C(wmain,x-2,y+5,x+2,y+5,ColourY(y+5,colr));
    Line2C(wmain,x-3,y+6,x+3,y+6,ColourY(y+6,colr));
END TankDraw;

(*************************************************************************)
(*  DrawLandscape: Draw the landscape.                                   *)
(*************************************************************************)
PROCEDURE DrawLandscape(w1:Window);

VAR     c,i,w,z,ClrXY,num:INTEGER;
        rand,up,down,right2,top2,left2,ClrX,ClrY:REAL;
        count:CARDINAL;

BEGIN
          right2:=REAL(RightBorder);
          top2:=REAL(TopBorder);

    IF (stars<>1) THEN
    FOR count := 1 TO TopBorder+1 DO
        Line2C(w1,1,count,RightBorder-1,count,ColourY(INTEGER(count),-1));
        END;
    END;

FOR num := -10 TO 0 DO
    GroundLine[num]:=y;
    END;

    x:=1;
    WHILE x< RightBorder DO
          rand:=RANDOM();

          IF (x>INTEGER(right2/3.2)) AND (x<INTEGER(right2/2.3)) THEN
                 up:= VAL(REAL,up1);
                 z:=TRUNC(up*rand);
                 y:=y+z-up2;
          ELSIF (x<INTEGER(right2/1.5)) AND (x>INTEGER(right2/1.9))  THEN
                 down:= VAL(REAL,down1);
                 z:=TRUNC(down*rand);
                 y:=y+z-down2;
          ELSE
                 z:=TRUNC(5.0*rand);
                 y:=y+z-2;

          END;  (*IF*)

          IF y>INTEGER(top2/1.2) THEN y:=y-5;
          END (*FOR*);
          IF y<20 THEN y:=23;
          END (*FOR*) ;
          IF y>MaxGround THEN MaxGround :=y;END;
          GroundLine[x]:=y;
          FOR count:= y TO 13 BY -12 DO
             ClrX:=((26.0+REAL(GroundLine[x]-GroundLine[x-10]))*64.0/52.0);
             ClrY:=  30.0;
             ClrXY:=INTEGER((REAL(count)*ClrX+REAL(y-INTEGER(count))*ClrY)/REAL(y));
             IF ClrXY > 63 THEN ClrXY := 63;
             ELSIF ClrXY < 0 THEN ClrXY :=0;
               END;
               Line2C(w1,x,count,x,count-12,GroundColour[ClrXY]);

           END;
           Line2C(w1,x,12,x,1,GroundColour[ClrXY]);

          (* draw snow *)
          IF y >INTEGER(top2/1.5) THEN Line2C(w1,x,y,x,INTEGER(top2/1.5)-INTEGER(5.0*rand),7); END;
          x:=x+1;
          END; (*while*)

          END DrawLandscape;

(****************************************************************************
**  Plots the explosion circle and calculates the distance for dirt to fall **
******************************************************************************)
PROCEDURE PlotCircle2(w1:Window;x,y,radius:INTEGER;  colr: ColourType);
VAR       a,c,r,p,s,t:INTEGER;
          u,v,w:LONGREAL;

BEGIN
    IF y-radius<0 THEN s:= -y+1;
    ELSE s:= 1-radius;
    END;
    FOR s:= s TO radius-1 DO
        v:=VAL(LONGREAL,s);
        w:=VAL(LONGREAL,radius);
        u:=w*w-v*v;
        t:=VAL(INTEGER,Sqrt(u));
        IF x-t<1 THEN r:=1;
        ELSE r:=x-t; END;
        IF x+t >= RightBorder THEN p:=RightBorder-1;
        ELSE p:=x+t;
        END;
        IF s+y >=TopBorder THEN s:=radius-1;
        ELSE Line2C(w1,r,s+y,p,s+y,ColourY(s+y,colr));
             IF s+x > 0 THEN DirtFall[s+x]:=y+t; END;
             END;
       END;
END PlotCircle2;




(************************************************************************
**   EXPLODE **  When missile hits ground or tank:  Draw explosion flash,
                 Remove dirt, landslide dirt, reset ground level.
*************************************************************************)
PROCEDURE Explode (w:Window;x,y,radius:INTEGER);
          VAR i,colr:SHORTCARD;
              x1,x2,y1,l,k:INTEGER;
          BEGIN


          explode.x :=x;
          explode.y:=y;
          k:=GroundLine[200];
          PlotCircle2(w,x,y,radius,4);    (* Draw explosion *)
          Sleep(100);
          PlotCircle2(w,x,y,radius,6);
          Sleep(100);
          PlotCircle2(w,x,y,radius,12);
          Sleep(100);
          PlotCircle2(w,x,y,radius,14);
          Sleep(100);
          PlotCircle2(w,x,y,radius,-1);

          IF x-radius+1 > 0 THEN x2 :=x-radius+1;
          ELSE x2:=1;
          END;
           LOOP
            FOR x1:=x2 TO x+radius-1 DO             (* Dirt falling down*)
              l:=1;
              IF x1 >=RightBorder THEN EXIT; END;
              IF GroundLine[x1] > DirtFall[x1] THEN  (* after explosion  *)
                 WHILE l<= 2*(DirtFall[x1]-y)+1 DO
                  PutPixel2C(w,x1,GroundLine[x1],ColourY(GroundLine[x1],-1));
                  PutPixel2C(w,x1,DirtFall[x1]-l+1,GroundColour[40]);
                  GroundLine[x1]:=GroundLine[x1]-1;
                  IF DirtFall[x1]-l =0 THEN l:=2*DirtFall[x1];END;         (* reset ground *)
                  INC(l);
                  END (*WHILE*);                              (* after explosion*)
              ELSIF GroundLine[x1] > 2*y-DirtFall[x1] THEN
                    GroundLine[x1]:=2*y-DirtFall[x1];
          (*    Sleep(100);*)
              END (*IF*);
              IF GroundLine[x1] <1 THEN GroundLine[x1] :=0; END;
            END (*FOR*);
            EXIT (*LOOP*);
           END (*LOOP*);
          END Explode;

PROCEDURE TankExplode(w:Window;x,y,bombsize2:INTEGER);
BEGIN
     Explode(w,x,y,bombsize2);
     IF Sound THEN ExplodeSound(2);END;
     Explode(w,x-5,y,5);
     Explode(w,x+5,y,5);
     END TankExplode;


(*******Shoot a line . When it hits the ground or tank call explode.***)
PROCEDURE Fire (w:Window; Velh,Vely,Gravity:REAL;wind,x0,y0,player:INTEGER;colr:SHORTCARD);
VAR maxy,x,y,xt,j,count,upoctave,downoctave:INTEGER;
    time,Vel1:REAL;
    Vel2:CARDINAL;

BEGIN

      x:=20;
      xt:=x0;

     IF Sound THEN SetNoteDuration(0);  (**** setting NoteDuraton ****)
        END;
     time:=0.07;
     count:=0;
     maxy:=y0;
     LOOP
              y:= y0+VAL(INTEGER,Vely*time -Gravity*time*time/2.0);
              IF y>maxy THEN maxy:=y; END;
              x:=x0+VAL(INTEGER,(Velh+REAL(wind))*time);
              IF (x>RightBorder) OR(x<1) THEN EXIT; END;
           IF Sound THEN
              Vel1:=ABS(VAL(REAL,Sqrt(VAL(LONGREAL,((Vely-9.8*time)*(Vely-9.8*time))))));

               Vel2:=CARDINAL(Vel1/35.0);
               SetOctave(Vel2);

               (*
               (* Use this if NoteDuration is set >= 1                      *)
               (* as the velocity of projectile increase the more often the *)
               (* sound must be played. This is a poor substitute to not    *)
               (* being able to make the note duration small enough         *)
               IF   ((Vel2<50)AND((count MOD 15) = 0))
                 OR ((Vel2<100)AND((count MOD 10) = 0))
                 OR ((Vel2<150)AND((count MOD 7) = 0))
                 OR ((Vel2<200)AND((count MOD 5) = 0))
                 OR ((Vel2>250)AND((count MOD 2) = 0))
               THEN PlayMusic("C");END;
               *)

                (* Use this if NoteDuration is set to zero *)
                (* produces smooth continuous notes but    *)
                (* the volume is low and changing octave   *)
                (* has no noticable effect.                *)

                PlayMusic("C");
               END;


           IF  Trace AND ((count MOD 15) = 0) AND (x<RightBorder) AND
               (x>1) AND (y<TopBorder) AND (y>1) THEN
               PutPixel2C(w,x,y,colr);
               PutPixel2C(w,x,y+1,colr);
               END (*IF*);



       IF Weapon = 1 THEN bombsize:=30;
       ELSE bombsize := 15;
       END;




       (*** Hit Tank1 ***)
    IF (Weapon<>3)AND((x > Tank1.x-9) AND (x < Tank1.x +9) AND (y<Tank1.y+9 )AND (y>Tank1.y-5)) THEN

       TankExplode(w,x,y,bombsize);
       Tank1Explode:=TRUE;
       EXIT;

       (*** Hit Tank2  ***)
    ELSIF (Weapon<>3)AND((x > Tank2.x-9) AND (x < Tank2.x +9) AND (y<Tank2.y+9 )AND (y>Tank2.y-5)) THEN
       TankExplode(w,x,y,bombsize);
       Tank2Explode:=TRUE;
       EXIT;

       (*** vertical guidence hit Tank1***)
    ELSIF (Weapon = 2) AND (x > Tank1.x-3) AND (x < Tank1.x +3) AND
          (y-GroundLine[Tank1.x]<INTEGER(REAL(Tank2.x-Tank1.x)/4.0)) THEN
       IF (Trace) THEN Line2C(w,Tank1.x,y,Tank1.x,Tank1.y,colr); END;
       TankExplode(w,Tank1.x,Tank1.y,bombsize);
       Tank1Explode:=TRUE;
       EXIT;

       (*** vertical guidence hit Tank2 ***)
    ELSIF (Weapon = 2) AND (x > Tank2.x-3) AND (x < Tank2.x +3) AND
          (y-GroundLine[Tank2.x]<INTEGER(REAL(Tank2.x-Tank1.x)/4.0)) THEN
       IF (Trace) THEN Line2C(w,Tank2.x,y,Tank2.x,Tank2.y,colr); END;
       TankExplode(w,Tank2.x,Tank2.y,bombsize);
       Tank2Explode:=TRUE;
       EXIT;



    ELSIF (x<RightBorder-2)AND((GroundLine[x]>=y)
       OR ((GroundLine[x]<=y) AND (GroundLine[x+1] >=y))) THEN
       IF (Weapon =3) THEN
          Explode(w,x,y+8,10);
          IF Sound THEN ExplodeSound(4);END;
          Explode(w,x-5,y+4,10);
          IF Sound THEN ExplodeSound(4);END;
          Explode(w,x+5,y+4,10);
          IF Sound THEN ExplodeSound(4);END;
         ELSE
          IF Sound THEN
          IF (Weapon =2) THEN ExplodeSound(3);
             ELSE ExplodeSound(1); END;
             END;
          Explode(w,x,y,bombsize);
         END;

        EXIT;
       END;

    INC(count);
    time:=time+0.01;
    xt:=x;
    END;  (*loop*)
    IF player = 2 THEN
       CompExplode.x:=x;
       CompExplode.y:=y;
       IF y < maxy THEN MorAng:=FALSE;
          ELSE MorAng:=TRUE;
          END;
       END;

    END Fire;

(******************************************************************************
** Tests to see if the tank has been hit or fallen. Redraw the tank in its   **
** new position.                                                             **
******************************************************************************)
PROCEDURE TankFall(w:Window;colr1,colr2:SHORTCARD);
VAR x1,x2,y1,y2:INTEGER;
BEGIN
    x1:=Tank1.x+INTEGER(8.0*Cos(angle1));
    y1:=Tank1.y+7+INTEGER(8.0*Sin(angle1));
    x2:=Tank2.x+INTEGER(8.0*Cos(angle2));
    y2:=Tank2.y+7+INTEGER(8.0*Sin(angle2));

    (* Tank1 has fallen *)
    IF Tank1.y <> GroundLine[Tank1.x] THEN
            TankDraw(w,Tank1.x,Tank1.y,-1);
            Line2C(w,Tank1.x,Tank1.y+7,x1,y1,ColourY(Tank1.y+7,-1));
            Line2C(w,Tank1.x+1,Tank1.y+7,x1+1,y1,ColourY(Tank1.y+7,-1));

            IF (Weapon=1) THEN INC(damage1,40);
               ELSIF(Weapon<>3) THEN INC(damage1,20);
               END;

            IF (damage1 >99) THEN
               TankExplode(w,Tank1.x,Tank1.y,15);
               Tank1Explode:=TRUE;
               ELSE Tank1.y := GroundLine[Tank1.x];
                    TankDraw(w,Tank1.x,Tank1.y,INTEGER(colr1));
                    Line2C(w,Tank1.x,Tank1.y+7,Tank1.x+8,Tank1.y+7,colr1);
                    angle1:=0.0;
                    Vel1:=50.0;
               END;

            (* Tank2 has fallen *)
       ELSIF Tank2.y <> GroundLine[Tank2.x] THEN
            TankDraw(w,Tank2.x,Tank2.y,-1);
            Line2C(w,Tank2.x,Tank2.y+7,x2,y2,ColourY(Tank2.y+7,-1));
            Line2C(w,Tank2.x+1,Tank2.y+7,x2+1,y2,ColourY(Tank2.y+7,-1));

            IF (Weapon=1) THEN INC(damage2,40);
               ELSIF(Weapon<>3) THEN INC(damage2,20);
               END;

            IF (damage2 >99) THEN
               TankExplode(w,Tank2.x,Tank2.y,15);
               Tank2Explode:=TRUE;
               ELSE Tank2.y := GroundLine[Tank2.x];
                    TankDraw(w,Tank2.x,Tank2.y,INTEGER(colr2));
                    IF Human THEN
                       angle2:=pi;
                       Line2C(w,Tank2.x,Tank2.y+7,Tank2.x-8,Tank2.y+7,colr2);
                    ELSE angle2:=3.0*pi/4.0;
                       Line2C(w,Tank2.x,Tank2.y+7,Tank2.x-5,Tank2.y+12,colr2);
                    END;
                    Vel2:=50.0;
               END;

      (* Tank1 hit but has not fallen *)
      ELSIF (Tank1.x > explode.x-bombsize-7) AND (Tank1.x < explode.x +bombsize+7) AND
            (Tank1.y<explode.y+bombsize) THEN
            Line2C(w,Tank1.x,Tank1.y+7,x1,y1,ColourY(Tank1.y+7,-1));
            Line2C(w,Tank1.x+1,Tank1.y+7,x1+1,y1,ColourY(Tank1.y+7,-1));

            IF (Weapon=1) THEN INC(damage1,40);
               ELSIF(Weapon<>3) THEN INC(damage1,20);
               END;

            IF (damage1 >99) THEN
               TankExplode(w,Tank1.x,Tank1.y,15);
               Tank1Explode:=TRUE;
               ELSE TankDraw(w,Tank1.x,Tank1.y,INTEGER(colr1));
                    Line2C(w,Tank1.x,Tank1.y+7,Tank1.x+8,Tank1.y+7,colr1);
                    angle1:=0.0;
                    Vel1:=50.0;
               END;

      (* Tank2 hit but has not fallen *)
      ELSIF (Tank2.x > explode.x-bombsize-7) AND (Tank2.x < explode.x +bombsize+7) AND
            (Tank2.y< explode.y+bombsize) THEN
            Line2C(w,Tank2.x,Tank2.y+7,x2,y2,ColourY(Tank2.y+7,-1));
            Line2C(w,Tank2.x+1,Tank2.y+7,x2+1,y2,ColourY(Tank2.y+7,-1));

            IF (Weapon=1) THEN INC(damage2,40);
               ELSIF(Weapon<>3) THEN INC(damage2,20);
               END;

            IF (damage2 >99) THEN
               TankExplode(w,Tank2.x,Tank2.y,15);
               Tank2Explode:=TRUE;
               ELSE TankDraw(w,Tank2.x,Tank2.y,INTEGER(colr2));

                    IF Human THEN
                       angle2:=pi;
                       Line2C(w,Tank2.x,Tank2.y+7,Tank2.x-8,Tank2.y+7,colr2);
                    ELSE angle2:=3.0*pi/4.0;
                       Line2C(w,Tank2.x,Tank2.y+7,Tank2.x-5,Tank2.y+12,colr2);
                    END;
                   IF NOT (SmartCom)THEN  Vel2:=50.0;
                   ELSE Vel2:=0.9*Vel2;
                   END;
               END;

            END;

END TankFall;

(*****************************************************************************
** Set the colour of tanks.                                                 **
*****************************************************************************)
PROCEDURE TankColour(wblast:Window;colrin:SHORTCARD;player:INTEGER):SHORTCARD;
VAR key:CHAR;
    colr:SHORTCARD;
BEGIN


    colr:=13;
    key := "~";

    WriteStringC(wblast,"          TANKBLAST  SVGA",4);
    WriteLn(wblast);
    WriteLn(wblast);
    WriteStringC(wblast,"   A PMOS demonstration program",2);
    WriteLn(wblast);
    WriteLn(wblast);
    WriteStringC(wblast,    "         By  JASON HARPER ",2);
    WriteLn(wblast);
    WriteLn(wblast);

    WriteStringC(wblast,    "         ELEC 460   1993",2);

    SetCursor(wblast,12,3);
        IF player = 1 THEN
           WriteStringC(wblast, "Player1 choose your tank colour",colr1);
           ELSE WriteStringC(wblast, "Player2 choose your tank colour",colr2)
        END (*IF*);
        WriteLn(wblast);
        WriteLn(wblast);
        WriteStringC(wblast,'  "C"    ',14);
        WriteStringC(wblast,'To change colour ',9);
        WriteLn(wblast);
        WriteLn(wblast);
        WriteStringC(wblast,"  SPACE  ",14);
        WriteStringC(wblast,"when done  ",9);


    WHILE (CAP(key) <> " ")AND (key<>CHR(0DH)) DO



           IF (CAP(key) = 'C')  THEN
              IF colr > 14 THEN colr:=1;
              ELSIF (colr = 12) AND (NOT Human) THEN colr:=13;
              ELSE INC(colr);
              END;
           END;
           IF colr <> colrin THEN
              MainTank(wblast,150,50,colr);
              key:=InKey();
              ELSE INC(colr);
              END;
           END (*WHILE *);
    ClearWindow(wblast,colr);
    RETURN colr;
END TankColour;

(*****************************************************************************
** Set the palette colours for a 'random'  sky.                             **
*****************************************************************************)
PROCEDURE Sky;
VAR red,green,blue:SHORTCARD;
    rand,rand2:INTEGER;
    colour,r,g,b:SHORTCARD;
    R,G,B:REAL;

BEGIN
     rand:=INTEGER(4.0*RANDOM());
     rand2:=INTEGER(11.0*RANDOM());
     FOR colour:= 64 TO 127  DO
     BkgrndColr[INTEGER(colour-64)]:= colour;
     CASE rand OF
          0:   red :=191-colour;            (* SUNSET *)
               green:=90;
               blue:=42+SHORTCARD(REAL(colour)/1.5);
          |
          1:   red:= 0;   (* STARY NIGHT *)
               green:=0;
               blue:=0;
          |
          2:   red :=191-colour;         (* DAY *)
               green:=191-colour;
               blue:=84+SHORTCARD(REAL(colour)/3.0);
          |
          3:   red:=0;     (* STORMY *)
               green:=0;
               blue:=colour;
          END (*CASE*);
     CASE rand2 OF
          0:    R:=0.75;
                G:=0.5;
                B:= 0.0;
          |
          1:    R:=0.5;
                G:=0.5;
                B:=0.25;
          |
          2:    R:=0.75;
                G:=0.45;
                B:= 0.3;
          |
          3:    R:= 0.0;
                G:=0.99;
                B:= 0.0;
          |
          4:    R:=0.25;
                G:=0.5;
                B:= 0.0;
          |
          5:    R:=0.4;
                G:=0.85;
                B:= 0.45;
          |
          6:    R:=0.65;
                G:=0.50;
                B:= 0.25;
          |
          7:    R:=0.68;
                G:=0.55;
                B:= 0.4;
          |
          8:    R:=0.75;
                G:=0.75;
                B:=0.75;
          |
          9:    R:=0.5;
                G:=0.5;
                B:= 0.25;
          |
         10:    R:=0.35;
                G:=0.45;
                B:= 0.45;
         END (*CASE*);

     r:=SHORTCARD(R*REAL(colour-64));
     g:=SHORTCARD(G*REAL(colour-64));
     b:=SHORTCARD(B*REAL(colour-64));

     GroundColour[INTEGER(colour-64)]:=colour+64;
     SetPaletteColour(colour+64,r,g,b);
     SetPaletteColour(colour,red,green,blue);



     END;
     stars :=rand;


END Sky;

(*****************************************************************************
** Draws a rectangle.                                                       **
*****************************************************************************)
PROCEDURE DrawRectangle(w:Window;left,bottom,right,top:INTEGER;colr:SHORTCARD);
BEGIN
     Line2C(w,left,bottom,right,bottom,colr);
     Line2C(w,left,bottom,left,top,colr);
     Line2C(w,left,top,right,top,colr);
     Line2C(w,right,bottom,right,top,colr);
END DrawRectangle;


(******************************************************************************
** Draw a border around the playing window.                                  **
******************************************************************************)
PROCEDURE DrawBackground;
VAR cnt1,cnt2,cnt3:CARDINAL;
    incr:BOOLEAN;
    BackColour:ARRAY [0..63] OF SHORTCARD;
    colour:SHORTCARD;
    ran1,ran2,ran3:REAL;
BEGIN

        ran1:=RANDOM();
        ran2:=RANDOM();
        ran3:=RANDOM();
       incr:=TRUE;
       cnt2:=0;
       cnt3:=0;
       FOR colour:= 0 TO 63  DO
          BackColour[INTEGER(colour)]:=colour+192;
          SetPaletteColour(colour+192,SHORTCARD(ran1*REAL(colour)),
                SHORTCARD(ran2*REAL(colour)),SHORTCARD(ran3*REAL(colour)));
          END;
       LOOP;
       FOR cnt1 :=63 TO 0 BY -1 DO
          INC(cnt2);
          DrawRectangle(wback,cnt2,cnt2,XMAX-cnt2-1,YMAX-cnt2,BackColour[cnt1]);
          IF 2*cnt2>YMAX THEN EXIT; END;
          IF 2*cnt2>XMAX THEN EXIT; END;
         END;
       FOR cnt1 :=0 TO  63 DO
          INC(cnt2);
          DrawRectangle(wback,cnt2,cnt2,XMAX-cnt2-1,YMAX-cnt2,BackColour[cnt1]);
         IF 2*cnt2>YMAX THEN EXIT; END;
         IF 2*cnt2>XMAX THEN EXIT; END;
         END;


       END (*LOOP*);
END DrawBackground;


(*****************************************************************************
**  The main function.                                                      **
*****************************************************************************)
PROCEDURE Battle;
BEGIN


       MaxGround:=0;
       Sky();
       GetScreenShape(XMAX,YMAX,MaxColour);
       OpenWindow(wback,0,0,XMAX-1,YMAX-1,1,0,single);
       DrawBackground();
       Sky();
       colr1:=13;         (* the colour of tank1                  *)
       colr2:=12;         (* the colour of tank2                  *)
       left:=  INTEGER(0.127*REAL(XMAX));
       bottom:=INTEGER(0.17*REAL(YMAX));
       right:= INTEGER(0.875*REAL(XMAX));
       top:=   INTEGER(0.83*REAL(YMAX));
       L:= left;
       R:=right;
       B:=bottom;
       T:=top;


       OpenWindow(wblast,95,95,XMAX-100,YMAX-100,2,0,single);
        (** set tank colours **)
       colr1:=TankColour(wblast,16,1);
       IF Human THEN colr2:=TankColour(wblast,colr1,2); END;
       TankDraw(wblast,150,100,0);
       CloseWindow(wblast);
       DrawBackground();

       OpenWindow(wblast,CARDINAL(REAL(XMAX)/2.0)-150,
       CARDINAL(REAL(YMAX)/2.0)-45,CARDINAL(REAL(XMAX)/2.0)+150,
       CARDINAL(REAL(YMAX)/2.0)+45,2,0,single);

       SetCursor(wblast,0,0);
       WriteStringC(wblast,"  Position and size playing window ",14);
       WriteLn(wblast);
       WriteLn(wblast);
       WriteStringC(wblast,"  CURSOR keys to move window",2);
       WriteLn(wblast);
       WriteLn(wblast);
       WriteStringC(wblast,"  Ctrl - CURSOR keys to",3);
       WriteLn(wblast);
       WriteStringC(wblast,"  resize the window",3);
       WriteLn(wblast);
       WriteLn(wblast);
       WriteStringC(wblast,"  'F' for full screen ",5);
       WriteLn(wblast);
       WriteLn(wblast);
       WriteStringC(wblast,"  ENTER or SPACE when done",4);

       CtrlPress:=FALSE;

       LOOP   (** draw and position the playing window **)

          DrawRectangle(wback,L-1,B-1,R+1,T+1,0);
          DrawRectangle(wback,L,B,R,T,0);
          DrawRectangle(wback,L+1,B+1,R-1,T-1,0);
          DrawRectangle(wback,left-1,bottom-1,right+1,top+1,1);
          DrawRectangle(wback,left,bottom,right,top,2);
          DrawRectangle(wback,left+1,bottom+1,right-1,top-1,12);
          L:=left;
          R:=right;
          B:=bottom;
          T:=top;
          ScanCode:=GetScanCode();
          CASE ScanCode OF
               Up:        IF CtrlPress THEN
                             (* increase vertical size*)
                             IF top < YMAX-10 THEN INC(top,10);
                                ELSIF top - bottom >11 THEN DEC(bottom,10);
                                END;
                             (* move window up *)
                            ELSIF top <YMAX-10 THEN
                              INC(top,10);
                              INC(bottom,10);
                           END;
               |

               Left:
                           (* decrease horizontal size of window *)
                           IF CtrlPress THEN
                              IF right-left >280 THEN DEC(right,10);
                                END;
                           (* move window left *)
                           ELSIF left >11 THEN
                              DEC(left,10);
                              DEC(right,10);
                           END;
               |
               Right:  (* Increase horizontal size of window *)
                           IF CtrlPress THEN
                              IF right < XMAX-10 THEN INC(right,10);
                              ELSIF right-left >11 THEN DEC(left,10);
                              END;
                           (* move window right *)
                           ELSIF right < XMAX-10 THEN
                             INC(right,10);
                             INC(left,10);
                           END;
               |
               Down:  (* decrease vertical size of window *)
                           IF CtrlPress THEN
                                IF top - bottom >130 THEN DEC(top,10);
                                END;
                        (* Move window down *)
                           ELSIF bottom >11 THEN
                             DEC(bottom,10);
                             DEC(top,10);
                           END;

               |
               CtrlCode:  CtrlPress:=TRUE;
               |
               CtrlRelease:       CtrlPress:=FALSE;

               |
               F:
                     (* FULL SCREEN *)
                        left:=1;
                        right:=XMAX-1;
                        bottom:=1;
                        top:=YMAX;

               |
               Space,Enter:       EXIT;
                  END (*CASE*);

            END;  (*LOOP*)

    CloseWindow(wblast);
    DrawBackground();

    DEC(top,50);
    PlayAgain := "Y";
    WHILE CAP(PlayAgain) ="Y" DO

        Weapons1[0]:=99;         (* 99 Missiles           *)
        Weapons2[0]:=99;
        Weapons1[1]:=2;          (* 2 Super Missiles       *)
        Weapons2[1]:=2;
        Weapons1[2]:=1;          (* 1 Vertical Guidance   *)
        Weapons2[2]:=1;
        Weapons1[3]:=1;          (* 1 DirtBlaster         *)
        Weapons2[3]:=1;
        HitToLeft:=FALSE;
        HitToRight:=FALSE;
        velocity:=10.0;
        damage1:=0;
        damage2:=0;
        explode.x:=-100;   (* dummy values out of range*)
        explode.y:=-100;
        a:=RANDOM();       (* generate random numbers *)
        b:=RANDOM();

        up1:=2*TRUNC(4.0*a) +3;            (* values used to generate random landscape *)
        up2:= TRUNC(4.0*b) +TRUNC(4.0*a);
        up2:=up2-1;

        a:=RANDOM();
        b:=RANDOM();
        down1:=2*TRUNC(4.0*b)+3;
        down2:= TRUNC(4.0*a)+TRUNC(4.0*b);
        down2:=down2-0;

        screensize := 1;
        y:=INTEGER(REAL(top-bottom)/15.0)*INTEGER(1.0+10.0*a);
        x:=1;
        RightBorder := right-left;
        TopBorder:= top-bottom;
        OpenWindow(wmain,left,bottom,right,top,4,0,single);
        Sky();  (* generate sky colours *)
        DrawLandscape(wmain);
        IF (stars=1) THEN
           FOR count:= 1 TO 5 DO StaryNight(wmain); END;
           END;
        a:=RANDOM();
        b:=RANDOM();

        Tank1.x := INTEGER(REAL(right-left)*a/3.2)+15;
        Tank1.y :=GroundLine[Tank1.x];
        Tank2.x := INTEGER(REAL(right-left)*b/3.2)+INTEGER(REAL(right-left)/1.66);
        Tank2.y := GroundLine[Tank2.x];
        CompExplode.x:=9999;

        VelChanged:=TRUE;
        angle1 := 0.0;     (* intiial angle of the barrel of tank1 *)
        Vel1:=50.0;        (* initial tank1 fireing velocity       *)
        IF SmartCom THEN
          IF Tank2.y >Tank1.y THEN
            Vel2:=REAL( 10*(Tank2.x-Tank1.x) -6*(Tank2.y-Tank1.y))/100.0 +10.0;
          ELSE
            Vel2:=REAL( 10*(Tank2.x-Tank1.x) -15*(Tank2.y-Tank1.y))/100.0 +10.0;
          END;
        ELSE Vel2:=50.0;        (* initial tank2 fireing velocity       *)
        END;
       IF Vel2 <30.0 THEN Vel2:=30.0; END;
       IF Human THEN
          angle2 := pi;      (* initial angle of the barrel of tank2 *)
       ELSE angle2:= 2.35619;  (* 3pi/4 *)
       END;

       TankDraw(wmain,Tank1.x,Tank1.y,INTEGER(colr1));

       TankDraw(wmain,Tank2.x,Tank2.y,INTEGER(colr2));
       IF Human THEN
          Line2C(wmain,Tank2.x,Tank2.y+7,Tank2.x-7,Tank2.y+7,colr2);
       ELSE Line2C(wmain,Tank2.x,Tank2.y+7,Tank2.x-5,Tank2.y+12,colr2);
       END;

       Tank1Explode:=FALSE;
       Tank2Explode:=FALSE;

       LOOP
             (* RefreshDisplay; *)
             OpenWindow(w1,left,top+3,right,top+50,colr1,0,single);
             wind := Wind*(21.0*RANDOM() -10.0);
             angle1:=Aim(Weapons1,damage1,screensize,player1,Tank1.x,Tank1.y+7,angle1,colr1,Vel1,wind,w1,wmain);
             CloseWindow(w1);
             IF angle1= quitnow THEN
                        PlayAgain := "N";
                        EXIT; END;
             IF Tank1Explode OR Tank2Explode THEN EXIT (*LOOP*);
             END (*IF*);
             TankFall(wmain,colr1,colr2);
             IF Tank1Explode OR Tank2Explode THEN EXIT (*LOOP*);
             END (*IF*);



             OpenWindow(w2,left,top+3,right,top+50,colr2,0,single);
             wind:=Wind*(21.0*RANDOM() -10.0) ;
             angle2:=Aim(Weapons2,damage2,screensize,player2,Tank2.x,Tank2.y+7,angle2,colr2,Vel2,wind,w2,wmain);

             CloseWindow(w2);
             IF angle2= quitnow THEN
                        PlayAgain := "N";
                        EXIT; END;
             IF Tank1Explode OR Tank2Explode THEN EXIT (*LOOP*);
             END (*IF*);
             TankFall(wmain,colr1,colr2);
             IF Tank1Explode OR Tank2Explode THEN EXIT (*LOOP*);
             END (*IF*);



        END (*LOOP*);

      IF (angle1<>quitnow)AND (angle2<>quitnow) THEN
        OpenWindow(wquit,left+20,bottom+20,left+270,bottom+70,1,0,double);
        WriteLn(wquit);
        IF Tank2Explode THEN
             WriteStringC(wquit,"  PLAYER 1 WON THAT BATTLE ",14);
        ELSE WriteStringC(wquit,"  PLAYER 2 WON THAT BATTLE ",14);
        END;
        WriteLn(wquit);
        WriteLn(wquit);
        WriteStringC(wquit,"  Fight Another Battle?  Y/N ",12);
        REPEAT
        PlayAgain:= InKey();
        UNTIL (CAP(PlayAgain) = "Y") OR (CAP(PlayAgain) = "N");
        CloseWindow(wquit);
       END;
        CloseWindow(wmain);
        END (*WHILE*);
      CloseWindow(wback);
     END Battle;

(***************************************************************************
**** THE MAIN PROGRAM                                                     **
***************************************************************************)
BEGIN


    PlayMusic("ABCDEFG");
    init.Play:=TRUE;

    Randomize(RANDNUM);     (* a new seed for the random number generator *)
    WHILE init.Play DO
          InitGraphics(2);
          init:=BlastMenu();
          IF init.Play THEN
            IF init.Player2 = human THEN Human:=TRUE;
              ELSE Human:=FALSE; END;
            IF init.Player2 = SmartComp THEN SmartCom :=TRUE;
              ELSE SmartCom:=FALSE; END;
            Sound:=init.Sound;
            Wind:=init.Wind;
            Trace:=init.Trace;
            IF init.ScreenRes = 1 THEN InitGraphics(SVG1b);
            ELSIF init.ScreenRes= 2 THEN InitGraphics(SVG2b);
            ELSE InitGraphics(SVG3b);
            END;
            Battle;
          END; (*IF*)


    END;   (*WHILE*)
END BlastWin.