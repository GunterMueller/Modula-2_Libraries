IMPLEMENTATION MODULE Menu2;
(*****************************************************************************)
(**   Menu routines to set up and start the game.                           **)
(*****************************************************************************)

FROM Windows IMPORT Window,Colour,OpenWindow,FrameType,DividerType,SetCursor,
             WriteString,WriteLn,EraseLine;
FROM Menus IMPORT CreateMenu,SelectFromMenu,DestroyMenu,PositionMenu,DisplayMenu,
           Menu,ItemText;
FROM TankHelp IMPORT Help;

VAR i:initvalu;

PROCEDURE BlastMenu():initvalu;
VAR M,wind,sound,trace,res,play:Menu;
    items,WindOn,TraceOn,SoundOn,Res,Play2:ARRAY [0..9] OF ItemText;
    MenuItem,count,select:CARDINAL;
    Init:initvalu;
    w:Window;
BEGIN
    Init.Wind:=1.0;
    Init.Sound:=TRUE;
    Init.Trace:=TRUE;
    Init.ScreenRes:=1;
    Init.Player2:=human;
    items[0]:= "TankBlast Startup Menu";
    items[1]:=" Instructions";
    items[2]:=" Sound ";
    items[3]:=" Wind  ";
    items[4]:=" Projectile Traces";
    items[5]:=" Screen Resolution";
    items[6]:=" Player 2 ";
    items[7]:=" BEGIN THE BATTLE";
    items[8]:=" QUIT";
    WindOn[0]:="WIND";
    WindOn[1]:="ON";
    WindOn[2]:="OFF";
    TraceOn[0]:="Projectile Traces";
    TraceOn[1]:="ON";
    TraceOn[2]:="OFF";
    SoundOn[0]:="Sound";
    SoundOn[1]:="ON";
    SoundOn[2]:="OFF";
    Res[0]:="Screen Resolution";
    Res[1]:="640 x 480";
    Res[2]:="800 x 600";
    Res[3]:="1024 x 768";
    Play2[0]:="PLAYER 2";
    Play2[1]:="HUMAN";
    Play2[2]:="COMPUTER";
    Play2[3]:="SUPER COMPUTER";
    CreateMenu(M,1,items,8);
    PositionMenu(M,intensewhite,blue,10,21,10,33);
    CreateMenu(wind,1,WindOn,2);
    PositionMenu(wind,intensewhite,blue,10,21,34,45);
    CreateMenu(sound,1,SoundOn,2);
    PositionMenu(sound,intensewhite,blue,10,21,34,45);
    CreateMenu(trace,1,TraceOn,2);
    PositionMenu(trace,intensewhite,blue,10,21,34,54);
    CreateMenu(res,1,Res,3);
    PositionMenu(res,intensewhite,blue,10,21,34,54);
    CreateMenu(play,1,Play2,3);
    PositionMenu(play,intensewhite,blue,10,21,34,54);

    OpenWindow(w,intensewhite,blue,10,21,34,45,simpleframe,nodivider);
    SetCursor(w,4,1);
    FOR count:= 1 TO 3 DO
        WriteString(w,"ON");
        WriteLn(w);
        END (*FOR*);
    WriteString(w,Res[1]);
    WriteLn(w);
    WriteString(w,Play2[1]);
    Init.Play:=TRUE;
    REPEAT
    MenuItem:=SelectFromMenu(M);
    CASE MenuItem OF
         1:       Help;
         |
         2:       SetCursor(w,4,1);
                  EraseLine(w,0);
                  IF SelectFromMenu(sound) = 2 THEN
                     Init.Sound:=FALSE;
                     WriteString(w,"OFF");
                  ELSE Init.Sound:=TRUE;
                     WriteString(w,"ON");
                  END;
         |
         3:       SetCursor(w,5,1);
                  EraseLine(w,0);
                  IF SelectFromMenu(wind) = 2 THEN
                     Init.Wind:=0.0;
                     WriteString(w,"OFF");
                  ELSE Init.Wind :=1.0;
                     WriteString(w,"ON");
                  END;
         |
         4:       SetCursor(w,6,1);
                  EraseLine(w,0);
                  IF SelectFromMenu(trace) = 2 THEN
                     Init.Trace:=FALSE;
                     WriteString(w,"OFF");
                  ELSE Init.Trace:=TRUE;
                     WriteString(w,"ON");
                  END;
         |
         5:     SetCursor(w,7,1);
                EraseLine(w,0);
                select:=SelectFromMenu(res);
                IF select= 1 THEN
                   Init.ScreenRes:=1;
                   WriteString(w,"640 x 480");
                ELSIF select = 2 THEN
                    Init.ScreenRes := 2;
                    WriteString(w,"800 x 600");
                ELSE Init.ScreenRes:=3;
                     WriteString(w,"1024 x 768");
                END;
         |
         6:     SetCursor(w,8,1);
                EraseLine(w,0);
                select:=SelectFromMenu(play);
                IF select= 1 THEN
                   Init.Player2:=human;
                   WriteString(w,"HUMAN");
                ELSIF select = 2 THEN
                    Init.Player2 := DumbComp;
                    WriteString(w,"COMPUTER");
                ELSE Init.Player2:=SmartComp;
                     WriteString(w,"SUPER COMP");
                END;
         |
         8:       Init.Play := FALSE;
         END;
    UNTIL (MenuItem=7) OR (MenuItem = 8);
      RETURN Init;
    END BlastMenu;

END Menu2.