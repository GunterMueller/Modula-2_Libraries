IMPLEMENTATION MODULE TankHelp;
(*****************************************************************************)
(**  Procedure to print out help messages                                   **)
(*****************************************************************************)

FROM Windows IMPORT Colour,Window,OpenWindow,FrameType,DividerType,CloseWindow,WriteString,
             WriteLn,SetCursor,PressAnyKey;
PROCEDURE Help;

VAR w:Window;
    c:CHAR;
BEGIN

    OpenWindow(w,green,black,0,24,0,79,doubleframe,nodivider);
    WriteLn(w);
    WriteString(w," HELP FOR TANKBLAST SVGA");
    WriteLn(w);
    WriteLn(w);
    WriteString(w," Use the up and down arrow key to change velocity.");
    WriteLn(w);
    WriteString(w," Use the right and left arrow keys to change angle.");
    WriteLn(w);
    WriteString(w," Use the Ctrl key with the above keys to make small changes.");
    WriteLn(w);
    WriteString(w," Press the space bar or Enter to fire. ");
    WriteLn(w);
    WriteLn(w);
    WriteString(w," Press 'w' or Alt key to change the weapon type. ");
    WriteLn(w);
    WriteString(w,"      MISSILES");
    WriteString(w,              ":  Cause a small explosion. A direct hit will destroy a tank,          and a near hit will cause 20% dammage");
    WriteLn(w);
    WriteLn(w);
    WriteString(w,"      SUPER MISSILES");
    WriteString(w,                    ":   Cause a large explosion, a direct hit will destroy a         tank, and a near hit will cause 40% dammage.");
    WriteLn(w);
    WriteLn(w);
    WriteString(w,"      Vertical guidance");
    WriteString(w,                       ":   When directly above a tank, and within a certain          height above the tank, will drop down destroying tank.  The closer            together the two tanks, the smaller the heigt must be.");
    WriteLn(w);
    WriteLn(w);
    WriteString(w,"      Dirt blaster");
    WriteString(w,                  ":   Will cause a small explosion, and remove dirt without          causing any damage to tanks.");
    WriteLn(w);

    PressAnyKey(w);
    CloseWindow(w);
    OpenWindow(w,green,black,0,24,0,79,doubleframe,nodivider);

    WriteLn(w);
    WriteString(w," PLAYER 2 OPTIONS:");
    WriteLn(w);
    WriteLn(w);
    WriteString(w,"     HUMAN: 2 player game.");
    WriteLn(w);
    WriteLn(w);
    WriteString(w,"     COMPUTER: Tank 2 is controlled by a computer that will increase power          if the last projectile fell short of its target and will decrease power        otherwise.  It does not take into acount the wind or any other factors.");
    WriteLn(w);
    WriteLn(w);
    WriteString(w,"     SUPER COMPUTR:  Tank 2 is controlled by a 'smarter' computer.  It takes        into account wind, distance and hieght between tanks and if there is a         mountain in between the tanks. ");
    WriteLn(w);

    PressAnyKey(w);
    CloseWindow(w);
    OpenWindow(w,red,black,0,24,0,79,doubleframe,nodivider);

    WriteLn(w);
    WriteString(w," PROBLEMS WITH THE PROGRAM");
    WriteLn(w);
    WriteString(w," The main problem is that for some reason the keys 'lock up' occasionaly.");
    WriteLn(w);
    WriteString(w," This can be fixed by pressing the control key or the arrow keys until normal     control is gained again.");
    WriteLn(w);
    WriteString(w," Sometimes during an explosion a variable goes out of range. This causes the       program to crash.");
    WriteLn(w);

    PressAnyKey(w);
    CloseWindow(w);
    END Help;

END TankHelp.