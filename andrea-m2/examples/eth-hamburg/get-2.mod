MODULE TestGet;

(* test the GetCharacter module *)
(* and use this as an example to show how it is used *)

(* revitalized, J. Andrea, Aug.12/91 *)
(* J. Andrea, 1984 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteCard, WriteLn, WriteString;

FROM GetCharacter IMPORT Get,   SetPassallCharacterMode,
                              UnSetPassallCharacterMode;

CONST
   control_z = 32C;
   max_gets  = 100;

VAR
   c         :CHAR;
   v, n_gets :CARDINAL;

BEGIN (* TestGet *)

WriteLn;
WriteString('This program reads (with no carriage-return needed) a character');
WriteLn;
WriteString('until a Control-Z is given.'); WriteLn;
WriteString('And displays the ASCII number and value of the character.');
WriteLn; WriteLn;
WriteString('Because this program cannot be broken');
WriteString(', a maximum of 100 characters can be given.');
WriteLn;

SetPassallCharacterMode;

Get(c);   n_gets := 1;

WHILE ( c # control_z ) & ( n_gets < max_gets ) DO

  v := ORD(c);

  WriteCard(v,3); WriteString('  ');   (* show its ascii value *)

  IF c >= ' ' THEN
     (* just an ordinary character *)
     WriteString(c);
  ELSE
     (* it was a control character, so show which one *)
     WriteString('^'); WriteString(CHR(v + 64));
  END;(* if *)

  WriteLn; WriteLn;

  Get(c);    n_gets := n_gets + 1;
END; (* while *)

UnSetPassallCharacterMode;

WriteLn; WriteString('All done.'); WriteLn;

END TestGet.
