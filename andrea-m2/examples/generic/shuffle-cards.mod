MODULE ShuffleCards;

(* This program shows how to use the module Shuffle, as it might be used
   in a card game program *)

(* J. Andrea, Jun.22/93 *)
(* This code may be freely used and distributed, it may not be sold *)

FROM InOut IMPORT WriteString, WriteCard, WriteLn;
FROM Shuffle IMPORT Deck, Create, Next;
FROM StringOperations IMPORT Assign, Append;
FROM Break IMPORT EnableBreak;

CONST
   n_suits  = 4;
   per_suit = 13;
   
   n_cards  = n_suits * per_suit;
   
   n_hands  = 4;
   per_hand = 4;
   
TYPE
  CardName = ARRAY [1..18] OF CHAR;
  
VAR
  card :ARRAY [1..n_cards] OF CardName;
  deck :Deck;
  i, j :CARDINAL;
  
  (* ------------------------------------------------ *)
  PROCEDURE MakeCards;
  (* Give names to the cards *)

  TYPE
    SuitNames = ARRAY [1..8] OF CHAR;
  VAR
    n    :CARDINAL;
    suit :ARRAY [1..n_suits] OF SuitNames;
    
    (* ------------------------------------ *)
    PROCEDURE MakeSuit( name :ARRAY OF CHAR );
    BEGIN
      n := n + 1;
      Assign( name, suit[n] );
    END MakeSuit;
    
    (* ------------------------------------ *)
    PROCEDURE MakeCard( name :ARRAY OF CHAR );
    VAR
      i, m :CARDINAL;
    BEGIN
      n := n + 1;
      FOR i := 1 TO n_suits DO
        m := n + ( i - 1 ) * per_suit;
        Assign( name,    card[m] );
        Append( ' of ',  card[m] );
        Append( suit[i], card[m] );
      END;
    END MakeCard;
    
  BEGIN

    n := 0;  
    MakeSuit( 'Diamonds' );  MakeSuit( 'Hearts' );
    MakeSuit( 'Spades' );    MakeSuit( 'Clubs' );

    IF n # n_suits THEN
      WriteString( '!WARNING! programmer error' );
      WriteLn;
      WriteString( 'The number of suits defined doesnt equal the expected number' );
      WriteLn;
      WriteString( 'The results might not be what you expect' );
      WriteLn;
    END;
    
    n := 0;
    MakeCard( 'Ace' );   MakeCard( 'Two' );   MakeCard( 'Three' );
    MakeCard( 'Four' );  MakeCard( 'Five' );  MakeCard( 'Six' );
    MakeCard( 'Seven' ); MakeCard( 'Eight' ); MakeCard( 'Nine' );
    MakeCard( 'Ten' );   MakeCard( 'Jack' );  MakeCard( 'Queen' );
    MakeCard( 'King' );

    IF n # per_suit THEN
      WriteString( '!WARNING! programmer error' );
      WriteLn;
      WriteString( 'The number of cards defined doesnt equal the expected number' );
      WriteLn;
      WriteString( 'The results might not be what you expect' );
      WriteLn;
    END;

  END MakeCards;
  
BEGIN EnableBreak;

  MakeCards;

  Create( deck, 1, n_cards );
    
  FOR i := 1 TO n_hands DO
    WriteLn;
    WriteString( 'Hand ' ); WriteCard( i, 0 ); WriteLn;
    
    FOR j := 1 TO per_hand DO
       WriteString( card[ Next( deck ) ] ); WriteLn;
    END;
  END;
  
END ShuffleCards.
