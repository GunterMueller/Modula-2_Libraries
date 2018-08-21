IMPLEMENTATION MODULE LHash;
(*==============================================================
    Version  : 2.0 16 Sep 1990 C. Lins
    Compiler : Generic pc Modula-2
    Component: Hash Table - Sequential Unbounded Managed Iterator
              Linear hashing

    REVISION HISTORY
    v1.00  23 Apr 1989  C. Lins:
        Initial Modula-2 implementation
    v1.01  30 Jun 1989  C. Lins:
        Removed separation of key and data.
    v1.02  12 Aug 1989  C. Lins:
        Change table to VAR parameter in Insert, Remove, Update.
       Fixed setting of currSize in Clear.
       Optimized Assign to directly call AddTableEntry.
    v1.03  13 Aug 1989  C. Lins:
        Changed active iteration mechanism to match that for
       Direct Chaining.
    v1.04  21 Aug 1989  C. Lins:
       Added HashOf selector.
       Removed calls to HUnlock since HSetState restores the
       handle bits correctly.
    v1.05  29 Aug 1989  C. Lins:
       Separate key and data.
    v1.06  03 Sep 1989  C. Lins:
       Copy hashing function to target hash table in Assign.
    v1.07  09 Sep 1989  C. Lins:
       Pass back result from hash table for key from searches
       instead of key parameter.

   v2.00  16 Sep 1990  C. Lins
     Created generic pc version
   (C) Copyright 1990 Charles A. Lins
==============================================================*)

FROM SCLStorage IMPORT
   (*--proc*) Allocate, Deallocate;

FROM MacTypes IMPORT
   (*--type*) SignedByte;

FROM MemoryManager IMPORT
   (*--cons*) noErr,
   (*--type*) Size,
   (*--proc*) MemError, SetHandleSize, GetHandleSize,
              NewHandle, DisposHandle, HLock,
              HSetState, HGetState;

FROM Relations IMPORT
   (*--type*) Relation;

FROM HashTypes IMPORT
   (*--type*) Exceptions, Operations, States, ComponentID,
              FoundProc, NotFoundProc, UpdateProc, LoopProc,
              AccessProc, HashFunction, Key, Data;

FROM Items IMPORT
   (*--cons*) NullItem,
   (*--type*) Item, AssignProc, CompareProc, DisposeProc;

FROM ErrorHandling IMPORT
   (*--cons*) NullHandler,
   (*--type*) HandlerProc,
   (*--proc*) Raise;

FROM TypeManager IMPORT
   (*--cons*) NullType,
   (*--type*) TypeID,
   (*--proc*) AssignOf, CompareOf, DisposeOf;

   (*-------------------------*)


TYPE  TableEntryPtr = POINTER TO TableEntry;
TYPE  TableEntry = RECORD
        key   : Key;
        data  : Data;
        next  : TableEntryPtr;
     END (*-- TableEntry --*);
TYPE  HashingTable = ARRAY [0..0] OF TableEntryPtr;

TYPE  UnboundedHashTable = RECORD
        keyID   : TypeID;
        dataID  : TypeID;
        hash    : HashFunction;
        initSize: CARDINAL;     (*-- initial number of buckets *)
        currSize: CARDINAL;     (*-- current number of buckets *)
        minLoad : REAL;         (*-- lower bound on load factor *)
        maxLoad : REAL;         (*-- upper bound on load factor *)
        extent  : CARDINAL;     (*-- number of items in table *)
        p       : CARDINAL;     (*-- next bucket to be split *)
        maxp    : CARDINAL;     (*-- upper bound on p during this expansion *)
        table   : HashingTable; (*-- array [1..size] *)
     END (*-- UnboundedHashTable --*);

TYPE  HashTablePtr = POINTER TO UnboundedHashTable;
TYPE  HashTable  = POINTER TO HashTablePtr;

(*-------------------------*)

VAR   tableError : Exceptions;
VAR   handlers   : ARRAY Exceptions OF HandlerProc;

PROCEDURE TableError () : Exceptions               (*--out  *);
BEGIN
  RETURN tableError;
END TableError;
(*-------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions  (*--in   *);
                          theHandler : HandlerProc (*--in   *));
BEGIN
  handlers[theError] := theHandler;
END SetHandler;
(*-------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions  (*--in   *))
                                     : HandlerProc (*--out  *);
BEGIN
  RETURN handlers[theError];
END GetHandler;
(*-------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations  (*--in   *);
                          theError   : Exceptions  (*--in   *));
BEGIN
  tableError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*-------------------------*)


PROCEDURE InitToEmpty (    theTable : HashTable (*--inout*));

VAR   index : CARDINAL;

BEGIN
  WITH theTable^^ DO
    FOR index := 0 TO currSize-1 DO
      table[index] := NIL;
    END (*--for*);
  END (*--with*);
END InitToEmpty;
(*-------------------------*)

PROCEDURE FreeTableEntry (    theTable : HashTable  (*--in   *);
                              theEntry : TableEntry (*--inout*));

VAR   free : DisposeProc;  (*-- key/data disposal routine *)
      oldState : SignedByte;

BEGIN
  oldState := HGetState(theTable);
  HLock(theTable);
  free := DisposeOf(theTable^^.keyID);
  free(theEntry.key);
  free := DisposeOf(theTable^^.dataID);
  free(theEntry.data);
  HSetState(theTable, oldState);
END FreeTableEntry;
(*-------------------------*)

PROCEDURE Hash (    theTable : HashTable    (*--in   *);
                    theKey   : Key          (*--in   *))
                             : CARDINAL     (*--out  *);

CONST prime = 1048583;

VAR  h     : LONGCARD;
     addr  : CARDINAL;
     oldState : SignedByte;

BEGIN
  oldState := HGetState(theTable);
  HLock(theTable);
  WITH theTable^^ DO
    h := VAL(LONGCARD, hash(theKey)) MOD prime;
    addr := h MOD VAL(LONGCARD, maxp);
   IF (addr < p) THEN
     addr := h MOD VAL(LONGCARD, (2 * maxp));
   END (*--if*);
  END (*--with*);
  HSetState(theTable, oldState);
  RETURN addr;
END Hash;
(*-------------------------*)

(*
Expand the hash table by one bucket. p is the bucket to be split.
*)

PROCEDURE ExpandTable (    theTable : HashTable        (*--inout*);
                          routine  : Operations    (*--in   *));

VAR   oldBucket : CARDINAL;
     lastOfNew : TableEntryPtr;
     current   : TableEntryPtr;
     previous  : TableEntryPtr;
     oldState  : SignedByte;

BEGIN
  (*-- expand the table size, if possible *)
  SetHandleSize(theTable, GetHandleSize(theTable) +
                           VAL(Size, SIZE(TableEntryPtr)));
  IF (MemError() = noErr) THEN
    oldState := HGetState(theTable);
   HLock(theTable);
   WITH theTable^^ DO
     oldBucket := p;

     (*-- adjust state variables *)
     INC(p);
     IF (p = maxp) THEN
       maxp := 2 * maxp;
       p    := 0;
     END (*--if*);
     INC(currSize);

     (*-- relocate records to the new bucket (at currSize-1) *)
     current   := table[oldBucket];
     previous  := NIL;
     lastOfNew := NIL;
     table[currSize-1] := NIL;
     WHILE (current # NIL) DO
       IF (Hash(theTable, current^.key) = currSize-1) THEN
         (*-- attach it to new chain *)
         IF (lastOfNew = NIL) THEN
           table[currSize-1] := current;
         ELSE
           lastOfNew^.next := current;
         END (*--if*);
         IF (previous = NIL) THEN
           table[oldBucket] := current^.next;
         ELSE
           previous^.next := current^.next;
         END (*--if*);
         lastOfNew := current;
         current := current^.next;
         lastOfNew^.next := NIL;
       ELSE
         (*-- leave it on old chain *)
         previous := current;
         current := current^.next;
       END (*--if*);
     END (*--while*);
   END (*--with*);
   HSetState(theTable, oldState);
  ELSE
   RaiseErrIn(routine, overflow);
  END (*--if*);
END ExpandTable;
(*-------------------------*)

(*
Contract the hash table by one bucket.
*)

PROCEDURE ContractTable (    theTable : HashTable  (*--inout*));

VAR   current   : TableEntryPtr;
     oldState  : SignedByte;

BEGIN
  oldState := HGetState(theTable);
  HLock(theTable);
  WITH theTable^^ DO
    (*-- adjust state variables *)
    IF (p = 0) THEN
     maxp := maxp DIV 2;
     p    := maxp - 1;
   ELSE
      DEC(p);
    END (*--if*);

    (*-- relocate records from the last bucket (at currSize-1)
     -- to the bucket pointed to by p. *)
   IF (table[p] = NIL) THEN
     table[p] := table[currSize-1];
   ELSIF (table[currSize-1] # NIL) THEN
     current := table[currSize-1];
     WHILE (current^.next # NIL) DO
       current := current^.next;
     END (*--while*);
     current^.next := table[p];
     table[p] := table[currSize-1];
   END (*--if*);
    DEC(currSize);
  END (*--with*);
  HSetState(theTable, oldState);
  SetHandleSize(theTable, GetHandleSize(theTable) -
                         VAL(Size, SIZE(TableEntryPtr)));
END ContractTable;
(*-------------------------*)

(*
Search the given hash table for the given key returning the index
into the hash table where the key was found along with pointers
to the current and prior nodes in the list.
*)

PROCEDURE Search   (    theTable   : HashTable     (*--in   *);
                        theKey     : Key           (*--in   *);
                    VAR theIndex   : CARDINAL      (*--out  *);
                    VAR priorNode  : TableEntryPtr (*--out  *);
                    VAR currNode   : TableEntryPtr (*--out  *))
                                   : BOOLEAN       (*--out  *);

VAR   compare  : CompareProc;  (*-- key comparison routine *)
     oldState  : SignedByte;

BEGIN
  priorNode := NIL;
  oldState := HGetState(theTable);
  HLock(theTable);
  WITH theTable^^ DO
    compare   := CompareOf(keyID);
   theIndex  := Hash(theTable, theKey);
   currNode  := table[theIndex];
  END (*--with*);
  HSetState(theTable, oldState);
  WHILE (currNode # NIL) DO
   IF (compare(theKey, currNode^.key) = equal) THEN
     RETURN TRUE;
   END (*--if*);
   priorNode:= currNode;
   currNode := currNode^.next;
  END (*--while*);
  RETURN FALSE;
END Search;
(*-------------------------*)

PROCEDURE AddTableEntry (    theTable : HashTable  (*--in   *);
                            theKey   : Key         (*--in   *);
                            theData  : Data        (*--in   *);
                            theIndex : CARDINAL    (*--in   *);
                            theProc  : Operations  (*--in   *));

VAR   newEntry : TableEntryPtr;
     loadFactor: REAL;         (*-- overall load factor *)
     oldState  : SignedByte;

BEGIN
  Allocate(newEntry, SIZE(TableEntry));
  IF (newEntry = NIL) THEN
   RaiseErrIn(theProc, overflow);
  ELSE
   WITH newEntry^ DO
     key  := theKey;
     data := theData;
     next := theTable^^.table[theIndex];
   END (*--with*);
    oldState := HGetState(theTable);
    HLock(theTable);
   WITH theTable^^ DO
     table[theIndex] := newEntry;
     INC(extent);
     loadFactor := FLOAT(extent) / FLOAT(currSize);
     IF (loadFactor > maxLoad) THEN
       ExpandTable(theTable, theProc);
     END (*--if*);
   END (*--with*);
   HSetState(theTable, oldState);
  END (*--if*);
END AddTableEntry;
(*-------------------------*)


PROCEDURE Create   (    keyTypeID  : TypeID        (*--in   *);
                        dataTypeID : TypeID        (*--in   *);
                        theSize    : CARDINAL      (*--in   *);
                        minLoadFact: REAL          (*--in   *);
                        maxLoadFact: REAL          (*--in   *);
                        hashFunc   : HashFunction  (*--in   *))
                                   : HashTable     (*--out  *);

CONST baseSize  = SIZE(UnboundedHashTable) - SIZE(HashingTable);
CONST entrySize = SIZE(TableEntryPtr);

VAR   newTable : HashTable; (*--temporary for new hash table object *)

BEGIN
  tableError := noerr;
  newTable := NewHandle(baseSize + (VAL(INTEGER, theSize) * entrySize));
  IF (newTable = NullTable) OR (MemError() # noErr) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newTable^^ DO
     keyID     := keyTypeID;
     dataID    := dataTypeID;
     extent    := 0;
     initSize  := theSize;
     currSize  := theSize;
     minLoad   := minLoadFact;
     maxLoad   := maxLoadFact;
     p         := 0;
     maxp      := theSize;
     hash      := hashFunc;
   END (*--with*);
   InitToEmpty(newTable);
  END (*--if*);
  RETURN newTable;
END Create;
(*-------------------------*)

PROCEDURE Destroy  (VAR theTable   : HashTable     (*--inout*));
BEGIN
  Clear(theTable);
  IF (tableError = noerr) THEN
    DisposHandle(theTable);
   theTable := NullTable;
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear        (    theTable   : HashTable     (*--inout*));

VAR   index : CARDINAL;
     curr  : TableEntryPtr;
     oldState : SignedByte;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(clear, undefined);
  ELSE
    oldState := HGetState(theTable);
   HLock(theTable);
   WITH theTable^^ DO
     FOR index := 0 TO currSize-1 DO
       WHILE (table[index] # NIL) DO
         curr := table[index];
         table[index] := table[index]^.next;
         FreeTableEntry(theTable, curr^);
         Deallocate(curr);
       END (*--while*);
     END (*--for*);
     extent := 0;
     p      := 0;
     maxp   := currSize;
   END (*--with*);
   HSetState(theTable, oldState);
   InitToEmpty(theTable);
  END (*--if*);
END Clear;
(*-------------------------*)

PROCEDURE Assign   (    theTable   : HashTable     (*--in   *);
                    VAR toTable    : HashTable     (*--inout*));

  PROCEDURE RecreateTarget () : BOOLEAN (*--out *);
  BEGIN
    IF (theTable = NullTable) THEN
     RaiseErrIn(assign, undefined);
    ELSIF (toTable = NullTable) THEN
     WITH theTable^^ DO
       toTable := Create(keyID, dataID, initSize, minLoad, maxLoad, hash);
     END (*--with*);
    ELSIF (toTable = theTable) THEN
     RETURN FALSE;
   ELSE
     Clear(toTable);
     toTable^^.keyID  := theTable^^.keyID;
     toTable^^.dataID := theTable^^.dataID;
     toTable^^.hash   := theTable^^.hash;
    END (*--if*);
   RETURN tableError = noerr;
  END RecreateTarget;

VAR   assignKey  : AssignProc; (*-- key assignment routine *)
     assignData : AssignProc; (*-- data assignment routine *)
     index      : CARDINAL;   (*-- loop index over table entries *)
     current    : TableEntryPtr;
     oldState   : SignedByte;
     newKey     : Key;
     newData    : Data;
     newIndex   : CARDINAL;

BEGIN
  tableError := noerr;
  IF RecreateTarget() THEN
    oldState := HGetState(theTable);
    HLock(theTable);
    WITH theTable^^ DO
      assignKey  := AssignOf(keyID);
      assignData := AssignOf(dataID);
     FOR index := 0 TO currSize-1 DO
       current := table[index];
       WHILE (current # NIL) & (tableError = noerr) DO
         newKey  := assignData(current^.key);
         newData := assignData(current^.data);
         newIndex:= Hash(toTable, newKey);
         AddTableEntry(toTable, newKey, newData, newIndex, assign);
         current := current^.next;
       END (*--while*);
     END (*--for*);
   END (*--with*);
   HSetState(theTable, oldState);
  END (*--if*);
END Assign;
(*-------------------------*)

PROCEDURE Insert   (    theTable   : HashTable     (*--inout*);
                        theKey     : Key           (*--in   *);
                        theData    : Data          (*--in   *));

VAR   index    : CARDINAL;     (*-- bucket where to insert *)
     prior     : TableEntryPtr;
     current   : TableEntryPtr;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(insert, undefined);
  ELSIF Search(theTable, theKey, index, prior, current) THEN
   RaiseErrIn(insert, duplicatekey);
  ELSE
   AddTableEntry(theTable, theKey, theData, index, insert);
  END (*--if*);
END Insert;
(*-------------------------*)

PROCEDURE Remove   (    theTable   : HashTable     (*--inout*);
                        theKey     : Key           (*--in   *);
                        notFound   : NotFoundProc  (*--in   *));

VAR   index    : CARDINAL;     (*-- bucket where key was found *)
     prior     : TableEntryPtr;
     current   : TableEntryPtr;
     loadFactor: REAL;         (*-- overall load factor *)

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(remove, undefined);
  ELSIF Search(theTable, theKey, index, prior, current) THEN
   IF (prior = NIL) THEN
     (*-- current is at the head of the list *)
     theTable^^.table[index] := current^.next;
   ELSE
     prior^.next := current^.next;
   END (*--if*);
   FreeTableEntry(theTable, current^);
   Deallocate(current);
   WITH theTable^^ DO
     DEC(extent);
     loadFactor := FLOAT(extent) / FLOAT(currSize);
   END (*--with*);
   IF (loadFactor < theTable^^.minLoad) THEN
     ContractTable(theTable);
   END (*--if*);
  ELSE
   notFound(theKey);
  END (*--if*);
END Remove;
(*-------------------------*)

PROCEDURE Update   (    theTable   : HashTable     (*--inout*);
                        theKey     : Key           (*--in   *);
                        theData    : Data          (*--in   *);
                        updateEntry: UpdateProc    (*--in   *));

VAR   index    : CARDINAL;     (*-- bucket where key was found *)
     prior     : TableEntryPtr;
     current   : TableEntryPtr;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(remove, undefined);
  ELSIF Search(theTable, theKey, index, prior, current) THEN
    updateEntry(current^.key, current^.data, theData);
  ELSE
   AddTableEntry(theTable, theKey, theData, index, update);
  END (*--if*);
END Update;
(*-------------------------*)


PROCEDURE IsDefined (    theTable  : HashTable     (*--in   *))
                                   : BOOLEAN       (*--out  *);
BEGIN
  RETURN (theTable # NullTable);
END IsDefined;
(*-------------------------*)

PROCEDURE IsEmpty  (    theTable   : HashTable     (*--in   *))
                                   : BOOLEAN       (*--out  *);
BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(isempty, undefined);
   RETURN TRUE;
  END (*--if*);
  RETURN theTable^^.extent = 0;
END IsEmpty;
(*-------------------------*)

PROCEDURE KeyTypeOf (    theTable  : HashTable     (*--in   *))
                                   : TypeID        (*--out  *);
BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(typeof, undefined);
   RETURN NullType;
  END (*--if*);
  RETURN theTable^^.keyID;
END KeyTypeOf;
(*-------------------------*)

PROCEDURE DataTypeOf(    theTable  : HashTable     (*--in   *))
                                   : TypeID        (*--out  *);
BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(typeof, undefined);
   RETURN NullType;
  END (*--if*);
  RETURN theTable^^.dataID;
END DataTypeOf;
(*-------------------------*)

PROCEDURE HashOf    (    theTable  : HashTable     (*--in   *))
                                   : HashFunction  (*--out  *);
BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(hashof, undefined);
   RETURN VAL(HashFunction, NIL);
  END (*--if*);
  RETURN theTable^^.hash;
END HashOf;
(*-------------------------*)

PROCEDURE SizeOf    (    theTable  : HashTable     (*--in   *))
                                   : CARDINAL      (*--out  *);
BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(sizeof, undefined);
   RETURN 0;
  END (*--if*);
  RETURN theTable^^.currSize;
END SizeOf;
(*-------------------------*)

PROCEDURE ExtentOf  (    theTable  : HashTable     (*--in   *))
                                   : CARDINAL      (*--out  *);
BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(extentof, undefined);
   RETURN 0;
  END (*--if*);
  RETURN theTable^^.extent;
END ExtentOf;
(*-------------------------*)

PROCEDURE IsPresent    (    theTable   : HashTable     (*--in   *);
                        theKey     : Key           (*--in   *);
                        found      : FoundProc     (*--in   *);
                        notFound   : NotFoundProc  (*--in   *));

VAR   index    : CARDINAL;     (*-- bucket where item was found *)
     prior     : TableEntryPtr;
     current   : TableEntryPtr;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(ispresent, undefined);
  ELSIF Search(theTable, theKey, index, prior, current) THEN
   found(current^.key, current^.data);
  ELSE
   notFound(theKey);
  END (*--if*);
END IsPresent;
(*-------------------------*)


PROCEDURE LoopOver (    theTable   : HashTable     (*--in   *);
                        process    : LoopProc      (*--in   *));

VAR   index : CARDINAL;        (*-- loop index over hashing table *)
     curr  : TableEntryPtr;
     oldState : SignedByte;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(loopover, undefined);
  ELSE
    oldState := HGetState(theTable);
    HLock(theTable);
   WITH theTable^^ DO
     FOR index := 0 TO currSize-1 DO
       curr := table[index];
       WHILE (curr # NIL) DO
         WITH curr^ DO
           IF ~process(key, data) THEN
             HSetState(theTable, oldState);
             RETURN;
           END (*--if*);
           curr := next;
         END (*--with*);
       END (*--while*);
     END (*--for*);
   END (*--with*);
   HSetState(theTable, oldState);
  END (*--if*);
END LoopOver;
(*-------------------------*)

PROCEDURE Traverse (    theTable   : HashTable     (*--in   *);
                        process    : AccessProc    (*--in   *));

VAR   index : CARDINAL;        (*-- loop index over hashing table *)
     curr  : TableEntryPtr;
     oldState : SignedByte;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(traverse, undefined);
  ELSE
    oldState := HGetState(theTable);
    HLock(theTable);
   WITH theTable^^ DO
     FOR index := 0 TO currSize-1 DO
       curr := table[index];
       WHILE (curr # NIL) DO
         WITH curr^ DO
           process(key, data);
           curr := next;
         END (*--with*);
       END (*--while*);
     END (*--for*);
   END (*--with*);
   HSetState(theTable, oldState);
  END (*--if*);
END Traverse;
(*-------------------------*)


TYPE  HashTableEntry = POINTER TO TableEntry;
CONST NullTableEntry = NIL;

VAR   entryState : ARRAY BOOLEAN OF States;

PROCEDURE Iterate  (    theTable   : HashTable     (*--in   *);
                        process    : IterateProc   (*--in   *));

VAR   index : CARDINAL;        (*-- loop index over hashing table *)
     curr  : TableEntryPtr;
     oldState : SignedByte;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(traverse, undefined);
  ELSE
    oldState := HGetState(theTable);
    HLock(theTable);
   WITH theTable^^ DO
     FOR index := 0 TO currSize-1 DO
       process(index, entryState[table[index] # NIL],
                      VAL(HashTableEntry, table[index]));
     END (*--for*);
   END (*--with*);
   HSetState(theTable, oldState);
  END (*--if*);
END Iterate;
(*-------------------------*)

PROCEDURE IsNull   (    theEntry   : HashTableEntry    (*--in   *))
                                   : BOOLEAN           (*--out  *);
BEGIN
  RETURN theEntry = NullTableEntry;
END IsNull;
(*-------------------------*)

PROCEDURE KeyOf        (    theEntry   : HashTableEntry(*--in   *))
                                   : Key           (*--out  *);
BEGIN
  IF (theEntry = NullTableEntry) THEN
   RETURN NullItem;
  ELSE
   RETURN theEntry^.key;
  END (*--if*);
END KeyOf;
(*-------------------------*)

PROCEDURE DataOf   (    theEntry   : HashTableEntry(*--in   *))
                                   : Data          (*--out  *);
BEGIN
  IF (theEntry = NullTableEntry) THEN
   RETURN NullItem;
  ELSE
   RETURN theEntry^.data;
  END (*--if*);
END DataOf;
(*-------------------------*)

PROCEDURE NextOf   (    theEntry   : HashTableEntry    (*--in   *))
                                   : HashTableEntry    (*--out  *);

BEGIN
  IF (theEntry = NullTableEntry) THEN
   RETURN NullTableEntry;
  ELSE
   RETURN VAL(HashTableEntry, theEntry^.next);
  END (*--if*);
END NextOf;
(*-------------------------*)


BEGIN
  FOR tableError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(tableError, NullHandler);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  tableError := noerr;
  entryState[FALSE] := empty;
  entryState[TRUE ] := used;
  NullTable := NIL;
END LHash.