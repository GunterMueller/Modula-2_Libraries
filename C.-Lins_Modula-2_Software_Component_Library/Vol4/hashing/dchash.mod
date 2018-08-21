IMPLEMENTATION MODULE DCHash;
(*==============================================================
    Version  : 2.0 16 Sep 1990 C. Lins
    Compiler : Generic pc Modula-2
    Component: Hash Table - Sequential Bounded Managed Iterator
              Collision resolution using direct chaining.
              The list of pointers is unordered.

    REVISION HISTORY
    v1.00  22 Apr 1989  C. Lins:
        Initial Modula-2 implementation
    v1.01  12 Jun 1989  C. Lins:
       Removed notFound procedure parameter from Update. Changed
       semantics to automatically insert the new entry if not
       found in the table.
    v1.02  18 Jun 1989  C. Lins:
       Eliminated separation of key and data.
    v1.03  22 Jul 1989  C. Lins:
       Revised active hash table iterator.
    v1.04  21 Aug 1989  C. Lins:
       Add HashOf selector.
    v1.05  29 Aug 1989  C. Lins:
       Separate key and data.
    v1.06  03 Sep 1989  C. Lins:
       Copy hashing function to target hash table in Assign..
    v1.07  19 Sep 1989  C. Lins:
       Pass back result from hash table for key from searches
       instead of key parameter.

   v2.00  16 Sep 1990  C. Lins
     Created generic pc version
   (C) Copyright 1990 Charles A. Lins
==============================================================*)

FROM SCLStorage IMPORT
   (*--proc*) Allocate, Deallocate;

FROM Relations IMPORT
   (*--type*) Relation;

FROM HashTypes IMPORT
   (*--type*) Exceptions, Operations, ComponentID, States,
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


TYPE  HashTable  = POINTER TO BoundedHashTable;
TYPE  TableEntryPtr = POINTER TO TableEntry;
TYPE  TableEntry = RECORD
       key   : Key;
       data  : Data;
       next  : TableEntryPtr;
     END (*-- TableEntry --*);
TYPE  HashingTable = ARRAY [0..0] OF TableEntryPtr;

TYPE  BoundedHashTable = RECORD
       keyID   : TypeID;
       dataID  : TypeID;
       h       : HashFunction;
       size    : CARDINAL;     (*-- table size *)
       extent  : CARDINAL;     (*-- # of items in table *)
       table   : HashingTable; (*-- array [1..size] *)
     END (*-- BoundedHashTable --*);


PROCEDURE InitToEmpty (    theTable : HashTable (*--inout*));

VAR   index : CARDINAL;

BEGIN
  WITH theTable^ DO
    FOR index := 0 TO size-1 DO
      table[index] := NIL;
    END (*--for*);
  END (*--with*);
END InitToEmpty;
(*-------------------------*)

PROCEDURE FreeTableEntry (    theTable : HashTable  (*--in   *);
                              theEntry : TableEntry (*--inout*));

VAR   free : DisposeProc;  (*-- Item disposal routine *)

BEGIN
  free := DisposeOf(theTable^.keyID);
  free(theEntry.key);
  free := DisposeOf(theTable^.dataID);
  free(theEntry.data);
END FreeTableEntry;
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

BEGIN
  priorNode := NIL;
  WITH theTable^ DO
    compare   := CompareOf(keyID);
    theIndex  := h(theKey) MOD size;
    currNode  := table[theIndex];
  END (*--with*);
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

PROCEDURE AddTableEntry (    theTable : HashTable  (*--inout*);
                            theIndex : CARDINAL    (*--in   *);
                            theKey   : Key         (*--in   *);
                            theData  : Data        (*--in   *);
                            theProc  : Operations  (*--in   *));

VAR      newEntry  : TableEntryPtr;

BEGIN
  Allocate(newEntry, SIZE(TableEntry));
  IF (newEntry = NIL) THEN
    RaiseErrIn(theProc, overflow);
  ELSE
    WITH newEntry^ DO
     key  := theKey;
     data := theData;
     next := theTable^.table[theIndex];
    END (*--with*);
    WITH theTable^ DO
      table[theIndex] := newEntry;
      INC(extent);
    END (*--with*);
  END (*--if*);
END AddTableEntry;
(*-------------------------*)


PROCEDURE Create   (    keyTypeID  : TypeID        (*--in   *);
                        dataTypeID : TypeID        (*--in   *);
                        theSize    : CARDINAL      (*--in   *);
                        hashFunc   : HashFunction  (*--in   *))
                                   : HashTable     (*--out  *);

CONST baseSize  = SIZE(BoundedHashTable) - SIZE(HashingTable);
CONST entrySize = SIZE(TableEntryPtr);

VAR   newTable : HashTable; (*--temporary for new hash table object *)

BEGIN
  tableError := noerr;
  Allocate(newTable, baseSize + theSize * entrySize);
  IF (newTable = NullTable) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newTable^ DO
      keyID  := keyTypeID;
      dataID := dataTypeID;
      extent := 0;
      size   := theSize;
      h      := hashFunc;
   END (*--with*);
   InitToEmpty(newTable);
  END (*--if*);
  RETURN newTable;
END Create;
(*-------------------------*)

PROCEDURE Destroy  (VAR theTable   : HashTable     (*--inout*));
CONST baseSize  = SIZE(BoundedHashTable) - SIZE(HashingTable);
CONST entrySize = SIZE(TableEntryPtr);
BEGIN
  Clear(theTable);
  IF (tableError = noerr) THEN
    Deallocate(theTable, baseSize + theTable^.size * entrySize);
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear        (    theTable   : HashTable     (*--inout*));

VAR   index : CARDINAL;
      curr  : TableEntryPtr;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(clear, undefined);
  ELSE
    WITH theTable^ DO
      FOR index := 0 TO size-1 DO
        WHILE (table[index] # NIL) DO
          curr := table[index];
          table[index] := table[index]^.next;
          FreeTableEntry(theTable, curr^);
          Deallocate(curr);
        END (*--while*);
      END (*--for*);
      extent := 0;
    END (*--with*);
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
      WITH theTable^ DO
        toTable := Create(keyID, dataID, size, h);
      END (*--with*);
    ELSIF (toTable = theTable) THEN
      RETURN FALSE;
    ELSIF (toTable^.size >= theTable^.extent) THEN
      Clear(toTable);
      WITH theTable^ DO
        toTable^.keyID  := keyID;
        toTable^.dataID := dataID;
        toTable^.h      := h;
      END (*--with*);
    ELSE
      RaiseErrIn(assign, overflow);
    END (*--if*);
    RETURN tableError = noerr;
  END RecreateTarget;

VAR   assignKey  : AssignProc; (*-- key assignment routine *)
      assignData : AssignProc; (*-- data assignment routine *)
      index      : CARDINAL;   (*-- loop index over table entries *)
      current    : TableEntryPtr;

BEGIN
  tableError := noerr;
  IF RecreateTarget() THEN
    WITH theTable^ DO
      assignKey  := AssignOf(keyID);
      assignData := AssignOf(dataID);
      FOR index := 0 TO size-1 DO
        current := table[index];
        WHILE (current # NIL) DO
          WITH current^ DO
            Insert(toTable, assignKey(key), assignData(data));
          END (*--with*);
          current := current^.next;
        END (*--while*);
      END (*--for*);
    END (*--with*);
  END (*--if*);
END Assign;
(*-------------------------*)

PROCEDURE Insert   (    theTable   : HashTable     (*--inout*);
                        theKey     : Key           (*--in   *);
                        theData    : Data          (*--in   *));

VAR   index    : CARDINAL;     (*-- bucket where to insert *)
      prior    : TableEntryPtr;
      current  : TableEntryPtr;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(insert, undefined);
  ELSIF Search(theTable, theKey, index, prior, current) THEN
    RaiseErrIn(insert, duplicatekey);
  ELSE
    AddTableEntry(theTable, index, theKey, theData, insert);
  END (*--if*);
END Insert;
(*-------------------------*)

PROCEDURE Remove   (    theTable   : HashTable     (*--inout*);
                        theKey     : Key           (*--in   *);
                        notFound   : NotFoundProc  (*--in   *));

VAR  index     : CARDINAL;     (*-- bucket where key was found *)
     prior     : TableEntryPtr;
     current   : TableEntryPtr;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(remove, undefined);
  ELSIF Search(theTable, theKey, index, prior, current) THEN
    IF (prior = NIL) THEN
      (*-- current is at the head of the list *)
      theTable^.table[index] := current^.next;
    ELSE
      prior^.next := current^.next;
    END (*--if*);
    FreeTableEntry(theTable, current^);
    Deallocate(current);
    DEC(theTable^.extent);
  ELSE
    notFound(theKey);
  END (*--if*);
END Remove;
(*-------------------------*)

PROCEDURE Update   (    theTable   : HashTable     (*--inout*);
                        theKey     : Key           (*--in   *);
                        theData    : Data          (*--in   *);
                        updateEntry: UpdateProc    (*--in   *));

VAR  index     : CARDINAL;     (*-- bucket where key was found *)
     prior     : TableEntryPtr;
     current   : TableEntryPtr;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(update, undefined);
  ELSIF Search(theTable, theKey, index, prior, current) THEN
    updateEntry(current^.key, current^.data, theData);
  ELSE
    AddTableEntry(theTable, index, theKey, theData, update);
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
  RETURN theTable^.extent = 0;
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
  RETURN theTable^.keyID;
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
  RETURN theTable^.dataID;
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
  RETURN theTable^.h;
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
  RETURN theTable^.size;
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
  RETURN theTable^.extent;
END ExtentOf;
(*-------------------------*)

PROCEDURE IsPresent    (    theTable   : HashTable     (*--in   *);
                            theKey     : Key           (*--in   *);
                            found      : FoundProc     (*--in   *);
                            notFound   : NotFoundProc  (*--in   *));

VAR  index     : CARDINAL;     (*-- bucket where item was found *)
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

VAR  index : CARDINAL;        (*-- loop index over hashing table *)
     curr  : TableEntryPtr;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(loopover, undefined);
  ELSE
    WITH theTable^ DO
      FOR index := 0 TO size-1 DO
        curr := table[index];
        WHILE (curr # NIL) DO
          WITH curr^ DO
            IF ~process(key, data) THEN
              RETURN;
            END (*--if*);
            curr := next;
          END (*--with*);
        END (*--while*);
      END (*--for*);
    END (*--with*);
  END (*--if*);
END LoopOver;
(*-------------------------*)

PROCEDURE Traverse (    theTable   : HashTable     (*--in   *);
                        process    : AccessProc    (*--in   *));

VAR  index : CARDINAL;        (*-- loop index over hashing table *)
     curr  : TableEntryPtr;

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(traverse, undefined);
  ELSE
    WITH theTable^ DO
      FOR index := 0 TO size-1 DO
        curr := table[index];
        WHILE (curr # NIL) DO
          WITH curr^ DO
            process(key, data);
            curr := next;
          END (*--with*);
        END (*--while*);
      END (*--for*);
    END (*--with*);
  END (*--if*);
END Traverse;
(*-------------------------*)

TYPE  HashTableEntry = POINTER TO TableEntry;
CONST NullTableEntry = NIL;

VAR   entryState : ARRAY BOOLEAN OF States;

PROCEDURE Iterate  (    theTable   : HashTable     (*--in   *);
                        process    : IterateProc   (*--in   *));

VAR   index : CARDINAL;        (*-- loop index over hashing table *)

BEGIN
  tableError := noerr;
  IF (theTable = NullTable) THEN
    RaiseErrIn(traverse, undefined);
  ELSE
    WITH theTable^ DO
      FOR index := 0 TO size-1 DO
        process(index, entryState[table[index] # NIL],
                       VAL(HashTableEntry, table[index]));
      END (*--for*);
    END (*--with*);
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

PROCEDURE DataOf   (    theEntry   : HashTableEntry    (*--in   *))
                                   : Data              (*--out  *);
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
END DCHash.