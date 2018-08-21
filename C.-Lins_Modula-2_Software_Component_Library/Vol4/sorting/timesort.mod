MODULE TimeSort;

FROM SCLStorage IMPORT
   (*--proc*) Allocate, Deallocate;

FROM EventManager IMPORT
   (*--proc*) TickCount;

FROM Utilities IMPORT
   (*--proc*) GetDateTime;

FROM IUPackage IMPORT
   (*--type*) DateForm,
   (*--proc*) IUDateString;

FROM QuickDraw IMPORT
   (*--var *) randSeed,
   (*--proc*) Random;

FROM InOut IMPORT
   (*--proc*) WriteString, WriteLn, WriteReal, WriteInt, WriteLongCard,
              WriteCard;

FROM InOutUtils IMPORT
   (*--proc*) WriteBlanks, WriteLnString, WriteLblCardinal, WriteLblBoolean;

FROM Arguments IMPORT
   (*--proc*) ArgumentCount, Argument;

FROM IntEnv IMPORT
   (*--proc*) Exit;

FROM BDPackage IMPORT
   (*--proc*) StringToNum;

FROM StringUtils IMPORT
   (*--proc*) IsEqual;

FROM SortData IMPORT
   (*--cons*) kMaxArraySize,
   (*--type*) DataArray,
   (*--var *) theRandomData;

IMPORT Relations;
IMPORT Sort;
(*
IMPORT QuickSorts;
IMPORT InsertionSorts;
*)
IMPORT MergeSorts;
IMPORT Diagnostic;
(*--------------------*)

CONST kDefaultArraySize = 3000;
VAR   arraySize : CARDINAL;

CONST kDefaultNumRuns = 1;
VAR   numRuns : CARDINAL;

CONST kDefaultSeed = 1D;
VAR   theSeed : LONGINT;

TYPE   SortAlgorithms = (insertion,
                         selection,
                         bubble,
                         shaker,        (*-- Gonnet *)
                         heap,          (*-- Gonnet *)
                         shell,         (*-- Segdewick *)
                         quick,
                         bsort,         (*-- Wainwright *)
                         mean,          (*-- Motzkin *)
                         merge          (*-- various merge sorts *)
                        );

TYPE  SortSet = SET OF SortAlgorithms;
CONST kDefaultSorts = SortSet{};
VAR   theSorts : SortSet;
VAR   theValue : INTEGER;

CONST  kDefaultCutoff = 10;
VAR    cutoff   : INTEGER;

TYPE   Orderings = (unordered,         (*-- random data set *)
                    ascending,         (*-- ordered list *)
                    descending,        (*-- ordered list in reverse order *)
                    same               (*-- ordered list of the same value *)
                   );
CONST  kDefaultOrder = unordered;
VAR    theOrder : Orderings;
VAR    orderName: ARRAY Orderings, [0..10] OF CHAR;

CONST kDefaultRatio = 0;
VAR   theRatio : INTEGER;

CONST  kDefaultVerify = FALSE;
VAR    verifyOrder : BOOLEAN;

CONST  kDefaultDataSet = 0;
VAR    dataSet : [0..8];

(*--------------------*)


VAR        comparisons : LONGCARD;
VAR        interchanges: LONGCARD;
VAR        assignments : LONGCARD;
VAR        statistics  : BOOLEAN;

VAR        doCompare2  : Sort.CompareProc;
VAR        doAssign2   : Sort.AssignProc;
VAR        doSwap      : Sort.SwapProc;
VAR        doValue     : Sort.ValueProc;
VAR        doAssignMean: Sort.AssignItemProc;

TYPE   pInteger = POINTER TO INTEGER;
VAR    theIntegerArray : ARRAY [0..kMaxArraySize] OF INTEGER;
(*--------------------*)


PROCEDURE GetArguments;

CONST  caseInsensitive = FALSE;

VAR    index : CARDINAL;
       argc  : CARDINAL;
       theArg: ARRAY [0..31] OF CHAR;
       theLongArg : LONGINT;

   PROCEDURE NextArgument;
   BEGIN
       Argument(index, theArg);
       INC(index);
   END NextArgument;

BEGIN
   arraySize   := kDefaultArraySize;
   numRuns     := kDefaultNumRuns;
   theSeed     := kDefaultSeed;
   theSorts    := kDefaultSorts;
   theOrder    := kDefaultOrder;
   theValue    := 1;
   statistics  := FALSE;
   verifyOrder := kDefaultVerify;
   theRatio    := kDefaultRatio;
   cutoff      := kDefaultCutoff;
   dataSet     := kDefaultDataSet;

   argc := ArgumentCount()-1;
   index := 1;
   WHILE (index <= argc) DO
       NextArgument;

       IF IsEqual(theArg, '-s', caseInsensitive) THEN
           NextArgument;
           StringToNum(theArg, theLongArg);
           IF (theLongArg < 1D) THEN
               theLongArg := 1;
           ELSIF (theLongArg > VAL(LONGINT, kMaxArraySize)) THEN
               theLongArg := kMaxArraySize;
           END (*--if*);
           arraySize := theLongArg;

       ELSIF IsEqual(theArg, '-n', caseInsensitive) THEN
           NextArgument;
           StringToNum(theArg, theLongArg);
           IF (theLongArg < 1D) THEN
               theLongArg := 1;
           END (*--if*);
           numRuns := theLongArg;

       ELSIF IsEqual(theArg, '-ds', caseInsensitive) THEN
           NextArgument;
           StringToNum(theArg, theLongArg);
           IF (theLongArg < 0) OR (theLongArg > 8) THEN
               theLongArg := 0;
           END (*--if*);
           dataSet := theLongArg;

       ELSIF IsEqual(theArg, '-seed', caseInsensitive) THEN
           NextArgument;
           StringToNum(theArg, theLongArg);
           IF (theLongArg < 1D) THEN
               theLongArg := 1;
           END (*--if*);
           theSeed := theLongArg;

       ELSIF IsEqual(theArg, '-ratio', caseInsensitive) THEN
           NextArgument;
           StringToNum(theArg, theLongArg);
           IF (theLongArg < 0D) OR (theLongArg > 100) THEN
               theLongArg := 0D;
           END (*--if*);
           theRatio := theLongArg;

       ELSIF IsEqual(theArg, '-cutoff', caseInsensitive) THEN
           NextArgument;
           StringToNum(theArg, theLongArg);
           IF (theLongArg < 0D) OR (theLongArg > 100) THEN
               theLongArg := kDefaultCutoff;
           END (*--if*);
           cutoff := theLongArg;

       ELSIF IsEqual(theArg, '-insertion', caseInsensitive) THEN
           INCL(theSorts, insertion);
       ELSIF IsEqual(theArg, '-selection', caseInsensitive) THEN
           INCL(theSorts, selection);
       ELSIF IsEqual(theArg, '-bubble', caseInsensitive) THEN
           INCL(theSorts, bubble);
       ELSIF IsEqual(theArg, '-shaker', caseInsensitive) THEN
           INCL(theSorts, shaker);
       ELSIF IsEqual(theArg, '-heap', caseInsensitive) THEN
           INCL(theSorts, heap);
       ELSIF IsEqual(theArg, '-shell', caseInsensitive) THEN
           INCL(theSorts, shell);
       ELSIF IsEqual(theArg, '-quick', caseInsensitive) THEN
           INCL(theSorts, quick);
       ELSIF IsEqual(theArg, '-bsort', caseInsensitive) THEN
           INCL(theSorts, bsort);
       ELSIF IsEqual(theArg, '-mean', caseInsensitive) THEN
           INCL(theSorts, mean);
       ELSIF IsEqual(theArg, '-merge', caseInsensitive) THEN
           INCL(theSorts, merge);

       ELSIF IsEqual(theArg, '-all', caseInsensitive) THEN
           INCL(theSorts, insertion);
           INCL(theSorts, selection);
           INCL(theSorts, heap);
           INCL(theSorts, bubble);
           INCL(theSorts, shaker);
           INCL(theSorts, shell);
           INCL(theSorts, quick);
           INCL(theSorts, bsort);
           INCL(theSorts, mean);
           INCL(theSorts, merge);

       ELSIF IsEqual(theArg, '-random', caseInsensitive) THEN
           theOrder := unordered;
       ELSIF IsEqual(theArg, '-ordered', caseInsensitive) THEN
           theOrder := ascending;
       ELSIF IsEqual(theArg, '-reverse', caseInsensitive) THEN
           theOrder := descending;
       ELSIF IsEqual(theArg, '-same', caseInsensitive) THEN
           theOrder := same;
           NextArgument;
           StringToNum(theArg, theLongArg);
           theValue := theLongArg;

       ELSIF IsEqual(theArg, '-verify', caseInsensitive) THEN
           verifyOrder := TRUE;

       ELSIF IsEqual(theArg, '-stats', caseInsensitive) THEN
           statistics := TRUE;

       ELSE
           Diagnostic.WriteString('### Unidentified option: ');
           Diagnostic.WriteString(theArg);
           Diagnostic.WriteLn;
           Diagnostic.WriteString('### Usage: TimeSort [options…]');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-s nnn      array size where nnn is a positive integer <= 8192');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-n nnn      number of runs');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-seed nnn   random number seed');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-ratio nn   sortedness ratio where 0 < nn < 100. Valid with -ordered or -reverse');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-cutoff nn  cutoff value for some quicksort algorithms');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-random     randomize array (duplicates allowed)');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-ordered    identity permutation');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-reverse    reverse identity permutation');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-same nnn   same value in all positions in the array');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-verify     verify sorted result');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-stats      statistics on number of comparisons, etc');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-all        run all sorting algorithms');
           Diagnostic.WriteLn;
           Diagnostic.WriteString('-insertion  insertion sorts');
           Diagnostic.WriteLn;
           Exit(1D);
       END (*--if*);
   END (*--while*);
END GetArguments;
(*--------------------*)

PROCEDURE Title ();

VAR   dateTime   : LONGINT;
VAR   dateString : ARRAY [0..99] OF CHAR;

BEGIN
   orderName[unordered]    := "Random";
   orderName[ascending]    := "Ascending";
   orderName[descending]   := "Descending";
   orderName[same]         := "Same Value";

   WriteLnString("Sorting Performance tests in Bob Campbell's Modula-2 v1.4d11");
   GetDateTime(dateTime);
   IUDateString(dateTime, shortDate, dateString);
   WriteString("Run on ");
   WriteLnString(dateString);
   WriteLblCardinal("# of runs: ", numRuns, 1);
   WriteLblCardinal(".  Array Size: ", arraySize, 1);
   WriteLblBoolean(".  Statistics: ", statistics);
   WriteLblBoolean(".  Verification: ", verifyOrder);
   WriteLn;
   WriteString("Data Set #");
   WriteCard(dataSet, 1);
   WriteString("  Ordering: ");
   WriteString(orderName[theOrder]);
   WriteString(".  ");
   CASE theOrder OF
     unordered:
           WriteLblCardinal("Seed: ", theSeed, 1);
   | ascending,
     descending:
           WriteLblCardinal("Sortedness Ratio: ", theRatio, 1);
           WriteLblCardinal("% with ", TRUNC((FLOAT(theRatio) / 100.) * FLOAT(arraySize)), 1);
           WriteString(" elements out-of-order.");
   | same:
           WriteLblCardinal("Value: ", theValue, 1);
   END (*--case*);
   WriteLn;
   IF (quick IN theSorts) THEN
       WriteLblCardinal("Cutoff: ", cutoff, 1);
       WriteLn;
   END (*--if*);
   WriteLn;
   WriteBlanks(22);
   WriteString("Ticks");
   WriteBlanks(2);
   WriteString("Seconds");
   IF statistics THEN
       WriteBlanks(2);
       WriteString("Interchanges");
       WriteBlanks(2);
       WriteString("Comparisons");
       WriteBlanks(2);
       WriteString("Assignments");
   END (*--if*);
   WriteLn;
END Title;
(*--------------------*)

PROCEDURE CompareIntegersAt (left, right : CARDINAL) : Relations.Relation;
BEGIN
   IF (theIntegerArray[left] < theIntegerArray[right]) THEN
       RETURN Relations.less;
   ELSIF (theIntegerArray[left] > theIntegerArray[right]) THEN
       RETURN Relations.greater;
   ELSE
       RETURN Relations.equal;
   END (*--if*);
END CompareIntegersAt;
(*--------------------*)

PROCEDURE CompareIntegersAtStats (left, right : CARDINAL) : Relations.Relation;
BEGIN
   INC(comparisons);
   IF (theIntegerArray[left] < theIntegerArray[right]) THEN
       RETURN Relations.less;
   ELSIF (theIntegerArray[left] > theIntegerArray[right]) THEN
       RETURN Relations.greater;
   ELSE
       RETURN Relations.equal;
   END (*--if*);
END CompareIntegersAtStats;
(*--------------------*)

PROCEDURE AssignIntegersAt (left, right : CARDINAL);
BEGIN
   theIntegerArray[left] := theIntegerArray[right];
END AssignIntegersAt;
(*--------------------*)

PROCEDURE AssignIntegersAtStats (left, right : CARDINAL);
BEGIN
   INC(assignments);
   theIntegerArray[left] := theIntegerArray[right];
END AssignIntegersAtStats;
(*--------------------*)

PROCEDURE SwapIntegersAt (left, right : CARDINAL);

VAR temp : INTEGER;

BEGIN
   temp := theIntegerArray[left];
   theIntegerArray[left] := theIntegerArray[right];
   theIntegerArray[right] := temp;
END SwapIntegersAt;
(*--------------------*)

PROCEDURE SwapIntegersAtStats (left, right : CARDINAL);

VAR temp : INTEGER;

BEGIN
   INC(interchanges);
   temp := theIntegerArray[left];
   theIntegerArray[left] := theIntegerArray[right];
   theIntegerArray[right] := temp;
END SwapIntegersAtStats;
(*--------------------*)

(*-- The next three routines are especially for MeanSort. *)

PROCEDURE ValueAt (index : CARDINAL) : INTEGER;
BEGIN
   RETURN theIntegerArray[index];
END ValueAt;
(*--------------------*)

PROCEDURE AssignMean (index : CARDINAL; mean : INTEGER);
BEGIN
   theIntegerArray[index] := mean;
END AssignMean;
(*--------------------*)

PROCEDURE AssignMeanStats (index : CARDINAL; mean : INTEGER);
BEGIN
   INC(assignments);
   theIntegerArray[index] := mean;
END AssignMeanStats;
(*--------------------*)

PROCEDURE GetRandomData;

VAR        i : CARDINAL;

   (*
   PermuteArray alters the strict ordering of the array based on the sortedness
   ratio as given in the command line. The sortedness ratio of a list of N
   records is defined as k/N, where k is the minimum number of elements that
   must be removed so that the remaining portion of the list is sorted. For a
   sorted list, this ratio is zero, and for a completely out-of-order list, the
   ratio approaches one. Each list of size N with a sortedness ratio k/N is
   created from the identify permutation:
       First, k randomly chosen elements are removed from the permutation and
       randomly inserted into another list of size N.
       Then the remaining N-k elements of the permutation are inserted in order
       in the vacant list slots. If, in so doing, one of the random k elements
       lies between, and has a value between, two of the inserted elements, then
       the random element and one of the inserted elements are exchanged. Thus,
       the k elements of the permutation are the smallest subset whose removal
       leaves the list sorted; hence, the sortedness ratio of the list is k/n.
   See further references [Cook, Wainwright].
   
   In our case, we initialize the permuted array to zeroes to indicate vacant
   slots.
   *)

   PROCEDURE PermuteArray;

   VAR     k : CARDINAL;       (*-- minimum number of elements that are out-of-order *)
   VAR     i : CARDINAL;       (*-- loop index over permutedArray *)
   VAR     j : CARDINAL;       (*-- number of randomly permuted elements so far *)
   VAR     h : CARDINAL;       (*-- index into permuted array for random element *)
   VAR     permutedArray : POINTER TO DataArray;

   BEGIN
       k := TRUNC((FLOAT(theRatio) / 100.) * FLOAT(arraySize));
       IF (k > 0) THEN
           Allocate(permutedArray, SIZE(DataArray));
           IF (permutedArray) = NIL THEN
               Diagnostic.WriteString('Could not allocate permuted array');
               Diagnostic.WriteLn;
               Exit(3);
           END (*--if*);
           
           (*-- Initialize the permuted array --*)
           FOR i := 0 TO arraySize-1 DO
               permutedArray^[i] := 0;
           END (*--for*);
           
           (*-- Select k elements at random to insert into permuted array --*)
           j := k;
           WHILE (j # 0) DO
               i := VAL(CARDINAL, ABS(Random())) MOD arraySize;        (*-- get a random slot in data array *)
               h := VAL(CARDINAL, ABS(Random())) MOD arraySize;        (*-- get a random slot in permuted array *)
               (*-- make sure we haven't used either slot before *)
               IF (theRandomData^[i] # 0) & (permutedArray^[h] = 0) THEN
                   permutedArray^[h] := theRandomData^[i];
                   theRandomData^[i] := 0;
                   DEC(j);
               END (*--if*);
           END (*--while*);
           
           (*-- Insert the elements of the identity permutation into the permuted array --*)
           h := 0;
           i := 0;
           WHILE (i <= arraySize-1) DO
               IF (theRandomData^[i] # 0) THEN
                   IF (permutedArray^[h] = 0) THEN
                       permutedArray^[h] := theRandomData^[i];

                       (*-- advance both indices --*)
                       INC(h);
                       INC(i);
                   ELSE

                       (*-- ensure we haven't put permuted elements in order --*)
                       IF (theOrder = ascending) & (h > 0) & (permutedArray^[h] = permutedArray^[h-1]+1) THEN
                           j := permutedArray^[h];
                           permutedArray^[h] := permutedArray^[h-1];
                           permutedArray^[h-1] := j;
                       ELSIF (theOrder = descending) & (permutedArray^[h] = permutedArray^[h-1]-1) THEN
                           j := permutedArray^[h];
                           permutedArray^[h] := permutedArray^[h-1];
                           permutedArray^[h-1] := j;
                       END (*--if*);
                       INC(h);
                   END (*--if*);
               ELSE
                   INC(i);
               END (*--if*);
           END (*--while*);
           
           (*-- Copy permuted data back into original array for sort tests --*)
           FOR i := 0 TO arraySize-1 DO
               theRandomData^[i] := permutedArray^[i];
           END (*--for*);
           
           Deallocate(permutedArray, SIZE(DataArray));
       END (*--if*);
   END PermuteArray;

BEGIN
   Allocate(theRandomData, SIZE(DataArray));
   IF theRandomData = NIL THEN
       Diagnostic.WriteString('Could not allocate the data array');
       Diagnostic.WriteLn;
       Exit(2);
   END (*--if*);

   randSeed := theSeed;
   CASE theOrder OF
       unordered : FOR i := 0 TO arraySize-1 DO
                     theRandomData^[i] := Random();
                   END (*--for*);
   |   ascending : FOR i := 0 TO arraySize-1 DO
                     theRandomData^[i] := i+1;
                   END (*--for*);
                   PermuteArray;
   |   descending: FOR i := 0 TO arraySize-1 DO
                     theRandomData^[i] := arraySize-i;
                   END (*--for*);
                   PermuteArray;
   |   same      : FOR i := 0 TO arraySize-1 DO
                     theRandomData^[i] := theValue;
                   END (*--for*);
   END (*--case*);
END GetRandomData;
(*--------------------*)

PROCEDURE FillIntegerArray;

VAR        i : CARDINAL;

BEGIN
   comparisons := 0D;
   interchanges:= 0D;
   assignments := 0D;
   FOR i := 0 TO arraySize-1 DO
       theIntegerArray[i] := theRandomData^[i];
   END (*--for*);
END FillIntegerArray;
(*--------------------*)

VAR startTicks : LONGINT;
    stopTicks  : LONGINT;

PROCEDURE PrintResults (    theMsg : ARRAY OF CHAR (*--in   *));

VAR elapsedTicks : LONGINT;
    elapsedSeconds : REAL;
    width : INTEGER;
    index : CARDINAL;

BEGIN
   elapsedTicks := stopTicks - startTicks;
   WriteString(theMsg);
   width := 20 - HIGH(theMsg);
   IF (width > 0) THEN
       WriteBlanks(width);
   END (*--if*);
   WriteInt(elapsedTicks, 6);
   elapsedSeconds := FLOAT(elapsedTicks) / 60.;
   WriteReal(elapsedSeconds, 9, 2, TRUE);

   IF statistics THEN
       WriteLongCard(interchanges, 14);
       WriteLongCard(comparisons, 13);
       WriteLongCard(assignments, 13);
   END (*--if*);

   IF verifyOrder THEN
       index := 0;
       LOOP
           IF (theIntegerArray[index] > theIntegerArray[index+1]) THEN
               WriteBlanks(2);
               WriteString("### Assertion failed! Out of order at [");
               WriteCard(index, 1);
               WriteString("]. ");
               WriteInt(theIntegerArray[index], 1);
               WriteString(" > ");
               WriteInt(theIntegerArray[index+1], 1);
               EXIT (*--loop*);
           END (*--if*);
           INC(index);
           IF (index >= arraySize-1) THEN
               EXIT (*--loop*);
           END (*--if*);
       END (*--loop*);
   END (*--if*);
   WriteLn;
END PrintResults;
(*--------------------*)

VAR  index : CARDINAL;

BEGIN
   GetArguments();
   Title();
   GetRandomData();
   IF statistics THEN
       doCompare2 := CompareIntegersAtStats;
       doAssign2  := AssignIntegersAtStats;
       doSwap       := SwapIntegersAtStats;
       doValue      := ValueAt;
       doAssignMean := AssignMeanStats;
   ELSE
       doCompare2 := CompareIntegersAt;
       doAssign2  := AssignIntegersAt;
       doSwap       := SwapIntegersAt;
       doValue      := ValueAt;
       doAssignMean := AssignMean;
   END (*--if*);
   
   FOR index := 1 TO numRuns DO

       IF insertion IN theSorts THEN
           FillIntegerArray;
           startTicks:=TickCount();
           Sort.InsertionSort(arraySize, doCompare2, doSwap);
           stopTicks:=TickCount();
           PrintResults("Insertion Sort");

           FillIntegerArray;
           startTicks:=TickCount();
           Sort.BinaryInsertionSort(arraySize, doCompare2, doAssign2);
           stopTicks:=TickCount();
           PrintResults("Binary Insertion");
       END (*--if*);

       IF selection IN theSorts THEN
           FillIntegerArray;
           startTicks:=TickCount();
           Sort.SelectionSort (arraySize,      (*-- # of items in the array *)
                               doCompare2,     (*-- item comparison routine *)
                               doSwap);        (*-- item swap routine *)
           stopTicks:=TickCount();
           PrintResults("Selection Sort");
       END (*--if*);
   
       IF bubble IN theSorts THEN
           FillIntegerArray;
           startTicks:=TickCount();
           Sort.BubbleSort(arraySize,      (*-- # of items in the array *)
                           doCompare2,     (*-- item comparison routine *)
                           doSwap);        (*-- item swap routine *)
           stopTicks:=TickCount();
           PrintResults("Bubble Sort");
       END (*--if*);
       IF shaker IN theSorts THEN
           FillIntegerArray;
           startTicks:=TickCount();
           Sort.ShakerSort(arraySize,      (*-- # of items in the array *)
                           doCompare2,     (*-- item comparison routine *)
                           doSwap);        (*-- item swap routine *)
           stopTicks:=TickCount();
           PrintResults("Shaker Sort");
       END (*--if*);
       IF shell IN theSorts THEN
           FillIntegerArray;
           startTicks:=TickCount();
           Sort.ShellSort (arraySize,  (*-- # of items in the array *)
                           doCompare2, (*-- item comparison routine *)
                           doAssign2); (*-- item swap routine *)
           stopTicks:=TickCount();
           PrintResults("Shell Sort");
       END (*--if*);
   
       IF heap IN theSorts THEN
           FillIntegerArray;
           startTicks:=TickCount();
           Sort.HeapSort (arraySize,     (*-- # of items in the array *)
                          doCompare2,    (*-- item comparison routine *)
                          doSwap);       (*-- item swap routine *)
           stopTicks:=TickCount();
           PrintResults("Heap Sort");
       END (*--if*);
   
       IF quick IN theSorts THEN
           FillIntegerArray;
           startTicks:=TickCount();
           Sort.QuickSort(arraySize, doCompare2, doAssign2, doSwap, cutoff);
           stopTicks:=TickCount();
           PrintResults("Quick Sort");
       END (*--if*);
   
       IF bsort IN theSorts THEN
           FillIntegerArray;
           startTicks:=TickCount();
           Sort.BSort(arraySize,         (*-- # of items in the array *)
                      doCompare2,        (*-- item comparison routine *)
                      doAssign2,         (*-- item assignment routine *)
                      doSwap);           (*-- item swap routine *)
           stopTicks:=TickCount();
           PrintResults("BSort");
       END (*--if*);

       IF mean IN theSorts THEN
           FillIntegerArray;
           startTicks:=TickCount();
           Sort.MeanSort(arraySize,     (*-- # of items in the array *)
                         doCompare2,    (*-- item comparison routine *)
                         doSwap,        (*-- item swap routine *)
                         doValue,       (*-- item retrieval routine *)
                         doAssignMean); (*-- assign mean to array[numItems] *)
           stopTicks:=TickCount();
           PrintResults("Mean Sort ");
       END (*--if*);

       IF merge IN theSorts THEN
           FillIntegerArray;
           startTicks:=TickCount();
           MergeSorts.StraightMerge(arraySize-1, doCompare2, doAssign2);
           stopTicks:=TickCount();
           PrintResults("Straight Merge Sort ");
       END (*--if*);
   END (*--for*);
   WriteLn;
   Deallocate(theRandomData, SIZE(DataArray));
END TimeSort.
