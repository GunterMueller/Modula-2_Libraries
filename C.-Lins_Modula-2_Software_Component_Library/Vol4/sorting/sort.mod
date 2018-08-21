IMPLEMENTATION MODULE Sort;
(*==============================================================
    Version  : 2.0 16 Sep 1990 C. Lins
    Compiler : Generic pc Modula-2
    Component: Tools - Sorting Utilities

    REVISION HISTORY
    v1.00  13-21 May 1989  C. Lins:
        Initial implementation

   v2.00  16 Sep 1990  C. Lins
     Created generic pc version
   (C) Copyright 1990 Charles A. Lins
==============================================================*)

FROM Relations IMPORT
   (*--type*)  Relation;

(*
This version of Insertion Sort is from Jon Bentley, "Writing
Efficient Programs", pg 64. The use of assignment to break the
swap operation into its components may not be applicable here
as our assignments involve the overhead of procedure calls.
*)

PROCEDURE InsertionSort  (    numItems : CARDINAL    (*--in   *);
                              compare  : CompareProc (*--in   *);
                              swap     : SwapProc    (*--in   *));

VAR        index : CARDINAL;
           jndex : CARDINAL;

BEGIN
   FOR index := 1 TO numItems-1 DO
       jndex := index;
       WHILE (jndex > 0) & (compare(jndex, jndex-1) = less) DO
           swap(jndex, jndex-1);
           DEC(jndex);
       END (*--while*);
   END (*--for*);
END InsertionSort;
(*--------------------*)

(*
A variation on Insertion Sort using a binary (instead of linear)
search of the array. This particular implementation is based on the one
given in Wirth []. While the algorithmic complexity is no better than
linear insertion sort, the actual execution time tends to be less due to
a reduction in the number of comparisons. This improvement does not hold
when the array is already initially sorted or when the array contains
a large number of identical values.
*)

PROCEDURE BinaryInsertionSort (    numItems : CARDINAL    (*--in   *);
                                   compare  : CompareProc (*--in   *);
                                   assign   : AssignProc  (*--in   *));

VAR        index  : CARDINAL;      (*--loop index over items of the array *)
           jndex  : CARDINAL;
           middle : CARDINAL;      (*--middle of the subarray *)
           left   : CARDINAL;      (*--left of of the subarray *)
           right  : CARDINAL;      (*--right of of the subarray *)

BEGIN
   FOR index := 1 TO numItems-1 DO
       assign(numItems, index);
       left := 0;
       right:= index;
       WHILE (left < right) DO
           middle := (left+right) DIV 2;
           IF (compare(middle, numItems) # greater) THEN
               left := middle + 1;
           ELSE
               right := middle;
           END (*--if*);
       END (*--while*);
       FOR jndex := index TO right+1 BY -1 DO
           assign(jndex, jndex-1);
       END (*--for*);
       assign(right, numItems);
   END (*--for*);
END BinaryInsertionSort;
(*--------------------*)

(*
The following is a delayed selection sort algorithm derived from Sedgewick
[pg. 95]. This implementation, while its performance is on the order of
O(n**2), in practice performs better than straight selection sort by
reducing the number of exchanges. The actual running time of the algorithm
varies little regardless of the input.
*)

PROCEDURE SelectionSort  (    numItems : CARDINAL    (*--in   *);
                              compare  : CompareProc (*--in   *);
                              swap     : SwapProc    (*--in   *));

VAR        index   : CARDINAL;     (*--loop index over items of the array *)
           jndex   : CARDINAL;     (*--inner loop index *)
           min     : CARDINAL;     (*--smallest item so far *)

BEGIN
   FOR index := 0 TO numItems-1 DO
       min := index;
       FOR jndex := index+1 TO numItems-1 DO
           IF (compare(jndex, min) = less) THEN
               min := jndex;
           END (*--if*);
       END (*--for*);
       swap(min, index);
   END (*--for*);
END SelectionSort;
(*--------------------*)

(*
ShellSort is a sort algorithm with a long history going back to 1959.
Sedgewick's version which dynamically calculates the offset based
on the equation: h(k-1) = 3*h(k)+1, giving the sequence 1, 4, 13, 40, 121,
364, 1093, 3280, 9841, .... This version has a minor enhancement avoiding
assignment of the temporary back into the same array location from which
it was extracted. Such an optimization may be acceptable here as the
assignment is accomplished using a procedure call Ñ a relatively expensive
operation. For an excellent discussion of this particular implementation
the reader is referred to [, pp. 97-99].
*)

PROCEDURE ShellSort      (    numItems : CARDINAL    (*--in   *);
                              compare  : CompareProc (*--in   *);
                              assign   : AssignProc  (*--in   *));

VAR   index : CARDINAL;
      jndex : INTEGER;
      h     : INTEGER;

BEGIN
   h := 1;
   REPEAT
       h := 3 * h + 1;
   UNTIL VAL(CARDINAL, h) > numItems;
   REPEAT
       h := h DIV 3;
       FOR index := h TO numItems-1 DO
           assign(numItems, index);
           jndex := index;
           LOOP
               IF (compare(jndex-h, numItems) = greater) THEN
                   assign(jndex, jndex-h);
                   DEC(jndex, h);
                   IF (jndex < h) THEN
                       EXIT (*--loop*);
                   END (*--if*);
               ELSE
                   EXIT (*--loop*);
               END (*--if*);
           END (*--loop*);
           IF (jndex # VAL(INTEGER, index)) THEN
               assign(jndex, numItems);
           END (*--if*);
       END (*--for*);
   UNTIL (h = 1);
END ShellSort;
(*--------------------*)

(*
HeapSort is an unusual sort algorithm. It initially constructs a priority
queue followed by repeated extraction of the maximum element from the
queue until the queue is empty. The heap data structure is used for the
priority queue and this shares the space with the array being sorted.
HeapSort has been proven to guarantee sorting N elements in time
proportional to N log N, regardless of the input. This is significant in
that there is no worst case, and therefore, no input sequence causing
HeapSort to run slower.

Both Bentley [] and Sedgewick [] devote considerable attention to the
priority queue data structure and heapsort.
*)

PROCEDURE HeapSort       (    numItems : CARDINAL    (*--in   *);
                              compare  : CompareProc (*--in   *);
                              swap     : SwapProc    (*--in   *));

VAR        index  : CARDINAL;

   PROCEDURE SiftUp (    index : CARDINAL (*--in   *);
                         upper : CARDINAL (*--in   *));

   VAR     jndex  : CARDINAL;

   BEGIN
       WHILE (2 * index) <= upper DO
           jndex := 2 * index;
           IF (jndex < upper) THEN
               IF (compare(jndex, jndex+1) = less) THEN
                   INC(jndex);
               END (*--if*);
           END (*--if*);
           IF (compare(index, jndex) = less) THEN
               swap(index, jndex);
               index := jndex;
           ELSE
               index := upper+1;
           END (*--if*);
       END (*--while*);
   END SiftUp;

BEGIN
  IF (numItems > 1) THEN
       (*-- construct heap --*)
       FOR index := numItems DIV 2 TO 1 BY -1 DO
           SiftUp(index, numItems);
       END (*--for*);

       (*-- repeatedly extract minimum from priority queue heap --*)
       FOR index := numItems-1 TO 1 BY -1 DO
           SiftUp(0, index);
           swap(0, index);
       END (*--for*);
   END (*--if*);
END HeapSort;
(*--------------------*)

(*
Quicksort Ñ the preeminent sort algorithm. Since its introduction in
1961 by C.A.R. Hoare [, ] this algorithm has been subjected to rigorous
mathematical analysis and empirical experiment []. Numerous suggestions
to Hoare's original implementation have been made over the years towards
improving Quicksort especially with regard to the worst case scenarios.
These are discussed in greater detail below and in the references.

This particular implementation has all the optimizations thrown in
(other than using an non-recursive implementation):
   ¥ use an outer while loop to switch to a faster algorithm for
       nearly sorted small arrays (in this case, Insertion sort). Cutoff
       can be any value between 5 and 30, though on our system a value
       of 9 or 10 gives the best results. This is confirmed by empirical
       studies [].
   ¥ call the simplier sort only once (this is slightly more efficient
       when the elements have repeated keys or with large random arrays).
   ¥ reduce the growth of the stack by processing the smaller of the
       two subarrays first ("tail-recursion removal"). This ensures that
       the stack grows to at most about lg N entries.
   ¥ reduce the time in the inner loops by using two indices moving
       towards one another.
   ¥ code the 'split' algorithm in-line instead of using a procedure
       call.
   ¥ instead of selecting a partitioning element at random (or simply
       choosing the first element in a subarray as the partitioning
       element) this variant uses the median-of-three optimization.
       Here we take the median of a small section of the subarray (three
       elements). These are chosen from the first element, the last element,
       and the middle element of the subarray. This should improve the
       running time in the worst-case scenarios.

The only data set on which this algorithm does very poorly is when all
the values in the array are the same (a very unlikely occurance).
*)

PROCEDURE QuickSort      (    numItems : CARDINAL    (*--in   *);
                              compare  : CompareProc (*--in   *);
                              assign   : AssignProc  (*--in   *);
                              swap     : SwapProc    (*--in   *);
                              cutoff   : INTEGER     (*--in   *));

   PROCEDURE InsertionSort (    lower : INTEGER (*--in   *);
                                upper : INTEGER (*--in   *));

   VAR     index : INTEGER;
           jndex : INTEGER;

   BEGIN
       FOR index := lower+1 TO upper DO
           jndex := index;
           WHILE (jndex > lower) & (compare(jndex, jndex-1) = less) DO
               swap(jndex, jndex-1);
               DEC(jndex);
           END (*--while*);
       END (*--for*);
   END InsertionSort;

   PROCEDURE quicksort (    lower : INTEGER (*--in   *);
                            upper : INTEGER (*--in   *));

   VAR     index : INTEGER;
           jndex : INTEGER;
           middle: INTEGER;

   BEGIN
       WHILE (upper - lower > cutoff ) DO
       
           (*--
               Choose a partitioning element by taking the median of the
             lower, middle, and upper elements of the subarray.
           --*)
           middle := (lower+upper) DIV 2;
           IF (compare(middle, upper) = greater) THEN
               swap(middle, upper);
           END (*--if*);
           IF (compare(middle, lower) = greater) THEN
               swap(middle, lower);
           END (*--if*);
           IF (compare(lower, upper) = greater) THEN
               swap(lower, upper);
           END (*--if*);
           index := lower;
           jndex := upper;
           assign(numItems, lower);
           
           (*-- split array in two --*)
           WHILE (index < jndex) DO
               WHILE (compare(jndex, numItems) = greater) DO
                   DEC(jndex);
               END (*--while*);
               assign(index, jndex);
               WHILE (index < jndex) & (compare(index, numItems) # greater) DO
                   INC(index);
               END (*--while*);
               assign(jndex, index);
           END (*--while*);
           assign(index, numItems);

           (*-- sort recursively, the smallest first --*)
           IF (index-lower < upper-index) THEN
               quicksort(lower, index-1);
               lower := index + 1;
           ELSE
               quicksort(index+1, upper);
               upper := index - 1;
           END (*--if*);
       END (*--while*);
   END quicksort;

BEGIN
  IF (numItems > 1) THEN
    quicksort(0, numItems-1);
       InsertionSort(0, numItems-1);
  END (*--if*);
END QuickSort;
(*--------------------*)

(*
BSort from CACM April 1985, pp 400-402 by Roger L. Wainwright. Far
superior to standard QuickSort on arrays that are already sorted or
nearly so. The idea is a combination of Bubble Sort (of all things!)
and Quicksort taking advantage of partially sorted subarrays. The
implementation below is a direct translation into Modula-2 from 
the Pascal version as given in the aforementioned article. 

The reader is referred to Wainwright's original article for a complete
discussion of the operation of this algorithm with a few caveats.
First, the performance results for Quicksort are unfair in that a
non-optimized implementation was obviously used (based on the number
of comparisons). Second, there was no comparative measurement to other
traditional sort algorithms such as Shellsort and Heapsort. The experimental
results shown earlier demonstrate that there are many cases where these
algorithms outperform BSort considerably. The only area where we found
BSort to be an improvement over an optimized implementation of Quicksort
(or other sort algorithms) was on small (<= 200 elements) arrays with a
sortedness ratio <= 2%.
*)

PROCEDURE BSort          (    numItems : CARDINAL    (*--in   *);
                              compare  : CompareProc (*--in   *);
                              assign   : AssignProc  (*--in   *);
                              swap     : SwapProc    (*--in   *));

   PROCEDURE bsort (    m : INTEGER      (*--in   *);
                        n : INTEGER      (*--in   *);
                        midKey : INTEGER (*--in   *));

   VAR     continue : BOOLEAN; (*-- false when partitioning process are completed *)
           leftFlag : BOOLEAN; (*-- true whenever left subfile is not in sorted order *)
           rightFlag: BOOLEAN; (*-- true whenever right subfile is not in sorted order *)
           index    : INTEGER; (*-- used to partition the subfiles so that at any time *)
           jndex    : INTEGER; (*-- k[index] < k[midKey] and k[jndex] >= k[midKey] *)
           size     : INTEGER; (*-- number of items in a subfile *)

   BEGIN
       IF (m < n) THEN
           assign(numItems, midKey);
           leftFlag    := FALSE;
           rightFlag   := FALSE;
           index       := m;
           jndex       := n;
           continue    := TRUE;
           WHILE continue DO
               WHILE (index # jndex) & (compare(index, numItems) = less) DO
                   (*--
                       build the left subfile ensuring that the
                       rightmost key is always the largest of the subfile
                   --*)
                   IF (index # m) & (compare(index-1, index) = greater) THEN
                       swap(index-1, index);
                       leftFlag := TRUE;
                   END (*--if*);
                   INC(index);
               END (*--while*);
           
               WHILE (index # jndex) & (compare(jndex, numItems) # less) DO
                   (*-- 
                       build the right subfile ensuring that the
                       leftmost key is always the smallest of the subfile
                   --*)
                   IF (jndex # n) & (compare(jndex, jndex+1) = greater) THEN
                       swap(jndex+1, jndex);
                       rightFlag := TRUE;
                   END (*--if*);
                   DEC(jndex);
               END (*--while*);
               
               IF (index # jndex) THEN
                   (*--
                       interchange k[index] from the left subfile with
                       k[jndex] from the right subfile
                    --*)
                   swap(jndex, index);
               ELSE
                   (*-- partitioning into left and right subfiles has been completed --*)
                   IF (compare(jndex, numItems) # less) THEN
                       (*-- check the right subfile to ensure that the first element, k[jndex] is the smallest --*)
                       IF (compare(jndex, jndex+1) = greater) THEN
                           swap(jndex+1, jndex);
                           rightFlag := TRUE;
                       END (*--if*);
                   ELSE
                       (*-- check the left subfile to ensure that the last element, k[index-1] is the largest --*)
                       IF (compare(index-1, index) = greater) THEN
                           swap(index-1, index);
                           leftFlag := TRUE;
                       END (*--if*);
                       IF (compare(index-2, index-1) = greater) THEN
                           swap(index-1, index-2);
                       END (*--if*);
                   END (*--if*);
                   continue := FALSE;
               END (*--if*);
           END (*--while*);
           
           (*-- process the left subfile --*)
           size := index - m;
           IF (size > 2) & leftFlag THEN (*-- subfile must have at least 3 elements and not be already sorted *)
               IF (size = 3) THEN                  (*-- special case of 3 elements --*)
                   (*-- place k[m] and k[m+1] in sorted order --*)
                   IF (compare(m, m+1) = greater) THEN
                       swap(m, m+1);
                   END (*--if*);
               ELSE
                   bsort(m, index-2, (m+index-2) DIV 2);
               END (*--if*);
           END (*--if*);
           
           (*-- process the left subfile --*)
           size := n - jndex + 1;
           IF (size > 2) & rightFlag THEN (*-- subfile must have at least 3 elements and not be already sorted *)
               IF (size = 3) THEN                   (*-- special case of 3 elements --*)
                   (*-- place k[j+1] and k[j+2] in sorted order --*)
                   IF (compare(jndex+1, jndex+2) = greater) THEN
                       swap(jndex+1, jndex+2);
                   END (*--if*);
               ELSE
                   bsort(jndex+1, n, (jndex+1+n) DIV 2);
               END (*--if*);
           END (*--if*);
       END (*--if*);
   END bsort;

BEGIN
  IF (numItems > 1) THEN
    bsort(0, numItems-1, numItems DIV 2);
  END (*--if*);
END BSort;
(*--------------------*)

(*
MeanSort derived from algorithm by Dalia Motzkin, MeanSort, Communications of the ACM 26 (4),
April 1983, 250-251. The author believes this algorithm is better than QuickSort, but
Wainwright contends that this isn't so. Obviously, this algorithm is only works for arrays
containing keys whose values have arithmetic operators applicable on them. In this version,
we assume that the array is of INTEGERs and that the sum of any given subarray does not
exceed MAX(LONGINT). We found that on large (2000 elements) random arrays with a sortedness
ratio >= 8% the running time for MeanSort outperformed BSort. In all other cases, BSort was
superior.
*)

PROCEDURE MeanSort       (    numItems : CARDINAL       (*--in   *);
                              compare  : CompareProc    (*--in   *);
                              swap     : SwapProc       (*--in   *);
                              value    : ValueProc      (*--in   *);
                              assign   : AssignItemProc (*--in   *));

   PROCEDURE meansort (    lower : INTEGER (*--in   *);
                           upper : INTEGER (*--in   *);
                           mean  : INTEGER (*--in   *));

   VAR     index    : INTEGER;
           jndex    : INTEGER;
           leftSum  : LONGINT;
           rightSum : LONGINT;

   BEGIN
       IF (lower < upper) THEN
           leftSum := 0;
           rightSum:= 0;
           index   := lower;
           jndex   := upper;
           assign(numItems, mean);
           LOOP
               WHILE (index # jndex) & (compare(index, numItems) = less) DO
                   INC(leftSum, value(index));
                   INC(index);
               END (*--while*);
               WHILE (index # jndex) & (compare(jndex, numItems) # less) DO
                   INC(rightSum, value(jndex));
                   DEC(jndex);
               END (*--while*);
               IF (index # jndex) THEN
                   swap(index, jndex);
               ELSE
                   EXIT (*--loop*);
               END (*--if*);
           END (*--loop*);
           IF (index = lower) THEN
               RETURN;
           END (*--if*);
           INC(rightSum, value(jndex));
           meansort(lower, index-1, VAL(INTEGER, leftSum DIV VAL(LONGINT, index - lower)));
           meansort(jndex, upper, VAL(INTEGER, rightSum DIV VAL(LONGINT, upper - jndex + 1)));
       END (*--if*);
   END meansort;

VAR        sum   : LONGINT;      (*-- sum of array values *)
           index : CARDINAL;       (*-- loop index over the array for computing initial mean *)

BEGIN
  IF (numItems > 1) THEN
       sum := 0;
       FOR index := 0 TO numItems-1 DO
           INC(sum, value(index));
       END (*--for*);
    meansort(0, numItems-1, VAL(INTEGER, sum DIV VAL(LONGINT, numItems)));
  END (*--if*);
END MeanSort;
(*--------------------*)

(*
From Wirth, Algorithms & Data Structures pg 104.
*)

PROCEDURE StraightMerge  (    numItems : CARDINAL    (*--in   *);
                              compare  : CompareProc (*--in   *);
                              assign   : AssignProc  (*--in   *));

VAR      index : CARDINAL;
         jndex : CARDINAL;
         kndex : CARDINAL;
         limit : CARDINAL;
         t     : CARDINAL;
         h, m, p, q, r : INTEGER;
         up : BOOLEAN;

BEGIN
  up := TRUE;
  p  := 1;
  REPEAT
   h := 1;
       m := numItems;
       IF up THEN
           index := 1;
           jndex := numItems;
           kndex := numItems + 1;
           limit := 2 * numItems;
       ELSE
           kndex := 1;
           limit := numItems;
           index := numItems + 1;
           jndex := 2 * numItems;
       END (*--if*);
       REPEAT
           (*-- merge a run from i- and j- sources into k-destination *)
           IF (m >= p) THEN
               q := p;
           ELSE
               q := m;
           END (*--if*);
           DEC(m, q);
           IF (m >= p) THEN
               r := p;
           ELSE
               r := m;
           END (*--if*);
           DEC(m, r);
           WHILE (q > 0) & (r > 0) DO
               IF (compare(index, jndex) = less) THEN
                   assign(kndex, index);
                   kndex := VAL(INTEGER, kndex) + h;
                   INC(index);
                   DEC(q);
               ELSE
                   assign(kndex, jndex);
                   kndex := VAL(INTEGER, kndex) + h;
                   DEC(jndex);
                   DEC(r);
               END (*--if*);
           END (*--while*);
   
           WHILE (r > 0) DO
               assign(kndex, jndex);
               kndex := VAL(INTEGER, kndex) + h;
               DEC(jndex);
               DEC(r);
           END (*--while*);
   
           WHILE (q > 0) DO
               assign(kndex, index);
               kndex := VAL(INTEGER, kndex) + h;
               INC(index);
               DEC(q);
           END (*--while*);
           h := -h;
           t := kndex;
           kndex := limit;
           limit := t;
       UNTIL m = 0;
       up := ~up;
       p  := 2 * p;
   UNTIL VAL(CARDINAL, p) >= numItems;
   IF ~up THEN
       FOR index := 1 TO numItems DO
           assign(index, index + numItems);
       END (*--for*);
   END (*--if*);
END StraightMerge;
(*--------------------*)

(*
Gonnet's version of Bubble Sort. More efficient than the usual version
when the array is in random order or ordered in reverse. When the
array is ordered, or contains the same value in every position of the
array, it uses more time than other implementations. The author does not
recommend this algorithm for any purpose.
*)

PROCEDURE BubbleSort     (    numItems : CARDINAL    (*--in   *);
                              compare  : CompareProc (*--in   *);
                              swap     : SwapProc    (*--in   *));

VAR        index   : CARDINAL;     (*--loop index over items of the array *)
           jndex   : CARDINAL;     (*--lower bound at start of each FOR loop *)
           lower   : CARDINAL;     (*--lower bound of the array to sort *)
           upper   : CARDINAL;     (*--upper bound of the array to sort *)

BEGIN
   lower := 0;
   upper := numItems-1;
   WHILE (upper > lower) DO
       jndex := lower;
       FOR index := lower TO upper-1 DO
           IF (compare(index, index+1) = greater) THEN
               swap(index, index+1);
           END (*--if*);
           jndex := index;
       END (*--for*);
       upper := jndex;
   END (*--while*);
END BubbleSort;
(*--------------------*)

(*
Gonnet's version of Shaker Sort (double-direction bubble sort). This algorithm
performs reasonably well in comparison with the standard Bubblesort. For small,
nearly ordered arrays (<= 200 elements with a sortedness ratio <= 2%) the performance
was better than Quicksort but not as good as Insertion sort.
*)

PROCEDURE ShakerSort     (    numItems : CARDINAL    (*--in   *);
                              compare  : CompareProc (*--in   *);
                              swap     : SwapProc    (*--in   *));

VAR        index   : CARDINAL;     (*--loop index over items of the array *)
           jndex   : CARDINAL;     (*--lower bound at start of each FOR loop *)
           lower   : CARDINAL;     (*--lower bound of the array to sort *)
           upper   : CARDINAL;     (*--upper bound of the array to sort *)

BEGIN
   lower := 0;
   upper := numItems-1;
   WHILE (upper > lower) DO
       jndex := lower;
       FOR index := lower TO upper-1 DO
           IF (compare(index, index+1) = greater) THEN
               swap(index, index+1);
               jndex := index;
           END (*--if*);
       END (*--for*);
       upper := jndex;
       FOR index := upper TO lower+1 BY -1 DO
           IF (compare(index, index-1) = less) THEN
               swap(index, index-1);
               jndex := index;
           END (*--if*);
       END (*--for*);
       lower := jndex;
   END (*--while*);
END ShakerSort;
(*--------------------*)

END Sort.