MODULE TopoSortTest;

(* Test the Topological sorting procedure with a simple list of numbers *)
(* J. Andrea, Dec.19/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

FROM InOut IMPORT WriteString, WriteLn, WriteCard;
FROM Sorting IMPORT TopoSort;

CONST
  max = 5;

VAR
  x_index, y_index :ARRAY [1..max] OF CARDINAL;
  i, j             :CARDINAL;

  (* -------------------------------------------------- *)
  PROCEDURE DoSort;
  VAR
    solution         :ARRAY [1..max+1] OF CARDINAL;
    n_solution       :CARDINAL;

    error, sorted, ok :BOOLEAN;

    i                :CARDINAL;

  BEGIN

      WriteLn; WriteString( 'the list is:' ); WriteLn;
      FOR i := 1 TO max DO
         WriteCard( x_index[i], 2 ); WriteString( '   ' );
         WriteCard( y_index[i], 2 ); WriteLn;
      END;

      WriteLn;   ok := TRUE;

      TopoSort( x_index, y_index, max, solution, n_solution, error, sorted );

      WriteString( 'topologically sorted is: ' );

      IF error THEN
        WriteString( 'error in passing solution' ); WriteLn;
        ok := FALSE;
      ELSE

        FOR i := 1 TO n_solution DO
           WriteCard( solution[i], 0 ); WriteString( '  ' );
           IF solution[i] # i THEN ok := FALSE; END;
        END;
        WriteLn;

        IF NOT sorted THEN
          WriteString( 'set is not partially ordered' ); WriteLn;
          ok := FALSE;
        END;

      END;

      IF ok THEN
        WriteString( 'results as expected, all ok' );
      ELSE
        WriteString( 'something wrong !, not as expected !' );
      END;
      WriteLn;

    END DoSort;

BEGIN

  (* forward pointing list *)
  FOR i := 1 TO max DO
    x_index[i] := i;
    y_index[i] := i + 1;
  END;
  DoSort;

  (* backward pointing list *)
  FOR i := 1 TO max DO
    j := max + 1 - i;
    x_index[j] := i;
    y_index[j] := i + 1;
  END;
  DoSort;

END TopoSortTest.
