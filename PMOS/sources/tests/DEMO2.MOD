IMPLEMENTATION MODULE Demo2;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore;

VAR x: CARDINAL;
    S: Semaphore;

PROCEDURE P(v: CARDINAL): CARDINAL;
    BEGIN
        RETURN v+1;
    END P;

BEGIN
    x := 0;
    CreateSemaphore (S, 1);
END Demo2.
