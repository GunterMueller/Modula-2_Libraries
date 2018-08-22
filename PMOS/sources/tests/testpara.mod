MODULE TestParameterPassing;

FROM ParTest IMPORT TestProc;

VAR a, b: CARDINAL;

BEGIN
    TestProc (a, b);

    (* Sets a:=1 and b:=2, provided that the parameters are passed so	*)
    (* that b is deepest on the stack.  We can use the debugger to see	*)
    (* whether this really happens.					*)
    (* Result: with TopSpeed 1.17 we get a=2, b=1, which means that	*)
    (* the parameter-passing mechanism pushes a and then b.		*)

END TestParameterPassing.
