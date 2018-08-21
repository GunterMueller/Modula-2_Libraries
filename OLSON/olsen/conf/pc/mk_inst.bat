echo off
rem Can't save variables, so must be "in-line"
if %1. == . goto Usage
if not %2. == . goto DoOne
:Usage
    echo Usage: %0 installdir suffix1 .. suffixN
    goto End

:DoOne
if %2 == exe goto exe
copy *.%2 %1\%2
goto label
:exe
rem Exe's get installed into a place which isn't named after the suffix.
copy *.%2 %1
:label
if %3. == . goto End
copy *.%3 %1\%3
if %4. == . goto End
copy *.%4 %1\%4
if %5. == . goto End
copy *.%5 %1\%5
if %6. == . goto End
copy *.%6 %1\%6
if %7. == . goto End
copy *.%7 %1\%7
if %8. == . goto End
copy *.%8 %1\%8
if %9. == . goto End
copy *.%9 %1\%9

:End
