echo off
rem need to use the ovly version for space considerations.
m2comp %1/B/NOA/NOQ
if not errorlevel 1 goto End
    echo hello > makestop.$$$
:End
