echo off
m2 c:\m2lod\comp %1/B/NOA/NOQ
if not errorlevel 1 goto End
    echo hello > makestop.$$$
:End
