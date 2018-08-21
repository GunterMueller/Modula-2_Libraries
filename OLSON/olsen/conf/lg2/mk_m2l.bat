echo off
m2 c:\m2lod\link %1/A-%2
rem link doesn't return error codes
if exist %1.lod goto End
    echo hello > makestop.$$$
:End
