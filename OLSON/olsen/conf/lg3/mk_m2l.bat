echo off
rem the linker prompts even though you say batch!
m2l %1/B%2
if not errorlevel 1 goto End
    echo hello > makestop.$$$
:End
