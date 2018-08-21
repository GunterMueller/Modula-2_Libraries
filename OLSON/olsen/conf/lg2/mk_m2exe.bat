echo off
if .%2 == . goto NoArgs
if not exist %2 == goto NotFound
    copy %2 l2erts.l2e
    m2 c:\m2lod\lod2exe %1%3
    if not exist %1.exe goto Error
    del %1.lod
    goto End
:NoArgs
    echo You must supply two argments: "file.lod" and "l2erts"
    goto Error
:NotFound
    echo "%2": couldn't find.
:Error
    echo hello > makestop.$$$
:End
    del l2erts.l2e
