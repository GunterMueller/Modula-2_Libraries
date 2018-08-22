@echo Archiving PMOS/2 to drive A:
copy pmos2.zip pmos2.zip.bak
copy a:pmos2.zip
zip -r -u pmos2.zip . -x *.red *.bak *.obj *.rsp *.map *.exe *.sym *.db* *.$$$
copy pmos2.zip a:



