#
# List of dependencies for the hackm2pp.
#
hackm2pp.o: M2PScanner.sym M2PParser.sym HackNameLists.sym HackTextIO.sym
M2PParser.sym: M2PScanner.sym HackNameLists.sym
M2PParser.o: M2PScanner.sym HackNameLists.sym
M2PScanner.sym: HackTextIO.sym
M2PScanner.o: M2PScanner.sym HackNameLists.sym HackTextIO.sym
