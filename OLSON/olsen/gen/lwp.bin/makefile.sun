#
# Makefile for compiling/linking/etc.:
# 	Sun 
#
# This is a generic makefile for "bin" directories.
# If you want to make modifications, please make them 
# in environ.mak and/or depend.mak files.
#
include environ.mak

all: $(Targets)

depend: $(DependSources)
	$(Depend)

include depend.mak

install: all
	$(Install)

clean:
	$(Clean)
