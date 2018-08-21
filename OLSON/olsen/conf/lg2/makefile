#
# Makefile for compiling/linking/etc.:
# 	Logitech 2.0, IBM PC, MS-DOS
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
zoo:
	set

# The following rules are useful for setting up the proper debugging 
# environment.  See the global rules for more info.
$(M2env): environ.mak $(MakeRules)
	echo echo off > $@
	echo set M2SYM=$(M2SYM)>>$@
	echo set M2LNK=$(M2LNK)>>$@
	echo set M2REF=$(M2REF)>>$@
	echo set M2MOD=$(M2MOD)>>$@
	echo set M2MAP=$(M2MAP)>>$@
$(M2pmd): $(M2env)
	copy $? $@
	echo $(StandardM2pmd)>>$@
$(M2rtd): $(M2env)
	copy $? $@
	echo $(StandardM2rtd)>>$@
