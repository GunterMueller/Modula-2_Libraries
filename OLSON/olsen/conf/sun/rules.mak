#
# GLOBAL make configuration for compiling/linking/etc.:
#      Sun Modula-2, OS, Architecture.
# This file is included in a file called "environ.mak" in the directory
# to be made.
#
# If you want GLOBAL changes, make them HERE.  If you want LOCAL changes,
# add a declaration of the particular macro on any line AFTER the line
# that contains the "include" statement.

# The base directory for all the files (must be absolute).
Root		= /usr/local/yaml

# Directory for executables (make, etc.) and installed home for this file.
# The $(BinDir) should also be in your PATH environment variable.
# The names "bin" and "rules.mak" are hardwired in all the makefiles, 
# so don't change them unless you want to edit things.
BinDir		= $(Root)/bin
MakeRules	= $(BinDir)/rules.mak

# Where to install the main library files and lightweight process library.
LibInstallDir	= $(Root)/lib
LwpInstallDir 	= $(Root)/lwp.lib

# The compiler, linker, and dependency generator require the following
# variable.  The value specified here may not meet your needs, so it 
# should be specified in your "environ.mak".
M2Path		= $(LibInstallDir)

# Modula-2 Preprocessor Environment
#
# If you want to set a variable to FALSE (undefined), set LocalM2ppFlags
# in your "environ.mak" file to be something like:
#	M2ppFlags= -UFalse1 -UFalse2
# To set a variable to TRUE, change the "U" in the above example to a "D".
# -D and -U flags are evaluated from left to right and always override
# the value of M2ppVars in your environment.
M2pp		= m2pp
M2ppFlags= -DBigEndian -DSunM2 -DSunOS -DSunArch -DAssert -DDebug -DM2V3 \
	   $(LocalM2ppFlags)


# Programs and flags
#
# Descriptions of programs we use.  If you change any of these, you
# had better change the rules as well.  Note that $(BinDir) must be
# in your path for make to work.
# 
# There is a bug in the Sun compiler which requires this "-M." to
# be here.  The problem is that the compiler searches the current
# directory after the rest of the -M flags.  By putting the "-M."
# here, we are working around the bug until it is fixed.
M2comp		= m2c -M.
M2compFlags 	= -g
M2link		= m2c -M.
M2linkFlags	= -g 

# Linking is a bit complicated.  The one assembly language file must
# be added on the command line.  There are four options for AsmLib.
# The default is to use the non-lwp installed version.  You can
# change the default (if you wish) or just set AsmLib in "environ.mak"
# after this file is included.  You may also add special files by
# setting the variable LocalM2linkLibraries in "environ.mak".
M2linkLibraries = $(LocalM2linkLibraries) $(AsmLib) -ltermcap
AsmLib			= $(InstalledAsmLib)

InstalledAsmLib		= $(LibInstallDir)/AsmLib.o
InstalledLwpAsmLib	= $(LwpInstallDir)/AsmLib.o
TestAsmLib		= $(Root)/gen/bin/AsmLib.o
TestLwpAsmLib		= $(Root)/gen/lwp.bin/AsmLib.o


# How to make the files "m2depend.mak" and "m2source.mak".  Note that
# the "m2depend" program always searches all files in the current directory.
Depend 		= m2depend $(M2Path)
# To make sure the files are preprocessed before m2depend executes.
DependSources	= $(M2Defs) $(M2Imps) $(M2Mods)

# Where to gather the binaries.  The default is just to install programs,
# but you may want to set InstallFiles to be $(LibInstallFiles) in your
# "environ.mak".  You may want to add $(M2Imps) if you want to do source 
# level debugging of the library after it is installed.   Note set
# InstallDir to be $(LibInstallDir) or $(LwpInstallDir) when installing
# library files.
Install		= mk_install $(InstallOptions) $(InstallFiles) $(InstallDir) 
InstallOptions		= -strip
InstallDir		= $(BinDir)
InstallFiles		= $(Programs)
LibInstallFiles 	= $(Libraries)
# Don't strip libraries!
LibInstallOptions 	=

# Cleaning is a dangerous thing (My mother has never believed me).
# If you want to clean, you will probably want a line that looks
# like: Clean = rm -f $(Libraries) $(Programs) $(Auxiliaries)
# Note that this line gets rid of preprocessed files.
Clean		= @echo No "clean" defined.  See $(BinDir)/rules.mak

#
# You probably don't need to configure anything below this line.
#
M2Programs	= $(M2Mods:.mod=) 

M2Libraries	= $(M2Defs:.def=.sym) $(M2Imps:.mod=.o)
Auxiliaries	= $(M2Defs) $(M2Imps) $(M2Mods)
Libraries	= $(M2Libraries)  $(AsmLibraries)
Programs	= $(M2Programs) $(ScriptPrograms) $(CPrograms)
Targets		= $(Libraries) $(Programs)

# And now the rules.
.SUFFIXES: .mpp .dpp .def .mod .sym .lnk .lod .exe .bat

.dpp.def:
	$(M2pp) $(M2ppFlags) $*.dpp $*.def

# The Sun compiler leaves behind a bad symbol file when the 
# def mod fails to compile.  The "if ..." instruction, 
# removes the sym file if the make fails.
.def.sym:
	if $(M2comp) $(M2compFlags) $(M2Path) $*.def; then \
	    : ; else rm -f $*.sym; exit 1; fi

.mpp.mod:
	$(M2pp) $(M2ppFlags) $*.mpp $*.mod

.mod.o:
	$(M2comp) $(M2compFlags) $(M2Path) $*.mod

.mod:
	$(M2comp) $(M2compFlags) $(M2Path) $*.mod
	$(M2link) $(M2linkFlags) $(M2Path) -e $@ -o $@ $(M2linkLibraries)

.o:
	$(M2link) $(M2linkFlags) $(M2Path) -e $@ -o $@ $(M2linkLibraries)

#
# These are needed, because the sources live in a separate 
# directory from the targets *or* for targets which don't
# have suffixes (e.g. programs).
#
StandardM2pp	= $(M2pp) $(M2ppFlags) $? $@
StandardM2link	= $(M2link) $(M2linkFlags) $(M2Path) -e $@ -o $@ $(M2linkLibraries)
StandardScript	= cat $? > $@; chmod +x $@
