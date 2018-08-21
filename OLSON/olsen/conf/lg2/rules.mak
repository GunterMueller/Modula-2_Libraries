#
# GLOBAL make configuration for compiling/linking/etc.:
#      Logitech 2.X, IBM-PC, MS-DOS
# This file is included in a file called "environ.mak" in the directory
# to be made.  
#
# If you want GLOBAL changes, make them HERE.  If you want LOCAL changes,
# add a declaration of the particular macro on any line AFTER the line
# that contains the "include" statement.

# The base directory for all the files (must be absolute).
Root		= \yaml

# Where Logitech lives (note: drive specification is necessary for M2LOD)
LogiM2LOD	= c:\m2lod
LogiM2SYM	= c:\m2lib\sym
LogiM2LNK	= c:\m2lib\lnk
LogiM2REF	= c:\m2lib\ref
LogiM2MOD	= c:\m2lib\mod
LogiM2MAP	= c:\m2lib\map

# Directory for executables (make, etc.) and installed home for this file.
# The $(BinDir) should also be in your PATH environment variable.
# The names "bin" and "rules.mak" are hardwired in all the makefiles, 
# so don't change them unless you want to edit things.
BinDir		= $(Root)\bin
MakeRules	= $(BinDir)\rules.mak

# Where to install the main library files and lightweight process library.
# NOTE: the sub-dirs def, mod, lnk, ref, sym, & map will be used.  If you
# don't like this scheme, then you will need to change how "mk_inst.bat"
# works.  See below and see the script.
LibInstallDir	= $(Root)\lib
LwpInstallDir 	= $(Root)\lwp.lib


# The following paths are defaults for once the library is installed.
# Set the "LocalM2XXX" values in your "environ.mak" with the exact
# list you need.  LogiM2XXX are set above, so don't modify the M2XXX =
# declarations.
LocalM2SYM	= $(LibInstallDir)\sym
LocalM2LNK	= $(LibInstallDir)\lnk
LocalM2REF 	= $(LibInstallDir)\ref
LocalM2MAP	=
LocalM2MOD	= $(LibInstallDir)\mod

# Variables to be exported in the environment.  You should have a line
# in your "environ.mak" of the form:
# 	export M2PPVARS M2SYM M2LNK M2MAP
M2SYM		= $(LocalM2SYM);$(LogiM2SYM)
M2LNK		= $(LocalM2LNK);$(LogiM2LNK)
M2REF		= $(LocalM2REF);$(LogiM2REF)
M2MAP		= $(LocalM2MAP);$(LogiM2MAP)
M2MOD		= $(LocalM2MOD);$(LogiM2MOD)

# Modula-2 Preprocessor Environment
#
# If you want to set a variable to FALSE (undefined), set M2ppFlags
# in your "environ.mak" file to be something like:
#	M2ppFlags=/U=Assert/U=Debug
# To set a variable to TRUE, change the "U" in the above example to a "D".
# /D and /U flags are evaluated from left to right and always override
# the value of M2PPVARS.
# WARNING: the dos command interpreter does something funny with "=", 
#	   so we can't put m2pp in a batch file like the other commands.
M2pp		= m2pp
# Please don't break up this line into two parts.
M2PPVARS        = LittleEndian;LogitechM2;LogitechM2V2;MsDosOS;IbmPcArch;Assert;Debug;M2V2


# Programs and flags
#
# Descriptions of programs we use.  If you change any of these, you
# had better change the rules as well.  Note that $(BinDir) must be
# in your path for make to work.
# 
M2comp		= mk_m2c
# e.g  /R+/S+/NOV -- mk_m2c sets /NOQ/NOA/B
M2compFlags 	=

M2link		= mk_m2l
# e.g. /M+/L+ -- mk_m2l sets /A-
M2linkFlags	=

M2exe		= mk_m2exe
# /w=N sets the stack size.
M2exeFlags	=
# Default M2exeRTS is set.   If it doesn't normally live in \m2lod, change
# this value.   If you want to make an .exe in an "lwp" directory, set
# M2exeRTS = $(M2exeRTS.lwp) on a line AFTER you include this file. 
M2exeRTS	= $(LogiM2LOD)\l2erts.l2e
M2exeRTS.lwp	= $(BinDir)\l2erts.lwp


# In order to make life simpler for those people that have to debug
# their programs, we have defined the following script files which
# get automatically generated if "environ.mak" or this file changes.
DebuggingAides  = $(M2env) $(M2pmd) $(M2rtd)
M2env		= m2env.bat
M2pmd		= m2pmd.bat
M2rtd		= m2rtd.bat
# This is how we execute the debugger commands.
StandardM2pmd	= m2 $(LogiM2LOD)\pmd %1
StandardM2rtd	= m2 $(LogiM2LOD)\rtd/L %1


# How to make the files "m2depend.mak" and "m2source.mak".  Note that
# the "m2depend" program always searches all files in the current directory.
# It also looks in the environment variables M2SYM and M2LNK.
Depend 		= mk_m2dep /v2
# To make sure the files are preprocessed before m2depend executes.
DependSources	= $(M2Defs) $(M2Imps) $(M2Mods)

# Where to gather the binaries.  The default is just to install ".exe"
# files.  However, library directories install "lnk", "ref", and "sym".
# If you want to do source level debugging of the library files *after*
# the library is installed, add "mod" to the LibInstallFiles and all
# the library make installs will change.  Note set InstallDir to be 
# $(LibInstallDir) or $(LwpInstallDir) when installing library files.
Install		= mk_inst $(InstallDir) $(InstallFiles)
InstallDir	= $(BinDir)
# Important: if "exe" is in the InstallFiles list, then it must come first.
InstallFiles	= exe
LibInstallFiles = lnk ref sym

# Cleaning is a dangerous thing (My mother has never believed me).
# If you want to clean, you will probably have to make a batch file
# which does it for you, e.g. mk_clean which deletes *.lnk, *.mod,
# *.ref, *.sym, *.def, memory.pmd, and *.exe.  Note that we delete
# the preprocessed sources as well.
Clean		= @echo No "clean" defined.  See $(BinDir)\rules.mak

#
# This is probably the last thing you need to configure.  By default
# programs are made into their ".exe" versions as opposed to ".lod".
# If you want to type "m2 program", then just set "exe" in the next
# line to be "lod".  Note that you can always say: "make program.lod"
# if you need the ".lod" versions.
M2Programs	= $(M2Mods:.mod=.exe) 

M2Libraries	= $(M2Defs:.def=.sym) $(M2Imps:.mod=.lnk)
Auxiliaries	= $(M2Mods) $(M2Imps) $(M2Defs) $(M2Mods:.mod=.ref) \
		  $(M2Imps:.mod=.ref)
Libraries	= $(M2Libraries) 
Programs	= $(M2Programs) $(ScriptPrograms) $(DebuggingAides)
Targets		= $(Libraries) $(Programs)

# Rules needed for our world.
.SUFFIXES: .mpp .dpp .def .mod .sym .lnk .lod .exe .bat

.dpp.def:
	$(M2pp) $*.dpp $*.def $(M2ppFlags)

.def.sym:
	$(M2comp) $*.def $(M2compFlags)

.mpp.mod:
	$(M2pp) $*.mpp $*.mod $(M2ppFlags)

.mod.lnk:
	$(M2comp) $*.mod $(M2compFlags)

.lnk.lod:
	$(M2link) $* $(M2linkFlags)

.lnk.exe:
	$(M2link) $* $(M2linkFlags)
	$(M2exe) $* $(M2exeRTS) $(M2exeFlags)

.lod.exe:
	$(M2exe) $* $(M2exeRTS) $(M2exeFlags)

# The following rules screw up make for some reason so they have not
# been included: .mod.exe and .mod.lod.

#
# These are needed, because the sources live in a separate 
# directory from the targets.  The implicit rules don't
# work in this case.
#
StandardM2pp	= $(M2pp) $(M2ppFlags) $? $@
StandardScript	= copy $? $@
