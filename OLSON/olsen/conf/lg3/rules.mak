#
# GLOBAL make configuration for compiling/linking/etc.:
#      Logitech 3.0, IBM-PC, MS-DOS
# This file is included in a file called "environ.mak" in the directory
# to be made.  
#
# If you want GLOBAL changes, make them HERE.  If you want LOCAL changes,
# add a declaration of the particular macro on any line AFTER the line
# that contains the "include" statement.

# The base directory for all the files (must be absolute).
Root		= \yaml

# Where Logitech lives.
LogiM2SYM	= c:\m2lib\sym
LogiM2OBJ	= c:\m2lib\obj
LogiM2LIB	= c:\m2lib\lib
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
LocalM2OBJ	= $(LibInstallDir)\obj
LocalM2LIB	=
LocalM2REF 	= $(LibInstallDir)\ref
LocalM2MAP	=
LocalM2MOD	= $(LibInstallDir)\mod

# Variables to be exported in the environment.  You should have a line
# in your "environ.mak" of the form:
# 	export M2PPVARS M2SYM M2OBJ M2LIB M2MAP
M2SYM		= $(LocalM2SYM);$(LogiM2SYM)
M2OBJ		= $(LocalM2OBJ);$(LogiM2OBJ)
M2LIB		= $(LocalM2LIB);$(LogiM2LIB)
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
# Note: LogitechM2V2 is not defined which means we default to 
#       LogitechM2V3 & M2V3.  This keeps the path shorter.
M2PPVARS        = LittleEndian;LogitechM2;MsDosOS;IbmPcArch;Assert;Debug


# Programs and flags
#
# Descriptions of programs we use.  If you change any of these, you
# had better change the rules as well.  Note that $(BinDir) must be
# in your path for make to work.
# 
M2comp		= mk_m2c
# e.g  /O+/R+/S+/NOV -- mk_m2c sets /NOQ/NOA/B
M2compFlags 	=

M2link		= mk_m2l
# e.g. /NOM/S=4000 -- mk_m2l sets /B
M2linkFlags	= /O

# In order to make life simpler for those people that have to debug
# their programs, we have defined the following script files which
# get automatically generated if "environ.mak" or this file changes.
DebuggingAides  = $(M2env) $(M2pmd) $(M2rtd)
M2env		= m2env.bat
M2pmd		= m2pmd.bat
M2rtd		= m2rtd.bat
# This is how we execute the debugger commands.
StandardM2pmd	= pmd %1
StandardM2rtd	= rtd %1


# How to make the files "m2depend.mak" and "m2source.mak".  Note that
# the "m2depend" program always searches all files in the current directory.
# It also looks in the environment variables M2SYM and M2OBJ.
Depend 		= mk_m2dep
# To make sure the files are preprocessed before m2depend executes.
DependSources	= $(M2Defs) $(M2Imps) $(M2Mods)

# Where to gather the binaries.  The default is just to install ".exe"
# files.  However, library directories install "obj", "ref", and "sym".
# If you want to do source level debugging of the library files *after*
# the library is installed, add "mod" to the LibInstallFiles and all
# the library make installs will change.  Note set InstallDir to be 
# $(LibInstallDir) or $(LwpInstallDir) when installing library files.
Install		= mk_inst $(InstallDir) $(InstallFiles)
InstallDir	= $(BinDir)
# Important: If you list an "exe" entry, it must come first (see mk_inst)
InstallFiles	= exe
LibInstallFiles = obj ref sym

# Cleaning is a dangerous thing (My mother has never believed me).
# If you want to clean, you will probably have to make a batch file
# which does it for you, e.g. mk_clean which deletes *.obj, *.mod,
# *.ref, *.sym, *.def, memory.pmd, and *.exe.  Note that we delete
# the preprocessed sources as well.
Clean		= @echo No "clean" defined.  See $(BinDir)\rules.mak

# You probably don't need to configure anything past this point.
M2Programs	= $(M2Mods:.mod=.exe) 
M2Libraries	= $(M2Defs:.def=.sym) $(M2Imps:.mod=.obj)
Auxiliaries	= $(M2Mods) $(M2Imps) $(M2Defs) $(M2Mods:.mod=.ref) \
		  $(M2Imps:.mod=.ref)
Libraries	= $(M2Libraries) 
Programs	= $(M2Programs) $(ScriptPrograms) $(DebuggingAides)
Targets		= $(Libraries) $(Programs)

# Rules needed for our world.
.SUFFIXES: .mpp .dpp .def .mod .sym .obj .lod .exe .bat

.dpp.def:
	$(M2pp) $*.dpp $*.def $(M2ppFlags)

.def.sym:
	$(M2comp) $*.def $(M2compFlags)

.mpp.mod:
	$(M2pp) $*.mpp $*.mod $(M2ppFlags)

.mod.obj:
	$(M2comp) $*.mod $(M2compFlags)

.obj.exe:
	$(M2link) $*.obj $(M2linkFlags)

#
# These are needed, because the sources live in a separate 
# directory from the targets.  The implicit rules don't
# work in this case.
#
StandardM2pp	= $(M2pp) $(M2ppFlags) $? $@
StandardScript	= copy $? $@
