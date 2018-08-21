#
# LOCAL "environment" for makes in the current directory.  See the 
# global file "rules.mak" and the sample "environ.mak" in the 
# same location.

# Global description of the environment.
include ..\..\bin\rules.mak

# Include automatically generated "M2Defs", "M2Imps", and "M2Mods".
include m2source.mak

# Set the M2Path macro to the list of directories (.e.g. dir1;dir2;dir3)
# which contain library files you will need.  The default path is to use
# the non-lwp installed library.  If you want something different, uncomment
# the lines below and fill them in.  Note that the Logitech directory is 
# included in the "rules.mak", so it need not be included here.  You may 
# need to have special trailing directories on the individual lines if 
# using installed softare e.g. LocalM2SYM = $(LwpInstallDir)\sym
# LocalM2SYM	=
# LocalM2LNK	= 
# LocalM2REF 	=
# LocalM2MAP	=
# LocalM2MOD	=

# Uncomment the next two lines, if this directory uses lightweight processes.
# M2ppFlags	= /D=Tasks
# M2exeRTS	= $(M2exeRTS.lwp)

# If you don't want to install anything when "install" is called,
# uncomment the following line:
# Install 	=

# If you are installing library files, uncomment this line.  And select
# the approriate destination directory (Lib or Lwp) and uncomment that line.
# InstallFiles	= $(LibInstallFiles)
# InstallDir 	= $(LibInstallDir)
# InstallDir 	= $(LwpInstallDir)

# Put things in the PC environment to save space on the command line.
# Note that we don't need to export M2MOD and M2REF since they are
# only used by the debugger.
# THIS COMMAND SHOULD BE THE LAST ONE IN THIS FILE.
export M2PPVARS M2SYM M2LNK M2MAP
