#
# LOCAL "environment" for makes in the current directory.  See the 
# global file "rules.mak" and the sample "environ.mak" in the 
# same location.

# Global description of the environment.  Make sure you have the
# appropriate number of relative directories.
include ../../bin/rules.mak

# Include automatically generated "M2Defs", "M2Imps", and "M2Mods".
include m2source.mak

# Set this variable to the list of directories (.e.g. -Mdir1 -Mdir2 ...)
# which contain library files you will need.  The default path is to use
# the non-lwp installed library.  If you want somthing different, uncomment
# the lines below and fill them in.  Note Sun's library directory
# is automatically included (whether you want it or not).
#  M2Path	=

# Uncomment the following line, if this directory uses lightweight processes
# or you may want to set your own flags. 
# LocalM2ppFlags 	= -DTasks

# You will need to set the AsmLib variable to one of the following,
# if you don't want the installed non-lwp version.  The AsmLib is needed
# by the library.
# AsmLib = $(TestAsmLib)
# AsmLib = $(TestLwpAsmLib)
# AsmLib = $(InstalledLwpAsmLib)

# If you have any *other* link libraries of interest, add them to:
# LocalM2linkLibraries 	=

# If you don't want to install anything when "install" is called,
# uncomment the following line:
# Install 	   	=

# If you are installing library files, uncomment this line.  And select
# the approriate destination directory (Lib or Lwp) and uncomment the line.
# InstallFiles	= $(LibInstallFiles)
# InstallDir	= $(LibInstallDir)
# InstallDir	= $(LwpInstallDir)
