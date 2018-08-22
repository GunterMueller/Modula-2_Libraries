           README file for the PMOS multitasking software
           ==============================================

Welcome to PMOS.  PMOS is a set of modules, mostly written in Modula-2,
to support multitasking.  PMOS was designed primarily with real-time
applications in mind.  It is not an operating system in the conventional
sense; rather, it is a collection of modules which you can import
into your own programs, and which in particular allow you to write
multi-threaded programs.

Even if you don't need multi-threaded programs, you might find some
of the general-purpose modules useful.

Subject to certain conditions which are described below, this
software is free.

The notes in this file show you how to install PMOS and how to get
started.  More detailed information may be found in a report called
the "PMOS Reference Manual", which can probably be found at the
same site where you obtained this software.  (If not, contact the
author - see contact information below.)  The current edition of that
report is slightly out of date (since PMOS is updated more often than
new documentation is produced) but discrepancies are easily resolved
by looking at the definition modules.  Since complete source files are
included with this distribution, experienced programmers may well find
that the additional documentation is redundant.


DISCLAIMER

Like all free software, this package is supplied "as is", and no
warranties are made or implied as to its fitness for any particular
purpose.  The author does not accept responsibility for any damage,
including consequential damage, caused by the use of this software,
except to the extent to which this disclaimer may be overridden by law.


LICENCE INFORMATION

The ACCOUNTS directory of this package contains a shareware product.
Licence information for that product is given in a separate README
file in the ACCOUNTS directory.  You may, if you wish, delete the
contents of the ACCOUNTS directory and its subdirectories without
affecting the functionality of the PMOS package.

All of the remaining components of the PMOS package may freely be
used subject to the following conditions.  If you do not accept the
conditions, do not use the package.  Your use of any part of PMOS
implies your acceptance of these conditions.

1. Neither PMOS nor any derived product may be sold or distributed
   for a fee, or used to enhance the value of a product for which a
   fee is charged.  The author is willing to negotiate agreements with
   individuals or companies for commercial use of PMOS, but in the
   absence of such an agreement PMOS is for non-profit use only.

   [Note to shareware authors and others involved in low-profit
   ventures: under certain circumstances I am willing to grant a
   fee-free licence for the commercial use of PMOS.  As a general
   rule I will charge a fee only for substantial commercial use or
   when you are in competition with a company in which I hold an
   interest; and I will not charge a fee for small-scale use or for
   educational use.  Nevertheless the present licence covers only
   non-profit use of PMOS, and any other use must be separately
   negotiated.]

   The present licence permits the use of PMOS for evaluation purposes
   or for the building of prototypes which are not for sale.

2. You may give copies of PMOS to other people, but only if this
   README file is included.

3. You may modify PMOS, but only if you agree that the altered
   version continues to be subject to the conditions given here.
   The name of the original author(s), and any copyright notices
   contained in these files, may not be deleted in the course of
   making such alterations.


PREREQUISITES

Hardware: IBM-PC compatible computer with 80186 processor or better.
    The software can be made to run on an 8086 or 8088 processor,
    but in that case you would have to modify or replace the
    assembly language components.  (The required modifications are
    minor - contact me if you run into trouble.)

    PMOS contains a number of device drivers which might have to be
    rewritten to suit your own hardware configuration.  The software
    seems to work on most "standard" configurations, but testing
    has not been extensive enough to check all possibilities.

Operating system: This software has been developed using Microsoft's
    MS-DOS, versions 5 and 6.  Since PMOS "takes over" the machine once a
    program is running, there are few operating system dependencies;
    but a small number of DOS calls (mainly during module initialisation)
    remain.

    Programs using PMOS will run successfully in an OS/2 DOS session
    provided that they don't depend too critically on precise timing
    and that they don't try to violate the OS/2 protection constraints.
    The PMOS disk drivers won't work under OS/2, but an alternative
    FileSys module is provided to get around that problem.  See the
    file DOC\STATUS.DOC for other details of restrictions under OS/2.

    PMOS has been tested only superficially under Microsoft Windows.

Compiler: The software is currently compatible with TopSpeed Modula-2,
    either version 1.17 or version 3.0 or later.  (A small subset of
    modules has been ported to the somewhat different FST dialect of
    Modula-2, but the FST porting is a long way from being complete.)
    Although reasonable attempts have been made to keep the code
    portable, you will probably find that you have to do some editing
    to make this software compile using other compilers.  In particular,
    the assembly language components will certainly need to be replaced.
    (Replacements for those components are included with this
    distribution, but you might prefer to write your own to take
    advantage of the features of your own compiler.)


INSTALLATION

The installation procedure assumes that you are installing
PMOS in the directory C:\PMOS.  If this is not the case, make
appropriate modifications to the instructions below.

If you have read this far, then you have no doubt already reached
the point of creating a \PMOS directory and unzipping the files
into that directory.  The next step is to edit the configuration
file and run the configuration preprocessor.  (You may skip this
step if you are willing to accept the default options.  The
preprocessor can be run again at any time to change the options.)

1. Using any text editor, edit the file PP.CFG, to specify the
   compiler, mouse driver, etc., that you are going to be using.
   The comments in that file explain what the valid options are.
      NOTES:
         A. There are potential incompatibilities between PMOS and
            Microsoft mouse drivers.  If you have a Microsoft mouse
            driver running then there is a strong risk that the
            INT33 option won't work.  It is safer to specify either
            the "MS" or the "NoMouse" options.
         B. The ChainTimerInt option is experimental at present, and
            doesn't work on all configurations.  (It was supposed to
            solve the problem caused by the Microsoft mouse driver, but
            in some situations it makes things worse.)  You can try it out
            if you wish, but (depending on what TSRs are running) it
            could cause your programs to crash.

2. Run the program PP.EXE.  This will implement the options specified
   in your PP.CFG.

(Remark: the file SOURCES\GENERAL\CONFIGUR.DEF also contains some
customisation options.  However it's best not to change those until
after you have a better understanding of PMOS.)

You now have a customised version of PMOS.  Read on, however, because
what you can do with it depends on what compiler you have.


DE-INSTALLATION

PMOS does not alter your AUTOEXEC.BAT or your CONFIG.SYS, it
doesn't tamper with your PATH, and it does not scatter its files
through your system directories.  If you decide that you don't
want PMOS, all you have to do is to delete directory C:\PMOS and
all of its contents, including subdirectories.

This is just one of the things that make PMOS unusual in the PC world.


WHAT'S IN THE DIRECTORIES

After you have installed PMOS, you will have the following directories.

      PMOS
          Files needed for installation.

      PMOS\ASM
          Source files for some assembly language modules - see later
          for what to do about the assembly language modules.

      PMOS\ACCOUNTS
          A separate package which is described in the README file in
          that directory.  This directory can be deleted if you don't
          want the separate package.

      PMOS\CONTROL
          A project file to build a control systems laboratory program.
          May need modification to fit your own needs.  Can be ignored
          by most users.

      PMOS\CONTROL\SOURCES
          Source files for the control laboratory program.

      PMOS\DEMO
          Project files for some demonstration programs.

      PMOS\DOC
          Some documentation files.  You should read through these
          at some stage.

      PMOS\LIB
          This directory is to hold the library files PMOSx.LIB.  You
          don't need it if you don't have TopSpeed version 3 or if you
          plan to work directly with object files.

      PMOS\OBJECT
          This directory is for the main PMOS object files.  (It will
          be almost empty until after you've done your first "make".)

      PMOS\OBJECT\TS1
          Object files needed only by users of TopSpeed version 1.

      PMOS\OBJECT\TS3
          Object files needed only by users of TopSpeed version 3
          who don't have the TopSpeed assembler.  This directory has
          several subdirectories, one for each supported memory model.

      PMOS\SOURCES\DEMO
          Sources for some demonstration programs.  Useful as
          programming examples, but otherwise not essential.

      PMOS\SOURCES\GENERAL
          The main directory for PMOS source files.  This is where
          you will find almost all of the PMOS source code.

      PMOS\SOURCES\SPECIAL
          Alternative versions for some of the PMOS source files.
          Useful if you want to port PMOS to another compiler, or if
          you don't have an assembler, but can be ignored otherwise.

      PMOS\SOURCES\TESTS
          Source files for a few test programs.  Not needed, but you
          might find them useful as programming examples.  (Don't expect
          these programs to do anything interesting; they are basically
          "acceptance tests" for pre-release testing of PMOS.)

      PMOS\SOURCES\UTIL
          Source files for some utility programs.

      PMOS\TESTS
          Project files for making some test programs.

      PMOS\UTIL
          Project files for making some utility programs.

Because PMOS is updated from time to time, you might find some other
subdirectories.  Typically these contain things such as temporary
files and new modules which are not yet working; I suggest that
you ignore them.

It is strongly recommended that you not put any of your own files
into any of these directories.  Mixing up your own work with the PMOS
files is potentially very confusing.  The preferred method is to make
a new top-level directory for your own work, and to use the PMOS
directory and its subdirectories exclusively for the PMOS components.


REPLACING THE ASSEMBLY LANGUAGE MODULES

There are five assembly language modules - InnerKernel, LowLevel,
RandCard, Play3S, and PlayBuffer - included in the SOURCES\GENERAL
directory.  Because of the peculiar syntax required by the
TopSpeed assembler, these are not portable to other compilers.
In fact, they are not even portable to TopSpeed version 1, because
of a difference in the way parameters are passed.

If you have TopSpeed version 3, including the TopSpeed assembler, then
you can use these modules precisely as they are.

If you have some other compiler, or if you have a TopSpeed 3 compiler
but not the assembler, then you need an alternative approach.  This
distribution provides two solutions:

 (a) For some of the modules in question, pre-compiled object modules
     are included in the PMOS package.  These can be found in the
     OBJECT\TS1 directory (for TopSpeed version 1) and in subdirectories
     of OBJECT\TS3 (for TopSpeed version 3 - make sure you choose the
     subdirectory appropriate to the memory model you are using).
     I've put them there rather than in the main OBJECT directory to
     reduce the risk that you'll accidentally overwrite them.

 (b) Alternative versions of some of these modules, written entirely
     in Modula-2, are provided in the SOURCES\SPECIAL directory.
     These are less efficient but more portable versions of the
     assembly language modules.

(There are a couple of modules for which neither solution (a) nor
solution (b) is available.  Luckily the modules affected are
inessential components of PMOS.)

Solution (a) is superior where it is applicable.  Solution (b) is
recommended for those modules for which the .OBJ file is not yet
available, and when porting to compilers which are not yet supported
by PMOS.

The source files for solution (a), in .ASM format, can be found in
the ASM directory.  There is a very good chance that these can also
be used with other compilers, either directly or with minor changes.

Remark: the existence of multiple versions for some modules creates
the risk that you will pick up the wrong version.  Make sure that
your search paths are set correctly.  If you plan to stick with the
same compiler for the foreseeable future, you might find it worthwhile
to reorganise the directories and to eliminate the versions that you
are never going to use.


USING PMOS WITH TOPSPEED VERSION 1

You will find that some directories already contain M2.RED files.
These can be used directly with TopSpeed version 1.17, and the
search paths are already set up correctly.

Don't try to use the *.A files, even if you have the TopSpeed assembler.
The parameter-passing conventions are different between TopSpeed
versions 1 and 3.

The files TS.RED, *.PR, and *.PI are for use with TopSpeed version 3.
You can delete these if you don't plan to upgrade your compiler.


USING PMOS WITH TOPSPEED VERSION 3

Unlike earlier releases of PMOS, the present release no longer confines
you to using the MThread memory model.  You can use any memory model
from Small to XLarge.

It is strongly recommended that you create object libraries for each
memory model.  A batch file MAKELIB.BAT to do this is in the LIB
directory.  It's a slow operation, but will save you a lot of time
later in the "make" step.  If, however, you choose not to do this,
you should remove the line "#pragma link(pmos%M%.lib)" which occurs
in a number of project files.

     NOTE 1: Before running the batch file, modify the projects that
             it invokes to get the right settings for your processor
             and coprocessor type.
     NOTE 2: Running this batch file can take a considerable amount
             of time.  I recommend that you leave this operation until
             you're taking a lunch break.
     NOTE 3: The end result of MAKELIB, as it is supplied, is to
             create a full set of library files (in the LIB directory)
             and object files (in subdirectories of the OBJECT directory)
             for each supported memory model.  Although this gives
             maximum flexibility, it does use a lot of disk space.
             It is not absolutely necessary to have either the libraries
             or the object files - since you have the source files in
             any case - so if you are short of disk space you might well
             decide to do without the object libraries or to do without
             the object files, or both.  Alternatively, you might choose
             to create libraries only for one or two memory models.
             You should find it easy to modify the batch files and/or
             the project files to tailor the system to your own needs.

The system as distributed assumes that you have the TopSpeed assembler,
i.e. that you can use the *.A files directly.  If this is not so, then
you will need to
 (a) modify the .PI file in the OBJECT directory, to remove the last
     few lines which compile the *.A files; and
 (b) modify the TS.RED in various directories to include the appropriate
     subdirectory of OBJECT\TS3 in the search path for *.OBJ files.


USING PMOS WITH THE FST COMPILER

PMOS has not yet been completely updated to work with the FST compiler.
(And, unfortunately, the job will take some time, because there are
many places in the PMOS sources which use Modula-2 language features not
supported by FST.)  A small subset of the modules has been converted,
but sadly it's not yet feasible to use PMOS with FST unless you're
prepared to do a significant amount of editing.  I'm hoping that this
problem will be solved by the next release.


USING PMOS WITH OTHER COMPILERS

Although there is now, in effect, a language standard for Modula-2,
most existing compilers don't conform to the standard.  As a result,
you will undoubtedly find numerous constructs in the PMOS sources
which aren't acceptable to your compiler, i.e. you're going to have
to do a bit of editing to get the software to compile.  Most of the
changes are likely to be straightforward, but watch out for several
problem areas:
 (a) If your compiler does not support the LONGCARD data type (for
     32-bit cardinals), you are likely to need major changes to a
     small number of modules.  Probably the most painless way to
     implement these changes would be to write a new module which
     exports a new type LONGCARD and operations such as addition.
 (b) If your compiler uses a 32-bit representation for type CARDINAL,
     then you have a lot of work ahead of you.  Because so much of
     PMOS (device drivers, etc.) is concerned with low-level hardware
     interactions, there are undoubtedly many modules which make
     assumptions about things like the number of bits in an I/O port,
     the number of screen pixels per memory word, etc.
 (c) Type conversions (e.g. converting a CARDINAL value to LONGCARD)
     are a potential headache, because every compiler seems to have
     different rules.  You'll probably find a few places where I've
     used a construct which looks like a type cast; this is peculiar
     to the TopSpeed dialect, therefore I've tried to avoid it, but no
     doubt some examples have still slipped through.  You'll find even
     more places where I've conformed to the standard but your compiler
     doesn't.
Luckily, all such examples are likely to show up as compilation errors,
and it will usually be obvious what to do.


CONTACT INFORMATION

The author of PMOS is

            Peter Moylan
            Department of Electrical and Computer Engineering
            The University of Newcastle, NSW 2308
            Australia.

            Phone: +61 49 21 6023      (time zone GMT+10)
            Fax:   +61 49 60 1712
            e-mail:   peter@ee.newcastle.edu.au
                      peter@tesla.newcastle.edu.au
                      eepjm@cc.newcastle.edu.au

The preferred method of contacting me is via e-mail; this will probably
bring a faster response than with paper mail.

If you find PMOS useful, or if you don't use it but still want to
know about future releases, please register yourself as a user by sending
me a note.  (Include both e-mail and postal addresses, especially if
you have a hard-to-reach e-mail address.)  It won't cost you anything,
but it will let me contact you with news of corrections and new versions.
Note: the PMOS mailing list is a confidential document, and will not
be released to any other person or organisation.

If you find any faults in PMOS, or would like to suggest desirable
new features, please let me know.  I am also interested in hearing
about ports of PMOS to other compilers.

You have permission to upload the PMOS package to an archive site
(subject, of course, to any conditions which might be imposed by
the owner of the archive site).  Please let me know if you do so,
so that I can keep track of where updates should be sent.
