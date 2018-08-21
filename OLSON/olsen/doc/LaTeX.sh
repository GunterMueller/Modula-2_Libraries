#!/bin/csh
# latex script that knows whether to run bibtex and/or rerun latex.
if ( $#argv != 1 ) then
	echo "Format: LaTeX sourcefile"
	exit 1
endif

# Program names--because some people alias rm to rm -i, we avoid just "rm"
# Other programs are invoked by name so a user can substitute his own copies.
set RM=/bin/rm
set CP=/bin/cp

set BASENAME=$1:r
set LOGNAME=$BASENAME.log
# Zero out index files to see if one is being written.
$CP /dev/null $BASENAME.idx
# Supply an empty index file to avoid no file errors.
$CP /dev/null $BASENAME.index

latex $1
if ( { grep -s "Citation.*undefined" $LOGNAME } ) then
    echo "***Unresolved citations founds, need to run bibtex."
    bibtex $BASENAME
    echo "***Rerunning LaTeX to get bibtex citations."
    latex $1
endif
if ( -z $BASENAME.idx ) then
    $RM -f $BASENAME.idx
    $RM -f $BASENAME.index
else
    echo "***Post-processing index entries."
    sh MakeIndexFile.sh $BASENAME.idx > $BASENAME.idx1
    sh FixIndex.sh $BASENAME.idx1 > $BASENAME.index
endif
if ( { grep -s "Label(s) may have changed" $LOGNAME } ) then
    echo "***Need one more LaTeX pass to resolve forward references."
    latex $1
else if ( -e $BASENAME.index ) then 
    echo "***Need one more LaTeX pass to format the index."
    latex $1
endif
if ( { grep -s "Citation.*undefined" $LOGNAME } ) then
    echo "***There are still undefined citations--check spelling and .bib files"
endif
