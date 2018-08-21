#!/bin/sh
# Copyright 1988 by Olsen & Associates (O&A), Zurich, Switzerland.
# 
#                        All Rights Reserved
# 
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted,
# provided that the above copyright notice appear in all copies, and
# that both that copyright notice and this permission notice appear in
# supporting documentation, and that all modifications of this software
# or its documentation not made by O&A or its agents are accompanied
# by a prominent notice stating who made the modifications and the date
# of the modifications.
# 
# O&A DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE AND ITS
# DOCUMENTATION, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
# FITNESS.  IN NO EVENT SHALL O&A BE LIABLE FOR ANY SPECIAL, INDIRECT OR
# CONSEQUENTIAL DAMAGES, ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
# USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
# OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE OR ITS DOCUMENTATION.
##########################
# 
#
# For the files in the list, runs a comment checker.   Uses the
# the suffix to determine the type of comments:
#	.h, .c - are "C" style.
#	.mpp, .dpp, .mod, .def - are "Modula-2" style.
#
if [ $# -eq 0 ]; then
    ProgramName=`basename $0`
    cat << EOF
Usage: $ProgramName file1 .. fileN
       Checks comments in Modula-2 or C files.  Detects the language
       by the file suffixes: ".h", ".c", ".mpp", ".dpp", ".mod", & ".def"
EOF
    exit 1	
fi
for f in $@; do
    case $f in
	*.c|*.h)
	    OpenComment='\/\*'
	    CloseComment='\*\/'
	    IsNestable=0
	    ;;
	*.dpp|*.mpp|*.mod|*.def)
	    OpenComment='(\*'
	    CloseComment='\*)'
	    IsNestable=1
	    ;;
	*)
	    echo "$f: unknown suffix type, can't check comments."
	    continue
	    ;;
    esac
    expand $f | \
        sed -e "s/ OPEN /FOO/g" -e "s/ CLOSE /FOO/g" \
        -e "s/$OpenComment/ OPEN /g" -e "s/$CloseComment/ CLOSE /g" $f | \
        awk '
    BEGIN { level=0; opens[ 0 ] = 0; isNestable='"$IsNestable"' }
    / OPEN / || / CLOSE / {
	for ( i = 1; i <= NF; i++ ) {
	    if ( $i == "OPEN" ) {
		if ( level == 0 || isNestable )
		    opens[ level++ ] = NR;
		else {
		    line=opens[ level - 1 ];
		    printf( "\"%s\", line %d: ", "'$f'", NR );
		    print "open within comment (previous open on line " line ")"

		}
	    } else if ( $i == "CLOSE" ) {
		if ( level == 0 ) 
		    print "\"'$f'\", line " NR ": unmatched close"
		else
		    level--;
	    }
	}
    }
    END {
	while ( level != 0 ) {
	    print "\"'$f'\", line " opens[ --level ] ": unmatched open"
	}
    }'
done
