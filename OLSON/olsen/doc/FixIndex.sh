#!/bin/sh
File=$1

# MAKE PRETTY
sed -e 'a\
indexentrybreak' \
	-e 's/code\{\[\}/code{leftssquarebracket}/g' \
	-e 's/code\{\]\}/code{rightssquarebracket}/g' \
	-e 's/tt w\}/tt zzwwzz}/g' \
	-e 's/tt f\}/tt zzffzz}/g' \
	-e 's/tt j\}/tt zzjjzz}/g' \
	-e 's/tt m\}/tt zzmmzz}/g' \
	-e 's/=/xxeeqquuaallss/g' \
	-e 's/0/xxzzeerroo/g' \
	-e 's/1/xxoonnee/g' \
	-e 's/2/xxttwwoo/g' \
	-e 's/3/xxtthhrreeee/g' \
	-e 's/4/xxffoouurr/g' \
	-e 's/5/xxffiivvee/g' \
	-e 's/6/xxssiixx/g' \
	-e 's/7/xxsseevveenn/g' \
	-e 's/8/xxeeiigghhtt/g' \
	-e 's/9/xxnniinnee/g' \
     $File \
     | tr '[A-Z]' '[a-z]' \
     | delatex \
     | sed \
	-e 's/zzwwzz/w/g' \
	-e 's/zzffzz/f/g' \
	-e 's/zzjjzz/j/g' \
	-e 's/zzmmzz/m/g' \
	-e 's/leftssquarebracket/[/g' \
	-e 's/rightssquarebracket/]/g' \
	-e 's/xxeeqquuaallss/=/g' \
	-e 's/xxzzeerroo/0/g' \
	-e 's/xxoonnee/1/g' \
	-e 's/xxttwwoo/2/g' \
	-e 's/xxtthhrreeee/3/g' \
	-e 's/xxffoouurr/4/g' \
	-e 's/xxffiivvee/5/g' \
	-e 's/xxssiixx/6/g' \
	-e 's/xxsseevveenn/7/g' \
	-e 's/xxeeiigghhtt/8/g' \
	-e 's/xxnniinnee/9/g' \
     | awk 'BEGIN { xx = 1; hold=0} 
	    {  if ( hold ) {
		 if ( $1 == "indexentrybreak" ) printf( " %03d", holdval );
		 else printf( " %s", holdval );
		 hold=0;
		}
		if ( $1 == "indexentrybreak" ) {
				printf( "\n" );
				xx=1;
			} else if ( !xx ) {
			    if ( $1 ~ /^[0-9][0-9]*$/ ) {
				hold=1
				holdval=$1
			    }
			    else
				printf( " %s", $1 );
			} else {
			    if ( $1 ~ /^[0-9][0-9]*$/ )
				printf( "!help %s", $1 );
			    else
				printf( "%s", $1 );
			    xx=0
			}}' > $File-1
# SORT
pr -t -s\! -w200 -m $File-1 $File | sort -u > $File-2
# CLEAN
sed -e 's/^.*!//'  $File-2
