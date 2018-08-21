/*$Copyright 1988 by Olsen & Associates (O&A), Zurich, Switzerland.

                       All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies, and
that both that copyright notice and this permission notice appear in
supporting documentation, and that all modifications of this software
or its documentation not made by O&A or its agents are accompanied
by a prominent notice stating who made the modifications and the date
of the modifications.

O&A DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE AND ITS
DOCUMENTATION, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS.  IN NO EVENT SHALL O&A BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES, ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE OR ITS DOCUMENTATION.
******************/

#include <fcntl.h>
#include <sys/file.h>
#include <stdio.h>

extern void fprintf();
extern void perror();
extern int open();
extern int fcntl();
extern void close();

# define USAGE "Usage: %s [ file-to-reset ]\ndefault: /dev/tty\n"
main ( argc, argv ) 
    int argc;
    char *argv[];
  {
    char *name = "/dev/tty";
    int i;
    int status = 0;

    if ( argc > 2 )
	fprintf( stderr, USAGE, argv[ 0 ] );
    else if ( argc == 2 )
	name = argv[ 1 ];

    if ( ( i = open( name, 0 ) ) < 0 ) {
	perror( name );
	fprintf( stderr, USAGE, argv[ 0 ] );
	exit( 1 );
    }

    if ( fcntl( i, F_SETFL, FASYNC + FNDELAY ) < 0 ) {
	perror( "fcntl" );
	status = 1;
    }
    close( i );
    exit( status );
  } /* main */
