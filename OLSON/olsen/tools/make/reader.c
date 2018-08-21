/*
 *	Read in makefile
 *
 * MODIFICATIONS:
 *   13.9.88 - Robert J. Nagler (RJN)
 *	- Added include feature.
 *	- Modified "error" to print the file name if in an include.
 *      - Tested on MS-DOS (Turbo-C) and Sun Unix only.
 */


#include <stdio.h>
#include	<ctype.h>
#include "h.h"


int			lineno;

/* RJN - 12.9.88
 * Used by include to check include file recursion and by error to
 * print nicer messages.
 */
struct incl_file {
	char *			inf_name;	/* name of current include */
	struct incl_file *	inf_prev;	/* previous include */
} * infhead = 0;


/*
 *	Syntax error handler.  Print message, with line number, and exits.
 */
void
error(msg, a1, a2, a3)
char *			msg;
{
	fprintf(stderr, "%s: ", infhead == (struct incl_file *)0 
						? myname : infhead->inf_name);
	fprintf(stderr, msg, a1, a2, a3);
	if (lineno)
		fprintf(stderr, " near line %d", lineno);
	fputc('\n', stderr);
	exit(1);
}


/*
 *	Read a line into the supplied string of length LZ.  Remove
 *	comments, ignore blank lines. Deal with	quoted (\) #, and
 *	quoted newlines.  If EOF return TRUE.
 */
bool
getline(str, fd)
char *		str;
FILE *		fd;
{
	register char *		p;
	char *			q;
	int			pos = 0;


	for (;;)
	{
		if (fgets(str+pos, LZ-pos, fd) == (char *)0)
			return TRUE;		/*  EOF  */

		lineno++;

		if ((p = index(str+pos, '\n')) == (char *)0)
			error("Line too long");

		if (p[-1] == '\\')
		{
			p[-1] = '\n';
			pos = p - str;
			continue;
		}

		p = str;
		while (((q = index(p, '#')) != (char *)0) &&
		    (p != q) && (q[-1] == '\\'))
		{
			char	*a;

			a = q - 1;	/*  Del \ chr; move rest back  */
			p = q;
			while ((*a++ = *q++) != 0)
				;
		}
		if (q != (char *)0)
		{
			q[0] = '\n';
			q[1] = '\0';
		}

		p = str;
		while (isspace(*p))	/*  Checking for blank  */
			p++;

		if (*p != '\0')
			return FALSE;
		pos = 0;
	}
}


/*
 *	Get a word from the current line, surounded by white space.
 *	return a pointer to it. String returned has no white spaces
 *	in it.
 */
char *
gettok(ptr)
char	**ptr;
{
	register char *		p;


	while (isspace(**ptr))	/*  Skip spaces  */
		(*ptr)++;

	if (**ptr == '\0')	/*  Nothing after spaces  */
		return NULL;

	p = *ptr;		/*  word starts here  */

	while ((**ptr != '\0') && (!isspace(**ptr)))
		(*ptr)++;	/*  Find end of word  */

	*(*ptr)++ = '\0';	/*  Terminate it  */

	return(p);
}

/* RJN - 12.9.88 */
/*
 * Opens an include file and calls "input" with the new file.
 * Resets filename and lineno to the new include file so that
 * error message make sense (I wish Sun's make did this!).
 */
void
include(name)
char *		name;
{
	int 			prevlineno = lineno;
	FILE *			fd;
	char *			newname;
	struct incl_file      	inf, *p;

	for(p = infhead; p; p = p->inf_prev)
		if (strcmp(p->inf_name, name) == 0)
			error("Include file recursion");

	if ((fd = fopen(name, "r")) == (FILE *)0)
		error("Could not open include file \"%s\"", name);
	lineno = 0;
	if ((newname = malloc(strlen(name)+1)) == (char *)0)
		fatal("No memory for include");
	(void) strcpy(newname,name);

	/* Save for recursion check -- safety first! */
	inf.inf_name = newname;
	inf.inf_prev = infhead;
	infhead = &inf;

	input(fd);	/* recurse */

	/* remove list of includes */
	infhead = infhead->inf_prev;

	/* restore the state */
	lineno = prevlineno;
	free(newname);

} /* include */
