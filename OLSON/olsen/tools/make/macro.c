/*
 *	Macro control for make
 *
 * MODIFICATIONS:
 *   13.9.88 - Robert J. Nagler (RJN)
 *	- "export" always puts into the environ.
 *      - Added "replace" and "findrepl" in "doexp" to handle macro
 * 	  expansions of the form "${XXX:.def=.mod}"
 *      - Tested on MS-DOS (Turbo-C) and Sun Unix only.
 */


#include	<ctype.h>
#include "h.h"


struct macro *		macrohead;


struct macro *
getmp(name)
char *			name;
{
	register struct macro *	rp;

	for (rp = macrohead; rp; rp = rp->m_next)
		if (strcmp(name, rp->m_name) == 0)
			return rp;
	return (struct macro *)0;
}


char *
getmacro(name)
char *			name;
{
	struct macro *		mp;

	if ((mp = getmp(name)) != 0)
		return mp->m_val;
	else
		return "";
}


struct macro *
setmacro(name, val)
char *			name;
char *			val;
{
	register struct macro *	rp;
	register char *		cp;


			/*  Replace macro definition if it exists  */
	for (rp = macrohead; rp; rp = rp->m_next)
		if (strcmp(name, rp->m_name) == 0)
		{
			free(rp->m_val);	/*  Free space from old  */
			break;
		}

	if (!rp)		/*  If not defined, allocate space for new  */
	{
		if ((rp = (struct macro *)malloc(sizeof (struct macro)))
					 == (struct macro *)0)
			fatal("No memory for macro");

		rp->m_next = macrohead;
		macrohead = rp;
		rp->m_flag = FALSE;

		if ((cp = malloc(strlen(name)+1)) == (char *)0)
			fatal("No memory for macro");
		strcpy(cp, name);
		rp->m_name = cp;
	}

	if ((cp = malloc(strlen(val)+1)) == (char *)0)
		fatal("No memory for macro");
	strcpy(cp, val);		/*  Copy in new value  */
	rp->m_val = cp;

	return rp;
}

/*
 * 13.9.88 - RJN
 * There are two parts to handling "${XXX:oldsuffix=newsuffix}".
 * "findrepl" parses the string and "replace" does the replacing.
 * They communicate through dynamically allocated memory, because
 * we want to allow recursion.
 */
char *					/* from + num chars parsed */
findrepl(buf, from, right, oldsuf, newsuf)
register char *		buf;
register char *		from;		/* pointing at '=' */
char			right;		/* which character ends search? */
char **			oldsuf;		/* left side of colon */
char **			newsuf;		/* right side of colon */
{
	int 			len;

	*oldsuf = buf;
	*newsuf = (char *)0;
	while(*++from && *from != right)
	{
		if (isspace(*from))
		{
			/* No spaces allowed in the suffixes, keep goin' */
			*oldsuf = (char *)0;
			continue;
		}
		if (*from != '=')
		{
			*buf++ = *from;
			continue;
		}
		*buf++ = '\0';
		if (*newsuf)
			/* Too many '=' characters, but got to keep goin' */
			*oldsuf = (char *)0;
		else
			*newsuf = buf;

	}

	/* If there was a syntax error, just return */
	if (!*oldsuf || **oldsuf == '\0' || *from == '\0')
	{
		oldsuf = 0;
		return from;
	}

	/* {XXX:.foo=} is ok, but newsuf must be set! */
	if (*newsuf == (char *) 0) 
		*(*newsuf = from) = '\0';
	
	/* terminate new suffix now, so we can use buf as temporary */
	*buf = '\0';
	
	/* Allocate enough memory for both old and new, then copy in */
	len = strlen(*oldsuf);
	if ((buf = malloc(strlen(*newsuf)+len+2)) == (char *)0)
		fatal("No memory for macro expansion");
	*oldsuf = strcpy(buf, *oldsuf);
	*newsuf = strcpy(buf+len+1, *newsuf);

	return from;

} /* findrepl */

/*
 * Go from start to end stopping at white space and searching backwards
 * for oldsuf.  When oldsuf matches exactly, we replace it with newsf
 */
void
replace(start, end, len, oldsuf, newsuf, buf)
char *			start;		/* first char */
char **			end;		/* after last char */
int *			len;		/* mustn't get to 0! */
char *			oldsuf;		/* what to replace */
char *			newsuf;		/* with what */
char *			buf;		/* temporary of newsuf is bigger */
{
	register char *		sp = start;
	register char *		bp;
	register char *		ep = *end;
	register char *		cp;
	char *			tmp;
	int			startinglen = *len;
	int			diff;
	char			first = *oldsuf;

	/* Diff tells if we need to use the temporary buffer */
	diff = strlen(newsuf) - strlen(oldsuf);

	*ep = ' ';	/* makes last suffix easy to detect */
	for (bp = diff <= 0 ? sp : buf; sp < ep; sp++)
	{
		if (*sp != first)
		{
			*bp++ = *sp;
			continue;
		}

		/* See if the rest matches */
		tmp = sp++;
		cp = oldsuf + 1;
		for ( ; *cp && sp < ep && *sp == *cp; cp++, sp++)
			;
		/* must be perfect and end in a space */
		if (*cp != '\0' || !isspace(*sp))
		{
			sp = tmp;
			*bp++ = *sp;
			continue;
		}
		*len -= diff;
		if (*len < 0)	
			error("Expanded line too long");
		for (cp = newsuf; *cp; )
			*bp++ = *cp++;	/* don't copy the null */
		/* Copy the space at the end of the suffix */
		*bp++ = *sp;
	}

	free(oldsuf);		/* newsuf was allocated in same block */
	*end += startinglen - *len;		/* adjust as needed */

	if (diff <= 0)
		return;		/* don't need to copy */


	/* We used "buf" so we must copy back into "start" */
	for (bp = buf, sp = start, ep = *end; sp < ep; *sp++ = *bp++)
		;

} /* replace */



/*
 *	Do the dirty work for expand
 */
void
doexp(to, from, len, buf)
char **			to;
char *			from;
int *			len;
char *			buf;
{
	register char *		rp;
	register char *		p;
	register char *		q;
	register struct macro *	mp;
	char *			oldsuf = (char *)0;
	char *			newsuf;
	char			right;	/* ')' or '}' */


	rp = from;
	p = *to;
	while (*rp)
	{
		if (*rp != '$')
		{
			*p++ = *rp++;
			(*len)--;
		}
		else
		{
			q = buf;
			/* 13.9.88 - RJN - Changed to look for "XXX:" */
			if (*++rp == '{' || *rp == '(')
			{
				right = *rp == '{' ? '}' : ')';
				/*  Look for syntax "XXX:" or end of macro */
				while (*++rp && *rp != right && *rp != ':')
					*q++ = *rp;
				if (*rp == ':')
					/* 
					 * Parse "XXX:old=new" syntax 
					 * Dynamically allocates old/newsuf.
					 */
					rp = findrepl(q+1, rp, right,
						      &oldsuf, &newsuf);
			}
			else if (!*rp)
			{
				*p++ = '$';
				break;
			}
			else
				*q++ = *rp;
			*q = '\0';
			if (*rp)
				rp++;
			if (!(mp = getmp(buf))) 
				mp = setmacro(buf, "");
			if (mp->m_flag)
				fatal("Infinitely recursive macro %s", mp->m_name);
			mp->m_flag = TRUE;
			*to = p;
			doexp(to, mp->m_val, len, buf);
			/* 13.9.88 - RJN - Added suffix replacement check */
			if (oldsuf)
			{
				/* Perform action of ":old=new" */
				/* NOTE: old/new are dynamically allocated
					 by findrepl and freed by replace */
				replace(p, to, len, oldsuf, newsuf, buf);
				oldsuf = (char *)0;	/* always! */
			}
			p = *to;
			mp->m_flag = FALSE;
		}
		if (*len <= 0)
			error("Expanded line too long");
	}
	*p = '\0';
	*to = p;
}


/*
 *	Expand any macros in str.
 */
/* 13.9.88 - RJN - share buffers between "export" and "expand" */
static char 	buf1[LZ];
static char 	buf2[LZ];

void
expand(str)
char *		str;
{
	char *			p = str;
	int			len = LZ-1;

	strcpy(buf1, str);
	doexp(&p, buf1, &len, buf2);
}

/* 
 * 13.9.88 - RJN
 * Adds macros to the environment whether they exist in the
 * environment previously or not.
 */
void
export(name)
char *		name;
{
	char *			value;
	char *			env;
	int			namelen;
	char			*to = buf1;
	int			len = LZ - 1;

	/* Empty macros are put as well */
	value = getmacro(name);
	doexp(&to, value, &len, buf2);

	namelen=strlen(name);
	/* allocate room for "name=value" */
	if ((env = malloc(strlen(buf1)+namelen+2)) == (char *)0)
		fatal("No memory for environment expansion");
	(void) strcpy(env, name);
	(void) strcat(env+namelen, "=");
	(void) strcat(env+namelen+1, buf1);
	putenv(env);
} /* export */

/* 
 * 13.9.88 - RJN
 * Sets the named macro to the value of the environment variable by
 * the same name.  We only add them if the macro exists.
 */
void
import(name)
char *		name;
{
	char *			value;
	extern char *		getenv();

	if ((value = getenv(name)) != (char *)0)
		(void) setmacro(name, value);
} /* import */
