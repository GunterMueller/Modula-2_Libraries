        /********************************************************/
        /*                                                      */
        /*            String matching with wildcards            */
        /*                                                      */
        /*     A version of module WildCard modified to be      */
        /*          compatible with the rules used by           */
        /*                 Alessandro Cantatore                 */
        /*                                                      */
        /*   For easier comparison, I've translated it to C.    */
        /*                                                      */
        /*  Programmer:         P. Moylan                       */
        /*  Started:            11 May 2003                     */
        /*  Last edited:        12 May 2003                     */
        /*  Status:             Working                         */
        /*                                                      */
        /*    Remark: there are probably a few things that can  */
        /*    be done to tune this code for micro-efficiency,   */
        /*    but because of my well-known hatred of C I shall  */
        /*    leave that detail to those who are more familiar  */
        /*    with the language.                                */
        /*                                                      */
        /********************************************************/

#include "wildcc.h"
#define CARD unsigned int
#define FALSE 0
#define TRUE 1

/************************************************************************/
/*                         UTILITY FUNCTIONS                            */
/************************************************************************/

unsigned int Length (char* str)

    /* Length of a character string, assumed to be null-terminated. */

    {
    unsigned int k=0;
    while (*str++) k++;
    return k;
    }

/************************************************************************/

char CAP(char ch)

    /* Converts to upper case. */

    {
    if ((ch < 'a') || (ch > 'z')) return ch;
    return (ch - 'a' + 'A');
    };

/************************************************************************/
/*                         SUBSTRING MATCHING                           */
/************************************************************************/

BOOL SubstringMatch (char* input,
                     CARD j1, CARD k1,
                     char* template,
                     CARD j2, CARD k2);        /* forward */

/************************************************************************/

BOOL HeadMatch (char* input,
                     CARD j1, CARD k1,
                     char* template,
                     CARD j2, CARD k2)

    /* Returns TRUE if input[j1..k] matches template[j2..k2].  If the   */
    /* template is empty (j2 > k2), we have a match by definition.      */

    {
        while (1)
            {
            /* From the left, input[j1] and template[j2] are the        */
            /* first characters we haven't yet tested for a match.      */

            if (j2 > k2)

                /* No more template left; match by definition.  */

                return TRUE;

            else if (template[j2] == '*')

                break;

            else if (j1 > k1)

                /* Input exhausted, first unmatched template char */
                /* is not wild, so we have a definite mismatch.   */

                return FALSE;

            else if (template[j2] == '?')

                /* Special care needed here, because Alessandro has */
                /* modified the rules about matching '.'            */

                {
                if (input[j1] == '.')
                    return FALSE;
                }

            else if (CAP(input[j1]) != CAP(template[j2]))

                return FALSE;

            ++j1;  ++j2;
            }

        /* If we reach here, template[j2] = '*'. */

        do
            {++j2;}
        while ((j2 <= k2) && (template[j2] == '*'));

        return SubstringMatch (input, j1, k1, template, j2, k2);
        }

/************************************************************************/

BOOL SubstringMatch (char* input,
                     CARD j1, CARD k1,
                     char* template,
                     CARD j2, CARD k2)

    /* Returns TRUE if any contiguous substring of input[j1..k1]        */
    /* matches template[j2..k2].  If the template is empty (j2 > k2),   */
    /* we have a match by definition.                                   */
    /* On entry we are guaranteed that j1 <= k1.                        */

    {
        CARD j = j1;

        while(1)
            {
            if (HeadMatch (input, j, k1, template, j2, k2))
                return TRUE;
            else if (j >= k1)
                return FALSE;
            ++j;
            }
    }

/************************************************************************/
/*                  THE EXTERNALLY CALLABLE PROCEDURES                  */
/************************************************************************/

extern BOOL WildMatch (char* input, char* template)

    /* Returns TRUE if template and input are equal, with the extra     */
    /* rules:                                                           */
    /*   1. Character case is not significant.                          */
    /*   2. A '?' in template matches any single character except '.'.  */
    /*   3. A '*' in template matches any string of zero or more        */
    /*      characters.                                                 */

    {
        CARD j1, k1, j2, k2;

        j1 = 0;  k1 = Length (input);
        j2 = 0;  k2 = Length (template);

        if (k1 == 0)
            {

            /* Empty input; the only thing that can match is an */
            /* empty or all-star template.                      */

            while (1)
                {
                if (j2 == k2) return TRUE;
                else if (template[j2] == '*') ++j2;
                else return FALSE;
                }
            }
        else if (k2 == 0)

            /* Empty template, non-empty input. */

            return FALSE;

        --k1;  --k2;

        /* Having disposed of the "empty" cases, we are now comparing   */
        /* input[j1..k1] with template[j2..k2].                         */

        while (1)
            {
            /* From the left, input[j1] and template[j2] are the        */
            /* first characters we have not yet tested for a match.     */

            if (j2 > k2)

                /* No more template left; match iff we've also  */
                /* exhausted the input.                         */

                return j1 > k1;

            else if (template[j2] == '*')

                break;

            else if (j1 > k1)

                /* Input exhausted, first unmatched template char */
                /* is not '*', so we have a definite mismatch.    */

                return FALSE;

            else if (template[j2] == '?')

                /* Special care needed here, because Alessandro has */
                /* modified the rules about matching '.'            */

                {
                if (input[j1] == '.')
                    return FALSE;
                }

            else if (CAP(input[j1]) != CAP(template[j2]))

                return FALSE;

            ++j1;  ++j2;
            }

        /* If we reach here, template[j2] = '*'. */

        while (1)
            {

            /* From the right, input[k1] and template[k2] are the first */
            /* characters we have not yet checked for a match.          */

            if (template[k2] == '*')

                break;

            else if (k1 < j1)

                /* Input exhausted, last unmatched template char */
                /* is not '*', so we have a definite mismatch.   */

                return FALSE;

            else if (template[k2] == '?')

                /* Special care needed here, because Alessandro has */
                /* modified the rules about matching '.'            */

                {
                if (input[k1] == '.')
                    return FALSE;
                }

            else if (CAP(input[k1]) != CAP(template[k2]))

                return FALSE;

            /* Special case: if k1=0 then we have to record that we've  */
            /* exhausted the input without decrementing k1.  The same   */
            /* problem does not arise for k2 because at this point we   */
            /* know that we'll hit a '*' before exhausting the template.*/

            if (k1 == 0) ++j1; else --k1;
            --k2;
            }

        /* If we reach here, k2 >= j2, template[j2] = '*', and          */
        /* template[k2] = '*'.  If we have several '*'s in a row, here  */
        /* is where we reduce them down.                                */

        do {++j2;} while ((j2 <= k2) && (template[j2] == '*'));
        while ((j2 <= k2) && (template[k2] == '*')) --k2;

        /* If the original template contained only a single '*', or     */
        /* several (redundant) '*' characters not separated by a        */
        /* non-wild character, then by the time we reach this point     */
        /* we will have stripped down the template to the empty string, */
        /* i.e. j2 > k2.  SubstringMatch will always return TRUE in     */
        /* this case.  Since it is such a common case, we get a slight  */
        /* but useful gain in speed by checking for it before even      */
        /* called SubstringMatch.                                       */

        if (j2 > k2) return TRUE;
        return SubstringMatch (input, j1, k1, template, j2, k2);
        };

/************************************************************************/
