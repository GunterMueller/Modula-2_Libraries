        /********************************************************/
        /*                                                      */
        /*            String matching with wildcards            */
        /*                                                      */
        /*     A version of module WildCard modified to be      */
        /*          compatible with the rules used by           */
        /*                 Alessandro Cantatore                 */
        /*                                                      */
        /*   For easier comparison, I've translated it to C.    */
        /*   This version is supposed to be the non-recursive   */
        /*   version, but I don't yet have that working.        */
        /*                                                      */
        /*  Programmer:         P. Moylan                       */
        /*  Started:            11 May 2003                     */
        /*  Last edited:        18 May 2003                     */
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
                     CARD m1, CARD k1,
                     char* template,
                     CARD m2, CARD k2)

    /* Returns TRUE if input[m1..k] matches template[m2..k2].  If the   */
    /* template is empty (m2 > k2), we have a match by definition.      */

/* Alters: m1, m2 */
/* Does not alter: input, template, k1, k2 */

    {
        while (1)
            {
            /* From the left, input[m1] and template[m2] are the        */
            /* first characters we haven't yet tested for a match.      */

            if (m2 > k2)

                /* No more template left; match by definition.  */

                return TRUE;

            else if (template[m2] == '*')

                break;

            else if (m1 > k1)

                /* Input exhausted, first unmatched template char */
                /* is not wild, so we have a definite mismatch.   */

                return FALSE;

            else if (template[m2] == '?')

                /* Special care needed here, because Alessandro has */
                /* modified the rules about matching '.'            */

                {
                if (input[m1] == '.')
                    return FALSE;
                }

            else if (CAP(input[m1]) != CAP(template[m2]))

                return FALSE;

            ++m1;  ++m2;
            }

        /* If we reach here, template[m2] = '*'. */

        do
            {++m2;}
        while ((m2 <= k2) && (template[m2] == '*'));

        return SubstringMatch (input, m1, k1, template, m2, k2);
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


    /* Alters: j */
    /* Alters: j2, m1, m2 */
    /* Does not alter: j1, input, template, k1, k2 */

    {
    CARD j = j1;
    CARD m1, m2;
    BOOL result = FALSE;
    BOOL StarFound = FALSE;

    while(1)
        {
        /* The original version of this code had the call        */
        /*  result = HeadMatch (input, j, k1, template, j2, k2); */
        /* at this point, but I've now inserted the entire       */
        /* body of HeadMatch below.                              */

        /*******  Start HeadMatch operation *********/
        m1 = j;
        m2 = j2;

        /* This section of code returns TRUE if input[m1..k] matches        */
        /* template[m2..k2] for some k <= k2.  If we have a mismatch, we    */
        /* drop out of this inner loop, and the outer loop will bring us    */
        /* back with a new starting value for m1.                           */

        while (1)
            {
            /* From the left, input[m1] and template[m2] are the        */
            /* first characters we haven't yet tested for a match.      */

            if (m2 > k2)

                /* No more template left; match by definition.  */

                return TRUE;

            else if (template[m2] == '*')

                StarFound = TRUE;
                break;

            else if (m1 > k1)

                /* Input exhausted, first unmatched template char */
                /* is not wild, so we have a definite mismatch.   */

                break;

            else if (template[m2] == '?')

                /* Special care needed here, because Alessandro has */
                /* modified the rules about matching '.'            */

                {
                if (input[m1] == '.')
                    break;
                }

            else if (CAP(input[m1]) != CAP(template[m2]))

                break;

            ++m1;  ++m2;
            }

        if (StarFound)
            {
            /* If we reach here, template[m2] = '*'. */

            do
                {++m2;}
            while ((m2 <= k2) && (template[m2] == '*'));

            result = SubstringMatch (input, m1, k1, template, m2, k2);
            }

        /*******  End HeadMatch operation *********/

        if (result)
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