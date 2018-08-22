        /********************************************************/
        /*                                                      */
        /*            String matching with wildcards            */
        /*                                                      */
        /*     A version of module WildCard translated to C,    */
        /*     and modified to be compatible with the rules     */
        /*              used by Alessandro Cantatore            */
        /*                                                      */
        /*  Programmer:         P. Moylan                       */
        /*  Started:            11 May 2003                     */
        /*  Last edited:        11 May 2003                     */
        /*  Status:             OK                              */
        /*                                                      */
        /********************************************************/


#define BOOL unsigned char

extern BOOL WildMatch (char* input, char* template);

    /* Returns TRUE if template and input are equal, with the extra     */
    /* rules:                                                           */
    /*   1. Character case is not significant.                          */
    /*   2. A '?' in template matches any single character except '.'.  */
    /*   3. A '*' in template matches any string of zero or more        */
    /*      characters.                                                 */
