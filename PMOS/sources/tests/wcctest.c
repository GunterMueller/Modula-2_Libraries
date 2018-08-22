        /********************************************************/
        /*                                                      */
        /*              Test of the WildC2 module               */
        /*                                                      */
        /*  Programmer:         P. Moylan                       */
        /*  Started:            11 May 2003                     */
        /*  Last edited:        17 May 2003                     */
        /*  Status:             OK                              */
        /*                                                      */
        /********************************************************/

#include "wildccnr.h"

/************************************************************************/
/*                          TEST CODE                                   */
/************************************************************************/

void TestPair (char* input, char* template)

    {
        printf (input);
        if (WildMatch (input, template))
            printf (" matches ");
        else
            printf (" does not match ");
        printf ("template %s\n", template);
        };

/************************************************************************/

void RunTheTest()

    {
        printf ("Testing wildcard matches\n\n");
        TestPair ("a", "*a");
        TestPair ("a", "A");
        TestPair ("a", "b*");
        TestPair ("a", "*");
        TestPair ("ab", "*b*");
        TestPair ("a", "**");
        TestPair ("a", "**b");
        TestPair ("xyabuvdefabcmmdefmmm", "**abc*def*");
        TestPair ("xyz.uvw.abc.zip", "*.ZIP");
    };

/************************************************************************/
/*                            MAIN PROGRAM                              */
/************************************************************************/

void main()
    {
    RunTheTest();
    }
