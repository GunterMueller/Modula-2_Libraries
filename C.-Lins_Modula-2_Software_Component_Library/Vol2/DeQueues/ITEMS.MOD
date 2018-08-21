IMPLEMENTATION MODULE Items;
(*====================================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Structure - Generic Item Types

    REVISION HISTORY
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    IMPLEMENTATION DESIGN NOTES
    * The actual type for the opaque Item must be defined.

    Proprietary Notices
      Copyright (C) 1989 Charles A. Lins. All rights reserved.
====================================================================*)

FROM SYSTEM IMPORT
    (*--Type*) ADDRESS;

TYPE  Item = ADDRESS;

END Items.
