      ******************************************************************
      *                                                                *
      * Licensed Materials - Property of IBM                           *
      *                                                                *
      * IMSCBLJA SAMPLE COPYBOOK                                       *
      *                                                                *
      * (c) Copyright IBM Corp. 2020 All Rights Reserved               *
      *                                                                *
      * US Government Users Restricted Rights - Use, duplication or    *
      * disclosure restricted by GSA ADP Schedule Contract with IBM    *
      * Corp                                                           *
      *                                                                *
      ******************************************************************
      * DATA AREA FOR TERMINAL INPUT
       01 INPUT-MSG.
          05  IN-LL               PIC S9(3) COMP.
          05  IN-ZZ               PIC S9(3) COMP.
          05  IN-TRANCODE         PIC X(10).
          05  IN-JAVA-ARG         PIC X(20).
      * DATA AREA FOR TERMINAL OUTPUT
       01 OUTPUT-MSG.
          05  OUT-LL              PIC S9(3) COMP VALUE +0.
          05  OUT-ZZ              PIC S9(3) COMP VALUE +0.
          05  OUT-JAVA-ARG        PIC X(20).
          05  OUT-MESSAGE         PIC X(60).

