       CBL dll,thread
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IMSCBLJC RECURSIVE.
       AUTHOR.        YVES TOLOD.
       INSTALLATION.  ESYSMVS1
      ******************************************************************
      * (c) Copyright IBM Corp. 2021 All Rights Reserved               *
      *                                                                *
      * Licensed under the Apache License, Version 2.0 which you can   *
      * read at https://www.apache.org/licenses/LICENSE-2.0            *
      *                                                                *
      * Sample IMS COBOL program (MPP) that calls a Java class. This   *
      * sample will do multiple Java calls, the calls are              *
      *                                                                *
      *  (a) Calling a static void method in Java                      *
      *  (b) Calling a static method that returns a String output from *
      *      a REST API call in Java                                   *
      *                                                                *
      * Call (b) will return the output from Java program. The output  *
      * contains the result of the API call using REST client in Java. *
      * The sample program will be using mixed case for the Java       *
      * related calls.                                                 *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.
      ***********************
       CONFIGURATION SECTION.
      ************************
       REPOSITORY.
      *************

           Class ZUtil          is "com.ibm.jzos.ZUtil"
           Class JavaRestClient is "demo.ibm.sample.JavaRestClient"
           Class JavaException  is "java.lang.Exception"
           Class JavaObject     is "java.lang.Object"
           Class JavaString     is "java.lang.String"
           Class JavaClass      is "java.lang.Class".

       DATA DIVISION.
      ****************
       WORKING-STORAGE SECTION.
      **************************
       01 jStrClaimType             object reference JavaString.
       01 jStrClaimAmount           object reference JavaString.
       01 jStrOutput                object reference JavaString.
       01 ex                        object reference JavaException.
       01 stringClaimType           pic N(10) usage national.
       01 stringClaimAmount         pic N(10) usage national.
       01 strLenOutput              pic S9(3) comp value 0.
       01 strLenClaimType           pic S9(3) comp value 0.
       01 strLenClaimAmout          pic S9(3) comp value 0.
       01 strOutPtr                 usage pointer.
       01 jboolean1                 pic X.
          88 jboolean1-true         value X'01' through X'FF'.
          88 jboolean1-false        value X'00'.
      ******************************************************************
      * INCLUDE THE COPYBOOK FOR REQUEST AND RESPONSE DATA STRUCTURE
      * OF THE SAMPLE PROGRAM.
      ******************************************************************
       COPY IMSC2J2.
      ******************************************************************
      * DECLARE THE WORKING STORAGE VARIABLES SPECIFIC TO IMS
      ******************************************************************
       77 DLI-GET-UNIQUE            PIC X(4) VALUE 'GU  '.
       77 DLI-GET-NEXT              PIC X(4) VALUE 'GN  '.
       77 DLI-INSERT                PIC X(4) VALUE 'ISRT'.
       77 DLI-MESSAGE-EXISTS        PIC X(2) VALUE 'CF'.
       77 DLI-END-SEGMENTS          PIC X(2) VALUE 'QD'.
       77 DLI-END-MESSAGES          PIC X(2) VALUE 'QC'.
      ******************************************************************
      * DECLARE THE WORKING STORAGE VARIABLES USED IN THIS PROGRAM
      ******************************************************************
       01 WS-LOG-MESSAGE            PIC X(80).
       01 WS-CCSID                  PIC 9(5).
       01 WS-RETURN-STR             PIC X(60).
       01 WS-TEMP-TS.
          05 WS-TEMP-DATE-TIME.
             10 WS-TEMP-DATE.
                15 WS-TEMP-YEAR     PIC 9(4).
                15 WS-TEMP-MONTH    PIC 9(2).
                15 WS-TEMP-DAY      PIC 9(2).
             10 WS-TEMP-TIME.
                15 WS-TEMP-HOUR     PIC 9(2).
                15 WS-TEMP-MIN      PIC 9(2).
                15 WS-TEMP-SEC      PIC 9(2).
                15 WS-TEMP-MS       PIC 9(2).
             10 WS-DIFF-GMT         PIC S9(4).
       01 WS-FORMATTED-TS.
          05 WS-FORMATTED-DATE-TIME.
             10 WS-FORMATTED-YEAR   PIC 9(4).
             10 FILLER              PIC X VALUE '-'.
             10 WS-FORMATTED-MONTH  PIC 9(2).
             10 FILLER              PIC X VALUE '-'.
             10 WS-FORMATTED-DAY    PIC 9(2).
             10 FILLER              PIC X VALUE ' '.
             10 WS-FORMATTED-HOUR   PIC 9(2).
             10 FILLER              PIC X VALUE ':'.
             10 WS-FORMATTED-MIN    PIC 9(2).
             10 FILLER              PIC X VALUE ':'.
             10 WS-FORMATTED-SEC    PIC 9(2).
             10 FILLER              PIC X VALUE ':'.
             10 WS-FORMATTED-MS     PIC 9(2).
      *
       LINKAGE SECTION.
      ******************
       01 IO-PCB-MASK.
          05 IO-PCB-LTERM           PIC X(8).
          05 FILLER                 PIC XX.
          05 IO-PCB-STATUS-CODE     PIC XX.
          05 IO-PCB-DATE            PIC S9(7) COMP-3.
          05 IO-PCB-TIME            PIC S9(6)V9 COMP-3.
          05 IO-PCB-MSG-SEG-NUMBER  PIC S9(5) COMP.
          05 IO-PCB-MOD-NAME        PIC X(8).
          05 IO-PCB-USER-ID         PIC X(8).

       01 stringRsp                 pic N(50) usage national.

       COPY "JNI" SUPPRESS.
      ******************************************************************
      * MAIN PROGRAM
      ******************************************************************
       PROCEDURE DIVISION USING IO-PCB-MASK.
      *
       DO-MAIN SECTION.
      *
           INITIALIZE INPUT-MSG
           INITIALIZE OUTPUT-MSG

           MOVE 'PROGRAM CALLED' TO WS-LOG-MESSAGE
           PERFORM LOG-MESSAGE

           PERFORM GET-INPUT-MESSAGE
           PERFORM UNTIL IO-PCB-STATUS-CODE     = DLI-END-MESSAGES
                   OR    IO-PCB-STATUS-CODE NOT = SPACES
             PERFORM CALL-JAVA-METHOD
             PERFORM SET-OUTPUT-MESSAGE
             PERFORM GET-INPUT-MESSAGE
           END-PERFORM

           GOBACK
           .
      ******************************************************************
      * ROUTINE TO CALL THE JAVA METHOD
      ******************************************************************
       CALL-JAVA-METHOD.
      *
           Set address of JNIEnv to JNIEnvPtr
           Set address of JNINativeInterface to JNIENV
      *
      * (a) Calling a Java static void method
      *
      * The static JZOS method redirectStandardStreams will redirect
      * Java stdout/stderr to DD:STDOUT and DD:STDERR, which may be
      * spool files or data sets
      *
           MOVE 'Invoking ZUtil Java class' TO WS-LOG-MESSAGE
           PERFORM LOG-MESSAGE
           Invoke ZUtil "redirectStandardStreams"
           PERFORM CHECK-ERROR
      *
      * (b) Calling a Java static method that returns a string
      *
      * The static JavaRestClient method getClaimResult will call
      * a REST API to process a health claims request.  It requires
      * two arguments, claim type (MEDICAL, DRUG, DENTAL) and the
      * claim amount. The input arguments are strings.
      *
           MOVE 'Invoking JavaRestClient.getClaimResult'
             TO WS-LOG-MESSAGE
           PERFORM LOG-MESSAGE

           PERFORM BUILD-JAVA-INPUT-ARGS

           Invoke JavaRestClient "getClaimResult"
               using by value jStrClaimType jStrClaimAmount
               returning jStrOutput

           If jStrOutput not = null then
               MOVE 'METHOD getClaimResult SUCCESSFUL' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
           Else
               MOVE 'METHOD getClaimResult FAILED' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
               Stop run
           End-if

           PERFORM GET-JAVA-RESPONSE-STR
      *
      * Process the output that will be returned back to IMS
      *
           MOVE IN-CLAIMTYPE TO OUT-CLAIMTYPE
           MOVE IN-CLAIMAMT TO OUT-CLAIMAMT
           MOVE WS-RETURN-STR TO OUT-MESSAGE
           MOVE LENGTH OF OUTPUT-MSG TO OUT-LL
           MOVE 0 TO OUT-ZZ
           .
      ******************************************************************
      * ROUTINE TO GET INPUT MESSAGE FROM QUEUE
      ******************************************************************
       GET-INPUT-MESSAGE.
      *
           CALL 'CBLTDLI' USING DLI-GET-UNIQUE IO-PCB-MASK
                                INPUT-MSG
           IF IO-PCB-STATUS-CODE NOT = SPACES AND
              IO-PCB-STATUS-CODE NOT = DLI-END-MESSAGES
             DISPLAY 'GU FAILED WITH IO-PCB-STATUS-CODE('
                     IO-PCB-STATUS-CODE ')'
           END-IF
           .
      ******************************************************************
      * ROUTINE TO RETURN THE OUTPUT MESSAGE
      ******************************************************************
       SET-OUTPUT-MESSAGE.
      *
           CALL 'CBLTDLI' USING DLI-INSERT IO-PCB-MASK
                                OUTPUT-MSG
           IF IO-PCB-STATUS-CODE NOT = SPACES
             DISPLAY 'ISRT FAILED WITH IO-PCB-STATUS-CODE('
                     IO-PCB-STATUS-CODE ')'
           END-IF
           .
      ******************************************************************
      * ROUTINE TO CHECK FOR ERROR DURING JAVA CALL
      ******************************************************************
       CHECK-ERROR.
      *
           Call ExceptionOccurred
                using by value JNIEnvPtr
                returning ex
           If ex not = null then
                Call ExceptionClear using by value JNIEnvPtr
                MOVE 'JAVA EXCEPTION OCCURRED' TO WS-LOG-MESSAGE
                PERFORM LOG-MESSAGE
                Invoke ex "printStackTrace"
                Stop run
           End-if
           .
      ******************************************************************
      * ROUTINE TO BUILD THE ARGS TO PASS TO THE JAVA STATIC METHOD
      ******************************************************************
       BUILD-JAVA-INPUT-ARGS.
      *
      * public static String
      *        getClaimResult(String claimType, String claimAmount)
      *
           MOVE IN-CLAIMTYPE TO stringClaimType
           MOVE IN-CLAIMAMT TO stringClaimAmount
           MOVE LENGTH OF IN-CLAIMTYPE TO strLenClaimType
           MOVE LENGTH OF IN-CLAIMAMT TO strLenClaimAmout

      * Create a new string using value from IN-CLAIMTYPE
           Call NewString
                using by value JNIEnvPtr
                      address of stringClaimType
                      strLenClaimType
                returning jStrClaimType

           If jStrClaimType not = null then
               MOVE 'CREATE jStrClaimType SUCCESSFUL' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
           Else
               MOVE 'CREATE jStrClaimType FAILED' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
               Stop run
           End-if

      * Create a new string using value from IN-CLAIMAMT
           Call NewString
                using by value JNIEnvPtr
                      address of stringClaimAmount
                      strLenClaimAmout
                returning jStrClaimAmount

           If jStrClaimAmount not = null then
               MOVE 'CREATE jStrClaimAmount SUCCESSFUL'
                 TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
           Else
               MOVE 'CREATE jStrClaimAmount FAILED' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
               Stop run
           End-if
           .
      ******************************************************************
      * ROUTINE TO GET THE RESPONSE RETURNED BY THE JAVA CALL
      ******************************************************************
       GET-JAVA-RESPONSE-STR.

      * Get the length of the string returned by the Java call
           Call GetStringLength
                using by value JNIEnvPtr
                      jStrOutput
                returning strLenOutput

           If strLenOutput not = 0 then
               MOVE 'GetStringLength SUCCESSFUL' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
           Else
               MOVE 'GetStringLength FAILED' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
               Stop run
           End-if

      * Get the string class object returned by the Java call
           Call GetStringChars
                using by value JNIEnvPtr
                      jStrOutput
                      address of jboolean1
                returning strOutPtr

      * Copy the string response to a place holder so we can free it up
           SET ADDRESS OF stringRsp TO strOutPtr
           INITIALIZE WS-RETURN-STR

           MOVE 1140 to WS-CCSID
           MOVE FUNCTION DISPLAY-OF(stringRsp(1:strLenOutput), WS-CCSID)
                TO WS-RETURN-STR

           MOVE WS-RETURN-STR TO WS-LOG-MESSAGE
           PERFORM LOG-MESSAGE

      * Contents of the string class copied, release the string object
           Call ReleaseStringChars
                using by value JNIEnvPtr
                      jStrOutput
                      address of stringRsp
           .
      ******************************************************************
      * ROUTINE TO LOG MESSAGE TO SYSOUT
      ******************************************************************
       LOG-MESSAGE.
      *
           MOVE FUNCTION CURRENT-DATE TO WS-TEMP-DATE-TIME
           MOVE WS-TEMP-YEAR  TO WS-FORMATTED-YEAR
           MOVE WS-TEMP-MONTH TO WS-FORMATTED-MONTH
           MOVE WS-TEMP-DAY   TO WS-FORMATTED-DAY
           MOVE WS-TEMP-HOUR  TO WS-FORMATTED-HOUR
           MOVE WS-TEMP-MIN   TO WS-FORMATTED-MIN
           MOVE WS-TEMP-SEC   TO WS-FORMATTED-SEC
           MOVE WS-TEMP-MS    TO WS-FORMATTED-MS

           DISPLAY WS-FORMATTED-DATE-TIME ' IMSCBLJC VER 1.0 '
             WS-LOG-MESSAGE(1:50)
           .
       END PROGRAM IMSCBLJC.
