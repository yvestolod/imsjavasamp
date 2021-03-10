       CBL dll,thread
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IMSCBLJB RECURSIVE.
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
      *  (b) Calling the Java main method                              *
      *  (c) Calling a static method that returns a string in Java     *
      *                                                                *
      * Calls (b) will write a message to STDOUT to indicate the       *
      * argument that was passed to the Java program.  Call (c)        *
      * will return a response back to the COBOL program. The sample   *
      * program will be using mixed case for the Java related calls.   *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.
      ***********************
       CONFIGURATION SECTION.
      ************************
       REPOSITORY.
      *************

           Class ZUtil         is "com.ibm.jzos.ZUtil"
           Class JavaSample    is "demo.ibm.sample.JavaSample"
           Class JavaException is "java.lang.Exception"
           Class JavaObject    is "java.lang.Object"
           Class JavaString    is "java.lang.String"
           Class JavaClass     is "java.lang.Class"
           Class stringArray   is "jobjectArray:java.lang.String".

       DATA DIVISION.
      ****************
       WORKING-STORAGE SECTION.
      **************************
       01 args                      object reference stringArray.
       01 argsLen                   pic s9(9) binary value 0.
       01 jstring1                  object reference JavaString.
       01 jstring2                  object reference JavaString.
       01 stringClass               object reference JavaClass.
       01 ex                        object reference JavaException.
       01 stringBuf                 pic N(256) usage national.
       01 strLen1                   pic S9(3) comp value 0.
       01 strLen2                   pic S9(3) comp value 0.
       01 stringPtr                 usage pointer.
       01 jboolean1                 pic X.
          88 jboolean1-true         value X'01' through X'FF'.
          88 jboolean1-false        value X'00'.
      ******************************************************************
      * INCLUDE THE COPYBOOK FOR REQUEST AND RESPONSE DATA STRUCTURE
      * OF THE SAMPLE PROGRAM.
      ******************************************************************
       COPY IMSC2JC.
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
       01 WS-RETURN-STR             PIC X(50).
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
             PERFORM CALL-JAVA-CLASS
             PERFORM SET-OUTPUT-MESSAGE
             PERFORM GET-INPUT-MESSAGE
           END-PERFORM

           GOBACK
           .
      ******************************************************************
      * ROUTINE TO CALL THE JAVA CLASS
      ******************************************************************
       CALL-JAVA-CLASS.
      *
           Set address of JNIEnv to JNIEnvPtr
           Set address of JNINativeInterface to JNIENV
      *
      * (a) Calling a static void method in Java
      *
      * This static JZOS method will redirect Java stdout/stderr
      * to DD:STDOUT and DD:STDERR, which may be spool files or
      * data sets
      *
           MOVE 'Invoking ZUtil Java class' TO WS-LOG-MESSAGE
           PERFORM LOG-MESSAGE
           Invoke ZUtil "redirectStandardStreams"
           PERFORM CHECK-ERROR
      *
      * (b) Calling the Java main method
      *
      * We invoke demo.ibm.sample.JavaSample, but this could
      * be any arbitrary Java code
      *
           MOVE 'Invoking JavaSample.main' TO WS-LOG-MESSAGE
           PERFORM LOG-MESSAGE
           PERFORM BUILD-ARGS-ARRAY

           Invoke JavaSample "main"
               using by value args
           PERFORM CHECK-ERROR
      *
      * (c) Calling a static method that returns a string in Java
      *
      * Using the same demo.ibm.sample.JavaSample, but this time
      * calling a static method that returns a string
      *
           MOVE 'Invoking JavaSample.getResponse' TO WS-LOG-MESSAGE
           PERFORM LOG-MESSAGE

           Invoke JavaSample "getResponse"
               using by value jString1
               returning jString2

           If jString2 not = null then
               MOVE 'METHOD getResponse SUCCESSFUL' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
           Else
               MOVE 'METHOD getResponse FAILED' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
               Stop run
           End-if

           PERFORM GET-JAVA-RESPONSE-STR
      *
      * Process the output that will be returned back to IMS
      *
           MOVE IN-JAVA-ARG TO OUT-JAVA-ARG
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
      * ROUTINE TO BUILD THE ARGUMENTS TO PASS TO THE JAVA CLASS
      ******************************************************************
       BUILD-ARGS-ARRAY.
      *
           MOVE IN-JAVA-ARG TO stringBuf
           MOVE LENGTH OF IN-JAVA-ARG TO strLen1

      * Create a new string using value from IN-JAVA-ARG
           Call NewString
                using by value JNIEnvPtr
                      address of stringBuf
                      strLen1
                returning jstring1

           If jstring1 not = null then
               MOVE 'CREATE jstring1 SUCCESSFUL' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
           Else
               MOVE 'CREATE jstring1 FAILED' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
               Stop run
           End-if

      *  Get a reference to the String class object
           Call GetObjectClass
                using by value JNIEnvPtr jstring1
                returning stringClass

           If stringClass not = null then
               MOVE 'GET stringClass SUCCESSFUL' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
           Else
               MOVE 'GET stringClass FAILED' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
               Stop run
           End-if

      *  Create an object array with 1 argument
           move 1 to argsLen
           Call NewObjectArray
                using by value JNIEnvPtr
                argsLen stringClass jstring1
                returning args

           If args not = null then
               MOVE 'NewObjectArray SUCCESSFUL' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
           Else
               MOVE 'NewObjectArray FAILED' TO WS-LOG-MESSAGE
               PERFORM LOG-MESSAGE
               Stop run
           End-if
           .
      ******************************************************************
      * ROUTINE TO GET THE ARGUMENT PASSED BY THE JAVA METHOD
      ******************************************************************
       GET-JAVA-RESPONSE-STR.

      * Get the length of the string returned by the Java call
           Call GetStringLength
                using by value JNIEnvPtr
                      jString2
                returning strLen2

           If strLen2 not = 0 then
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
                      jString2
                      address of jboolean1
                returning stringPtr

      * Copy the string response to a place holder so we can free it up
           SET ADDRESS OF stringRsp TO stringPtr
           INITIALIZE WS-RETURN-STR

           MOVE 1140 to WS-CCSID
           MOVE FUNCTION DISPLAY-OF(stringRsp(1:strLen2), WS-CCSID)
                TO WS-RETURN-STR

           MOVE WS-RETURN-STR TO WS-LOG-MESSAGE
           PERFORM LOG-MESSAGE

      * Contents of the string class copied, release the string object
           Call ReleaseStringChars
                using by value JNIEnvPtr
                      jString2
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

           DISPLAY WS-FORMATTED-DATE-TIME ' IMSCBLJB VER 1.0.1 '
             WS-LOG-MESSAGE(1:50)
           .
       END PROGRAM IMSCBLJB.
