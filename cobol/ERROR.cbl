      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID. ERROR.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
      ******************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FI-ERRORS ASSIGN TO "files/data/errors.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
      ******************************************************************
        DATA                        DIVISION.
       FILE SECTION.
       FD FI-ERRORS.
       01 ERROR-RECORD. 
         05 F-ERR-CODE                 PIC X(04).
         05 F-ERR-TEMPLATE             PIC X(100).    
      ******************************************************************
       WORKING-STORAGE                 SECTION.

       01 PGM-ID                       PIC X(20) 
                                       VALUE "ERROR               ".
       01 FLAGS.
         05 FG-EOF-ERRORS              PIC X VALUE 'N'.
           88 FG-EOF-ERRORS-Y          VALUE 'Y'.
           88 FG-EOF-ERRORS-N          VALUE 'N'.
         05 FG-FOUND-ERROR             PIC X VALUE 'N'.
           88 FG-FOUND-ERROR-Y         VALUE 'Y'.
           88 FG-FOUND-ERROR-N         VALUE 'N'.

       01 INTERNAL-VARS.
         05 WS-TEMP-MESSAGE-PART-1     PIC X(100).
         05 WS-TEMP-MESSAGE-PART-2     PIC X(100).
         05 WS-PLACEHOLDER             PIC X(02).

       01 INDEXES.
         05 IND-1                      PIC 9(01).
      ******************************************************************
      *                        COPYLIB IMPORTS 
      ****************************************************************** 
       LINKAGE SECTION.
       COPY ERRINTERFACE.
      ******************************************************************
       PROCEDURE DIVISION USING BY REFERENCE ERROR-INTERFACE.
      ******************************************************************
       MAIN SECTION.
           SET O-ERR-STATUS-OK                        TO TRUE
           PERFORM F-FIND-ERROR-MESSAGE

           IF FG-FOUND-ERROR-Y
             PERFORM F-BUILD-MESSAGE
           ELSE
             SET O-ERR-STATUS-NOT-FOUND               TO TRUE
             MOVE "Error message not found for code." TO O-ERR-MESSAGE
           END-IF
           .
       MAIN-END.
           GOBACK.
      ******************************************************************
       F-FIND-ERROR-MESSAGE SECTION.
           SET FG-EOF-ERRORS-N           TO TRUE
           SET FG-FOUND-ERROR-N          TO TRUE
           OPEN INPUT FI-ERRORS

           PERFORM UNTIL FG-EOF-ERRORS-Y OR FG-FOUND-ERROR-Y
             READ FI-ERRORS
               AT END
                 SET FG-EOF-ERRORS-Y     TO TRUE
               NOT AT END
                 IF F-ERR-CODE = I-ERR-CODE
                   MOVE F-ERR-TEMPLATE   TO O-ERR-MESSAGE
                   SET FG-FOUND-ERROR-Y  TO TRUE
                 END-IF
             END-READ
           END-PERFORM

           CLOSE FI-ERRORS
           .
       F-FIND-ERROR-MESSAGE-END.
           EXIT.
      ******************************************************************
       F-BUILD-MESSAGE SECTION.
           IF I-ERR-PARAM-COUNT > 0
             *> REPLACE PARAMETERS ONE BY ONE
             PERFORM VARYING IND-1 FROM 1 BY 1
             UNTIL IND-1 > I-ERR-PARAM-COUNT
               STRING "%" IND-1 INTO WS-PLACEHOLDER
               UNSTRING O-ERR-MESSAGE
                 DELIMITED BY WS-PLACEHOLDER
                 INTO WS-TEMP-MESSAGE-PART-1
                      WS-TEMP-MESSAGE-PART-2
               END-UNSTRING
               MOVE SPACES TO O-ERR-MESSAGE
               STRING FUNCTION TRIM(WS-TEMP-MESSAGE-PART-1)
                      FUNCTION TRIM(I-ERR-PARAM(IND-1))
                      FUNCTION TRIM(WS-TEMP-MESSAGE-PART-2)
                 DELIMITED BY SIZE
                 INTO O-ERR-MESSAGE
               END-STRING
             END-PERFORM
           END-IF
           .
       F-BUILD-MESSAGE-END.
           EXIT.
      ******************************************************************
