      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID. LOGGER.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
      ******************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FO-LOG ASSIGN TO "files/log.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

      ******************************************************************
       DATA                      DIVISION.
      ******************************************************************
       FILE                      SECTION.
       FD FO-LOG.
       01 LOG-FILE               PIC X(10000).
       
      ******************************************************************
       WORKING-STORAGE           SECTION.
      ******************************************************************
       01 PROGNAME               PIC X(10) VALUE "LOGGER    ".
      *   
       01 INDEXES.
         05 I1                   PIC 9(2).
      ******************************************************************
      *                        COPYLIB IMPORTS 
      ******************************************************************
       LINKAGE SECTION.
       COPY LOGGERINTERFACE.
      ******************************************************************
       PROCEDURE DIVISION USING LOGGER-INTERFACE.
      ******************************************************************
       MAIN SECTION.
      *> Always open in EXTEND so we append, not overwrite
           OPEN EXTEND FO-LOG

           EVALUATE TRUE
             WHEN FG-LOG-MODE-SINGLE-LINE
               PERFORM WRITE-SINGLE-LINE
             WHEN FG-LOG-MODE-MULTI-LINE
               PERFORM WRITE-MULTI-LINE
           END-EVALUATE

           *> Always close after operation
           CLOSE FO-LOG
           .
       MAIN-END.
           GOBACK.
      ******************************************************************
       WRITE-MULTI-LINE SECTION.
           PERFORM VARYING I1 FROM 1 BY 1 
             UNTIL I1 >= I-IDX-LOG-SPLIT
             WRITE LOG-FILE FROM I-LOG-LINE-MESSAGE(I1)
           END-PERFORM
           .
       WRITE-MULTI-LINE-END.
           EXIT. 
      ******************************************************************
       WRITE-SINGLE-LINE SECTION.
           WRITE LOG-FILE FROM I-LOG-LONG-MESSAGE
           .
       WRITE-SINGLE-LINE-END.
       