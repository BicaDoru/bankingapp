      ****************************************************************** 
       IDENTIFICATION DIVISION.
      ****************************************************************** 
       PROGRAM-ID. GENERIC.
      ****************************************************************** 
       ENVIRONMENT                      DIVISION.
       CONFIGURATION                    SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
      ****************************************************************** 
       INPUT-OUTPUT                     SECTION.
        FILE-CONTROL.
           SELECT JSON-OUTPUT ASSIGN TO DYNAMIC WS-FILENAME
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FILE-STATUS.
      ******************************************************************
       DATA                             DIVISION.
      ****************************************************************** 
       FILE                             SECTION.
       FD  JSON-OUTPUT.
       01  JSON-OUTPUT-LINE   PIC X(10000).

       WORKING-STORAGE SECTION.
       01 PGM-ID              PIC X(20) 
                              VALUE "GENERIC             ".
       01 PROGNAME            PIC X(20).
         88 PGNAME-ERROR      VALUE "ERROR               ".
       01 INTERNAL-VARS.
         05 WS-FILENAME       PIC X(256) VALUE "files/tables.json".
         05 WS-FILE-STATUS    PIC XX     VALUE SPACES.

         05 WS-JSON-TEMP      PIC X(10000) VALUE SPACES.
         05 WS-POS            PIC 9(5) VALUE 1.
         05 WS-IDX            PIC 9(2) VALUE 0.
         05 WS-FIDX           PIC 9(2) VALUE 0.
         05 WS-CRLF           PIC X(2) VALUE X'0D0A'.
         05 WS-O-JSON         PIC X(10000).
      ******************************************************************
      *                        COPYLIB IMPORTS 
      ******************************************************************     
       COPY LOGGERINTERFACE.
       COPY ERRINTERFACE.
      * 
       LINKAGE SECTION.
       COPY GENERICINTERFACE.
       COPY DISPINTERFACE.
      ****************************************************************** 
       PROCEDURE DIVISION USING GENERIC-INTERFACE.
      ****************************************************************** 
       MAIN SECTION.
           PERFORM F-INIT

           PERFORM TRANSLATE-INTO-JSON
           PERFORM FILE-WRITING

           PERFORM F-FINISH
           .
       MAIN-END.
           GOBACK.
      ****************************************************************** 
       F-INIT SECTION.
           PERFORM UT-LOG-INIT
           MOVE PGM-ID                 TO U-LOG-RUNNABLE-PROG
           MOVE GENERIC-IN             TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-START
           .
       F-INIT-END.
           EXIT.
      ******************************************************************
       F-FINISH SECTION.
           MOVE PGM-ID                 TO U-LOG-RUNNABLE-PROG
           MOVE GENERIC-OUT            TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-FINISH
           .
       F-FINISH-END.
           EXIT.
      ******************************************************************
       FILE-WRITING SECTION.
           IF O-JSON = SPACES
             PERFORM LOG-WRITING-ERR
             SET GEN-ERR-NOT-WRITTEN-INFILE        TO TRUE
             EXIT SECTION
           END-IF
           MOVE O-JSON                             TO WS-O-JSON
           OPEN EXTEND JSON-OUTPUT
           EVALUATE WS-FILE-STATUS
             WHEN "00"
               CONTINUE  *>fisierul exista deja, append
             WHEN "35" *> fisierul nu exista, se creaza
               OPEN OUTPUT JSON-OUTPUT
               IF WS-FILE-STATUS NOT = "00"
                 SET GEN-ERR-NOT-WRITTEN-INFILE    TO TRUE
                 EXIT SECTION
               END-IF
             WHEN OTHER
               SET GEN-ERR-NOT-WRITTEN-INFILE      TO TRUE
               EXIT SECTION
           END-EVALUATE
       
           MOVE WS-O-JSON                          TO JSON-OUTPUT-LINE
           WRITE JSON-OUTPUT-LINE AFTER ADVANCING 1 LINE
           CLOSE JSON-OUTPUT
           .
       FILE-WRITING-END.
           EXIT.
      ******************************************************************
       TRANSLATE-INTO-JSON SECTION.
           MOVE SPACES                             TO WS-JSON-TEMP
           MOVE 1                                  TO WS-POS
           STRING
             '    {'              DELIMITED BY SIZE
             WS-CRLF              DELIMITED BY SIZE
             '      "table": "'   DELIMITED BY SIZE
             I-GEN-TAB-NAME       DELIMITED BY SPACE
             '",'                 DELIMITED BY SIZE
             WS-CRLF              DELIMITED BY SIZE
             '      "records": [' DELIMITED BY SIZE
             WS-CRLF              DELIMITED BY SIZE
           INTO WS-JSON-TEMP WITH POINTER WS-POS
           END-STRING

           PERFORM VARYING WS-IDX FROM 1 BY 1
                           UNTIL WS-IDX > I-GEN-ROW-NO OR WS-IDX > 50
             STRING 
               '        {' DELIMITED BY SIZE
               WS-CRLF DELIMITED BY SIZE
               INTO WS-JSON-TEMP WITH POINTER WS-POS
             END-STRING

             IF I-GEN-FIELD-CNT(WS-IDX) > 0
               PERFORM VARYING WS-FIDX FROM 1 BY 1
                 UNTIL WS-FIDX > I-GEN-FIELD-CNT(WS-IDX) OR WS-FIDX > 6
                 STRING
                   '          "' DELIMITED BY SIZE
                   I-GEN-F-NAME(WS-IDX, WS-FIDX) DELIMITED BY SPACE
                   '": "' DELIMITED BY SIZE
                   I-GEN-F-VAL(WS-IDX, WS-FIDX) DELIMITED BY SPACE
                   '"' DELIMITED BY SIZE
                   INTO WS-JSON-TEMP WITH POINTER WS-POS
                 END-STRING

                 IF WS-FIDX < I-GEN-FIELD-CNT(WS-IDX)
                   STRING
                     ',' DELIMITED BY SIZE
                     WS-CRLF DELIMITED BY SIZE
                     INTO WS-JSON-TEMP WITH POINTER WS-POS
                   END-STRING
                 ELSE
                   STRING WS-CRLF DELIMITED BY SIZE
                          INTO WS-JSON-TEMP WITH POINTER WS-POS
                   END-STRING
                 END-IF
               END-PERFORM
             END-IF

             STRING 
               '        }' DELIMITED BY SIZE
               WS-CRLF DELIMITED BY SIZE
               INTO WS-JSON-TEMP WITH POINTER WS-POS
             END-STRING

             IF WS-IDX < I-GEN-ROW-NO AND WS-IDX < 50
               STRING 
                 ',' DELIMITED BY SIZE
                 WS-CRLF DELIMITED BY SIZE
                 INTO WS-JSON-TEMP WITH POINTER WS-POS
               END-STRING
             END-IF
           END-PERFORM

           STRING
             '      ]' DELIMITED BY SIZE   
             WS-CRLF   DELIMITED BY SIZE
             '    }'   DELIMITED BY SIZE
             WS-CRLF   DELIMITED BY SIZE
             INTO WS-JSON-TEMP WITH POINTER WS-POS
           END-STRING

           MOVE WS-JSON-TEMP(1:WS-POS - 1)         TO O-JSON
           PERFORM LOG-WRITING-INTO-JSON
           .
       TRANSLATE-INTO-JSON-END.
           EXIT.
      ******************************************************************
       LOG-WRITING-INTO-JSON SECTION.
           STRING
             "Writing into json for table: " I-GEN-TAB-NAME,
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "Writing : " I-GEN-ROW-NO " records into JSON"
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      * 
           PERFORM UT-LOG-MULTI-LINE
           .
       LOG-WRITING-INTO-JSON-END.
           EXIT.
      ******************************************************************
       LOG-WRITING-ERR SECTION.
           MOVE "ERROR WRITING INTO JSON" TO U-LOG-LINE
      *     
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-WRITING-ERR-END.
           EXIT. 
      ******************************************************************     
      *   COPY LOGGERUTILSECTIONS TO USE GENERIC SECTIONS FOR LOGGER
      ****************************************************************** 
       COPY LOGGERUTILSECTIONS.     
