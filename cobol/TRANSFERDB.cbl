      ******************************************************************
       IDENTIFICATION          DIVISION.
      ******************************************************************
       PROGRAM-ID.             TRANSFERDB.
      ******************************************************************
       DATA                    DIVISION.
      ******************************************************************
       WORKING-STORAGE         SECTION.
       01 PGM-ID                      PIC X(20) 
                                      VALUE "TRANSFERDB          ".
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01 H-OFFSET                    PIC S9(9).
       01 H-PAGE-SIZE                 PIC S9(9).
       01 H-FLT-IBAN                  PIC X(30).
       01 H-FLT-CUSTID                PIC 9(05).
       01 H-PAGE-NUM                  PIC 9(05).

       EXEC SQL INCLUDE H-TRANSFER    END-EXEC.
       EXEC SQL INCLUDE DBUTILSVARS   END-EXEC.

       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.
      
      *> INTERNAL VARIABLES
       01 FLAGS.
         05 FG-HAS-FLT-IBAN           PIC X VALUE 'N'.
           88 FG-HAS-FLT-IBAN-Y       VALUE 'Y'.
           88 FG-HAS-FLT-IBAN-N       VALUE 'N'.
         05 FG-HAS-FLT-CUSTID         PIC X VALUE 'N'.
           88 FG-HAS-FLT-CUSTID-Y     VALUE 'Y'.
           88 FG-HAS-FLT-CUSTID-N     VALUE 'N'.

       01 CONSTANTS.
         05 K-PAGE-SIZE               PIC 9(02) VALUE 10.

       COPY LOGGERINTERFACE.
      * 
       LINKAGE SECTION.
       COPY TRANSFERINTERFACE.
      ******************************************************************
       PROCEDURE DIVISION USING TRANSFERDB-INTERFACE.
      ******************************************************************
       MAIN SECTION.
           PERFORM F-INIT
      *    
           IF FG-CONNECT-Y
             PERFORM F-PROCESS-REQUEST
           END-IF
      *    
           PERFORM F-FINISH
           .
       MAIN-END.
           GOBACK.
      ****************************************************************** 
       F-INIT SECTION.
           SET TRANSFERDB-STATUS-OK            TO TRUE
           INITIALIZE TRANSFERDB-OUT
      *    
           PERFORM UT-LOG-INIT
           MOVE PGM-ID                         TO U-LOG-RUNNABLE-PROG
           MOVE TRANSFERDB-IN                  TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-START
      *    
           PERFORM DB-INIT-CONNECTION
           IF FG-CONNECT-N
             SET TRANSFERDB-STATUS-ERROR-CONN  TO TRUE
             PERFORM UT-LOG-DBCONNECT-ERR
           END-IF
           .
       F-INIT-END.
           EXIT.
      ******************************************************************
       F-FINISH SECTION.
      *
           IF  NOT U-SQL-OK 
           AND TRANSFERDB-STATUS-OK
             PERFORM DB-ERROR-DEFAULT
           END-IF
      *    
           IF NOT TRANSFERDB-STATUS-OK
             SET FG-DB-COMMIT-N           TO TRUE
           END-IF

           PERFORM DB-FINISH
      *    
           MOVE PGM-ID                    TO U-LOG-RUNNABLE-PROG
           MOVE TRANSFERDB-OUT            TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-FINISH
           .
       F-FINISH-END.
           EXIT.
      ******************************************************************
       F-PROCESS-REQUEST SECTION.
      *
           EVALUATE TRUE
             WHEN I-TRANSFER-OP-POST
               PERFORM TRANSFER-POST
             WHEN I-TRANSFER-OP-DELETE
               PERFORM TRANSFER-DELETE
             WHEN I-TRANSFER-OP-GETLIST
               PERFORM TRANSFER-GET-LIST
           END-EVALUATE
           .
      *
       F-PROCESS-REQUEST-END.
           EXIT.
      ******************************************************************
       TRANSFER-POST SECTION.
      *
           MOVE I-TRANSFER-SRCIBAN     TO H-TRANSFER-SRCIBAN
           MOVE I-TRANSFER-DESTIBAN    TO H-TRANSFER-DESTIBAN
           MOVE I-TRANSFER-AMOUNT      TO H-TRANSFER-AMOUNT
           MOVE I-TRANSFER-TIMESTAMP   TO H-TRANSFER-TIMESTAMP
           MOVE I-TRANSFER-CURRENCY    TO H-TRANSFER-CURRENCY
           
           PERFORM LOG-TRANSFER-POST

           PERFORM DB-INS-TRANSFER1

           IF U-SQL-OK
             MOVE 1                    TO O-TRANSFER-COUNT
             MOVE H-TRANSFER-ID        TO O-TRANSFER-ID(1)
             MOVE H-TRANSFER-SRCIBAN   TO O-TRANSFER-SRCIBAN(1)
             MOVE H-TRANSFER-DESTIBAN  TO O-TRANSFER-DESTIBAN(1)
             MOVE H-TRANSFER-AMOUNT    TO O-TRANSFER-AMOUNT(1)
             MOVE H-TRANSFER-TIMESTAMP TO O-TRANSFER-TIMESTAMP(1)
             MOVE H-TRANSFER-CURRENCY  TO O-TRANSFER-CURRENCY(1)
           END-IF
           .
      *
       TRANSFER-POST-END.
           EXIT. 
      ******************************************************************
       TRANSFER-DELETE SECTION.
      *
           MOVE I-TRANSFER-ID          TO H-TRANSFER-ID

           PERFORM LOG-TRANSFER-DELETE

           PERFORM DB-DEL-TRANSFER

      * ONLY FOR ROLLBACK, REPLACE IF DELETE REQUEST IS IMPLEMENTED
           IF U-SQL-OK
             CONTINUE  
           END-IF
           .
      *
       TRANSFER-DELETE-END.
           EXIT. 
      ******************************************************************
       TRANSFER-GET-LIST SECTION.
           PERFORM TR-SETUP-PAGINATION-FILTERS
           PERFORM DB-OP-TR-LIST
           IF NOT U-SQL-OK
             EXIT SECTION
           END-IF

           MOVE 0             TO O-TRANSFER-COUNT

           PERFORM DB-FE-TR-LIST
           PERFORM UNTIL NOT U-SQL-OK
             ADD 1            TO O-TRANSFER-COUNT
             IF O-TRANSFER-COUNT > K-PAGE-SIZE
               EXIT PERFORM
             END-IF
             MOVE H-TRANSFER-ID
                              TO O-TRANSFER-ID       (O-TRANSFER-COUNT)
             MOVE H-TRANSFER-SRCIBAN
                              TO O-TRANSFER-SRCIBAN  (O-TRANSFER-COUNT)
             MOVE H-TRANSFER-DESTIBAN
                              TO O-TRANSFER-DESTIBAN (O-TRANSFER-COUNT)
             MOVE H-TRANSFER-AMOUNT
                              TO O-TRANSFER-AMOUNT   (O-TRANSFER-COUNT)
             MOVE H-TRANSFER-TIMESTAMP
                              TO O-TRANSFER-TIMESTAMP(O-TRANSFER-COUNT)
             MOVE H-TRANSFER-CURRENCY
                              TO O-TRANSFER-CURRENCY (O-TRANSFER-COUNT)
             PERFORM DB-FE-TR-LIST
           END-PERFORM

           IF U-SQL-OK OR U-SQL-NO-DATA
             PERFORM DB-CL-TR-CURS-PAGE
           END-IF
           .
       TRANSFER-GET-LIST-END.
           EXIT.

      ******************************************************************
       TR-SETUP-PAGINATION-FILTERS SECTION.
           MOVE I-TRANSFER-PAGE-NUMBER   TO H-PAGE-NUM
           IF H-PAGE-NUM < 1
             MOVE 1 TO H-PAGE-NUM
           END-IF
       
           COMPUTE H-OFFSET = (H-PAGE-NUM - 1) * K-PAGE-SIZE
           MOVE K-PAGE-SIZE              TO H-PAGE-SIZE
       
           MOVE I-TRANSFER-FILTER-IBAN   TO H-FLT-IBAN
           MOVE I-TRANSFER-FILTER-CUSTID TO H-FLT-CUSTID

           IF H-FLT-IBAN NOT = SPACES
             SET FG-HAS-FLT-IBAN-Y      TO TRUE
           ELSE
             SET FG-HAS-FLT-IBAN-N      TO TRUE
           END-IF

           IF H-FLT-CUSTID NOT = ZEROES
             SET FG-HAS-FLT-CUSTID-Y    TO TRUE
           ELSE
             SET FG-HAS-FLT-CUSTID-N    TO TRUE
           END-IF
       
           PERFORM LOG-OFFSET-CALC
           .
       TR-SETUP-PAGINATION-FILTERS-END.
           EXIT.
      ******************************************************************
       DB-OP-TR-LIST SECTION.
           EXEC SQL
           DECLARE CURS_TR_LIST CURSOR FOR
             SELECT T.ID, T.SOURCE_IBAN, T.DESTINATION_IBAN, 
             T.AMOUNT, T.CREATED_AT, T.CURRENCY
             FROM TRANSFER T
             WHERE (:FG-HAS-FLT-IBAN = 'N' OR 
                    T.SOURCE_IBAN      = :H-FLT-IBAN OR
                    T.DESTINATION_IBAN = :H-FLT-IBAN)
               AND (:FG-HAS-FLT-CUSTID = 'N' OR EXISTS 
                          (SELECT 1
                           FROM ACCOUNT A
                           WHERE A.CUSTOMERID = :H-FLT-CUSTID
                             AND (A.IBAN = T.SOURCE_IBAN OR
                                  A.IBAN = T.DESTINATION_IBAN)))
             ORDER BY T.ID DESC
             OFFSET :H-OFFSET ROWS
             FETCH FIRST :H-PAGE-SIZE ROWS ONLY
           END-EXEC
           EXEC SQL OPEN CURS_TR_LIST END-EXEC

           PERFORM DB-SQL-DEFAULT
       .
       DB-OP-TR-LIST-END.
           EXIT.
      ******************************************************************
       DB-FE-TR-LIST SECTION.
           EXEC SQL
             FETCH CURS_TR_LIST INTO
               :H-TRANSFER-ID,
               :H-TRANSFER-SRCIBAN,
               :H-TRANSFER-DESTIBAN,
               :H-TRANSFER-AMOUNT,
               :H-TRANSFER-TIMESTAMP,
               :H-TRANSFER-CURRENCY
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
       DB-FE-TR-LIST-END.
           EXIT.
      ******************************************************************
       DB-CL-TR-CURS-PAGE SECTION.
           EXEC SQL CLOSE CURS_TR_LIST END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
       DB-CL-TR-CURS-PAGE-END.
           EXIT.
      ******************************************************************
       DB-DEL-TRANSFER SECTION.
      *
           EXEC SQL
             DELETE FROM TRANSFER
             WHERE ID = :H-TRANSFER-ID
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-DEL-TRANSFER-END.
           EXIT. 
      ******************************************************************
       DB-INS-TRANSFER1 SECTION.
      *
           EXEC SQL
             INSERT INTO TRANSFER (SOURCE_IBAN, DESTINATION_IBAN,
             AMOUNT, CREATED_AT, CURRENCY)
             VALUES (:H-TRANSFER-SRCIBAN, 
                     :H-TRANSFER-DESTIBAN,
                     :H-TRANSFER-AMOUNT, 
                     :H-TRANSFER-TIMESTAMP, 
                     :H-TRANSFER-CURRENCY)
           END-EXEC

           IF U-SQL-OK
             EXEC SQL
               SELECT lastval() INTO :H-TRANSFER-ID
             END-EXEC
           END-IF

           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-INS-TRANSFER1-END.
           EXIT. 
      ******************************************************************
       DB-ERROR-DEFAULT SECTION.
           SET TRANSFERDB-STATUS-SQL-ERR   TO TRUE
           MOVE U-SQLCODE                  TO O-TRANSFER-SQLCODE
           .
       DB-ERROR-DEFAULT-END.
           EXIT.
      ******************************************************************
       LOG-TRANSFER-DELETE SECTION.
      *
           STRING
             "DELETING TRANSFER WITH ID   : ",
             H-TRANSFER-ID
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           PERFORM UT-LOG-MULTI-LINE
           .
      *
       LOG-TRANSFER-DELETE-END.
           EXIT. 
      ******************************************************************
       LOG-TRANSFER-POST SECTION.
           STRING
             "Creating new transfer: ",
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "SOURCE ACC             : ",
             H-TRANSFER-SRCIBAN
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "DESTINATION ACC        : ",
             H-TRANSFER-DESTIBAN
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      
           STRING
             "AMOUNT                 : ",
             H-TRANSFER-AMOUNT
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "TIMESTAMP              : ",
             H-TRANSFER-TIMESTAMP
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "CURRENCY               : ",
             H-TRANSFER-CURRENCY
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           PERFORM UT-LOG-MULTI-LINE
           .
      *
       LOG-TRANSFER-POST-END.
           EXIT. 
      ******************************************************************
       LOG-OFFSET-CALC SECTION.
           STRING
             "Calculated OFFSET: ",
             FUNCTION TRIM(H-OFFSET),
             " | Page Size: ",
             FUNCTION TRIM(H-PAGE-SIZE)
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-OFFSET-CALC-END.
           EXIT.
      ******************************************************************
      *   COPY DBUTILSECTIONS TO USE GENERIC SECTIONS FOR DB CONNECTION
      ******************************************************************
       EXEC SQL INCLUDE DBUTILSECTIONS END-EXEC.
      ******************************************************************     
      *   COPY LOGGERUTILSECTIONS TO USE GENERIC SECTIONS FOR LOGGER
      ****************************************************************** 
       COPY LOGGERUTILSECTIONS.     
    