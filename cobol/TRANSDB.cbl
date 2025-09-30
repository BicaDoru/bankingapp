      ******************************************************************
       IDENTIFICATION          DIVISION.
      ******************************************************************
       PROGRAM-ID.             TRANSDB.
      ******************************************************************
       DATA                    DIVISION.
      ******************************************************************
       WORKING-STORAGE         SECTION.
       01 PGM-ID                      PIC X(20) 
                                      VALUE "TRANSDB             ".
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 H-OFFSET                    PIC S9(9).
       01 H-PAGE-SIZE                 PIC S9(9).
       01 H-FROM-DATE                 PIC X(10).
       01 H-HAS-DATE                  PIC S9(4).
       01 H-HAS-ACCID                 PIC S9(4).
       EXEC SQL INCLUDE H-TRANSACTION END-EXEC.
       EXEC SQL INCLUDE DBUTILSVARS   END-EXEC.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.
      
      *> INTERNAL VARIABLES
       01 INTERNAL-VARS. 
         05 WS-AMOUNT                 PIC 9(8)V99.
         05 WS-TIMESTAMP              PIC X(35).
         05 WS-OPTYPE                 PIC X(10).
         05 WS-TRANS-CNT              PIC 9(04).
         05 WS-ACCBALANCE             PIC 9(08)V99.
         05 WS-PAGE-NUMBER            PIC 9(05).

       01 FLAGS.
         05 FG-HAS-ACCID              PIC X VALUE 'N'.
           88 FG-HAS-ACCID-Y          VALUE 'Y'.
           88 FG-HAS-ACCID-N          VALUE 'N'.   

       01 CONSTANTS.
         05 K-MAX-TRANS               PIC 9(02) VALUE 50. 
         05 K-PAGE-SIZE               PIC 9(02) VALUE 10.

       COPY LOGGERINTERFACE.
      * 
       LINKAGE SECTION.
       COPY TRANSINTERFACE.
      ******************************************************************
       PROCEDURE DIVISION USING TRANSDB-INTERFACE.
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
           SET TRANSDB-STATUS-OK               TO TRUE
           INITIALIZE TRANSDB-OUT
      *    
           PERFORM UT-LOG-INIT
           MOVE PGM-ID                         TO U-LOG-RUNNABLE-PROG
           MOVE TRANSDB-IN                     TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-START
      *    
           PERFORM DB-INIT-CONNECTION
           IF FG-CONNECT-N
             SET TRANSDB-STATUS-ERROR-CONN     TO TRUE
             PERFORM UT-LOG-DBCONNECT-ERR
           END-IF
           .
       F-INIT-END.
           EXIT.
      ******************************************************************
       F-FINISH SECTION.
           IF NOT U-SQL-OK 
           AND TRANSDB-STATUS-OK
             PERFORM DB-ERROR-DEFAULT
           END-IF
      *    
           IF NOT TRANSDB-STATUS-OK
             SET FG-DB-COMMIT-N        TO TRUE
           END-IF
           PERFORM DB-FINISH
      *    
           MOVE PGM-ID                 TO U-LOG-RUNNABLE-PROG
           MOVE TRANSDB-OUT            TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-FINISH
           .
       F-FINISH-END.
           EXIT.
      ******************************************************************
       F-PROCESS-REQUEST SECTION.
      *
           EVALUATE TRUE
             WHEN I-TRANS-OP-POST
               PERFORM TRANS-POST
             WHEN I-TRANS-OP-GET-LIST
               PERFORM TRANS-GET-LIST
           END-EVALUATE
           .
      *
       F-PROCESS-REQUEST-END.
           EXIT. 
      ******************************************************************
       TRANS-GET-LIST SECTION.
           PERFORM LOG-TRANS-GET-TRANS

           MOVE 0                    TO H-HAS-DATE
           IF I-TRANS-TIMESTAMP(1:10) NOT = SPACES
             MOVE I-TRANS-TIMESTAMP(1:10) 
                                     TO H-FROM-DATE
             MOVE 1                  TO H-HAS-DATE
           END-IF

           MOVE 'N'                  TO FG-HAS-ACCID
           MOVE 0                    TO H-HAS-ACCID
           IF I-TRANS-ACCOUNTID > 0
             SET FG-HAS-ACCID-Y      TO TRUE
             MOVE I-TRANS-ACCOUNTID  TO H-TRA-ACCOUNTID
             MOVE 1                  TO H-HAS-ACCID
           END-IF

           MOVE I-TRANS-PAGE-NUMBER  TO WS-PAGE-NUMBER
           IF WS-PAGE-NUMBER < 1
             MOVE 1                  TO WS-PAGE-NUMBER
           END-IF
           COMPUTE H-OFFSET = (WS-PAGE-NUMBER - 1) * K-PAGE-SIZE
           MOVE K-PAGE-SIZE          TO H-PAGE-SIZE

           PERFORM LOG-OFFSET-CALC

           PERFORM DB-OP-TRANS-LIST
           PERFORM DB-SQL-DEFAULT
           IF NOT U-SQL-OK
             EXIT SECTION
           END-IF

           MOVE 0                    TO WS-TRANS-CNT
           PERFORM DB-FE-TRANS-LIST
           PERFORM UNTIL NOT U-SQL-OK OR WS-TRANS-CNT >= K-PAGE-SIZE
             ADD 1                   TO WS-TRANS-CNT
             MOVE H-TRA-ID           TO O-TRANS-ID(WS-TRANS-CNT)
             MOVE H-TRA-ACCOUNTID    TO O-TRANS-ACCOUNTID(WS-TRANS-CNT)
             MOVE H-TRA-AMOUNT       TO O-TRANS-AMMOUNT(WS-TRANS-CNT)
             MOVE H-TRA-TRANTS       TO O-TRANS-TIMESTAMP(WS-TRANS-CNT)
             MOVE H-TRA-OPERATIONTYPE
                                     TO O-TRANS-TRANS-TYPE(WS-TRANS-CNT)
             MOVE H-TRA-ACCBALANCE   TO O-TRANS-ACCBALANCE(WS-TRANS-CNT)
             PERFORM DB-FE-TRANS-LIST
           END-PERFORM

           IF U-SQL-OK OR U-SQL-NO-DATA
             PERFORM DB-CL-CURS-PAGE
             MOVE WS-TRANS-CNT       TO O-TRANSDB-COUNT
           ELSE
             SET TRANSDB-STATUS-SQL-ERR
                                     TO TRUE
           END-IF
           .
       TRANS-GET-LIST-END.
           EXIT.
      ******************************************************************
       TRANS-POST SECTION.

           MOVE I-TRANS-ACCOUNTID      TO H-TRA-ACCOUNTID
           MOVE I-TRANS-AMMOUNT        TO H-TRA-AMOUNT
           MOVE I-TRANS-TRANS-TYPE     TO H-TRA-OPERATIONTYPE
           MOVE I-TRANS-TIMESTAMP      TO H-TRA-TRANTS
           MOVE I-TRANS-ACCBALANCE     TO H-TRA-ACCBALANCE

           PERFORM LOG-TRANS-POST
      *>   Perform the INSERT
           PERFORM DB-INS-TRANS1
           IF U-SQL-OK
             MOVE 1                    TO O-TRANSDB-COUNT
             MOVE H-TRA-ACCOUNTID      TO O-TRANS-ACCOUNTID(1)
             MOVE H-TRA-AMOUNT         TO O-TRANS-AMMOUNT(1)
             MOVE H-TRA-TRANTS         TO O-TRANS-TIMESTAMP(1)
             MOVE H-TRA-OPERATIONTYPE  TO O-TRANS-TRANS-TYPE(1)
             MOVE H-TRA-ACCBALANCE     TO O-TRANS-ACCBALANCE(1)
           ELSE 
             SET TRANSDB-STATUS-SQL-ERR 
                                       TO TRUE
           END-IF
           .
       TRANS-POST-END.
           EXIT.
      ******************************************************************
       DB-OP-TRANS-LIST SECTION.
           EXEC SQL
             DECLARE CURS_TRANS_LIST CURSOR FOR
               SELECT ID, ACCOUNTID, AMOUNT, TRANTS, OPERATIONTYPE, 
               ACCBALANCE
                 FROM TRANSACTION
                WHERE
                  ( :H-HAS-DATE  = 0 OR
                   ( TRANTS >= TO_TIMESTAMP(:H-FROM-DATE || ' 00:00:00',
                                             'YYYY-MM-DD HH24:MI:SS')
                      AND TRANTS <  TO_TIMESTAMP(:H-FROM-DATE || 
                      ' 00:00:00','YYYY-MM-DD HH24:MI:SS')
                      + INTERVAL '1 day'
                    )
                  )
                  AND
                  ( :H-HAS-ACCID = 0 OR ACCOUNTID = :H-TRA-ACCOUNTID )
                ORDER BY
                  CASE WHEN :H-HAS-DATE = 1 THEN TRANTS END ASC,
                  CASE WHEN :H-HAS-DATE = 1 THEN ID     END ASC,
                  CASE WHEN :H-HAS-DATE = 0 THEN ID     END DESC
                LIMIT  :H-PAGE-SIZE
                OFFSET :H-OFFSET
           END-EXEC
       
           EXEC SQL
             OPEN CURS_TRANS_LIST
           END-EXEC
           PERFORM DB-SQL-DEFAULT
           .
       DB-OP-TRANS-LIST-END.
           EXIT.

      ******************************************************************
       DB-FE-TRANS-LIST SECTION.
           EXEC SQL
             FETCH CURS_TRANS_LIST INTO
               :H-TRA-ID,
               :H-TRA-ACCOUNTID,
               :H-TRA-AMOUNT,
               :H-TRA-TRANTS,
               :H-TRA-OPERATIONTYPE,
               :H-TRA-ACCBALANCE
           END-EXEC
           PERFORM DB-SQL-DEFAULT
           .
       DB-FE-TRANS-LIST-END.
           EXIT.

      ******************************************************************
       DB-CL-CURS-PAGE SECTION.
           EXEC SQL 
             CLOSE CURS_TRANS_LIST
           END-EXEC
           PERFORM DB-SQL-DEFAULT
           .
       DB-CL-CURS-PAGE-END.
           EXIT.
      ******************************************************************
       DB-INS-TRANS1 SECTION.
      *
           EXEC SQL
             INSERT INTO TRANSACTION 
             (ACCOUNTID, AMOUNT, TRANTS, OPERATIONTYPE, ACCBALANCE)
             VALUES (:H-TRA-ACCOUNTID, 
                     :H-TRA-AMOUNT, 
                     :H-TRA-TRANTS, 
                     :H-TRA-OPERATIONTYPE,
                     :H-TRA-ACCBALANCE)
           END-EXEC
           
           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-INS-TRANS1-END.
           EXIT.
      ******************************************************************
       DB-ERROR-DEFAULT SECTION.
           SET TRANSDB-STATUS-SQL-ERR TO TRUE
           MOVE U-SQLCODE             TO O-TRANSDB-SQLCODE
           .
       DB-ERROR-DEFAULT-END.
           EXIT.
      ******************************************************************
       LOG-TRANS-POST SECTION.
           MOVE "Inserting transaction..." TO U-LOG-LINE
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "Account ID         : ",
             H-TRA-ACCOUNTID
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *            
           STRING
             "Amount             : ",
             H-TRA-AMOUNT
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "Timestamp          : ",
      *      H-TRA-TRANTS
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *
           STRING
             "Operation Type     : ",
             H-TRA-OPERATIONTYPE
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *            
           PERFORM UT-LOG-MULTI-LINE
           .
       LOG-TRANS-POST-END.
           EXIT.
      ******************************************************************
       LOG-TRANS-GET-TRANS SECTION.
           MOVE "Retrieving transactions..." TO U-LOG-LINE
           PERFORM UT-LOG-ADD-LINE

           IF H-HAS-DATE = 1
             STRING
               "Filter day (YYYY-MM-DD): ",
               H-FROM-DATE
               INTO U-LOG-LINE
             END-STRING
             PERFORM UT-LOG-ADD-LINE
           END-IF

           IF FG-HAS-ACCID-Y
             STRING
               "Filter account ID   : ",
               H-TRA-ACCOUNTID
               INTO U-LOG-LINE
             END-STRING
             PERFORM UT-LOG-ADD-LINE
           END-IF

           STRING
             "Page number          : ",
             FUNCTION TRIM(WS-PAGE-NUMBER)
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "Offset               : ",
             FUNCTION TRIM(H-OFFSET),
             " | Page Size: ",
             FUNCTION TRIM(H-PAGE-SIZE)
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           PERFORM UT-LOG-MULTI-LINE
           .
       LOG-TRANS-GET-TRANS-END.
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
    