      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 ACCDB.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.

       01 PGM-ID                     PIC X(20) 
                                     VALUE "ACCDB               ".
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 H-PAGE-NUMBER              PIC 9(04).
       01 H-PAGE-SIZE                PIC S9(04) VALUE 10.
       01 H-OFFSET                   PIC S9(09).
             
       EXEC SQL INCLUDE H-ACCOUNT END-EXEC.
       EXEC SQL INCLUDE DBUTILSVARS END-EXEC.

       EXEC SQL END DECLARE SECTION END-EXEC.
       EXEC SQL INCLUDE SQLCA END-EXEC.
      *> 
       01 INTERNAL-VARS.
         05 ACC-CNT                  PIC 9(04).
         05 WS-PAGE-NUMBER           PIC 9(05).

       01 FLAGS.
         05 FG-HAS-CUSTID            PIC X VALUE 'N'.
           88 FG-HAS-CUSTID-Y        VALUE 'Y'.
           88 FG-HAS-CUSTID-N        VALUE 'N'.     

       01 CONSTANTS.
         05 K-PAGE-SIZE              PIC 9(02) VALUE 10.  
       COPY LOGGERINTERFACE.
      * 
       LINKAGE SECTION.
       COPY ACCINTERFACE.
      ******************************************************************
       PROCEDURE DIVISION USING ACCDB-INTERFACE.
      ******************************************************************
       MAIN SECTION.
           PERFORM F-INIT

           IF ACCDB-STATUS-OK
             PERFORM F-PROCESS-REQUEST
           END-IF

           PERFORM F-FINISH
           .
       MAIN-END.
           GOBACK.
      ****************************************************************** 
       F-INIT SECTION.
           SET ACCDB-STATUS-OK                 TO TRUE
           INITIALIZE ACCDB-OUT
      *
           PERFORM UT-LOG-INIT
           MOVE PGM-ID                         TO U-LOG-RUNNABLE-PROG
           MOVE ACCDB-IN                       TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-START
      *    
           PERFORM DB-INIT-CONNECTION
           IF FG-CONNECT-N
             SET ACCDB-STATUS-ERROR-CONN       TO TRUE
             PERFORM UT-LOG-DBCONNECT-ERR
           END-IF
      * 
           .
       F-INIT-END.
           EXIT.     
      ******************************************************************
       F-FINISH SECTION.
      *
           IF NOT U-SQL-OK 
           AND ACCDB-STATUS-OK
             PERFORM DB-ERROR-DEFAULT
           END-IF
      *    
           IF NOT ACCDB-STATUS-OK
             SET FG-DB-COMMIT-N        TO TRUE
           END-IF
           PERFORM DB-FINISH
      *    
           MOVE PGM-ID                 TO U-LOG-RUNNABLE-PROG
           MOVE ACCDB-OUT              TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-FINISH
           .
       F-FINISH-END.
           EXIT.
      ******************************************************************
       F-PROCESS-REQUEST SECTION.
      *
           EVALUATE TRUE
             WHEN I-ACC-OP-PUT
               PERFORM ACC-PUT
             WHEN I-ACC-OP-GETLIST
               PERFORM ACC-GET-LIST
             WHEN I-ACC-OP-GETITEM
               IF I-ACC-ACCOUNTID <> ZEROS
                 PERFORM ACC-GET-ITEM
               END-IF
               IF I-ACC-IBAN <> SPACES
                 PERFORM ACC-GET-ITEM-BY-IBAN
               END-IF
             WHEN I-ACC-OP-POST
               PERFORM ACC-POST
             WHEN I-ACC-OP-DELETE
               PERFORM ACC-DELETE
           END-EVALUATE
           .
      *
       F-PROCESS-REQUEST-END.
           EXIT.
      ******************************************************************
       ACC-GET-ITEM SECTION.
           MOVE I-ACC-ACCOUNTID          TO H-ACC-ID
      *>   
           PERFORM LOG-ACC-GET-ITEM
           
           PERFORM DB-SE-ACC1

           MOVE 1                        TO ACC-CNT
           MOVE ACC-CNT                  TO O-ACC-COUNT
           MOVE H-ACC-ID                 TO O-ACC-ACCOUNTID(ACC-CNT)
           MOVE H-ACC-IBAN               TO O-ACC-IBAN(ACC-CNT)
           MOVE H-ACC-BALANCE            TO O-ACC-BALANCE(ACC-CNT)
           MOVE H-ACC-CUSTOMERID         TO O-ACC-CUSTOMERID(ACC-CNT)
           .
       ACC-GET-ITEM-END.
           EXIT.
      ******************************************************************
       ACC-GET-ITEM-BY-IBAN SECTION.
           MOVE I-ACC-IBAN        TO H-ACC-IBAN
      *>   
           PERFORM LOG-ACC-GET-ITEM-BY-IBAN
           
           PERFORM DB-SE-ACC2

           MOVE 1                 TO ACC-CNT
           MOVE ACC-CNT           TO O-ACC-COUNT
           MOVE H-ACC-ID          TO O-ACC-ACCOUNTID(ACC-CNT)
           MOVE H-ACC-BALANCE     TO O-ACC-BALANCE(ACC-CNT)
           MOVE H-ACC-CUSTOMERID  TO O-ACC-CUSTOMERID(ACC-CNT)
           MOVE H-ACC-CURRENCY    TO O-ACC-CURRENCY(ACC-CNT)

           IF U-SQL-NO-DATA
             SET ACCDB-STATUS-NOT-FOUND-ERR TO TRUE
           END-IF
           .
       ACC-GET-ITEM-BY-IBAN-END.
           EXIT.
      ******************************************************************
       ACC-GET-LIST SECTION.
           IF I-ACC-CUSTOMERID NOT = SPACES
             MOVE I-ACC-CUSTOMERID       TO H-ACC-CUSTOMERID
             PERFORM LOG-ACC-GET-ITEM
             SET FG-HAS-CUSTID-Y         TO TRUE
           ELSE
             PERFORM LOG-ACC-GET-ALLACCS
           END-IF

           MOVE I-ACC-PAGE-NUMBER        TO WS-PAGE-NUMBER
           IF WS-PAGE-NUMBER < 1
             MOVE 1                      TO WS-PAGE-NUMBER
           END-IF

           COMPUTE H-OFFSET   = (WS-PAGE-NUMBER - 1) * K-PAGE-SIZE
           MOVE K-PAGE-SIZE              TO H-PAGE-SIZE
           
           PERFORM LOG-OFFSET-CALC

           PERFORM DB-OP-ACC-LIST

           IF NOT U-SQL-OK 
             EXIT SECTION
           END-IF

           MOVE 0                        TO ACC-CNT
           PERFORM DB-FE-ACC-LIST

           PERFORM UNTIL NOT U-SQL-OK
           OR ACC-CNT >= K-PAGE-SIZE

             ADD 1                       TO ACC-CNT
             MOVE H-ACC-ID               TO O-ACC-ACCOUNTID(ACC-CNT)
             MOVE H-ACC-BALANCE          TO O-ACC-BALANCE(ACC-CNT)
             MOVE H-ACC-CUSTOMERID       TO O-ACC-CUSTOMERID(ACC-CNT)
             MOVE H-ACC-IBAN             TO O-ACC-IBAN(ACC-CNT)
             MOVE H-ACC-CURRENCY         TO O-ACC-CURRENCY(ACC-CNT)

             PERFORM DB-FE-ACC-LIST
           END-PERFORM
      *    
           MOVE ACC-CNT                  TO O-ACC-COUNT
           PERFORM LOG-SELECTED-RECORDS-CNT
      *    
           IF U-SQL-OK 
           OR U-SQL-NO-DATA
             PERFORM DB-CL-ACC-LIST
           END-IF
           .
       ACC-GET-LIST-END.
           EXIT.
      ******************************************************************
       ACC-DELETE SECTION.
           MOVE I-ACC-ACCOUNTID    TO H-ACC-ID

           PERFORM LOG-ACC-DELETE
           PERFORM DB-DE-ACC1
      *    
           IF U-SQL-OK
             ADD 1                 TO ACC-CNT
             MOVE ACC-CNT          TO O-ACC-COUNT
             MOVE H-ACC-ID         TO O-ACC-ACCOUNTID(1)
           END-IF
           .
       ACC-DELETE-END.
           EXIT. 
      ******************************************************************
       ACC-PUT SECTION.
           MOVE I-ACC-ACCOUNTID    TO H-ACC-ID
           MOVE I-ACC-BALANCE      TO H-ACC-BALANCE
  
           PERFORM LOG-ACC-PUT

           PERFORM DB-UP-ACC1
      *    
           IF U-SQL-OK
             ADD 1                 TO ACC-CNT
             MOVE ACC-CNT          TO O-ACC-COUNT
             MOVE H-ACC-ID         TO O-ACC-ACCOUNTID(1)
             MOVE H-ACC-BALANCE    TO O-ACC-BALANCE(1)
           END-IF
           .
       ACC-PUT-END.
           EXIT.
      ******************************************************************
       ACC-POST SECTION.
           MOVE I-ACC-CUSTOMERID   TO H-ACC-CUSTOMERID
           MOVE I-ACC-BALANCE      TO H-ACC-BALANCE
           MOVE I-ACC-IBAN         TO H-ACC-IBAN
           MOVE I-ACC-CURRENCY     TO H-ACC-CURRENCY

           PERFORM LOG-ACC-POST

           PERFORM DB-INS-ACC1

           IF U-SQL-OK
             ADD 1                 TO ACC-CNT
             MOVE ACC-CNT          TO O-ACC-COUNT
             MOVE H-ACC-CUSTOMERID TO O-ACC-CUSTOMERID(1)
             MOVE H-ACC-ID         TO O-ACC-ACCOUNTID(1)
             MOVE H-ACC-BALANCE    TO O-ACC-BALANCE(1)
             MOVE H-ACC-IBAN       TO O-ACC-IBAN(1)
             MOVE H-ACC-CURRENCY   TO O-ACC-CURRENCY(1)
           END-IF
           .
       ACC-POST-END.
           EXIT. 
      ******************************************************************
       DB-INS-ACC1 SECTION.
      *
           EXEC SQL
             INSERT INTO ACCOUNT (CUSTOMERID, BALANCE,
             IBAN, CURRENCY)
             VALUES (:H-ACC-CUSTOMERID, :H-ACC-BALANCE,
             :H-ACC-IBAN, :H-ACC-CURRENCY)
           END-EXEC
      *    
           PERFORM DB-SQL-DEFAULT
      *    
           IF U-SQL-OK
             EXEC SQL
               SELECT lastval() INTO :H-ACC-ID
             END-EXEC
             PERFORM DB-SQL-DEFAULT
           END-IF
           .
      *
       DB-INS-ACC1-END.
           EXIT. 
      ******************************************************************
       DB-UP-ACC1 SECTION.
      *
           EXEC SQL
             UPDATE ACCOUNT
             SET BALANCE = :H-ACC-BALANCE
             WHERE ID = :H-ACC-ID
           END-EXEC
      *    
           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-UP-ACC1-END.
           EXIT. 
      ******************************************************************
       DB-DE-ACC1 SECTION.
      *
           EXEC SQL
             DELETE FROM ACCOUNT
             WHERE ID = :H-ACC-ID
           END-EXEC
      *    
           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-DE-ACC1-END.
           EXIT.
      ******************************************************************
       DB-OP-ACC-LIST SECTION.
       
           IF FG-HAS-CUSTID-Y
             EXEC SQL
               DECLARE CURS_ACC_LIST_CUSTID CURSOR FOR
                 SELECT ID, CUSTOMERID, BALANCE, IBAN, CURRENCY
                 FROM ACCOUNT
                 WHERE CUSTOMERID = :H-ACC-CUSTOMERID
                 ORDER BY ID
                 LIMIT :H-PAGE-SIZE OFFSET :H-OFFSET
             END-EXEC
  
             EXEC SQL
               OPEN CURS_ACC_LIST_CUSTID
             END-EXEC
           ELSE
             EXEC SQL
             DECLARE CURS_ACC_LIST CURSOR FOR
               SELECT ID, CUSTOMERID, BALANCE, IBAN, CURRENCY
                 FROM ACCOUNT
                 ORDER BY ID
                 LIMIT :H-PAGE-SIZE OFFSET :H-OFFSET
             END-EXEC
  
             EXEC SQL
               OPEN CURS_ACC_LIST
             END-EXEC
           END-IF

           PERFORM DB-SQL-DEFAULT
           .
       DB-OP-ACC-LIST-END.
           EXIT.
      ******************************************************************
       DB-FE-ACC-LIST SECTION.
           IF FG-HAS-CUSTID-Y
             EXEC SQL
               FETCH CURS_ACC_LIST_CUSTID
               INTO :H-ACC-ID,
                    :H-ACC-CUSTOMERID,
                    :H-ACC-BALANCE,
                    :H-ACC-IBAN,
                    :H-ACC-CURRENCY
             END-EXEC
           ELSE
             EXEC SQL
               FETCH CURS_ACC_LIST
               INTO :H-ACC-ID,
                    :H-ACC-CUSTOMERID,
                    :H-ACC-BALANCE,
                    :H-ACC-IBAN,
                    :H-ACC-CURRENCY
             END-EXEC
           END-IF
           PERFORM DB-SQL-DEFAULT
           .
       DB-FE-ACC-LIST-END.
           EXIT.
      ******************************************************************
       DB-CL-ACC-LIST SECTION.
           IF FG-HAS-CUSTID-Y
             EXEC SQL
               CLOSE CURS_ACC_LIST_CUSTID
             END-EXEC
           ELSE
             EXEC SQL
               CLOSE CURS_ACC_LIST
             END-EXEC
           END-IF

           PERFORM DB-SQL-DEFAULT
           .
       DB-CL-ACC-LIST-END.
           EXIT.
      ******************************************************************
       DB-SE-ACC1 SECTION.
      *
           EXEC SQL
             SELECT ID, BALANCE, CUSTOMERID, IBAN
               INTO :H-ACC-ID,
                    :H-ACC-BALANCE,
                    :H-ACC-CUSTOMERID,
                    :H-ACC-IBAN
             FROM ACCOUNT
             WHERE ID = :H-ACC-ID
           END-EXEC
      *    
           PERFORM DB-SQL-DEFAULT
           .
       DB-SE-ACC1-END.
           EXIT.      
      ******************************************************************
       DB-SE-ACC2 SECTION.
      *
           EXEC SQL
             SELECT ID, BALANCE, CUSTOMERID, CURRENCY 
               INTO :H-ACC-ID,
                    :H-ACC-BALANCE,
                    :H-ACC-CUSTOMERID,
                    :H-ACC-CURRENCY
             FROM ACCOUNT
             WHERE IBAN = :H-ACC-IBAN
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-SE-ACC2-END.
           EXIT. 
      ******************************************************************
       DB-ERROR-DEFAULT SECTION.
           SET ACCDB-STATUS-SQL-ERROR TO TRUE
           MOVE U-SQLCODE             TO O-ACCDB-SQLCODE
           .
       DB-ERROR-DEFAULT-END.
           EXIT.
      ******************************************************************
       LOG-ACC-POST SECTION.
           STRING
             "Creating new account: ",
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "CUSTID              : ",
             H-ACC-CUSTOMERID
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "IBAN                : ",
             H-ACC-IBAN
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      
           STRING
             "CURRENCY            : ",
             H-ACC-CURRENCY
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "BALANCE             : ",
             H-ACC-BALANCE
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           PERFORM UT-LOG-MULTI-LINE
           .
      *
       LOG-ACC-POST-END.
           EXIT. 
      ******************************************************************
       LOG-ACC-DELETE SECTION.
      *
           STRING
             "Deleting account with ID: ",
             H-ACC-ID
             INTO U-LOG-LINE
           END-STRING

           PERFORM UT-LOG-SINGLE-LINE
           .
      *
       LOG-ACC-DELETE-END.
           EXIT. 
      ******************************************************************
       LOG-ACC-PUT SECTION.
           STRING
             "Updating account ID: ",
             H-ACC-ID
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      * 
           STRING
             "New balance        : ",
             H-ACC-BALANCE
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           PERFORM UT-LOG-MULTI-LINE
           .
       LOG-ACC-PUT-END.
           EXIT. 
      ******************************************************************
       LOG-ACC-GET-LIST SECTION.
           STRING
             "Retrieving accounts for CUSTID: ",
             H-ACC-CUSTOMERID
             INTO U-LOG-LINE
           END-STRING
      *     
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-ACC-GET-LIST-END.
           EXIT. 
      ******************************************************************
       LOG-ACC-GET-ITEM SECTION.
           STRING
             "Retrieving accounts for ID: ",
             H-ACC-ID
             INTO U-LOG-LINE
           END-STRING
      * 
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-ACC-GET-ITEM-END.
           EXIT. 
      ******************************************************************
       LOG-ACC-GET-ITEM-BY-IBAN SECTION.
           STRING
             "Retrieving account with IBAN: ",
             H-ACC-IBAN
             INTO U-LOG-LINE
           END-STRING
      * 
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-ACC-GET-ITEM-BY-IBAN-END.
           EXIT. 
      ******************************************************************
       LOG-SELECTED-RECORDS-CNT SECTION.
           STRING
             "SELECTED: ",
             O-ACC-COUNT,
             " RECORDS"
             INTO U-LOG-LINE
           END-STRING
      * 
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-SELECTED-RECORDS-CNT-END.
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
       LOG-ACC-GET-ALLACCS SECTION.
           MOVE "Retrieving all accounts..." TO U-LOG-LINE
      * 
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-TRANS-GET-ALLTRANS-END.
           EXIT.         
      ******************************************************************
      *   COPY DBUTILSECTIONS TO USE GENERIC SECTIONS FOR DB CONNECTION
      ******************************************************************
       EXEC SQL INCLUDE DBUTILSECTIONS END-EXEC.
      ******************************************************************     
      *   COPY LOGGERUTILSECTIONS TO USE GENERIC SECTIONS FOR LOGGER
      ****************************************************************** 
       COPY LOGGERUTILSECTIONS.      
       
  