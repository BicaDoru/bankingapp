      ******************************************************************
       IDENTIFICATION          DIVISION.
      ******************************************************************
       PROGRAM-ID.             CUSTDB.
      ******************************************************************
       DATA                    DIVISION.
      ******************************************************************
       WORKING-STORAGE         SECTION.
       01 PGM-ID                      PIC X(20) 
                                      VALUE "CUSTDB              ".
       01 PROGNAME                    PIC X(20).
       01 INTERNAL-VARS.
         05 WS-CUST-CNT               PIC  9(04).
         05 WS-PAGE-NUMBER            PIC 9(05).

       01 CONSTANTS.
         05 K-PAGE-SIZE               PIC 9(02) VALUE 10.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 H-OFFSET                    PIC S9(9).
       01 H-PAGE-SIZE                 PIC S9(9).

       EXEC SQL INCLUDE H-CUSTOMER    END-EXEC.
       EXEC SQL INCLUDE DBUTILSVARS   END-EXEC.

       EXEC SQL END DECLARE SECTION   END-EXEC.

       EXEC SQL INCLUDE SQLCA         END-EXEC.

       COPY LOGGERINTERFACE.
      * 
       LINKAGE SECTION.
       COPY CUSTINTERFACE.
      ******************************************************************
       PROCEDURE DIVISION USING CUSTDB-INTERFACE.
       INITIALIZE CUSTOMER-TABLE.
      ******************************************************************
       MAIN SECTION.
      *    
           PERFORM F-INIT
      *     
           IF CUST-STATUS-OK
             PERFORM F-PROCESS-REQUEST
           END-IF
      *    
           PERFORM F-FINISH
           .
       MAIN-END.
           GOBACK.
      ****************************************************************** 
       F-INIT SECTION.
           SET CUST-STATUS-OK                  TO TRUE
           INITIALIZE CUSTDB-OUT
      *    
           PERFORM UT-LOG-INIT
           MOVE PGM-ID                         TO U-LOG-RUNNABLE-PROG
           MOVE CUSTDB-IN                      TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-START
      *    
           PERFORM DB-INIT-CONNECTION
           IF FG-CONNECT-N
             SET CUST-STATUS-ERROR-CONN        TO TRUE
             PERFORM UT-LOG-DBCONNECT-ERR
           END-IF
      * 
           .
       F-INIT-END.
           EXIT.     
      ******************************************************************
       F-FINISH SECTION.
           IF  NOT U-SQL-OK 
           AND CUST-STATUS-OK
             PERFORM DB-ERROR-DEFAULT
           END-IF
      *    
           IF NOT CUST-STATUS-OK
             SET FG-DB-COMMIT-N        TO TRUE
           END-IF
           PERFORM DB-FINISH
      *    
           MOVE PGM-ID                 TO U-LOG-RUNNABLE-PROG
           MOVE CUSTDB-OUT             TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-FINISH
           .
       F-FINISH-END.
           EXIT.
      ******************************************************************
       F-PROCESS-REQUEST SECTION.
      *
           EVALUATE TRUE
             WHEN I-CUST-OP-GET-LIST 
               PERFORM CUST-GET-LIST
             WHEN I-CUST-OP-GET-ITEM
               PERFORM CUST-GET-ITEM
             WHEN I-CUST-OP-POST
               PERFORM CUST-POST-ITEM
             WHEN I-CUST-OP-DELETE
               PERFORM CUST-DELETE-ITEM
             WHEN I-CUST-OP-PUT
               PERFORM CUST-PUT-ITEM
           END-EVALUATE
           .
      *
       F-PROCESS-REQUEST-END.
           EXIT. 
      ******************************************************************
       CUST-GET-ITEM SECTION.
           PERFORM LOG-CUST-GET-ITEM

           MOVE I-CUST-CUSTID         TO H-CUST-ID
           PERFORM DB-SE-CUST1

           IF CUST-STATUS-OK  
             MOVE 1                   TO WS-CUST-CNT
             MOVE WS-CUST-CNT         TO O-CUST-COUNT
             MOVE H-CUST-ID           TO O-CUST-ID(WS-CUST-CNT)
             MOVE H-CUST-USERNAME     TO O-CUST-USERNAME(WS-CUST-CNT)
             MOVE H-CUST-ADDRESS      TO O-CUST-ADDRESS(WS-CUST-CNT)
             MOVE H-CUST-BANKUSERID   TO O-CUST-BANKUSERID(WS-CUST-CNT)
           END-IF

           IF U-SQL-NO-DATA
             SET CUST-STATUS-NOT-FOUND-ERR 
                                      TO TRUE
           END-IF
           .
       CUST-GET-ITEM-END.
           EXIT.
      ******************************************************************
       CUST-GET-LIST SECTION.
           MOVE I-CUST-PAGE-NUMBER    TO WS-PAGE-NUMBER
       
           IF WS-PAGE-NUMBER < 1
             MOVE 1                   TO WS-PAGE-NUMBER
           END-IF
       
           COMPUTE H-OFFSET = (WS-PAGE-NUMBER - 1) * K-PAGE-SIZE
           MOVE K-PAGE-SIZE           TO H-PAGE-SIZE
       
           PERFORM DB-OP-CUR-LIST
           IF NOT U-SQL-OK 
             EXIT SECTION 
           END-IF
       
           MOVE 0                     TO WS-CUST-CNT
       
           PERFORM DB-FE-CUR-LIST
           PERFORM UNTIL NOT U-SQL-OK
           OR WS-CUST-CNT >= K-PAGE-SIZE
             ADD 1                    TO WS-CUST-CNT
             MOVE H-CUST-ID           TO O-CUST-ID(WS-CUST-CNT)
             MOVE H-CUST-USERNAME     TO O-CUST-USERNAME(WS-CUST-CNT)
             MOVE H-CUST-ADDRESS      TO O-CUST-ADDRESS(WS-CUST-CNT)
             MOVE H-CUST-BANKUSERID   TO O-CUST-BANKUSERID(WS-CUST-CNT)
             PERFORM DB-FE-CUR-LIST
           END-PERFORM

           MOVE WS-CUST-CNT           TO O-CUST-COUNT
           PERFORM LOG-SELECTED-RECORDS-CNT

      *>   CLOSE CURSOR
           IF U-SQL-OK 
           OR U-SQL-NO-DATA
             PERFORM  DB-CL-CUR-LIST
           END-IF
           .
       CUST-GET-LIST-END.
           EXIT.
      ******************************************************************
       CUST-POST-ITEM SECTION.
           MOVE I-CUST-USERNAME      TO H-CUST-USERNAME
           MOVE I-CUST-ADDRESS       TO H-CUST-ADDRESS
           MOVE I-CUST-BANKUSERID    TO H-CUST-BANKUSERID
           PERFORM LOG-CUST-POST-ITEM

      *>   Perform the INSERT
           PERFORM DB-INS-CUST1

           IF CUST-STATUS-OK
             SET O-CUST-COUNT        TO 1
             MOVE H-CUST-ID          TO O-CUST-ID(1)
             MOVE H-CUST-USERNAME    TO O-CUST-USERNAME(1)
             MOVE H-CUST-ADDRESS     TO O-CUST-ADDRESS(1)
             MOVE H-CUST-BANKUSERID  TO O-CUST-BANKUSERID(1)
           END-IF
           .
       CUST-POST-ITEM-END.
           EXIT.
      ******************************************************************
       CUST-DELETE-ITEM SECTION.
           MOVE I-CUST-CUSTID      TO H-CUST-ID
           PERFORM LOG-CUST-DELETE

           *> SELECT for generic output
           PERFORM DB-SE-CUST1

           PERFORM DB-DE-CUST1
       
           IF CUST-STATUS-OK
             SET O-CUST-COUNT      TO 1
             MOVE H-CUST-ID        TO O-CUST-ID(1)
             MOVE H-CUST-USERNAME  TO O-CUST-USERNAME(1)
             MOVE H-CUST-ADDRESS   TO O-CUST-ADDRESS(1)
             MOVE H-CUST-BANKUSERID    
                                   TO O-CUST-BANKUSERID(1)
           END-IF
           .
       CUST-DELETE-ITEM-END.
           EXIT.
      ******************************************************************
       CUST-PUT-ITEM SECTION.
           MOVE I-CUST-CUSTID                TO H-CUST-ID
           MOVE I-CUST-USERNAME              TO H-CUST-USERNAME
           MOVE I-CUST-ADDRESS               TO H-CUST-ADDRESS
           
           PERFORM LOG-CUST-PUT-ITEM
      *>   Perform the UPDATE
           PERFORM DB-UP-CUST1

           IF CUST-STATUS-OK
             SET O-CUST-COUNT                TO 1
             MOVE H-CUST-ID                  TO O-CUST-ID(1)
             MOVE H-CUST-USERNAME            TO O-CUST-USERNAME(1)
             MOVE H-CUST-ADDRESS             TO O-CUST-ADDRESS(1)
           END-IF
           .
       CUST-PUT-ITEM-END.
           EXIT.
      ******************************************************************
       DB-OP-CUR-LIST SECTION.
           EXEC SQL
             DECLARE CURS_CUST_LIST CURSOR FOR
             SELECT ID, USERNAME, ADDRESS, BANKUSERID
             FROM CUSTOMER
             ORDER BY ID
             OFFSET :H-OFFSET ROWS
             FETCH FIRST :H-PAGE-SIZE ROWS ONLY
           END-EXEC
       
           EXEC SQL 
             OPEN CURS_CUST_LIST 
           END-EXEC
       
           PERFORM DB-SQL-DEFAULT
           .
       DB-OP-CUR-LIST-END. 
           EXIT.
      ******************************************************************
       DB-FE-CUR-LIST SECTION.
           EXEC SQL
             FETCH CURS_CUST_LIST 
             INTO :H-CUST-ID,
                  :H-CUST-USERNAME,
                  :H-CUST-ADDRESS,
                  :H-CUST-BANKUSERID
           END-EXEC
           PERFORM DB-SQL-DEFAULT
           .
       DB-FE-CUR-LIST-END. 
           EXIT.
      ******************************************************************
       DB-CL-CUR-LIST SECTION.
           EXEC SQL 
             CLOSE CURS_CUST_LIST 
           END-EXEC
           PERFORM DB-SQL-DEFAULT
           .
       DB-CL-CUR-LIST-END.
           EXIT. 
      ******************************************************************
       DB-UP-CUST1 SECTION.
      *
           EXEC SQL
             UPDATE CUSTOMER
             SET USERNAME = :H-CUST-USERNAME,
                 ADDRESS  = :H-CUST-ADDRESS
                 WHERE ID = :H-CUST-ID
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-UP-CUST1-END.
           EXIT. 
      ******************************************************************
       DB-DE-CUST1 SECTION.
      *
           EXEC SQL
             DELETE FROM CUSTOMER
             WHERE ID = :H-CUST-ID
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-DE-CUST1-END.
           EXIT. 
      ******************************************************************
       DB-INS-CUST1 SECTION.
      *
           EXEC SQL
             INSERT INTO CUSTOMER 
             (USERNAME, ADDRESS, BANKUSERID)
             VALUES (:H-CUST-USERNAME, 
                     :H-CUST-ADDRESS,
                     :H-CUST-BANKUSERID)
           END-EXEC
           PERFORM DB-SQL-DEFAULT

           EXEC SQL
             SELECT lastval() INTO :H-CUST-ID 
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-INS-CUST1-END.
           EXIT. 
      ******************************************************************
       DB-SE-CUST1 SECTION.
      *
           EXEC SQL 
             SELECT ID, USERNAME, ADDRESS, BANKUSERID 
               INTO :H-CUST-ID,
                    :H-CUST-USERNAME,
                    :H-CUST-ADDRESS,
                    :H-CUST-BANKUSERID
             FROM CUSTOMER
             WHERE ID = :H-CUST-ID
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
       DB-SE-CUST1-END.
           EXIT.

      ******************************************************************    
       DB-ERROR-DEFAULT SECTION.
           SET CUST-STATUS-SQL-ERROR TO TRUE
           MOVE U-SQLCODE            TO O-CUSTDB-SQLCODE
           .
       DB-ERROR-DEFAULT-END.
           EXIT.
      ******************************************************************
       LOG-SELECTED-RECORDS-CNT SECTION.
           STRING
             "SELECTED: ",
             O-CUST-COUNT,
             " RECORDS"
             INTO U-LOG-LINE
           END-STRING
      *            
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-SELECTED-RECORDS-CNT-END.
           EXIT. 
      ******************************************************************
       LOG-CUST-GET-ITEM SECTION.
           STRING
             'Retrieving customer with CUSTID: ',
             I-CUST-CUSTID
             INTO U-LOG-LINE
           END-STRING
      *     
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-CUST-GET-ITEM-END.
           EXIT. 
      ******************************************************************
       LOG-CUST-POST-ITEM SECTION.
           MOVE "Inserting customer..."        TO U-LOG-LINE
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "Username           : ",
             H-CUST-USERNAME
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "Address            : ",
             H-CUST-ADDRESS
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "BankUserID             : ",
             H-CUST-BANKUSERID
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           PERFORM UT-LOG-MULTI-LINE
           .
       LOG-CUST-POST-ITEM-END.
           EXIT.
      ******************************************************************
       LOG-CUST-DELETE SECTION.
           MOVE "Deleting customer..."         TO U-LOG-LINE
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "Customer ID        : ",
             H-CUST-ID
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           PERFORM UT-LOG-MULTI-LINE
           .
       LOG-CUST-DELETE-END.
           EXIT.
      ******************************************************************
       LOG-CUST-PUT-ITEM SECTION.
           MOVE "Updating customer..."        TO U-LOG-LINE
           PERFORM UT-LOG-ADD-LINE
      * 
           STRING
             "Customer ID        : ",
             H-CUST-ID
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      * 
           STRING
             "Username           : ",
             H-CUST-USERNAME
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "Address            : ",
             H-CUST-ADDRESS
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *              
           PERFORM UT-LOG-MULTI-LINE
           .
       LOG-CUST-PUT-ITEM-END.
           EXIT.
      ******************************************************************
      *   COPY DBUTILSECTIONS TO USE GENERIC SECTIONS FOR DB CONNECTION
      ******************************************************************
       EXEC SQL INCLUDE DBUTILSECTIONS END-EXEC.
      ******************************************************************     
      *   COPY LOGGERUTILSECTIONS TO USE GENERIC SECTIONS FOR LOGGER
      ****************************************************************** 
       COPY LOGGERUTILSECTIONS.     

  