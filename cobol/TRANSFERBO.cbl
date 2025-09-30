      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID. TRANSFERBO.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
      *
       01 PGM-ID                         PIC X(20) 
                                         VALUE "TRANSFERBO          ".
       01 PROGNAME                       PIC X(20).
         88 PGNAME-ERROR                 VALUE "ERROR               ".
         88 PGNAME-ACCDB                 VALUE "ACCDB               ".
         88 PGNAME-CUSTDB                VALUE "CUSTDB              ".
         88 PGNAME-BUSRDB                VALUE "BUSRDB              ".
         88 PGNAME-TRANSFERDB            VALUE "TRANSFERDB          ".
       
       01 INTERNAL-VARS.
         05 WS-ERROR-SOURCE              PIC X(20).

         05 WS-TIME-RAW                  PIC 9(08).
         05 WS-TIME REDEFINES WS-TIME-RAW.
           10 WS-TIME-HH                 PIC X(02).
           10 WS-TIME-MM                 PIC X(02).
           10 WS-TIME-SS                 PIC X(02).
           10 WS-TIME-TT                 PIC X(02).
         05 WS-DATE-RAW                  PIC 9(08).
         05 WS-DATE REDEFINES WS-DATE-RAW.
           10 WS-DATE-YYYY.
             15 WS-DATE-CC               PIC X(02).
             15 WS-DATE-YY               PIC X(02).
           10 WS-DATE-MM                 PIC X(02).
           10 WS-DATE-TT                 PIC X(02).
         05 WS-TIMESTAMP.
           10 T-DATE.
             15 T-YEAR                   PIC X(04).
             15 T-L1                     PIC X(01) VALUE "-".
             15 T-MONTH                  PIC X(02).
             15 T-L2                     PIC X(01) VALUE "-".
             15 T-DAY                    PIC X(02).
           10 T-L3                       PIC X(01) VALUE " ".
           10 T-TIME.
             15 T-HOUR                   PIC X(02).
             15 T-L4                     PIC X(01) VALUE ":".
             15 T-MIN                    PIC X(02).
             15 T-L5                     PIC X(01) VALUE ":".
             15 T-SEC                    PIC X(02).
             15 T-ZONE                   PIC X(03) VALUE "+00".

      *  DATA MOVED AROUND BETWEEN THE CALLS OF THE MODULES
         05 WS-BUFFER.
           10 WS-BF-TRANSFER-ID          PIC 9(05).
           10 WS-BF-NEW-BALANCE          PIC 9(08)V99.
           10 WS-BF-ACC-ID               PIC 9(05).
           10 WS-BF-ACC-BALANCE          PIC 9(08)V99.
           10 WS-BF-BUSR-ID              PIC 9(05).
           10 WS-BF-CUST-ID              PIC 9(05).
           10 WS-BF-ROLE                 PIC X(04).
           10 WS-BF-ACC-IBAN1            PIC X(30).
           10 WS-BF-ACC-IBAN2            PIC X(30).
           10 WS-BF-ACC-CURRENCY1        PIC X(3).
           10 WS-BF-ACC-CURRENCY2        PIC X(3).
           10 WS-BF-ACC-BALANCE1         PIC 9(08)V99.
           10 WS-BF-ACC-BALANCE2         PIC 9(08)V99.
           10 WS-BF-ACC-ID1              PIC 9(05).
           10 WS-BF-ACC-ID2              PIC 9(05).  
           10 WS-BF-AMOUNT               PIC 9(08)V99.

      *  RELEVANT DATA FOR THE USER ON WHICH THE ACTION IS PERFORMED
         05 WS-TARGET-USER.
           10 WS-TG-BUSR-ID              PIC 9(05).
           10 WS-TG-ROLE                 PIC X(04).
             88 WS-TG-ROLE-TELLER        VALUE "BaTe".
             88 WS-TG-ROLE-CLIENT        VALUE "BaCl".
             88 WS-TG-ROLE-ADMIN         VALUE "BaAd".

       01 FLAGS.
         05 FG-ROLLBACK-TRANSFER         PIC X.
           88 FG-ROLLBACK-TRANSFER-Y     VALUE 'Y'.
           88 FG-ROLLBACK-TRANSFER-N     VALUE 'N'.
         05 FG-ROLLBACK-SRCACC           PIC X.
           88 FG-ROLLBACK-SRCACC-Y       VALUE 'Y'.
           88 FG-ROLLBACK-SRCACC-N       VALUE 'N'.

       01 INDEXES.
         05 IND-1                        PIC 9(03).


      ******************************************************************
      *                        COPYLIB IMPORTS 
      ******************************************************************   
       COPY ERRINTERFACE.
       COPY LOGGERINTERFACE.
       COPY DBUTILSVARS.
       COPY ACCINTERFACE.
       COPY CUSTINTERFACE.
       COPY BUSRINTERFACE.
       COPY TRANSFERINTERFACE.
      *
       LINKAGE SECTION.
       COPY DISPINTERFACE.
      ******************************************************************
       PROCEDURE DIVISION USING BY REFERENCE DISPATCHER-INTERFACE.
      ******************************************************************
       MAIN SECTION.
           PERFORM F-INIT
           PERFORM F-PROCESS-REQUEST
           PERFORM F-FINISH
           .
       MAIN-END.    
           GOBACK.
      ****************************************************************** 
       F-INIT SECTION.
           PERFORM UT-LOG-INIT
           MOVE PGM-ID                 TO U-LOG-RUNNABLE-PROG
           MOVE DISPATCHER-IN          TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-START
      * 
           SET O-DISP-ERR-OK           TO TRUE
           MOVE SPACE                  TO O-DISP-ERROR-MESSAGE

           SET FG-ROLLBACK-TRANSFER-N  TO TRUE
           SET FG-ROLLBACK-SRCACC-N    TO TRUE
           INITIALIZE ERROR-INTERFACE
           .
       F-INIT-END.
           EXIT.
      ******************************************************************
       F-FINISH SECTION.
           IF NOT O-DISP-ERR-OK
             PERFORM F-HANDLE-ERROR
           END-IF

           MOVE PGM-ID                 TO U-LOG-RUNNABLE-PROG
           MOVE O-DISP-DATA-OUT        TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-FINISH
           .
       F-FINISH-END.
           EXIT.
      ******************************************************************
       F-PROCESS-REQUEST SECTION.
           EVALUATE TRUE ALSO TRUE
             WHEN I-DISP-METHOD-POST ALSO I-DISP-OBJ-TRANSFER
               PERFORM F-CREATE-TRANSFER
             WHEN I-DISP-METHOD-GET  ALSO I-DISP-OBJ-TRANSFER
               PERFORM F-READ-TRANSFER-LIST
             WHEN OTHER
               SET O-DISP-ERR-TRANSFER-BAD-METHOD TO TRUE
               MOVE 1                   TO I-ERR-PARAM-COUNT 
               MOVE I-DISP-METHOD       TO I-ERR-PARAM (1)
           END-EVALUATE
           .
       F-PROCESS-REQUEST-END.
           EXIT.
      ******************************************************************
       F-HANDLE-ERROR SECTION.
           MOVE O-DISP-ERROR-NO         TO I-ERR-CODE

           SET PGNAME-ERROR             TO TRUE
           CALL PROGNAME USING ERROR-INTERFACE

           IF WS-ERROR-SOURCE = SPACE
             MOVE PGM-ID                TO WS-ERROR-SOURCE
           END-IF

           STRING FUNCTION TRIM(WS-ERROR-SOURCE) ": "
                  FUNCTION TRIM(O-ERR-MESSAGE)
             DELIMITED BY SIZE
             INTO O-DISP-ERROR-MESSAGE
           .
       F-HANDLE-ERROR-END.
           EXIT.
      ******************************************************************
       F-READ-TRANSFER-LIST SECTION.
           MOVE SPACE                        TO WS-BF-ACC-IBAN1

           PERFORM CHECK-READ-TRANSFER-LIST-PERMISSIONS

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           *> IF BOTH ACCID AND CUSTID PROVIDED, FILTER ONLY BY ACCID
           IF I-GET-TRANSFER-ACCID NOT = ZEROES 
             AND I-GET-TRANSFER-CUSTID NOT = ZEROES
             MOVE ZEROES                     TO I-GET-TRANSFER-CUSTID
           END-IF

           INITIALIZE TRANSFERDB-INTERFACE
           SET I-TRANSFER-OP-GETLIST         TO TRUE
           MOVE I-GET-TRANSFER-PAGE-NUMBER   TO I-TRANSFER-PAGE-NUMBER
           MOVE WS-BF-ACC-IBAN1              TO I-TRANSFER-FILTER-IBAN
           MOVE I-GET-TRANSFER-CUSTID        TO I-TRANSFER-FILTER-CUSTID
           SET  PGNAME-TRANSFERDB            TO TRUE
           CALL PROGNAME USING TRANSFERDB-INTERFACE

           EVALUATE TRUE
             WHEN TRANSFERDB-STATUS-OK
               MOVE O-TRANSFER-COUNT  TO O-GET-TRANSFER-COUNT
               PERFORM VARYING IND-1 FROM 1 BY 1 UNTIL 
                 IND-1 > O-TRANSFER-COUNT
                 MOVE O-TRANSFER-SRCIBAN(IND-1)  
                                      TO WS-BF-ACC-IBAN1
                 PERFORM UT-GET-ACCID-FROM-IBAN
                 MOVE WS-BF-ACC-ID    TO O-GET-TRANSFER-SRCACCID(IND-1)
                 MOVE O-TRANSFER-SRCIBAN(IND-1)  
                                      TO O-GET-TRANSFER-SRCIBAN(IND-1)
                 MOVE O-TRANSFER-DESTIBAN(IND-1) 
                                      TO WS-BF-ACC-IBAN1
                 PERFORM UT-GET-ACCID-FROM-IBAN
                 MOVE WS-BF-ACC-ID    TO O-GET-TRANSFER-DESTACCID(IND-1)
                 MOVE O-TRANSFER-DESTIBAN(IND-1)
                                      TO O-GET-TRANSFER-DESTIBAN(IND-1)
                 MOVE O-TRANSFER-ID(IND-1) 
                                      TO O-GET-TRANSFER-ID(IND-1)
                 MOVE O-TRANSFER-AMOUNT(IND-1) 
                                      TO O-GET-TRANSFER-AMOUNT(IND-1)
                 MOVE O-TRANSFER-TIMESTAMP(IND-1) 
                                      TO O-GET-TRANSFER-TIMESTAMP(IND-1)
                 MOVE O-TRANSFER-CURRENCY(IND-1)  
                                      TO O-GET-TRANSFER-CURRENCY(IND-1)
               END-PERFORM
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL  TO TRUE
           END-EVALUATE
           .
       F-READ-TRANSFER-LIST-END.
           EXIT.
      ******************************************************************
       F-CREATE-TRANSFER SECTION.
      *
           *> GET THE DESTINATION OWNER ROLE
           MOVE I-POST-TRANSFER-SRCIBAN    TO WS-BF-ACC-IBAN1
           PERFORM UT-GET-USER-DATA
          
           IF O-DISP-ERR-OK
             MOVE WS-BF-BUSR-ID            TO WS-TG-BUSR-ID
             MOVE WS-BF-ROLE               TO WS-TG-ROLE
             PERFORM UT-DEFAULT-PERMISSION-CHECK
           END-IF

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           *> CHECK ACCOUNTS HAVE THE SAME CURRENCY
           MOVE I-POST-TRANSFER-SRCIBAN    TO WS-BF-ACC-IBAN1
           MOVE I-POST-TRANSFER-DESTIBAN   TO WS-BF-ACC-IBAN2
           PERFORM UT-CURRENCY-MATCH

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF
           
           *> CHECK ACC1 BALANCE IS VALID
           MOVE I-POST-TRANSFER-AMOUNT     TO WS-BF-AMOUNT
           PERFORM CHECK-BALANCE

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           *> POST TRANSFER
           INITIALIZE TRANSFERDB-INTERFACE
           SET I-TRANSFER-OP-POST          TO TRUE
           
           PERFORM UT-GET-TIMESTAMP
           MOVE WS-TIMESTAMP               TO I-TRANSFER-TIMESTAMP

           MOVE I-POST-TRANSFER-SRCIBAN    TO I-TRANSFER-SRCIBAN
           MOVE I-POST-TRANSFER-DESTIBAN   TO I-TRANSFER-DESTIBAN
           MOVE I-POST-TRANSFER-AMOUNT     TO I-TRANSFER-AMOUNT
           MOVE WS-BF-ACC-CURRENCY1        TO I-TRANSFER-CURRENCY

           SET PGNAME-TRANSFERDB           TO TRUE
           CALL PROGNAME USING TRANSFERDB-INTERFACE

           EVALUATE TRUE
             WHEN TRANSFERDB-STATUS-OK
               MOVE O-TRANSFER-ELEM(1)     TO O-DISP-POST-TRANSFER
             WHEN TRANSFERDB-STATUS-SQL-ERR
               SET O-DISP-ERR-DB-SQL       TO TRUE
           END-EVALUATE

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           *> UPDATE ACC1 AND ACC2 BALANCE
           PERFORM F-TRANSFER-ACC-BALANCE-UPDATE

           IF NOT O-DISP-ERR-OK
             PERFORM ROLLBACK-TRANSFER-POST
           END-IF
           .
      *
       F-CREATE-TRANSFER-END.
           EXIT. 
      ******************************************************************
       F-DELETE-TRANSFER SECTION.
      *
           INITIALIZE TRANSFERDB-INTERFACE
           SET I-TRANSFER-OP-DELETE        TO TRUE
           MOVE WS-BF-TRANSFER-ID          TO I-TRANSFER-ID
       
           SET PGNAME-TRANSFERDB           TO TRUE
           CALL PROGNAME USING TRANSFERDB-INTERFACE

           EVALUATE TRUE
             WHEN TRANSFERDB-STATUS-OK
               CONTINUE  *> ONLY FOR ROLLBACK SO NO OUTPUT
             WHEN TRANSFERDB-STATUS-SQL-ERR
               SET O-DISP-ERR-DB-SQL       TO TRUE
           END-EVALUATE
           .
      *
       F-DELETE-TRANSFER-END.
           EXIT. 
      ******************************************************************
       F-TRANSFER-ACC-BALANCE-UPDATE SECTION.
      *
           MOVE WS-BF-ACC-ID1              TO WS-BF-ACC-ID
           COMPUTE WS-BF-NEW-BALANCE = 
                   WS-BF-ACC-BALANCE1 - O-POST-TRANSFER-AMOUNT
           MOVE WS-BF-NEW-BALANCE          TO WS-BF-ACC-BALANCE
           PERFORM UT-ACC-PUT

           IF ACCDB-STATUS-SQL-ERROR
             SET FG-ROLLBACK-TRANSFER-Y    TO TRUE
             SET O-DISP-ERR-DB-SQL         TO TRUE
           END-IF

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           MOVE WS-BF-ACC-ID2              TO WS-BF-ACC-ID
           COMPUTE WS-BF-NEW-BALANCE = 
                   WS-BF-ACC-BALANCE2 + O-POST-TRANSFER-AMOUNT
           MOVE WS-BF-NEW-BALANCE          TO WS-BF-ACC-BALANCE
           PERFORM UT-ACC-PUT

           IF ACCDB-STATUS-SQL-ERROR
             SET FG-ROLLBACK-TRANSFER-Y    TO TRUE
             SET FG-ROLLBACK-SRCACC-Y      TO TRUE
             SET O-DISP-ERR-DB-SQL         TO TRUE
           END-IF
           .
      *
       F-TRANSFER-ACC-BALANCE-UPDATE-END.
           EXIT. 
      ******************************************************************
       ROLLBACK-TRANSFER-POST SECTION.
      *
           IF FG-ROLLBACK-TRANSFER-Y
             MOVE O-TRANSFER-ID(1)   TO WS-BF-TRANSFER-ID
             PERFORM F-DELETE-TRANSFER
           END-IF

           IF FG-ROLLBACK-SRCACC-Y
             MOVE WS-BF-ACC-ID2      TO WS-BF-ACC-ID
             MOVE WS-BF-ACC-BALANCE1 TO WS-BF-ACC-BALANCE    
             PERFORM UT-ACC-PUT
           END-IF
           .
      *
       ROLLBACK-TRANSFER-POST-END.
           EXIT. 
      ******************************************************************
       CHECK-BALANCE SECTION.
      *
           IF WS-BF-AMOUNT > WS-BF-ACC-BALANCE1
             SET O-DISP-ERR-TRANSFER-LOW-BALANCE TO TRUE
           END-IF
           .
      *
       CHECK-BALANCE-END.
           EXIT. 
      ******************************************************************
       CHECK-READ-TRANSFER-LIST-PERMISSIONS SECTION.
      *
           *> CHECK PERMISSION FROM ACCID
           IF I-GET-TRANSFER-ACCID NOT = ZEROES
             MOVE I-GET-TRANSFER-ACCID       TO WS-BF-ACC-ID
             PERFORM UT-GET-IBAN-FROM-ACCID

             IF NOT O-DISP-ERR-OK
               EXIT SECTION
             END-IF

             PERFORM UT-GET-USER-DATA
             MOVE WS-BF-BUSR-ID              TO WS-TG-BUSR-ID
             MOVE WS-BF-ROLE                 TO WS-TG-ROLE
             PERFORM UT-DEFAULT-PERMISSION-CHECK

           ELSE 
             *> CHECK PERMISSION FROM CUSTID
             IF I-GET-TRANSFER-CUSTID NOT = ZEROES
               MOVE I-GET-TRANSFER-CUSTID    TO WS-BF-CUST-ID
               PERFORM UT-GET-BUSRID-FROM-CUSTID

               IF O-DISP-ERR-OK
                 PERFORM UT-GET-ROLE
                 MOVE WS-BF-BUSR-ID          TO WS-TG-BUSR-ID
                 MOVE WS-BF-ROLE             TO WS-TG-ROLE
                 PERFORM UT-DEFAULT-PERMISSION-CHECK
               END-IF

             ELSE
               *> IF ACCID AND CUSTID NOT PROVIDED, CHECK ADMIN ROLE
               IF NOT U-DISP-LOGIN-ADMIN
                 SET O-DISP-ERR-AUTH-ROLE-FORBIDDEN
                                             TO TRUE
                 MOVE 1                      TO I-ERR-PARAM-COUNT
                 MOVE U-DISP-LOGIN-ROLE      TO I-ERR-PARAM (1)
               END-IF
             END-IF
           END-IF
           .
      *
       CHECK-READ-TRANSFER-LIST-PERMISSIONS-END.
           EXIT. 
      ******************************************************************
       UT-CURRENCY-MATCH SECTION.
      *    GET CURRENCY FOR SRC
           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-GETITEM                    TO TRUE
           MOVE WS-BF-ACC-IBAN1                    TO I-ACC-IBAN
      *         
           SET PGNAME-ACCDB                        TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE     
      *         
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE O-ACC-CURRENCY(1)              TO 
                                                   WS-BF-ACC-CURRENCY1
               MOVE O-ACC-BALANCE(1)               TO 
                                                   WS-BF-ACC-BALANCE1
               MOVE O-ACC-ACCOUNTID(1)             TO WS-BF-ACC-ID1
             WHEN ACCDB-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND    TO TRUE
             WHEN ACCDB-STATUS-SQL-ERROR
               SET O-DISP-ERR-DB-SQL               TO TRUE
           END-EVALUATE

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

      *    GET CURRENCY FOR DEST
           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-GETITEM                    TO TRUE
           MOVE WS-BF-ACC-IBAN2                    TO I-ACC-IBAN
      *         
           SET PGNAME-ACCDB                        TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE     
      *         
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE O-ACC-CURRENCY(1)              TO 
                                                   WS-BF-ACC-CURRENCY2
               MOVE O-ACC-BALANCE(1)               TO 
                                                   WS-BF-ACC-BALANCE2
               MOVE O-ACC-ACCOUNTID(1)             TO WS-BF-ACC-ID2
             WHEN ACCDB-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND    TO TRUE
             *> // TODO DB99(general) is thrown now, when new error 
             *> handling comes it should be DB90(item not found)
             WHEN ACCDB-STATUS-SQL-ERROR
               SET O-DISP-ERR-DB-SQL               TO TRUE
           END-EVALUATE

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           IF WS-BF-ACC-CURRENCY1 <> WS-BF-ACC-CURRENCY2
             SET O-DISP-ERR-TRANSFER-CURR-MISMATCH TO TRUE
             MOVE 2                                TO I-ERR-PARAM-COUNT
             MOVE WS-BF-ACC-CURRENCY1              TO I-ERR-PARAM (1)
             MOVE WS-BF-ACC-CURRENCY2              TO I-ERR-PARAM (2)
           END-IF
           .
      *
       UT-CURRENCY-MATCH-END.
           EXIT. 
      ******************************************************************
       UT-GET-CUSTID-FROM-IBAN SECTION.
      *
           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-GETITEM                 TO TRUE
           MOVE WS-BF-ACC-IBAN1                 TO I-ACC-IBAN
      *      
           SET PGNAME-ACCDB                     TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE
      *    
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE O-ACC-CUSTOMERID(1)         TO WS-BF-CUST-ID
             WHEN ACCDB-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND TO TRUE
             WHEN ACCDB-STATUS-SQL-ERROR
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE

           .
      *
       UT-GET-CUSTID-FROM-IBAN-END.
           EXIT.
      ******************************************************************
       UT-ACC-PUT SECTION.
      *
           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-PUT              TO TRUE
           MOVE WS-BF-ACC-ID             TO I-ACC-ACCOUNTID
           MOVE WS-BF-ACC-BALANCE        TO I-ACC-BALANCE

           SET PGNAME-ACCDB              TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE
      
           IF ACCDB-STATUS-SQL-ERROR
             SET O-DISP-ERR-DB-SQL       TO TRUE
           END-IF
           .
      *
       UT-ACC-PUT-END.
           EXIT. 
      ******************************************************************
       UT-GET-BUSRID-FROM-CUSTID SECTION.
      *
           INITIALIZE CUSTDB-INTERFACE
           MOVE WS-BF-CUST-ID                   TO I-CUST-CUSTID
           SET I-CUST-OP-GET-ITEM               TO TRUE
      *      
           SET PGNAME-CUSTDB                    TO TRUE
           CALL PROGNAME USING CUSTDB-INTERFACE
      *    
           EVALUATE TRUE
             WHEN CUST-STATUS-OK
               MOVE O-CUST-BANKUSERID (1)       TO WS-BF-BUSR-ID
             WHEN CUST-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND TO TRUE
             WHEN CUST-STATUS-SQL-ERROR
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE

           .
      *
       UT-GET-BUSRID-FROM-CUSTID-END.
           EXIT. 
      ******************************************************************
       UT-GET-ROLE SECTION.
           INITIALIZE BUSRDB-INTERFACE
           SET I-BUSR-OP-GETROLE                TO TRUE
           MOVE WS-BF-BUSR-ID                   TO I-BUSR-ID
      *    
           SET PGNAME-BUSRDB                    TO TRUE
           CALL PROGNAME USING BUSRDB-INTERFACE
      *    
           EVALUATE TRUE
             WHEN BUSRDB-STATUS-OK
               MOVE O-BUSR-ROLE                 TO WS-BF-ROLE
             WHEN BUSRDB-STATUS-SQL-ERROR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND TO TRUE
             WHEN BUSRDB-STATUS-BAD-USERNAME
               SET O-DISP-ERR-BUSR-BAD-USERNAME TO TRUE
             WHEN BUSRDB-STATUS-BAD-PASSWORD
               SET O-DISP-ERR-BUSR-BAD-PASSWORD TO TRUE
           END-EVALUATE

           .
       UT-GET-ROLE-END.
           EXIT. 
      ******************************************************************
       UT-DEFAULT-PERMISSION-CHECK SECTION.
      *
           EVALUATE TRUE
             WHEN U-DISP-LOGIN-ADMIN
      *        Always allowed
               CONTINUE 
      *        
             WHEN U-DISP-LOGIN-TELLER
      *        Teller can only act on CLIENT accounts or self
               EVALUATE TRUE
               WHEN WS-TG-ROLE-ADMIN
                 SET O-DISP-ERR-AUTH-TLR-TO-ADMIN  TO TRUE
               WHEN WS-TG-ROLE-TELLER
                 IF NOT WS-TG-BUSR-ID = U-DISP-LOGIN-ID
                   SET O-DISP-ERR-AUTH-TLR-TO-TLR  TO TRUE
                 END-IF
               WHEN WS-TG-ROLE-CLIENT
                 CONTINUE
               WHEN OTHER
                 SET O-DISP-ERR-AUTH-TLR-TO-NONE   TO TRUE
                 MOVE 1                            TO I-ERR-PARAM-COUNT
                 MOVE WS-TG-ROLE                   TO I-ERR-PARAM (1)
               END-EVALUATE
      *        
             WHEN U-DISP-LOGIN-CLIENT
      *        Client can only act on their own accounts
               IF NOT WS-TG-BUSR-ID = U-DISP-LOGIN-ID
                 SET O-DISP-ERR-AUTH-CLT-TO-OTHER  TO TRUE
               END-IF
             WHEN OTHER
               SET O-DISP-ERR-AUTH-ROLE-FORBIDDEN  TO TRUE
               MOVE 1                              TO I-ERR-PARAM-COUNT
               MOVE U-DISP-LOGIN-ROLE              TO I-ERR-PARAM (1)
           END-EVALUATE
           .
      *
       UT-DEFAULT-PERMISSION-CHECK-END.
           EXIT. 
      ******************************************************************
       UT-GET-USER-DATA SECTION.
      *
           PERFORM UT-GET-CUSTID-FROM-IBAN

           IF O-DISP-ERR-OK
             PERFORM UT-GET-BUSRID-FROM-CUSTID
           END-IF

           IF O-DISP-ERR-OK
             PERFORM UT-GET-ROLE
           END-IF
           .
      *
       UT-GET-USER-DATA-END.
           EXIT. 
      ******************************************************************
       UT-GET-TIMESTAMP SECTION.
      *
           ACCEPT WS-TIME-RAW FROM TIME
           ACCEPT WS-DATE-RAW FROM DATE
           MOVE '20'                       TO WS-DATE-CC
           MOVE WS-DATE-YYYY               TO T-YEAR
           MOVE WS-DATE-MM                 TO T-MONTH
           MOVE WS-DATE-TT                 TO T-DAY
           MOVE WS-TIME-HH                 TO T-HOUR
           MOVE WS-TIME-MM                 TO T-MIN
           MOVE WS-TIME-SS                 TO T-SEC
           .
      *
       UT-GET-TIMESTAMP-END.
           EXIT. 
      ******************************************************************
       UT-GET-IBAN-FROM-ACCID SECTION.
      *
           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-GETITEM                 TO TRUE
           MOVE WS-BF-ACC-ID                    TO I-ACC-ACCOUNTID
      *
           SET PGNAME-ACCDB                     TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE
      *
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE O-ACC-IBAN(1)               TO WS-BF-ACC-IBAN1
             WHEN ACCDB-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND TO TRUE
             WHEN ACCDB-STATUS-SQL-ERROR
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE
           .
       UT-GET-IBAN-FROM-ACCID-END.
           EXIT.
      ******************************************************************
       UT-GET-ACCID-FROM-IBAN SECTION.
           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-GETITEM                 TO TRUE
           MOVE WS-BF-ACC-IBAN1                 TO I-ACC-IBAN
      *
           SET PGNAME-ACCDB                     TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE
      *
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE O-ACC-ACCOUNTID(1)          TO WS-BF-ACC-ID
             WHEN ACCDB-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND TO TRUE
             WHEN ACCDB-STATUS-SQL-ERROR
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE
           .
       UT-GET-ACCID-FROM-IBAN-END.
           EXIT.
      ******************************************************************
      *   COPY LOGGERUTILSECTIONS TO USE GENERIC SECTIONS FOR LOGGER
      ****************************************************************** 
       COPY LOGGERUTILSECTIONS.

