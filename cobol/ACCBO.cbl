      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID. ACCBO.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
      ******************************************************************
       DATA                        DIVISION.

      ******************************************************************
       WORKING-STORAGE             SECTION.


       01 PGM-ID                         PIC X(20) 
                                         VALUE "ACCBO               ".
       01 PROGNAME                       PIC X(20).
         88 PGNAME-ACCDB                 VALUE "ACCDB               ".
         88 PGNAME-TRANSDB               VALUE "TRANSDB             ".
         88 PGNAME-ERROR                 VALUE "ERROR               ".
         88 PGNAME-BUSRDB                VALUE "BUSRDB              ".
         88 PGNAME-CUSTDB                VALUE "CUSTDB              ".

       01 INTERNAL-VARS.
         05 WS-ERROR-SOURCE              PIC X(20).
         05 WS-AMOUNT-SUM                PIC S9(08)V99.
         05 WS-AMOUNT-CURRENT            PIC S9(08)V99.
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
         05 WS-PAGE-NUMBER               PIC 9(05).
         05 WS-VALIDATED-PAGE-NUMBER     PIC 9(05).
         05 WS-PARAM-BUFFER              PIC X(50).
         05 WS-AMOUNT-EDITED             PIC +ZZZZZZZ9.99.
         05 WS-ACTING-ROLE               PIC X(04).
         05 WS-ACCID                     PIC 9(05).
         05 WS-PAGE-TEXT                 PIC X(10).
         05 WS-PAGE-NUMBER-TMP           PIC 9(05).

         05 WS-ACC-OWNER-CUSTID          PIC 9(05).
         05 WS-ACC-OWNER-BUSR-ROLE       PIC X(04).
           88 WS-ACC-OWNER-TELLER        VALUE "BaTe".
           88 WS-ACC-OWNER-CLIENT        VALUE "BaCl".
           88 WS-ACC-OWNER-ADMIN         VALUE "BaAd".
         05 WS-CLIENT-CUSTID             PIC 9(05) VALUE 0.
      *  
      *  DATA MOVED AROUND BETWEEN THE CALLS OF THE MODULES
         05 WS-BUFFER.
           10 WS-BF-BUSR-ID              PIC 9(05).
           10 WS-BF-CUST-ID              PIC 9(05).
           10 WS-BF-ACC-ID               PIC 9(05).
           10 WS-BF-ROLE                 PIC X(04).
           10 WS-DATE-LEN                PIC 9(04) VALUE 0.
      *  RELEVANT VARIABLES FOR CHECKING THE DATE
         05 DATE-CHECK-VARS.
           10 WS-YEAR-N                  PIC 9(04).
           10 WS-MONTH-N                 PIC 9(02).
           10 WS-DAY-N                   PIC 9(02).
           10 WS-MOD4                    PIC 9.
           10 WS-MOD100                  PIC 9.
           10 WS-MOD400                  PIC 9.
           10 WS-MAX-DAY                 PIC 9(02).
      *  
      *  RELEVANT DATA FOR THE USER ON WHICH THE ACTION IS PERFORMED
         05 WS-TARGET-USER.
           10 WS-TG-BUSR-ID              PIC 9(05).
           10 WS-TG-CUST-ID              PIC 9(05).
           10 WS-TG-ACC-ID               PIC 9(05).
           10 WS-TG-ROLE                 PIC X(04).
             88 WS-TG-ROLE-TELLER        VALUE "BaTe".
             88 WS-TG-ROLE-CLIENT        VALUE "BaCl".
             88 WS-TG-ROLE-ADMIN         VALUE "BaAd".
           
       01 FLAGS.
         05 FG-ACTION-PERMITTED-STATUS   PIC X VALUE 'N'.
           88 FG-ACTION-PERMITTED-Y            VALUE 'Y'.
           88 FG-ACTION-PERMITTED-N            VALUE 'N'.

         05 FG-CUST-LOOP-FOUND           PIC X VALUE 'N'.
           88 FG-CUST-FOUND-Y                  VALUE 'Y'.
           88 FG-CUST-FOUND-N                  VALUE 'N'.

       01 INDEXES.
         05 IND-1                        PIC 9(03).
      ******************************************************************
      *                        COPYLIB IMPORTS 
      ****************************************************************** 
       COPY TRANSINTERFACE.
       COPY ACCINTERFACE.
       COPY ERRINTERFACE.
       COPY LOGGERINTERFACE.
       COPY CUSTINTERFACE.
       COPY BUSRINTERFACE.
       COPY DBUTILSVARS.
      * 
       LINKAGE SECTION.
       COPY DISPINTERFACE.
 
      ******************************************************************
       PROCEDURE DIVISION USING DISPATCHER-INTERFACE.
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
           MOVE PGM-ID                        TO U-LOG-RUNNABLE-PROG
           MOVE DISPATCHER-IN                 TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-START
      * 
           SET O-DISP-ERR-OK                  TO TRUE
           MOVE SPACE                         TO O-DISP-ERROR-MESSAGE
           INITIALIZE ERROR-INTERFACE
           .
       F-INIT-END.
           EXIT.
      ******************************************************************
       F-FINISH SECTION.
      *
           IF NOT O-DISP-ERR-OK
             PERFORM F-HANDLE-ERROR
           END-IF

           MOVE PGM-ID                        TO U-LOG-RUNNABLE-PROG
           MOVE O-DISP-DATA-OUT               TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-FINISH
           .
       F-FINISH-END.
           EXIT.
      ******************************************************************
       F-PROCESS-REQUEST SECTION.
           EVALUATE TRUE ALSO TRUE
             WHEN I-DISP-METHOD-GET    ALSO I-DISP-OBJ-TRANS
               PERFORM F-READ-TRANS-LIST
             WHEN I-DISP-METHOD-GET    ALSO I-DISP-OBJ-ACC-LIST
               PERFORM F-READ-ACC-LIST
             WHEN I-DISP-METHOD-POST   ALSO I-DISP-OBJ-TRANS
               PERFORM F-PROCESS-TRANS-WRITE
             WHEN I-DISP-METHOD-POST   ALSO I-DISP-OBJ-ACC-ITEM
               PERFORM F-CREATE-ACC
             WHEN I-DISP-METHOD-PUT    ALSO I-DISP-OBJ-ACC-ITEM
               PERFORM F-UPDATE-ACC
             WHEN I-DISP-METHOD-DELETE ALSO I-DISP-OBJ-ACC-ITEM
               PERFORM F-DELETE-ACC
           END-EVALUATE
           .
       F-PROCESS-REQUEST-END.
           EXIT.
      ******************************************************************
       F-READ-TRANS-LIST SECTION.
           IF I-GET-TRANS-ACCID NOT = SPACES
             MOVE I-GET-TRANS-ACCID          TO WS-TG-ACC-ID
             PERFORM UT-GET-TARGET-USER-DATA-FROM-ACC-ID
             IF O-DISP-ERR-OK
               PERFORM UT-DEFAULT-PERMISSION-CHECK
             END-IF
           ELSE
             IF NOT U-DISP-LOGIN-ADMIN
               SET O-DISP-ERR-AUTH-ROLE-FORBIDDEN 
                                             TO TRUE
               MOVE 1                        TO I-ERR-PARAM-COUNT
               MOVE U-DISP-LOGIN-ROLE        TO I-ERR-PARAM (1) 
             END-IF
           END-IF

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           PERFORM F-VALIDATE-DATE-YYYYMMDD
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           IF I-GET-TRANS-DATE NOT = SPACES AND WS-DATE-LEN >= 10
             MOVE SPACES                     TO I-GET-TRANS-DATE
             MOVE WS-PARAM-BUFFER(1:10)      TO I-GET-TRANS-DATE(1:10)
           END-IF

           MOVE I-GET-TRANS-PAGE-NUMBER      TO WS-PAGE-NUMBER
           PERFORM F-VALIDATE-AND-SET-PAGE-NUMBER
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF
           IF I-GET-TRANS-DATE NOT = SPACES
             MOVE 1                          TO WS-VALIDATED-PAGE-NUMBER
           END-IF

           INITIALIZE TRANSDB-INTERFACE
           SET I-TRANS-OP-GET-LIST           TO TRUE

           EVALUATE TRUE
             WHEN I-GET-TRANS-DATE NOT = SPACES 
                                   AND I-GET-TRANS-ACCID NOT = SPACES
               MOVE I-GET-TRANS-ACCID        TO I-TRANS-ACCOUNTID
               MOVE I-GET-TRANS-DATE(1:10)   TO I-TRANS-TIMESTAMP
             WHEN I-GET-TRANS-DATE NOT = SPACES 
                                   AND I-GET-TRANS-ACCID     = SPACES
               MOVE ZEROES                   TO I-TRANS-ACCOUNTID
               MOVE I-GET-TRANS-DATE(1:10)   TO I-TRANS-TIMESTAMP
             WHEN I-GET-TRANS-DATE = SPACES 
                                   AND I-GET-TRANS-ACCID NOT = SPACES
               MOVE I-GET-TRANS-ACCID        TO I-TRANS-ACCOUNTID
               MOVE SPACES                   TO I-TRANS-TIMESTAMP
             WHEN OTHER
               MOVE ZEROES                   TO I-TRANS-ACCOUNTID
               MOVE SPACES                   TO I-TRANS-TIMESTAMP
           END-EVALUATE
           MOVE WS-VALIDATED-PAGE-NUMBER     TO I-TRANS-PAGE-NUMBER

           SET PGNAME-TRANSDB                TO TRUE
           CALL PROGNAME USING TRANSDB-INTERFACE

           EVALUATE TRUE
             WHEN TRANSDB-STATUS-OK
               MOVE O-TRANSDB-COUNT     TO O-GET-TRANS-COUNT
               PERFORM VARYING IND-1 FROM 1 BY 1 
                                     UNTIL IND-1 > O-TRANSDB-COUNT
                 MOVE O-TRANS-ACCOUNTID(IND-1) 
                                        TO O-GET-TRANS-ACCOUNTID(IND-1)
                 MOVE O-TRANS-AMMOUNT(IND-1)
                                        TO O-GET-TRANS-AMOUNT(IND-1)
                 MOVE O-TRANS-TIMESTAMP(IND-1) 
                                        TO O-GET-TRANS-TIMESTAMP(IND-1)
                 MOVE O-TRANS-TRANS-TYPE(IND-1) 
                                        TO O-GET-TRANS-TRANS-TYPE(IND-1)
                 MOVE O-TRANS-ACCBALANCE(IND-1) 
                                        TO O-GET-TRANS-BALANCE(IND-1)
               END-PERFORM
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL    TO TRUE
           END-EVALUATE
           .
       F-READ-TRANS-LIST-END.
           EXIT.
      ******************************************************************
       F-VALIDATE-DATE-YYYYMMDD SECTION.

           IF I-GET-TRANS-DATE = SPACES
             EXIT SECTION
           END-IF

           MOVE SPACES                                TO WS-PARAM-BUFFER
           MOVE I-GET-TRANS-DATE                      TO WS-PARAM-BUFFER
           INSPECT WS-PARAM-BUFFER
             REPLACING ALL X'09' BY SPACE
                       ALL X'0D' BY SPACE
                       ALL X'0A' BY SPACE
                       ALL '/'   BY '-'
                       ALL '.'   BY '-'
           MOVE FUNCTION TRIM(WS-PARAM-BUFFER)        TO WS-PARAM-BUFFER
           MOVE FUNCTION LENGTH(WS-PARAM-BUFFER)      TO WS-DATE-LEN

           PERFORM CHECK-DATE
           .
       F-VALIDATE-DATE-YYYYMMDD-END.
           EXIT.
      ******************************************************************
       CHECK-DATE SECTION.
           IF WS-DATE-LEN < 10
             SET O-DISP-ERR-TRANS-BAD-DATE-FORMAT     TO TRUE
             EXIT SECTION
           END-IF

           IF WS-PARAM-BUFFER(5:1) NOT = "-"
             SET O-DISP-ERR-TRANS-BAD-DATE-FORMAT     TO TRUE
             EXIT SECTION
           END-IF
           IF WS-PARAM-BUFFER(8:1) NOT = "-"
             SET O-DISP-ERR-TRANS-BAD-DATE-FORMAT     TO TRUE
             EXIT SECTION
           END-IF

           IF NOT WS-PARAM-BUFFER(1:4) IS NUMERIC
             SET O-DISP-ERR-TRANS-DATA-NOT-NUMERIC    TO TRUE
             EXIT SECTION
           END-IF
           IF NOT WS-PARAM-BUFFER(6:2) IS NUMERIC
             SET O-DISP-ERR-TRANS-DATA-NOT-NUMERIC    TO TRUE
             EXIT SECTION
           END-IF
           IF NOT WS-PARAM-BUFFER(9:2) IS NUMERIC
             SET O-DISP-ERR-TRANS-DATA-NOT-NUMERIC    TO TRUE
             EXIT SECTION
           END-IF

           MOVE FUNCTION NUMVAL(WS-PARAM-BUFFER(1:4)) TO WS-YEAR-N
           MOVE FUNCTION NUMVAL(WS-PARAM-BUFFER(6:2)) TO WS-MONTH-N
           MOVE FUNCTION NUMVAL(WS-PARAM-BUFFER(9:2)) TO WS-DAY-N

           IF WS-MONTH-N < 1 OR WS-MONTH-N > 12
             SET O-DISP-ERR-TRANS-INVALID-MONTH       TO TRUE
             EXIT SECTION
           END-IF

           COMPUTE WS-MOD4   = FUNCTION MOD(WS-YEAR-N, 4)
           COMPUTE WS-MOD100 = FUNCTION MOD(WS-YEAR-N, 100)
           COMPUTE WS-MOD400 = FUNCTION MOD(WS-YEAR-N, 400)

           EVALUATE WS-MONTH-N
             WHEN 1  WHEN 3  WHEN 5  WHEN 7  WHEN 8  WHEN 10 WHEN 12
               MOVE 31                                TO WS-MAX-DAY
             WHEN 4  WHEN 6  WHEN 9  WHEN 11
               MOVE 30                                TO WS-MAX-DAY
             WHEN 2
               IF WS-MOD4 = 0 AND WS-MOD100 NOT = 0
                 MOVE 29                              TO WS-MAX-DAY
               ELSE
                 IF WS-MOD400 = 0
                   MOVE 29                            TO WS-MAX-DAY
                 ELSE
                   MOVE 28                            TO WS-MAX-DAY
                 END-IF
               END-IF
           END-EVALUATE

           IF WS-DAY-N < 1 OR WS-DAY-N > WS-MAX-DAY
             SET O-DISP-ERR-TRANS-INVALID-DAY         TO TRUE
             EXIT SECTION
           END-IF
           .
       CHECK-DATE-END.
           EXIT.
      ******************************************************************
       F-READ-ACC-LIST SECTION.
      *   
           IF I-GET-ACC-CUSTID NOT = SPACES
             MOVE I-GET-ACC-CUSTID            TO WS-BF-CUST-ID
             PERFORM UT-GET-BUSRID-FROM-CUSTID
             IF O-DISP-ERR-OK
               PERFORM UT-GET-ROLE
             END-IF
           END-IF

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           MOVE WS-BF-CUST-ID                 TO WS-TG-CUST-ID
           MOVE WS-BF-BUSR-ID                 TO WS-TG-BUSR-ID
           MOVE WS-BF-ROLE                    TO WS-TG-ROLE

           PERFORM UT-DEFAULT-PERMISSION-CHECK
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF
           MOVE I-GET-ACC-PAGE-NUMBER         TO WS-PAGE-NUMBER
           PERFORM F-VALIDATE-AND-SET-PAGE-NUMBER
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-GETLIST               TO TRUE
           MOVE I-GET-ACC-CUSTID              TO I-ACC-CUSTOMERID
           MOVE WS-VALIDATED-PAGE-NUMBER                
                                              TO I-ACC-PAGE-NUMBER
           MOVE WS-VALIDATED-PAGE-NUMBER                
                                              TO I-ACC-PAGE-NUMBER
           SET PGNAME-ACCDB                   TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE
           
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE O-ACC-COUNT               TO O-GET-ACC-COUNT
               PERFORM VARYING IND-1 FROM 1 BY 1
                 UNTIL IND-1 > O-ACC-COUNT
                 MOVE O-ACC-ACCOUNTID(IND-1)  
                                         TO O-GET-ACC-ACCOUNTID(IND-1)
                 MOVE O-ACC-CUSTOMERID(IND-1)  
                                         TO O-GET-ACC-CUSTOMERID(IND-1)
                 MOVE O-ACC-IBAN(IND-1)  TO O-GET-ACC-IBAN(IND-1)
                 MOVE O-ACC-CURRENCY(IND-1)  
                                         TO O-GET-ACC-CURRENCY(IND-1)
                 MOVE O-ACC-BALANCE(IND-1)  
                                         TO O-GET-ACC-BALANCE(IND-1)
               END-PERFORM
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL          TO TRUE
           END-EVALUATE
           
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE O-ACC-COUNT               TO O-GET-ACC-COUNT
               PERFORM VARYING IND-1 FROM 1 BY 1
                 UNTIL IND-1 > O-ACC-COUNT
                 MOVE O-ACC-ACCOUNTID(IND-1)  
                                         TO O-GET-ACC-ACCOUNTID(IND-1)
                 MOVE O-ACC-CUSTOMERID(IND-1)  
                                         TO O-GET-ACC-CUSTOMERID(IND-1)
                 MOVE O-ACC-IBAN(IND-1)  TO O-GET-ACC-IBAN(IND-1)
                 MOVE O-ACC-CURRENCY(IND-1)  
                                         TO O-GET-ACC-CURRENCY(IND-1)
                 MOVE O-ACC-BALANCE(IND-1)  
                                         TO O-GET-ACC-BALANCE(IND-1)
               END-PERFORM
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL          TO TRUE
           END-EVALUATE
           .
       F-READ-ACC-LIST-END.
           EXIT. 
      ******************************************************************
       F-DELETE-ACC SECTION.
           MOVE I-DEL-ACC-ID                  TO WS-TG-ACC-ID
           PERFORM UT-GET-TARGET-USER-DATA-FROM-ACC-ID
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           IF U-DISP-LOGIN-CLIENT
             SET O-DISP-ERR-AUTH-ROLE-FORBIDDEN 
                                              TO TRUE
             MOVE 1                           TO I-ERR-PARAM-COUNT
             MOVE U-DISP-LOGIN-ROLE           TO I-ERR-PARAM (1)                                 
             EXIT SECTION
           END-IF

           PERFORM UT-DEFAULT-PERMISSION-CHECK
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-DELETE                TO TRUE
           MOVE I-DEL-ACC-ID                  TO I-ACC-ACCOUNTID
           SET PGNAME-ACCDB                   TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE

           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE ACCDB-OUT                 TO O-DISP-DATA-OUT
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL          TO TRUE
           END-EVALUATE
           .
       F-DELETE-ACC-END.
           EXIT. 
      ******************************************************************
       F-UPDATE-ACC SECTION.

           MOVE I-PUT-ACC-ID                  TO WS-TG-ACC-ID
           PERFORM UT-GET-TARGET-USER-DATA-FROM-ACC-ID
           IF O-DISP-ERR-OK
             PERFORM UT-DEFAULT-PERMISSION-CHECK
           END-IF
           
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF
             
           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-PUT                   TO TRUE
           MOVE I-PUT-ACC-ID                  TO I-ACC-ACCOUNTID
           MOVE I-PUT-ACC-BALANCE             TO I-ACC-BALANCE
  
           SET PGNAME-ACCDB                   TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE

           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE ACCDB-OUT                 TO O-DISP-DATA-OUT
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL          TO TRUE
           END-EVALUATE
           .
       F-UPDATE-ACC-END.
           EXIT.
      ******************************************************************
       F-CREATE-ACC SECTION.
           MOVE I-POST-ACC-CUSTID             TO WS-BF-CUST-ID
           PERFORM UT-GET-BUSRID-FROM-CUSTID
           IF O-DISP-ERR-OK
             PERFORM UT-GET-ROLE
           END-IF

           IF O-DISP-ERR-OK
             MOVE WS-BF-CUST-ID               TO WS-TG-CUST-ID
             MOVE WS-BF-BUSR-ID               TO WS-TG-BUSR-ID
             MOVE WS-BF-ROLE                  TO WS-TG-ROLE
  
             PERFORM UT-DEFAULT-PERMISSION-CHECK
           END-IF

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-POST                  TO TRUE
           MOVE I-POST-ACC-CUSTID             TO I-ACC-CUSTOMERID
           MOVE I-POST-ACC-IBAN               TO I-ACC-IBAN
           MOVE I-POST-ACC-CURRENCY           TO I-ACC-CURRENCY
           MOVE 0                             TO I-ACC-BALANCE

           SET PGNAME-ACCDB                   TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE
           
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE ACCDB-OUT                 TO O-DISP-DATA-OUT
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL          TO TRUE
           END-EVALUATE
           .
       F-CREATE-ACC-END.
           EXIT. 
      ******************************************************************
       F-VALIDATE-AND-SET-PAGE-NUMBER SECTION.
           EVALUATE TRUE
             WHEN WS-PAGE-NUMBER IS NUMERIC
               IF WS-PAGE-NUMBER = 0
                 MOVE 1    TO WS-VALIDATED-PAGE-NUMBER
               ELSE
                 MOVE WS-PAGE-NUMBER 
                           TO WS-VALIDATED-PAGE-NUMBER
               END-IF
             WHEN WS-PAGE-NUMBER = SPACES
               MOVE 1      TO WS-VALIDATED-PAGE-NUMBER
             WHEN OTHER
               SET O-DISP-ERR-ACC-WRONG-PAGE-FORMAT 
                           TO TRUE
           END-EVALUATE
           .
       F-VALIDATE-AND-SET-PAGE-NUMBER-END.
           EXIT.

      *****************************************************************
       F-PROCESS-TRANS-WRITE SECTION.
      * 
           MOVE I-POST-TRANS-ACCID            TO WS-TG-ACC-ID
           PERFORM UT-GET-TARGET-USER-DATA-FROM-ACC-ID

           IF O-DISP-ERR-OK
             PERFORM UT-DEFAULT-PERMISSION-CHECK
           END-IF

           IF O-DISP-ERR-OK
             PERFORM F-PROCESS-OPERATION
           END-IF

           IF O-DISP-ERR-OK
             PERFORM F-WRITE-TRANSACTION
           END-IF

           IF O-DISP-ERR-OK
             PERFORM F-UPDATE-ACC-BALANCE
           END-IF
           .
       F-PROCESS-TRANS-WRITE-END.
           EXIT.
      ******************************************************************
       F-PROCESS-OPERATION SECTION.
           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-GETITEM               TO TRUE
           MOVE I-POST-TRANS-ACCID            TO I-ACC-ACCOUNTID
           SET PGNAME-ACCDB                   TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE
           
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE O-ACC-BALANCE(1)          TO WS-AMOUNT-CURRENT
               MOVE O-ACC-CUSTOMERID(1)       TO WS-ACC-OWNER-CUSTID
             WHEN OTHER
               EXIT SECTION
           END-EVALUATE

           EVALUATE I-POST-TRANS-TRTYPE
             WHEN 'WITHDRAW'
               IF I-POST-TRANS-AMOUNT > WS-AMOUNT-CURRENT
                 SET O-DISP-ERR-ACC-NO-FUNDS  TO TRUE
                 MOVE 2                       TO I-ERR-PARAM-COUNT 
                 MOVE WS-AMOUNT-CURRENT       TO WS-AMOUNT-EDITED
                 MOVE FUNCTION TRIM(WS-AMOUNT-EDITED)
                                              TO I-ERR-PARAM (1)
                 MOVE I-POST-TRANS-AMOUNT     TO WS-AMOUNT-EDITED
                 MOVE FUNCTION TRIM(WS-AMOUNT-EDITED)
                                              TO I-ERR-PARAM (2)
               ELSE
                 COMPUTE WS-AMOUNT-SUM =
                     WS-AMOUNT-CURRENT - I-POST-TRANS-AMOUNT
               END-IF
             WHEN "DEPOSIT"
               COMPUTE WS-AMOUNT-SUM =
                       WS-AMOUNT-CURRENT + I-POST-TRANS-AMOUNT
             WHEN OTHER
               SET O-DISP-ERR-ACC-BAD-TRTYPE  TO TRUE
               MOVE 1                         TO I-ERR-PARAM-COUNT
               MOVE I-POST-TRANS-TRTYPE       TO I-ERR-PARAM (1)
           END-EVALUATE
           .
       F-PROCESS-OPERATION-END.
           EXIT.
      ******************************************************************
       F-WRITE-TRANSACTION SECTION.
           INITIALIZE TRANSDB-INTERFACE
           SET I-TRANS-OP-POST           TO TRUE
           ACCEPT WS-TIME-RAW FROM TIME
           ACCEPT WS-DATE-RAW FROM DATE
           MOVE '20'                     TO WS-DATE-CC
           MOVE WS-DATE-YYYY             TO T-YEAR
           MOVE WS-DATE-MM               TO T-MONTH
           MOVE WS-DATE-TT               TO T-DAY
           MOVE WS-TIME-HH               TO T-HOUR
           MOVE WS-TIME-MM               TO T-MIN
           MOVE WS-TIME-SS               TO T-SEC
           MOVE WS-TIMESTAMP             TO I-TRANS-TIMESTAMP 
         
           MOVE I-POST-TRANS-ACCID       TO I-TRANS-ACCOUNTID 
           MOVE I-POST-TRANS-TRTYPE      TO I-TRANS-TRANS-TYPE
           MOVE I-POST-TRANS-AMOUNT      TO I-TRANS-AMMOUNT
           MOVE WS-AMOUNT-SUM            TO I-TRANS-ACCBALANCE
             
           SET PGNAME-TRANSDB            TO TRUE
           CALL PROGNAME USING TRANSDB-INTERFACE
       
           EVALUATE TRUE
             WHEN TRANSDB-STATUS-OK
               MOVE I-POST-TRANS-ACCID   TO O-POST-TRANS-ACCOUNTID
               MOVE I-POST-TRANS-TRTYPE  TO O-POST-TRANS-TRANS-TYPE
               MOVE I-POST-TRANS-AMOUNT  TO O-POST-TRANS-AMOUNT
               MOVE WS-TIMESTAMP         TO O-POST-TRANS-TIMESTAMP
               MOVE WS-AMOUNT-SUM        TO O-POST-TRANS-BALANCE
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL     TO TRUE
           END-EVALUATE
           .
       F-WRITE-TRANSACTION-END.
           EXIT.
      ******************************************************************
       F-UPDATE-ACC-BALANCE SECTION.
           INITIALIZE ACCDB-INTERFACE

           SET I-ACC-OP-PUT                   TO TRUE
           MOVE I-POST-TRANS-ACCID            TO I-ACC-ACCOUNTID 
           MOVE WS-AMOUNT-SUM                 TO I-ACC-BALANCE
      *
           SET PGNAME-ACCDB                   TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE
           
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE WS-AMOUNT-SUM             TO O-POST-TRANS-BALANCE
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL          TO TRUE
           END-EVALUATE
           .
       F-UPDATE-ACC-BALANCE-END.
           EXIT.
      ******************************************************************
       F-HANDLE-ERROR SECTION.
           MOVE O-DISP-ERROR-NO               TO I-ERR-CODE

           SET PGNAME-ERROR                   TO TRUE
           CALL PROGNAME USING ERROR-INTERFACE
           
           IF WS-ERROR-SOURCE = SPACE
             MOVE PGM-ID                      TO WS-ERROR-SOURCE
           END-IF
           STRING FUNCTION TRIM(WS-ERROR-SOURCE) ": "
                  FUNCTION TRIM(O-ERR-MESSAGE)
             DELIMITED BY SIZE
             INTO O-DISP-ERROR-MESSAGE
           .
       F-HANDLE-ERROR-END.
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
       UT-GET-TARGET-USER-DATA-FROM-ACC-ID SECTION.
      *
           MOVE WS-TG-ACC-ID                  TO WS-BF-ACC-ID
           PERFORM UT-GET-CUSTID-FROM-ACCID
      *
           IF O-DISP-ERR-OK
             PERFORM UT-GET-BUSRID-FROM-CUSTID
           END-IF
      *
           IF O-DISP-ERR-OK
             PERFORM UT-GET-ROLE
           END-IF
      *
           IF O-DISP-ERR-OK
             MOVE WS-BF-CUST-ID               TO WS-TG-CUST-ID
             MOVE WS-BF-BUSR-ID               TO WS-TG-BUSR-ID
             MOVE WS-BF-ROLE                  TO WS-TG-ROLE
           END-IF
           .
      *
       UT-GET-TARGET-USER-DATA-FROM-ACC-ID-END.
           EXIT. 
      ******************************************************************
       UT-GET-CUSTID-FROM-ACCID SECTION.
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
               MOVE O-ACC-CUSTOMERID(1)         TO WS-BF-CUST-ID
             WHEN ACCDB-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND TO TRUE
             WHEN ACCDB-STATUS-SQL-ERROR
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE
           .
      *
       UT-GET-CUSTID-FROM-ACCID-END.
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
      *   COPY LOGGERUTILSECTIONS TO USE GENERIC SECTIONS FOR LOGGER
      ****************************************************************** 
       COPY LOGGERUTILSECTIONS.
       