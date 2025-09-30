      ******************************************************************
       IDENTIFICATION                   DIVISION.
      ******************************************************************
       PROGRAM-ID.                      DISPATCHER.
      ******************************************************************
       ENVIRONMENT                      DIVISION.
       CONFIGURATION                    SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
      ******************************************************************
       DATA                             DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
       
       01 PGM-ID                        PIC X(20) 
                                        VALUE "DISPATCHER          ".
       01 PROGNAME                      PIC X(20).
         88 PGNAME-ACCBO                VALUE "ACCBO               ". 
         88 PGNAME-CUSTBO               VALUE "CUSTBO              ".
         88 PGNAME-BUSRBO               VALUE "BUSRBO              ".
         88 PGNAME-ERROR                VALUE "ERROR               ".
         88 PGNAME-GENERIC              VALUE "GENERIC             ".
         88 PGNAME-TRANSFERBO           VALUE "TRANSFERBO          ".
  
       01 FLAGS.
         05 FILLER                      PIC X(07).
           88 FG-LOGIN-SUCCESS          VALUE "SUCCESS".
           88 FG-LOGIN-FAILED           VALUE "FAILED ".
         05 FG-PROG-STATUS              PIC X VALUE 'Y'.
           88 FG-PROG-OK                VALUE 'Y'.
           88 FG-PROG-ERROR             VALUE 'N'.
         05 FG-FIRST-WRITE              PIC X VALUE 'Y'. 

       01 INDEXES.
         05  I1                         PIC 9(02).
         05 WS-IDX                      PIC 9(4) COMP VALUE 0.
         05 WS-FIDX                     PIC 9(4) COMP VALUE 0.

      *
       01 CONSTANTS.
         05 K-PP-HEADER                 PIC X(49) VALUE 
         "------======   OUTPUT PRETTY PRINT   ======------".
         05 K-PP-FOOTER                 PIC X(49) VALUE 
         "------======   -------------------   ======------".
         05 K-OUTPUT-OPEN-TAG           PIC X(09) VALUE 
         "<O_START>".
         05 K-OUTPUT-CLOSE-TAG          PIC X(07) VALUE 
         "<O_END>".
         05 K-ERR-OPEN-TAG              PIC X(11) VALUE 
         "<ERR_START>".
         05 K-ERR-CLOSE-TAG             PIC X(09) VALUE 
         "<ERR_END>".

       01 INTERNAL-VARS.
         05 WS-AMT-FMT                  PIC 9(7).99.
         05 WS-BAL-FMT                  PIC 9(7).99.
         05 WS-GET-BUSR-PASSWORD        PIC X(60).
         05 WS-PASSWORD-LEN             PIC 9(02).
         05 WS-COUNT                    PIC 9(04) VALUE 0.
         05 WS-SPACES-VALID             PIC 9(05).
         05 WS-ACTUAL-LENGTH            PIC 9(04) VALUE 0.
         05 WS-FORMATTED.
           10 WS-FORMATTED-DATE         PIC 99/99/99.
           10 WS-METHOD                 PIC X(08).
           10 WS-OBJECT                 PIC X(10).

       01 ARGUMENT-VARS.
         05 ARG-COMMAND-STRING          PIC X(218).
         05 ARG-WS-TIME-HHMMSSTT        PIC 9(08).
         05 ARG-WS-DATE-YYMMTT          PIC 9(06).

      ******************************************************************
      *                        COPYLIB IMPORTS 
      ******************************************************************
       COPY DISPINTERFACE.
       COPY ACCINTERFACE.
       COPY CUSTINTERFACE.
       COPY TRANSINTERFACE.
       COPY ERRINTERFACE.
       COPY LOGGERINTERFACE.
       COPY GENERICINTERFACE.
       COPY BUSRINTERFACE.
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       MAIN SECTION.
           ACCEPT ARG-COMMAND-STRING FROM COMMAND-LINE END-ACCEPT
           PERFORM F-INIT
      *
           SET WS-COUNT                    TO 0
           SET WS-ACTUAL-LENGTH            TO 0
           INSPECT FUNCTION REVERSE(ARG-COMMAND-STRING) 
             TALLYING WS-COUNT FOR LEADING SPACE
           SUBTRACT WS-COUNT FROM FUNCTION 
             LENGTH(ARG-COMMAND-STRING) 
             GIVING WS-ACTUAL-LENGTH
           DISPLAY "RECEIVED: '" 
                   ARG-COMMAND-STRING(1:WS-ACTUAL-LENGTH) "'"
           DISPLAY "TIMESTAMP: " ARG-WS-DATE-YYMMTT " / " 
                     ARG-WS-TIME-HHMMSSTT.
      *  
           PERFORM F-VALIDATE-REQUEST

           IF I-DISP-BANKUSERID NOT = SPACES
             PERFORM F-GET-BUSR-DATA
           END-IF

           IF FG-PROG-OK
             PERFORM F-CALL-BO
           END-IF
           IF O-DISP-ERR-OK
             PERFORM CHOOSE-RIGHT-CASE-JSON
           END-IF
           PERFORM F-PRETTY-PRINT
           PERFORM F-FINISH 
           .
       MAIN-END.
           STOP RUN.
      ******************************************************************
       F-INIT SECTION.
           PERFORM UT-LOG-INIT
           PERFORM LOG-DISPATCHER-START

           SET O-DISP-ERR-OK             TO TRUE
           MOVE SPACES                   TO O-DISP-ERROR-MESSAGE
           INITIALIZE ERROR-INTERFACE
           .
       F-INIT-END.
           EXIT.
      ******************************************************************
       F-FINISH SECTION.
      *
           IF NOT O-DISP-ERR-OK
             MOVE 1                        TO RETURN-CODE
             PERFORM F-HANDLE-ERROR
           END-IF

           PERFORM UT-LOG-NEWLINE
           .
       F-FINISH-END.
           EXIT.
      ******************************************************************
       F-GET-BUSR-DATA SECTION.
      *
           MOVE I-DISP-METHOD                    TO WS-METHOD
           MOVE I-DISP-OBJECT                    TO WS-OBJECT 
           SET I-DISP-METHOD-GET                 TO TRUE
           SET I-DISP-OBJ-BUSR-ITEM              TO TRUE
           SET PGNAME-BUSRBO                     TO TRUE
           CALL PROGNAME USING BY REFERENCE DISPATCHER-INTERFACE

           EVALUATE TRUE
             WHEN O-DISP-ERR-OK
               MOVE WS-METHOD                    TO I-DISP-METHOD
               MOVE WS-OBJECT                    TO I-DISP-OBJECT
             WHEN OTHER
               SET O-DISP-ERR-DISP-INVALID-LOGIN TO TRUE 
               SET FG-PROG-ERROR                 TO TRUE
           END-EVALUATE
           .
       F-GET-BUSR-DATA-END.
           EXIT. 
      ******************************************************************
       F-VALIDATE-REQUEST SECTION.
           MOVE ARG-COMMAND-STRING             TO DISPATCHER-IN
      *     
           IF NOT I-DISP-OBJ-OK
             SET O-DISP-ERR-DISP-BAD-OBJECT    TO TRUE
             SET FG-PROG-ERROR                 TO TRUE
           END-IF
           .
       F-VALIDATE-REQUEST-END.
           EXIT.
      ******************************************************************
       F-CALL-BO SECTION.
           INITIALIZE GENERIC-INTERFACE
           PERFORM RESET-GENERIC-ARRAYS

           EVALUATE TRUE
             WHEN I-DISP-OBJ-BUSR-LIST OR I-DISP-OBJ-LOGIN
               OR I-DISP-OBJ-BUSR-ITEM
               SET PGNAME-BUSRBO             TO TRUE 
             WHEN I-DISP-OBJ-CUST-ITEM OR I-DISP-OBJ-CUST-LIST
               SET PGNAME-CUSTBO             TO TRUE
             WHEN I-DISP-OBJ-ACC-LIST OR I-DISP-OBJ-ACC-ITEM OR 
               I-DISP-OBJ-TRANS
               SET PGNAME-ACCBO              TO TRUE
             WHEN I-DISP-OBJ-TRANSFER
               SET PGNAME-TRANSFERBO         TO TRUE
             WHEN OTHER
               SET O-DISP-ERR-DISP-BAD-ROUTE TO TRUE
               SET FG-PROG-ERROR             TO TRUE
           END-EVALUATE

           CALL PROGNAME USING BY REFERENCE DISPATCHER-INTERFACE
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF
           .
       F-CALL-BO-END.
           EXIT.
      ******************************************************************
       CHOOSE-RIGHT-CASE-JSON SECTION.
           *> Populate generic for CUSTOMER list
           IF I-DISP-OBJ-CUST-LIST
             PERFORM CUSTOMER-LIST
           END-IF
       *> Populate generic for CUSTOMER item
           IF I-DISP-OBJ-CUST-ITEM
             IF I-DISP-METHOD-GET
               PERFORM CUSTOMER-ITEM
             END-IF 
            *> Populate generic for CUSTOMER item POST
             IF I-DISP-METHOD-POST
               PERFORM CUSTOMER-ITEM-POST
             END-IF
            *> Populate generic for CUSTOMER item UPDATE
             IF I-DISP-METHOD-PUT
               PERFORM CUSTOMER-ITEM-PUT
             END-IF
            *> Populate generic for CUSTOMER item DELETE
             IF I-DISP-METHOD-DELETE
               PERFORM CUSTOMER-ITEM-DELETE
             END-IF
           END-IF
       *>Populate generic for ONE ACCOUNT 
           IF I-DISP-OBJ-ACC-ITEM
             IF I-DISP-METHOD-PUT
               PERFORM ACCOUNT-ITEM-PUT
             END-IF
             IF I-DISP-METHOD-DELETE 
               PERFORM ACCOUNT-ITEM-DELETE
             END-IF 
             IF I-DISP-METHOD-POST
               PERFORM ACCOUNT-ITEM-POST
             END-IF 
           END-IF
       *>Populate generic for ACCOUNTS OF A CLIENT
           IF I-DISP-OBJ-ACC-LIST
             PERFORM GENERIC-ACCOUNT
           END-IF
           *>Populate generic for BANKUSER
           IF I-DISP-OBJ-BUSR-LIST
             PERFORM GENERIC-BUSR-LIST
           END-IF
       *>GET ONE TRANSACTION
           IF I-DISP-OBJ-TRANS
             IF I-DISP-METHOD-GET
               PERFORM GENERIC-GET-TRANSACTION
             END-IF
      *POST ONE TRANSACTION
             IF I-DISP-METHOD-POST
               PERFORM GENERIC-POST-TRANSACTION
             END-IF
           END-IF
      *>Populate generic for TRANSFERS OF AN ACCOUNT
           IF I-DISP-OBJ-TRANSFER
             IF I-DISP-METHOD-GET
               PERFORM GENERIC-TRANSFER-LIST
             END-IF
           END-IF
           SET PGNAME-GENERIC                TO TRUE
           CALL PROGNAME USING GENERIC-INTERFACE
           .
       CHOOSE-RIGHT-CASE-JSON-END.
           EXIT.
      ******************************************************************
       RESET-GENERIC-ARRAYS SECTION.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 50
             MOVE 0                    TO I-GEN-FIELD-CNT(WS-IDX)
             PERFORM VARYING WS-FIDX FROM 1 BY 1 UNTIL WS-FIDX > 6
               MOVE SPACES             TO I-GEN-F-NAME(WS-IDX, WS-FIDX)
               MOVE SPACES             TO I-GEN-F-VAL(WS-IDX, WS-FIDX)
             END-PERFORM
           END-PERFORM
           .
       RESET-GENERIC-ARRAYS-END.
           EXIT.
      ******************************************************************
       ACCOUNT-ITEM-POST SECTION.

           MOVE "ACCOUNT"                   TO I-GEN-TAB-NAME
           MOVE 1                           TO I-GEN-ROW-NO
           MOVE 5                           TO I-GEN-FIELD-CNT(1)

           MOVE "ID"                        TO I-GEN-F-NAME(1, 1)
           MOVE O-GET-ACC-ACCOUNTID(1)      TO I-GEN-F-VAL (1, 1)

           MOVE "CUSTOMERID"                TO I-GEN-F-NAME(1, 2)
           MOVE O-GET-ACC-CUSTOMERID(1)     TO I-GEN-F-VAL (1, 2)

           MOVE "IBAN"                      TO I-GEN-F-NAME(1, 3)
           MOVE O-GET-ACC-IBAN(1)           TO I-GEN-F-VAL (1, 3)

           MOVE "CURRENCY"                  TO I-GEN-F-NAME(1, 4)
           MOVE O-GET-ACC-CURRENCY(1)       TO I-GEN-F-VAL (1, 4)

           MOVE "BALANCE"                   TO I-GEN-F-NAME(1, 5)
           MOVE O-GET-ACC-BALANCE(1)        TO WS-BAL-FMT
           MOVE WS-BAL-FMT                  TO I-GEN-F-VAL (1, 5)
           .
       ACCOUNT-ITEM-POST-END.
           EXIT.
      ******************************************************************
       ACCOUNT-ITEM-DELETE SECTION.
           MOVE "ACCOUNT"                   TO I-GEN-TAB-NAME
           MOVE 1                           TO I-GEN-ROW-NO
           MOVE 1                           TO I-GEN-FIELD-CNT(1)
           MOVE "ID"                        TO I-GEN-F-NAME(1, 1)
           MOVE O-GET-ACC-ACCOUNTID(1)      TO I-GEN-F-VAL (1, 1)
           .
       ACCOUNT-ITEM-DELETE-END.
           EXIT.
      ******************************************************************
       ACCOUNT-ITEM-PUT SECTION.
           MOVE "ACCOUNT"                   TO I-GEN-TAB-NAME
           MOVE 1                           TO I-GEN-ROW-NO
           MOVE 2                           TO I-GEN-FIELD-CNT(1)

           MOVE "ID"                        TO I-GEN-F-NAME(1, 1)
           MOVE O-GET-ACC-ACCOUNTID(1)      TO I-GEN-F-VAL (1, 1)

           MOVE "BALANCE"                   TO I-GEN-F-NAME(1, 2)
           MOVE O-GET-ACC-BALANCE(1)        TO WS-BAL-FMT
           MOVE WS-BAL-FMT                  TO I-GEN-F-VAL (1, 2)
           .
       ACCOUNT-ITEM-PUT-END.
           EXIT.
      ******************************************************************
       GENERIC-POST-TRANSACTION SECTION.

           MOVE "TRANSACTION"            TO I-GEN-TAB-NAME
           MOVE 1                        TO I-GEN-ROW-NO
           MOVE 5                        TO I-GEN-FIELD-CNT(1)
           MOVE "ACCOUNT ID"             TO I-GEN-F-NAME(1,1)
           MOVE O-POST-TRANS-ACCOUNTID   TO I-GEN-F-VAL(1,1)
           MOVE "AMOUNT"                 TO I-GEN-F-NAME(1,2)
           MOVE O-POST-TRANS-AMOUNT      TO WS-AMT-FMT
           MOVE WS-AMT-FMT               TO I-GEN-F-VAL(1,2)
           MOVE "TIMESTAMP"              TO I-GEN-F-NAME(1,3)
           MOVE O-POST-TRANS-TIMESTAMP   TO I-GEN-F-VAL(1,3)
           MOVE "OPERATION TYPE"         TO I-GEN-F-NAME(1,4)
           MOVE O-POST-TRANS-TRANS-TYPE  TO I-GEN-F-VAL(1,4)
           MOVE "BALANCE"                TO I-GEN-F-NAME(1,5)
           MOVE O-POST-TRANS-BALANCE     TO WS-BAL-FMT
           MOVE WS-BAL-FMT               TO I-GEN-F-VAL(1,5)
           .
       GENERIC-POST-TRANSACTION-END.
           EXIT.
      ******************************************************************
       GENERIC-GET-TRANSACTION SECTION.
           MOVE "TRANSACTION"                TO I-GEN-TAB-NAME

           MOVE O-GET-TRANS-COUNT            TO I-GEN-ROW-NO

           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > I-GEN-ROW-NO
             MOVE 5                          TO I-GEN-FIELD-CNT(WS-IDX)

             MOVE "ACCOUNT ID"               TO I-GEN-F-NAME(WS-IDX, 1)
             MOVE O-GET-TRANS-ACCOUNTID(WS-IDX)
                                             TO I-GEN-F-VAL(WS-IDX, 1)
             MOVE "AMOUNT"                   TO I-GEN-F-NAME(WS-IDX, 2)
             MOVE O-GET-TRANS-AMOUNT(WS-IDX) TO WS-AMT-FMT
             MOVE WS-AMT-FMT                 TO I-GEN-F-VAL(WS-IDX, 2)
             MOVE "TIMESTAMP"                TO I-GEN-F-NAME(WS-IDX, 3)
             MOVE O-GET-TRANS-TIMESTAMP(WS-IDX) 
                                             TO I-GEN-F-VAL(WS-IDX, 3)
             MOVE "OPERATION TYPE"           TO I-GEN-F-NAME(WS-IDX, 4)
             MOVE O-GET-TRANS-TRANS-TYPE(WS-IDX) 
                                             TO I-GEN-F-VAL(WS-IDX, 4)
             MOVE "BALANCE"                  TO I-GEN-F-NAME(WS-IDX, 5)
             MOVE O-GET-TRANS-BALANCE(WS-IDX) 
                                             TO WS-BAL-FMT
             MOVE WS-BAL-FMT                 TO I-GEN-F-VAL(WS-IDX, 5)
           END-PERFORM
           .
       GENERIC-GET-TRANSACTION-END.
           EXIT.
      ******************************************************************
       GENERIC-BUSR-LIST SECTION.
           MOVE "BANKUSER"                  TO I-GEN-TAB-NAME
           MOVE O-GET-BUSR-COUNT            TO I-GEN-ROW-NO
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > I-GEN-ROW-NO
             MOVE 4                         TO I-GEN-FIELD-CNT(WS-IDX)
             MOVE "ID"                      TO I-GEN-F-NAME(WS-IDX, 1)
             IF O-GET-BUSR-ID(WS-IDX) NOT = SPACES
               MOVE O-GET-BUSR-ID(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX, 1)
             ELSE
               MOVE "0"                     TO I-GEN-F-VAL(WS-IDX, 1)
             END-IF
             MOVE "USERNAME"                TO I-GEN-F-NAME(WS-IDX, 2)
             IF O-GET-BUSR-USERNAME(WS-IDX) NOT = SPACES
               MOVE O-GET-BUSR-USERNAME(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX, 2)
             ELSE
               MOVE "0"                     TO I-GEN-F-VAL(WS-IDX, 2)
             END-IF

             MOVE "ROLE"                    TO I-GEN-F-NAME(WS-IDX, 4)
             MOVE O-GET-BUSR-ROLE(WS-IDX) 
                                            TO I-GEN-F-VAL(WS-IDX, 4)
           END-PERFORM
           .
       GENERIC-BUSR-LIST-END.
           EXIT.
      ******************************************************************
       GENERIC-TRANSACTION-LIST SECTION.
           MOVE "TRANSACTION"               TO I-GEN-TAB-NAME
           MOVE O-GET-TRANS-COUNT           TO I-GEN-ROW-NO
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > I-GEN-ROW-NO
             MOVE 5                         TO I-GEN-FIELD-CNT(WS-IDX)
             MOVE "ACCOUNT ID"              TO I-GEN-F-NAME(WS-IDX, 1)
             IF O-GET-TRANS-ACCOUNTID(WS-IDX) NOT = SPACES
               MOVE O-GET-TRANS-ACCOUNTID(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX, 1)
             ELSE
               MOVE "0"                     TO I-GEN-F-VAL(WS-IDX, 1)
             END-IF
             MOVE "AMOUNT"                  TO I-GEN-F-NAME(WS-IDX, 2)
             IF O-GET-TRANS-AMOUNT(WS-IDX) NOT = SPACES
               MOVE O-GET-TRANS-AMOUNT(WS-IDX) 
                                            TO WS-AMT-FMT
               MOVE WS-AMT-FMT              TO I-GEN-F-VAL(WS-IDX, 2)
             ELSE
               MOVE "0"                     TO I-GEN-F-VAL(WS-IDX, 2)
             END-IF
             MOVE "TIMESTAMP"               TO I-GEN-F-NAME(WS-IDX, 3)
             MOVE O-GET-TRANS-TIMESTAMP(WS-IDX) 
                                            TO I-GEN-F-VAL(WS-IDX, 3)
             MOVE "OPERATION TYPE"          TO I-GEN-F-NAME(WS-IDX, 4)
             MOVE O-GET-TRANS-TRANS-TYPE(WS-IDX) 
                                            TO I-GEN-F-VAL(WS-IDX, 4)
             MOVE "BALANCE"                 TO I-GEN-F-NAME(WS-IDX, 5)
             MOVE O-GET-TRANS-BALANCE(WS-IDX)   
                                            TO WS-BAL-FMT
             MOVE WS-BAL-FMT                TO I-GEN-F-VAL(WS-IDX,5)
           END-PERFORM
           .
       GENERIC-TRANSACTION-LIST-END.
           EXIT.
      ******************************************************************
       GENERIC-ACCOUNT-LIST SECTION.
           MOVE "ACCOUNT"                   TO I-GEN-TAB-NAME
           MOVE O-GET-ACC-COUNT             TO I-GEN-ROW-NO
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > I-GEN-ROW-NO
             MOVE 5                         TO I-GEN-FIELD-CNT(WS-IDX)
             MOVE "ID"                      TO I-GEN-F-NAME(WS-IDX, 1)
             MOVE O-GET-ACC-ACCOUNTID(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX, 1)
             MOVE "CUSTOMERID"              TO I-GEN-F-NAME(WS-IDX, 2)
             MOVE O-GET-ACC-CUSTOMERID(WS-IDX) 
                                            TO I-GEN-F-VAL(WS-IDX, 2)
             MOVE "BALANCE"                 TO I-GEN-F-NAME(WS-IDX, 3)
             MOVE FUNCTION NUMVAL-C(O-GET-ACC-BALANCE(WS-IDX)) 
                                            TO I-GEN-F-VAL(WS-IDX, 3)
             MOVE "IBAN"                    TO I-GEN-F-NAME(WS-IDX, 4)
             MOVE O-GET-ACC-IBAN(WS-IDX) 
                                            TO I-GEN-F-VAL(WS-IDX, 4)
             MOVE "CURRENCY"                TO I-GEN-F-NAME(WS-IDX, 5)
             MOVE O-GET-ACC-CURRENCY(WS-IDX) 
                                            TO I-GEN-F-VAL(WS-IDX, 5)
           END-PERFORM
           .
        GENERIC-ACCOUNT-LIST-END.
           EXIT.
      ******************************************************************
       GENERIC-ACCOUNT SECTION.
           MOVE "ACCOUNT"                   TO I-GEN-TAB-NAME
           MOVE O-GET-ACC-COUNT             TO I-GEN-ROW-NO
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > I-GEN-ROW-NO
             MOVE 5                         TO I-GEN-FIELD-CNT(WS-IDX)
             MOVE "ID"                      TO I-GEN-F-NAME(WS-IDX, 1)
             MOVE O-GET-ACC-ACCOUNTID(WS-IDX) 
                                            TO I-GEN-F-VAL(WS-IDX,1)
             MOVE "CUSTOMERID"              TO I-GEN-F-NAME(WS-IDX, 2)
             MOVE O-GET-ACC-CUSTOMERID(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX,2)
             MOVE "BALANCE"                 TO I-GEN-F-NAME(WS-IDX, 3)
             MOVE FUNCTION NUMVAL-C(O-GET-ACC-BALANCE(WS-IDX)) 
                                            TO I-GEN-F-VAL(WS-IDX, 3)
             MOVE "IBAN"                    TO I-GEN-F-NAME(WS-IDX, 4)
             MOVE O-GET-ACC-IBAN(WS-IDX) 
                                            TO I-GEN-F-VAL(WS-IDX, 4)
             MOVE "CURRENCY"                TO I-GEN-F-NAME(WS-IDX, 5)
             MOVE O-GET-ACC-CURRENCY(WS-IDX) 
                                            TO I-GEN-F-VAL(WS-IDX, 5)
           END-PERFORM
           .
       GENERIC-ACCOUNT-END.
           EXIT.

      ******************************************************************
       GENERIC-TRANSFER-LIST SECTION.
           MOVE "TRANSFER"                  TO I-GEN-TAB-NAME
           MOVE O-GET-TRANSFER-COUNT        TO I-GEN-ROW-NO

           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > I-GEN-ROW-NO
             MOVE 6                         TO I-GEN-FIELD-CNT(WS-IDX)

             MOVE "ID"                      TO I-GEN-F-NAME(WS-IDX, 1)
             MOVE O-GET-TRANSFER-ID(WS-IDX) TO I-GEN-F-VAL(WS-IDX, 1)

             MOVE "SRC ACCID"               TO I-GEN-F-NAME(WS-IDX, 2)
             MOVE O-GET-TRANSFER-SRCACCID(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX, 2)

             MOVE "DEST ACCID"              TO I-GEN-F-NAME(WS-IDX, 3)
             MOVE O-GET-TRANSFER-DESTACCID(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX, 3)

             MOVE "AMOUNT"                  TO I-GEN-F-NAME(WS-IDX, 4)
             MOVE O-GET-TRANSFER-AMOUNT(WS-IDX) TO WS-AMT-FMT
             MOVE WS-AMT-FMT                TO I-GEN-F-VAL(WS-IDX, 4)

             MOVE "TIMESTAMP"               TO I-GEN-F-NAME(WS-IDX, 5)
             MOVE O-GET-TRANSFER-TIMESTAMP(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX, 5)

             MOVE "CURRENCY"                TO I-GEN-F-NAME(WS-IDX, 6)
             MOVE O-GET-TRANSFER-CURRENCY(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX, 6)
           END-PERFORM
           .
       GENERIC-TRANSFER-LIST-END.
           EXIT.
      ******************************************************************
       CUSTOMER-ITEM SECTION.

           MOVE "CUSTOMER"                  TO I-GEN-TAB-NAME
           MOVE 1                           TO I-GEN-ROW-NO
           MOVE 3                           TO I-GEN-FIELD-CNT(1) 
           MOVE "ID"                        TO I-GEN-F-NAME(1, 1)
           MOVE O-GET-CUST-ITEM-ID          TO I-GEN-F-VAL(1, 1)
           MOVE "USERNAME"                  TO I-GEN-F-NAME(1, 2)
           MOVE O-GET-CUST-ITEM-USERNAME
                                            TO I-GEN-F-VAL(1, 2)
           MOVE "ADDRESS"                   TO I-GEN-F-NAME(1, 3)
           MOVE O-GET-CUST-ITEM-ADDRESS
                                            TO I-GEN-F-VAL(1, 3)
           .
       CUSTOMER-ITEM-END.
           EXIT.
      ******************************************************************
       CUSTOMER-ITEM-PUT SECTION.
           MOVE "CUSTOMER"                  TO I-GEN-TAB-NAME
           MOVE 1                           TO I-GEN-ROW-NO
           MOVE 3                           TO I-GEN-FIELD-CNT(1) 
           MOVE "ID"                        TO I-GEN-F-NAME(1, 1)
           MOVE O-PUT-CUST-ITEM-ID          TO I-GEN-F-VAL(1, 1)
           MOVE "USERNAME"                  TO I-GEN-F-NAME(1, 2)
           MOVE  O-PUT-CUST-ITEM-USERNAME
                                            TO I-GEN-F-VAL(1, 2)
           MOVE "ADDRESS"                   TO I-GEN-F-NAME(1, 3)
           MOVE O-PUT-CUST-ITEM-ADDRESS
                                            TO I-GEN-F-VAL(1, 3)
           .
       CUSTOMER-ITEM-PUT-END.
           EXIT.
      ******************************************************************
       CUSTOMER-ITEM-DELETE SECTION.
           MOVE "CUSTOMER"                     TO I-GEN-TAB-NAME
           MOVE 1                              TO I-GEN-ROW-NO
           MOVE 4                              TO I-GEN-FIELD-CNT(1)
           MOVE "ID"                           TO I-GEN-F-NAME(1, 1)
           MOVE O-DEL-CUST-ITEM-ID             TO I-GEN-F-VAL (1, 1)

           MOVE "USERNAME"                     TO I-GEN-F-NAME(1, 2)
           MOVE O-DEL-CUST-ITEM-USERNAME       TO I-GEN-F-VAL (1, 2)

           MOVE "ADDRESS"                      TO I-GEN-F-NAME(1, 3)
           MOVE O-DEL-CUST-ITEM-ADDRESS        TO I-GEN-F-VAL (1, 3)

           MOVE "BANKUSERID"                   TO I-GEN-F-NAME(1, 4)
           MOVE O-DEL-CUST-ITEM-BANKUSERID     TO I-GEN-F-VAL (1, 4)
           .
       CUSTOMER-ITEM-DELETE-END.
           EXIT.
      ******************************************************************
       CUSTOMER-ITEM-POST SECTION.
           MOVE "CUSTOMER"                  TO I-GEN-TAB-NAME
           MOVE 1                           TO I-GEN-ROW-NO
           MOVE 3                           TO I-GEN-FIELD-CNT(1) 
           MOVE "ID"                        TO I-GEN-F-NAME(1, 1)
           MOVE O-POST-CUST-ITEM-ID         TO I-GEN-F-VAL(1, 1)
           MOVE "USERNAME"                  TO I-GEN-F-NAME(1, 2)
           MOVE  O-POST-CUST-ITEM-USERNAME
                                            TO I-GEN-F-VAL(1, 2)
           MOVE "ADDRESS"                   TO I-GEN-F-NAME(1, 3)
           MOVE O-POST-CUST-ITEM-ADDRESS
                                            TO I-GEN-F-VAL(1, 3)
           .
       CUSTOMER-ITEM-POST-END.
           EXIT.
      ******************************************************************
       CUSTOMER-LIST SECTION.
           MOVE "CUSTOMER"                  TO I-GEN-TAB-NAME
           MOVE O-GET-CUST-LIST-COUNT       TO I-GEN-ROW-NO
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > I-GEN-ROW-NO
             MOVE 3                         TO I-GEN-FIELD-CNT(WS-IDX)
             MOVE "ID"                      TO I-GEN-F-NAME(WS-IDX, 1)
             MOVE O-GET-CUST-LIST-ID(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX, 1)
             MOVE "USERNAME"                TO I-GEN-F-NAME(WS-IDX, 2)
             MOVE O-GET-CUST-LIST-USERNAME(WS-IDX) 
                                            TO I-GEN-F-VAL(WS-IDX, 2)
             MOVE "ADDRESS"                 TO I-GEN-F-NAME(WS-IDX, 3)
             MOVE O-GET-CUST-LIST-ADDRESS(WS-IDX)
                                            TO I-GEN-F-VAL(WS-IDX, 3)
           END-PERFORM
           .
       CUSTOMER-LIST-END.
           EXIT.
      ******************************************************************
       F-PRETTY-PRINT SECTION.
           DISPLAY K-PP-HEADER

           IF O-DISP-ERR-OK
             PERFORM LOG-PGSUCCESS
             EVALUATE TRUE ALSO TRUE
      *get cust item
               WHEN I-DISP-METHOD-GET ALSO I-DISP-OBJ-CUST-ITEM
                 DISPLAY "CUSTID    : "O-GET-CUST-ITEM-ID
                 DISPLAY "USERNAME  : "O-GET-CUST-ITEM-USERNAME
                 DISPLAY "ADDRESS   : "O-GET-CUST-ITEM-ADDRESS
                 DISPLAY "BANKUSERID: "O-GET-CUST-ITEM-BANKUSERID
                 DISPLAY "RECORDS   : "O-GET-CUST-ACC-COUNT

                 PERFORM VARYING I1 FROM 1 BY 1
                 UNTIL I1 > O-GET-CUST-ACC-COUNT
                   DISPLAY "ACCID     : "O-GET-CUST-ACC-ACCOUNTID(I1)
                   DISPLAY "CUST ID   : "O-GET-CUST-ACC-CUSTOMERID(I1)
                   DISPLAY "IBAN      : "O-GET-CUST-ACC-IBAN(I1)
                   DISPLAY "CURRENCY  : "O-GET-CUST-ACC-CURRENCY(I1)
                   DISPLAY "BALANCE   : "O-GET-CUST-ACC-BALANCE(I1)
                 END-PERFORM
      *  get cust list
               WHEN I-DISP-METHOD-GET ALSO I-DISP-OBJ-CUST-LIST
                 DISPLAY "RECORDS: "O-GET-CUST-LIST-COUNT
                 PERFORM VARYING I1 FROM 1 BY 1
                                 UNTIL I1 > O-GET-CUST-LIST-COUNT
                   DISPLAY "ID        : "O-GET-CUST-LIST-ID(I1)
                   DISPLAY "USERNAME  : "O-GET-CUST-LIST-USERNAME(I1)
                   DISPLAY "ADDRESS   : "O-GET-CUST-LIST-ADDRESS(I1)
                   DISPLAY "BANKUSERID: "O-GET-CUST-LIST-BANKUSERID(I1)
                 END-PERFORM
               WHEN I-DISP-METHOD-POST ALSO I-DISP-OBJ-CUST-ITEM
                 DISPLAY "ID        : "O-POST-CUST-ITEM-ID
                 DISPLAY "USERNAME  : "O-POST-CUST-ITEM-USERNAME
                 DISPLAY "ADDRESS   : "O-POST-CUST-ITEM-ADDRESS
                 DISPLAY "BANKUSERID: "O-POST-CUST-ITEM-BANKUSERID
      
      * delete cust item
               WHEN I-DISP-METHOD-DELETE ALSO I-DISP-OBJ-CUST-ITEM
                 DISPLAY "ID      : "O-DEL-CUST-ITEM-ID
      *update cust item
               WHEN I-DISP-METHOD-PUT ALSO I-DISP-OBJ-CUST-ITEM
                 DISPLAY "ID (DB)   : " O-PUT-CUST-ITEM-ID
                 DISPLAY "USERNAME  : " O-PUT-CUST-ITEM-USERNAME
                 DISPLAY "ADDRESS   : " O-PUT-CUST-ITEM-ADDRESS
                 DISPLAY "BANKUSERID: " O-PUT-CUST-ITEM-BANKUSERID
      * login details 
               WHEN I-DISP-METHOD-POST ALSO I-DISP-OBJ-LOGIN
                 DISPLAY "ID (DB) : " O-POST-LOGIN-ID   
                 DISPLAY "USERNAME: " O-POST-LOGIN-USERNAME 
                 DISPLAY "ROLE    : " O-POST-LOGIN-ROLE

               WHEN I-DISP-METHOD-GET ALSO I-DISP-OBJ-BUSR-LIST
                 DISPLAY "RECORDS: " O-GET-BUSR-COUNT
                 IF O-GET-BUSR-COUNT = 0
                   CONTINUE
                 ELSE
                   PERFORM VARYING I1 FROM 1 BY 1
                        UNTIL I1 > O-GET-BUSR-COUNT
                     DISPLAY "ID        : " O-GET-BUSR-ID(I1)
                     DISPLAY "USERNAME  : " O-GET-BUSR-USERNAME(I1)
                     DISPLAY "ROLE      : " O-GET-BUSR-ROLE(I1)
                     DISPLAY " "
                   END-PERFORM
                 END-IF

               WHEN I-DISP-METHOD-GET ALSO I-DISP-OBJ-ACC-LIST
                 DISPLAY "RECORDS   : "O-GET-ACC-COUNT
                 PERFORM VARYING I1 FROM 1 BY 1
                 UNTIL I1 > O-GET-ACC-COUNT
                   DISPLAY "ID        : "O-GET-ACC-ACCOUNTID(I1)
                   DISPLAY "CUST ID   : "O-GET-ACC-CUSTOMERID(I1)
                   DISPLAY "IBAN      : "O-GET-ACC-IBAN(I1)
                   DISPLAY "CURRENCY  : "O-GET-ACC-CURRENCY(I1)
                   DISPLAY "BALANCE   : "O-GET-ACC-BALANCE(I1)
                 END-PERFORM
      *    
               WHEN I-DISP-METHOD-GET ALSO I-DISP-OBJ-TRANS
                 DISPLAY "RECORDS  : "O-GET-TRANS-COUNT
                 PERFORM VARYING I1 FROM 1 BY 1
                                    UNTIL I1 > O-GET-TRANS-COUNT
                   DISPLAY "ID ACCOUNT: "O-GET-TRANS-ACCOUNTID(I1)
                   DISPLAY "TYPE      : "O-GET-TRANS-TRANS-TYPE(I1)
                   DISPLAY "AMOUNT    : "O-GET-TRANS-AMOUNT(I1)
                   DISPLAY "DATETIME  : "O-GET-TRANS-TIMESTAMP(I1)
                   DISPLAY "BALANCE   : "O-GET-TRANS-BALANCE(I1)
                 END-PERFORM

               WHEN I-DISP-METHOD-POST ALSO I-DISP-OBJ-TRANS
                 DISPLAY "ID ACCOUNT: " O-POST-TRANS-ACCOUNTID
                 DISPLAY "TYPE      : " O-POST-TRANS-TRANS-TYPE
                 DISPLAY "AMOUNT    : " O-POST-TRANS-AMOUNT
                 DISPLAY "DATETIME  : " O-POST-TRANS-TIMESTAMP
                 DISPLAY "BALANCE   : " O-POST-TRANS-BALANCE

               WHEN I-DISP-METHOD-POST ALSO I-DISP-OBJ-ACC-ITEM
                 DISPLAY "NEW ACCOUNT ADDED:"
                 DISPLAY "ID        : "O-GET-ACC-ACCOUNTID(1)
                 DISPLAY "CUST ID   : "O-GET-ACC-CUSTOMERID(1)
                 DISPLAY "IBAN      : "O-GET-ACC-IBAN(1)
                 DISPLAY "CURRENCY  : "O-GET-ACC-CURRENCY(1)
                 DISPLAY "BALANCE   : "O-GET-ACC-BALANCE(1)
               
               WHEN I-DISP-METHOD-PUT ALSO I-DISP-OBJ-ACC-ITEM
                 DISPLAY "ACCOUNT BALANCE UPDATED:"
                 DISPLAY "ID        : "O-GET-ACC-ACCOUNTID(1)
                 DISPLAY "BALANCE   : "O-GET-ACC-BALANCE(1)

               WHEN I-DISP-METHOD-DELETE ALSO I-DISP-OBJ-ACC-ITEM
                 DISPLAY "ACCOUNT DELETED:"
                 DISPLAY "ID        : "O-GET-ACC-ACCOUNTID(1)
               
               WHEN I-DISP-METHOD-POST ALSO I-DISP-OBJ-TRANSFER
                 DISPLAY "NEW TRANSFER CREATED:"
                 DISPLAY "ID        : "O-POST-TRANSFER-ID
                 DISPLAY "SOURCE ACC: "O-POST-TRANSFER-SRCIBAN
                 DISPLAY "DEST ACC  : "O-POST-TRANSFER-DESTIBAN
                 DISPLAY "AMOUNT    : "O-POST-TRANSFER-AMOUNT
                 DISPLAY "TIMESTAMP : "O-POST-TRANSFER-TIMESTAMP
                 DISPLAY "CURRENCY  : "O-POST-TRANSFER-CURRENCY

               WHEN I-DISP-METHOD-POST ALSO I-DISP-OBJ-BUSR-ITEM
                 DISPLAY "NEW BANKUSER CREATED:"
                 DISPLAY "ID        : "O-POST-BUSR-ID
                 DISPLAY "USERNAME  : "O-POST-BUSR-USERNAME
                 DISPLAY "ROLE      : "O-POST-BUSR-ROLE

               WHEN I-DISP-METHOD-DELETE ALSO I-DISP-OBJ-BUSR-ITEM
                 DISPLAY "BANKUSER DELETED:"
                 DISPLAY "ID        : "O-DEL-BUSR-ID

               WHEN I-DISP-METHOD-PUT ALSO I-DISP-OBJ-BUSR-ITEM
                 DISPLAY "BANKUSER UPDATED:"
                 DISPLAY "ID        : "O-PUT-BUSR-ID
                 DISPLAY "USERNAME  : "O-PUT-BUSR-USERNAME
                 DISPLAY "ROLE      : "O-PUT-BUSR-ROLE
              
               WHEN I-DISP-METHOD-GET ALSO I-DISP-OBJ-TRANSFER
                 DISPLAY "RECORDS  : "    O-GET-TRANSFER-COUNT
                 PERFORM VARYING I1 FROM 1 BY 1
                               UNTIL I1 > O-GET-TRANSFER-COUNT
                   DISPLAY "ID        : " O-GET-TRANSFER-ID(I1)
                   DISPLAY "SRC ACCID : " O-GET-TRANSFER-SRCACCID(I1)
                   DISPLAY "SRC IBAN  : " O-GET-TRANSFER-SRCIBAN(I1)
                   DISPLAY "DEST ACCID: " O-GET-TRANSFER-DESTACCID(I1)
                   DISPLAY "DEST IBAN : " O-GET-TRANSFER-DESTIBAN(I1)
                   DISPLAY "AMOUNT    : " O-GET-TRANSFER-AMOUNT(I1)
                   DISPLAY "TIMESTAMP : " O-GET-TRANSFER-TIMESTAMP(I1)
                   DISPLAY "CURRENCY  : " O-GET-TRANSFER-CURRENCY(I1)
                 END-PERFORM
                 
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           ELSE
             DISPLAY "ERROR...CHECK LOG"
             PERFORM LOG-ERROR
           END-IF

           DISPLAY K-PP-FOOTER
      *  
           SET WS-COUNT                  TO 0
           SET WS-ACTUAL-LENGTH          TO 0
           INSPECT FUNCTION REVERSE(O-DISP-DATA-OUT)
             TALLYING WS-COUNT FOR LEADING SPACE
           SUBTRACT WS-COUNT FROM FUNCTION
             LENGTH(O-DISP-DATA-OUT)
             GIVING WS-ACTUAL-LENGTH
           DISPLAY K-OUTPUT-OPEN-TAG
                   O-DISP-DATA-OUT(1:WS-ACTUAL-LENGTH)
                   K-OUTPUT-CLOSE-TAG
                   " "
                   
           SET WS-COUNT                  TO 0
           SET WS-ACTUAL-LENGTH          TO 0
           INSPECT FUNCTION REVERSE(O-DISP-ERROR)
             TALLYING WS-COUNT FOR LEADING SPACE
           SUBTRACT WS-COUNT FROM FUNCTION
             LENGTH(O-DISP-ERROR)
             GIVING WS-ACTUAL-LENGTH
           DISPLAY K-ERR-OPEN-TAG
                   O-DISP-ERROR(1:WS-ACTUAL-LENGTH)
                   K-ERR-CLOSE-TAG
                   " "
           .
       F-PRETTY-PRINT-END.
           EXIT.
      ****************************************************************** 

       F-HANDLE-ERROR SECTION.
           SET PGNAME-ERROR                 TO TRUE
           CALL PROGNAME USING BY REFERENCE ERROR-INTERFACE

           MOVE O-ERR-MESSAGE               TO O-DISP-ERROR-MESSAGE
           .
       F-HANDLE-ERROR-END.
           EXIT.
      ******************************************************************
       LOG-DISPATCHER-START SECTION.
           SET WS-COUNT                        TO 0
           SET WS-ACTUAL-LENGTH                TO 0
           INSPECT FUNCTION REVERSE(ARG-COMMAND-STRING) 
             TALLYING WS-COUNT FOR LEADING SPACE
           SUBTRACT WS-COUNT FROM FUNCTION 
             LENGTH(ARG-COMMAND-STRING) 
             GIVING WS-ACTUAL-LENGTH
      *      
           ACCEPT ARG-WS-TIME-HHMMSSTT FROM TIME
           ACCEPT ARG-WS-DATE-YYMMTT FROM DATE
           MOVE ARG-WS-DATE-YYMMTT             TO WS-FORMATTED-DATE
      *     
           STRING
             "--------------------------------- DISPATCHER STARTED AT ",
             WS-FORMATTED-DATE,
             " ",
             ARG-WS-TIME-HHMMSSTT,
             " ---------------------------------"
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "RECEIVED ARGUMENTS: '",
             ARG-COMMAND-STRING(1:WS-ACTUAL-LENGTH),
             "'"
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           PERFORM UT-LOG-MULTI-LINE
           .
       LOG-DISPATCHER-START-END.
           EXIT. 
      ******************************************************************
       LOG-ERROR SECTION.
           MOVE "*************************************************"
                TO U-LOG-LINE
           PERFORM UT-LOG-ADD-LINE
      * 
           STRING
             "*           !!! ERROR !!!                 *"
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "  CODE  : ",
             O-DISP-ERROR-NO
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             "  MSG   : ",
             FUNCTION TRIM(O-DISP-ERROR-MESSAGE)
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *
           MOVE "*************************************************"
                TO U-LOG-LINE
           PERFORM UT-LOG-ADD-LINE
      *     
           PERFORM UT-LOG-MULTI-LINE
           .
       LOG-ERROR-END.
           EXIT.
      ******************************************************************
       LOG-PGSUCCESS SECTION.
           STRING
             "RETURNED TO DISPATCHER - ",
             "PRINTING OUTPUT IN CONSOLE..."
             INTO U-LOG-LINE
           END-STRING
      * 
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-PGSUCCESS-END.
           EXIT. 
      ******************************************************************     
      *   COPY LOGGERUTILSECTIONS TO USE GENERIC SECTIONS FOR LOGGER
      ******************************************************************
       COPY LOGGERUTILSECTIONS.
