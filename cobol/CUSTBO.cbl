      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID. CUSTBO.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
      *
       01 PGM-ID                       PIC X(20) 
                                       VALUE "CUSTBO              ".
       01 PROGNAME                     PIC X(20).
         88 PGNAME-CUSTDB              VALUE "CUSTDB              ".
         88 PGNAME-ACCDB               VALUE "ACCDB               ".
         88 PGNAME-ERROR               VALUE "ERROR               ".
         88 PGNAME-BUSRDB              VALUE "BUSRDB              ".
       
       01 INTERNAL-VARS.
         05 WS-ERROR-SOURCE            PIC X(20).
         05 WS-PAGE-NUMBER             PIC 9(05).
         05 WS-VALIDATED-PAGE-NUMBER   PIC 9(05).

       01 WS-BUFFER.
         05 WS-BF-BUSR-ID              PIC 9(05).
         05 WS-BF-CUST-ID              PIC 9(05).
         05 WS-BF-ROLE                 PIC X(04).

       01 WS-TARGET-USER.
         05 WS-TG-BUSR-ID              PIC 9(05).
         05 WS-TG-CUST-ID              PIC 9(05).
         05 WS-TG-ROLE                 PIC X(04).
           88 WS-TG-ROLE-TELLER        VALUE "BaTe".
           88 WS-TG-ROLE-CLIENT        VALUE "BaCl".
           88 WS-TG-ROLE-ADMIN         VALUE "BaAd".
       01 INDEXES.
         05 IND-1                      PIC 9(03).
      ******************************************************************
      *                        COPYLIB IMPORTS 
      ******************************************************************   
       COPY CUSTINTERFACE.
       COPY ACCINTERFACE.
       COPY ERRINTERFACE.
       COPY LOGGERINTERFACE.
       COPY BUSRINTERFACE.
       COPY DBUTILSVARS.

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
             WHEN I-DISP-METHOD-GET    ALSO I-DISP-OBJ-CUST-LIST
               PERFORM F-READ-CUSTOMER-LIST
             WHEN I-DISP-METHOD-GET    ALSO I-DISP-OBJ-CUST-ITEM
               PERFORM F-READ-CUSTOMER-ITEM
             WHEN I-DISP-METHOD-POST   ALSO I-DISP-OBJ-CUST-ITEM
               PERFORM F-CREATE-CUSTOMER-ITEM
             WHEN I-DISP-METHOD-DELETE ALSO I-DISP-OBJ-CUST-ITEM
               PERFORM F-DELETE-CUSTOMER-ITEM
             WHEN I-DISP-METHOD-PUT    ALSO I-DISP-OBJ-CUST-ITEM
               PERFORM F-UPDATE-CUSTOMER-ITEM
             WHEN OTHER
               SET O-DISP-ERR-CUST-BAD-METHOD TO TRUE
               MOVE 1                         TO I-ERR-PARAM-COUNT 
               MOVE I-DISP-METHOD             TO I-ERR-PARAM (1)
           END-EVALUATE
           .
       F-PROCESS-REQUEST-END.
           EXIT.
      ******************************************************************
       F-READ-CUSTOMER-LIST SECTION.
           IF NOT U-DISP-LOGIN-ADMIN
             SET O-DISP-ERR-AUTH-ROLE-FORBIDDEN TO TRUE
             MOVE 1                             TO I-ERR-PARAM-COUNT 
             MOVE U-DISP-LOGIN-ROLE             TO I-ERR-PARAM (1)

             EXIT SECTION
           END-IF
       
           MOVE I-GET-CUST-LIST-PAGE-NUMBER     TO WS-PAGE-NUMBER
           PERFORM F-VALIDATE-AND-SET-PAGE-NUMBER
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF
       
           INITIALIZE CUSTDB-INTERFACE
           SET I-CUST-OP-GET-LIST               TO TRUE
           MOVE WS-VALIDATED-PAGE-NUMBER        TO I-CUST-PAGE-NUMBER
           SET PGNAME-CUSTDB                    TO TRUE
           CALL PROGNAME USING CUSTDB-INTERFACE

           EVALUATE TRUE
             WHEN CUST-STATUS-OK
               MOVE O-CUST-COUNT    TO O-GET-CUST-LIST-COUNT
               PERFORM VARYING IND-1 FROM 1 BY 1 
                               UNTIL IND-1 > O-CUST-COUNT
                 MOVE O-CUST-ID(IND-1)         
                                    TO O-GET-CUST-LIST-ID(IND-1)
                 MOVE O-CUST-USERNAME(IND-1)
                                    TO O-GET-CUST-LIST-USERNAME(IND-1)
                 MOVE O-CUST-ADDRESS(IND-1)
                                    TO O-GET-CUST-LIST-ADDRESS(IND-1)
                 MOVE O-CUST-BANKUSERID(IND-1)
                                    TO O-GET-CUST-LIST-BANKUSERID(IND-1)
               END-PERFORM  
             WHEN OTHER 
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE
           .
       F-READ-CUSTOMER-LIST-END.
           EXIT.
      ******************************************************************
       F-READ-CUSTOMER-ITEM SECTION.

           MOVE I-GET-CUST-ITEM-ID              TO WS-BF-CUST-ID
           PERFORM UT-GET-TARGET-USER-DATA-FROM-CUST-ID
           IF O-DISP-ERR-OK
             PERFORM UT-DEFAULT-PERMISSION-CHECK
           END-IF
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           INITIALIZE CUSTDB-INTERFACE
           SET I-CUST-OP-GET-ITEM               TO TRUE
           MOVE I-GET-CUST-ITEM-ID              TO I-CUST-CUSTID
           SET PGNAME-CUSTDB                    TO TRUE
           CALL PROGNAME USING CUSTDB-INTERFACE
           EVALUATE TRUE
             WHEN CUST-STATUS-OK
               EXIT SECTION
             WHEN CUST-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND TO TRUE
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE

           MOVE O-CUST-ELEM(1)                  TO O-GET-CUST-ITEM-ELEM
           MOVE I-GET-CUST-ITEM-PAGE-NUMBER     TO WS-PAGE-NUMBER
           PERFORM F-VALIDATE-AND-SET-PAGE-NUMBER
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           INITIALIZE ACCDB-INTERFACE
           SET I-ACC-OP-GETLIST                 TO TRUE
           MOVE WS-VALIDATED-PAGE-NUMBER        TO I-ACC-PAGE-NUMBER
           MOVE I-GET-CUST-ITEM-ID              TO I-ACC-CUSTOMERID
           SET PGNAME-ACCDB                     TO TRUE
           CALL PROGNAME USING ACCDB-INTERFACE
           EVALUATE TRUE
             WHEN ACCDB-STATUS-OK
               MOVE ACCDB-OUT                   TO O-GET-CUST-ACC-DATA
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE
           .
       F-READ-CUSTOMER-ITEM-END.
           EXIT.

      ******************************************************************
       F-CREATE-CUSTOMER-ITEM SECTION.
           IF U-DISP-LOGIN-CLIENT
             SET O-DISP-ERR-BUSR-BAD-ROLE     TO TRUE
             EXIT SECTION
           END-IF

           MOVE I-POST-CUST-ITEM-BANKUSERID   TO WS-BF-BUSR-ID
           PERFORM UT-GET-ROLE
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           MOVE WS-BF-BUSR-ID                 TO WS-TG-BUSR-ID
           MOVE 0                             TO WS-TG-CUST-ID
           MOVE WS-BF-ROLE                    TO WS-TG-ROLE

           PERFORM UT-DEFAULT-PERMISSION-CHECK
           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           INITIALIZE CUSTDB-INTERFACE
           SET I-CUST-OP-POST                 TO TRUE
           MOVE I-POST-CUST-ITEM-USNAME       TO I-CUST-USERNAME
           MOVE I-POST-CUST-ITEM-ADDRESS      TO I-CUST-ADDRESS
           MOVE I-POST-CUST-ITEM-BANKUSERID   TO I-CUST-BANKUSERID

           SET PGNAME-CUSTDB                  TO TRUE
           CALL PROGNAME USING CUSTDB-INTERFACE

           EVALUATE TRUE
             WHEN CUST-STATUS-OK
               MOVE O-CUST-ELEM(1)            TO O-DISP-DATA-OUT
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL          TO TRUE
           END-EVALUATE
           .
       F-CREATE-CUSTOMER-ITEM-END.
           EXIT.
      ******************************************************************
       F-DELETE-CUSTOMER-ITEM SECTION.
           IF U-DISP-LOGIN-CLIENT
             SET O-DISP-ERR-AUTH-ROLE-FORBIDDEN 
                                           TO TRUE
             MOVE 1                        TO I-ERR-PARAM-COUNT 
             MOVE U-DISP-LOGIN-ROLE        TO I-ERR-PARAM (1)
             EXIT SECTION
           END-IF

           MOVE I-DEL-CUST-ITEM-CUSTID     TO WS-BF-CUST-ID
           PERFORM UT-GET-TARGET-USER-DATA-FROM-CUST-ID
           IF O-DISP-ERR-OK
             PERFORM UT-DEFAULT-PERMISSION-CHECK
           END-IF

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           INITIALIZE CUSTDB-INTERFACE
           SET I-CUST-OP-DELETE            TO TRUE
           MOVE I-DEL-CUST-ITEM-CUSTID     TO I-CUST-CUSTID

           SET PGNAME-CUSTDB               TO TRUE
           CALL PROGNAME USING CUSTDB-INTERFACE

           EVALUATE TRUE
             WHEN CUST-STATUS-OK
               MOVE O-CUST-ID(1)           TO O-DEL-CUST-ITEM-ID
               MOVE O-CUST-USERNAME(1)     TO O-DEL-CUST-ITEM-USERNAME
               MOVE O-CUST-ADDRESS(1)      TO O-DEL-CUST-ITEM-ADDRESS
               MOVE O-CUST-BANKUSERID(1)   TO O-DEL-CUST-ITEM-BANKUSERID
             WHEN OTHER 
               SET O-DISP-ERR-DB-SQL       TO TRUE
           END-EVALUATE
           .
       F-DELETE-CUSTOMER-ITEM-END.
           EXIT.
      ******************************************************************
       F-UPDATE-CUSTOMER-ITEM SECTION.
           IF U-DISP-LOGIN-CLIENT
             SET O-DISP-ERR-BUSR-BAD-ROLE  TO TRUE
             EXIT SECTION
           END-IF

           MOVE I-PUT-CUST-ITEM-CUSTID     TO WS-BF-CUST-ID
           PERFORM UT-GET-TARGET-USER-DATA-FROM-CUST-ID
           IF O-DISP-ERR-OK
             PERFORM UT-DEFAULT-PERMISSION-CHECK
           END-IF

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           INITIALIZE CUSTDB-INTERFACE
           SET I-CUST-OP-PUT               TO TRUE
           MOVE I-PUT-CUST-ITEM-CUSTID     TO I-CUST-CUSTID
           MOVE I-PUT-CUST-ITEM-USNAME     TO I-CUST-USERNAME
           MOVE I-PUT-CUST-ITEM-ADDRESS    TO I-CUST-ADDRESS

           SET PGNAME-CUSTDB               TO TRUE
           CALL PROGNAME USING CUSTDB-INTERFACE

           EVALUATE TRUE
             WHEN CUST-STATUS-OK
               MOVE O-CUST-ELEM(1)         TO O-DISP-DATA-OUT
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL       TO TRUE
           END-EVALUATE
           .
       F-UPDATE-CUSTOMER-ITEM-END.
           EXIT.
      ******************************************************************
       F-HANDLE-ERROR SECTION.
           MOVE O-DISP-ERROR-NO            TO I-ERR-CODE

           SET PGNAME-ERROR                TO TRUE
           CALL PROGNAME USING ERROR-INTERFACE
           
           IF WS-ERROR-SOURCE = SPACE
             MOVE PGM-ID                   TO WS-ERROR-SOURCE
           END-IF
           STRING FUNCTION TRIM(WS-ERROR-SOURCE) ": "
                  FUNCTION TRIM(O-ERR-MESSAGE)
             DELIMITED BY SIZE
             INTO O-DISP-ERROR-MESSAGE
           .
       F-HANDLE-ERROR-END.
           EXIT.
      ******************************************************************
       F-VALIDATE-AND-SET-PAGE-NUMBER SECTION.
           EVALUATE TRUE
             WHEN WS-PAGE-NUMBER IS NUMERIC
               IF WS-PAGE-NUMBER = 0
                 MOVE 1 TO WS-VALIDATED-PAGE-NUMBER
               ELSE
                 MOVE WS-PAGE-NUMBER 
                        TO WS-VALIDATED-PAGE-NUMBER
               END-IF
             WHEN WS-PAGE-NUMBER = SPACES
               MOVE 0   TO WS-VALIDATED-PAGE-NUMBER
             WHEN OTHER
               SET O-DISP-ERR-ACC-WRONG-PAGE-FORMAT 
                        TO TRUE
           END-EVALUATE
           .
       F-VALIDATE-AND-SET-PAGE-NUMBER-END.
           EXIT.
      ******************************************************************     
       UT-DEFAULT-PERMISSION-CHECK SECTION.
           EVALUATE TRUE
             WHEN U-DISP-LOGIN-ADMIN
               CONTINUE 
             WHEN U-DISP-LOGIN-TELLER
               EVALUATE TRUE
                 WHEN WS-TG-ROLE-ADMIN
                   SET O-DISP-ERR-AUTH-TLR-TO-ADMIN TO TRUE
                 WHEN WS-TG-ROLE-TELLER
                   IF NOT WS-TG-BUSR-ID = U-DISP-LOGIN-ID
                     SET O-DISP-ERR-AUTH-TLR-TO-TLR TO TRUE
                   END-IF
                 WHEN WS-TG-ROLE-CLIENT
                   CONTINUE
                 WHEN OTHER
                   SET O-DISP-ERR-AUTH-TLR-TO-NONE  TO TRUE
               END-EVALUATE
             WHEN U-DISP-LOGIN-CLIENT
               IF NOT WS-TG-BUSR-ID = U-DISP-LOGIN-ID
                 SET O-DISP-ERR-AUTH-CLT-TO-OTHER   TO TRUE
               END-IF
             WHEN OTHER
               SET O-DISP-ERR-AUTH-ROLE-FORBIDDEN   TO TRUE
               MOVE 1                               TO I-ERR-PARAM-COUNT 
               MOVE U-DISP-LOGIN-ROLE               TO I-ERR-PARAM (1)
           END-EVALUATE
           .
       UT-DEFAULT-PERMISSION-CHECK-END.
           EXIT.
      ******************************************************************
       UT-GET-TARGET-USER-DATA-FROM-CUST-ID SECTION.
           PERFORM UT-GET-BUSRID-FROM-CUSTID
           IF O-DISP-ERR-OK
             PERFORM UT-GET-ROLE
           END-IF

           IF O-DISP-ERR-OK
             MOVE WS-BF-CUST-ID              TO WS-TG-CUST-ID
             MOVE WS-BF-BUSR-ID              TO WS-TG-BUSR-ID
             MOVE WS-BF-ROLE                 TO WS-TG-ROLE
           END-IF
           .
       UT-GET-TARGET-USER-DATA-FROM-CUST-ID-END.
           EXIT.
      ******************************************************************
       UT-GET-BUSRID-FROM-CUSTID SECTION.
           INITIALIZE CUSTDB-INTERFACE
           MOVE WS-BF-CUST-ID                   TO I-CUST-CUSTID
           SET I-CUST-OP-GET-ITEM               TO TRUE
           SET PGNAME-CUSTDB                    TO TRUE
           CALL PROGNAME USING CUSTDB-INTERFACE
           EVALUATE TRUE
             WHEN CUST-STATUS-OK
               MOVE O-CUST-BANKUSERID (1)       TO WS-BF-BUSR-ID
             WHEN CUST-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND TO TRUE
             WHEN CUST-STATUS-SQL-ERROR
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE
           .
       UT-GET-BUSRID-FROM-CUSTID-END.
           EXIT.
      ******************************************************************
       UT-GET-ROLE SECTION.
           INITIALIZE BUSRDB-INTERFACE
           SET I-BUSR-OP-GETROLE                TO TRUE
           MOVE WS-BF-BUSR-ID                   TO I-BUSR-ID
           SET PGNAME-BUSRDB                    TO TRUE
           CALL PROGNAME USING BUSRDB-INTERFACE
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
