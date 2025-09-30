      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID. BUSRBO.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
       01 PGM-ID                 PIC X(20) 
                                 VALUE "BUSRBO              ".

       01 PROGNAME               PIC X(20).
         88 PGNAME-BUSRDB        VALUE "BUSRDB              ".
         88 PGNAME-ERROR         VALUE "ERROR               ".
       
       01 INTERNAL-VARS.
         05 WS-ERROR-SOURCE      PIC X(20).

      *  DATA MOVED AROUND BETWEEN THE CALLS OF THE MODULES
         05 WS-BUFFER.
           10 WS-BF-ROLE         PIC X(04).
             88 WS-BF-ROLE-OK    VALUES "BaAd" "BaTe" "BaCl".
             88 WS-BF-ROLE-BAAD  VALUE "BaAd".
             88 WS-BF-ROLE-BATE  VALUE "BaTe".
             88 WS-BF-ROLE-BACL  VALUE "BaCl".
           10 WS-BF-BUSR-ID      PIC 9(05).
         05 WS-PAGE-NUMBER       PIC 9(05).
         05 WS-VALIDATED-PAGE-NUMBER 
                                 PIC 9(05).
      *  RELEVANT DATA FOR THE USER ON WHICH THE ACTION IS PERFORMED
         05 WS-TARGET-USER.
           10 WS-TG-BUSR-ID      PIC 9(05).
           10 WS-TG-ROLE         PIC 9(04).
             88 WS-TG-ROLE-TELLER        
                                 VALUE "BaTe".
             88 WS-TG-ROLE-CLIENT        
                                 VALUE "BaCl".
             88 WS-TG-ROLE-ADMIN         
                                 VALUE "BaAd".

       01  INDEXES.
         05 IND-1                PIC 9(02).

      ******************************************************************
      *                        COPYLIB IMPORTS 
      ******************************************************************
       COPY BUSRINTERFACE.
       COPY ERRINTERFACE.
       COPY LOGGERINTERFACE.
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
           MOVE PGM-ID                   TO U-LOG-RUNNABLE-PROG
           MOVE DISPATCHER-IN            TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-START
      * 
           SET O-DISP-ERR-OK             TO TRUE
           MOVE SPACES                   TO O-DISP-ERROR-MESSAGE
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
           EVALUATE TRUE               ALSO TRUE
             WHEN I-DISP-METHOD-GET    ALSO I-DISP-OBJ-BUSR-ITEM
               PERFORM F-GET-BUSR-ITEM
             WHEN I-DISP-METHOD-POST   ALSO I-DISP-OBJ-LOGIN
               PERFORM F-PROCESS-LOGIN
             WHEN I-DISP-METHOD-GET    ALSO I-DISP-OBJ-BUSR-LIST
               PERFORM F-GET-BUSR-LIST
             WHEN I-DISP-METHOD-POST   ALSO I-DISP-OBJ-BUSR-ITEM
               PERFORM F-CREATE-BUSR
             WHEN I-DISP-METHOD-DELETE ALSO I-DISP-OBJ-BUSR-ITEM
               PERFORM F-DELETE-BUSR
             WHEN I-DISP-METHOD-PUT    ALSO I-DISP-OBJ-BUSR-ITEM
               PERFORM F-UPDATE-BUSR
             WHEN OTHER
               SET O-DISP-ERR-BUSR-BAD-COMBO TO TRUE
               MOVE PGM-ID                   TO WS-ERROR-SOURCE
           END-EVALUATE
           .
       F-PROCESS-REQUEST-END.
           EXIT.            
      ****************************************************************** 
       F-GET-BUSR-ITEM SECTION.
           INITIALIZE BUSRDB-INTERFACE
           SET I-BUSR-OP-AUTHORIZE              TO TRUE
           MOVE I-DISP-BANKUSERID               TO I-BUSR-ID 
      *    
           SET PGNAME-BUSRDB                    TO TRUE
           CALL PROGNAME USING BUSRDB-INTERFACE
      *     
           EVALUATE TRUE
             WHEN BUSRDB-STATUS-OK
               MOVE O-BUSR-ID                   TO U-DISP-LOGIN-ID
               MOVE O-BUSR-USERNAME             TO U-DISP-LOGIN-USERNAME
               MOVE O-BUSR-ROLE                 TO U-DISP-LOGIN-ROLE
      *        
               MOVE "Authorization successful!" TO U-LOG-LINE
               PERFORM UT-LOG-SINGLE-LINE
             WHEN BUSRDB-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND TO TRUE
             WHEN OTHER 
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE
           .
       F-GET-BUSR-ITEM-END.
           EXIT.
      ******************************************************************
       F-GET-BUSR-LIST SECTION.
           IF NOT U-DISP-LOGIN-ADMIN
             SET O-DISP-ERR-AUTH-ROLE-FORBIDDEN    
                                           TO TRUE
             MOVE 1                        TO I-ERR-PARAM-COUNT
             MOVE U-DISP-LOGIN-ROLE        TO I-ERR-PARAM (1)                          
             EXIT SECTION
           END-IF

           MOVE I-GET-BUSR-PAGE-NUMBER     TO WS-PAGE-NUMBER 
           PERFORM F-VALIDATE-AND-SET-PAGE-NUMBER

           INITIALIZE BUSRDB-INTERFACE
           SET I-BUSR-OP-GETUSERS          TO TRUE         
                               
           MOVE WS-VALIDATED-PAGE-NUMBER   TO I-BUSR-PAGE-NUMBER
           SET PGNAME-BUSRDB               TO TRUE
           CALL PROGNAME USING BUSRDB-INTERFACE
             
           EVALUATE TRUE
             WHEN BUSRDB-STATUS-OK 
               MOVE O-BUSR-COUNT           TO O-GET-BUSR-COUNT
  
               PERFORM VARYING IND-1 FROM 1 BY 1 
                                     UNTIL IND-1 > O-BUSR-COUNT
                 MOVE O-BUSR-L-ID(IND-1)      
                                           TO O-GET-BUSR-ID(IND-1)
                 MOVE O-BUSR-L-USERNAME(IND-1) 
                                           TO O-GET-BUSR-USERNAME(IND-1)
                 MOVE O-BUSR-L-ROLE(IND-1) TO O-GET-BUSR-ROLE(IND-1)
               END-PERFORM 
             WHEN OTHER 
               SET O-DISP-ERR-DB-SQL       TO TRUE
           END-EVALUATE
           .
       F-GET-BUSR-LIST-END.
           EXIT.
      ****************************************************************** 
       F-PROCESS-LOGIN SECTION.
           INITIALIZE BUSRDB-INTERFACE
           SET I-BUSR-OP-LOGIN                  TO TRUE
           MOVE I-POST-LOGIN-USERNAME           TO I-BUSR-USERNAME 
           MOVE I-POST-LOGIN-PASSWORD           TO I-BUSR-PASSWORD 
      *      
           SET PGNAME-BUSRDB                    TO TRUE
           CALL PROGNAME USING BUSRDB-INTERFACE
      *     
           EVALUATE TRUE
             WHEN BUSRDB-STATUS-OK
               MOVE O-BUSR-ID                   TO O-POST-LOGIN-ID
               MOVE O-BUSR-USERNAME             TO O-POST-LOGIN-USERNAME
               MOVE O-BUSR-ROLE                 TO O-POST-LOGIN-ROLE
      *        
               MOVE "Login successful!"         TO U-LOG-LINE
               PERFORM UT-LOG-SINGLE-LINE
             WHEN BUSRDB-STATUS-BAD-USERNAME
               SET O-DISP-ERR-BUSR-BAD-USERNAME TO TRUE
               MOVE 1                           TO I-ERR-PARAM-COUNT
               MOVE I-POST-LOGIN-USERNAME       TO I-ERR-PARAM (1)
             WHEN BUSRDB-STATUS-BAD-PASSWORD
               SET O-DISP-ERR-BUSR-BAD-PASSWORD TO TRUE
               MOVE 1                           TO I-ERR-PARAM-COUNT 
               MOVE I-POST-LOGIN-USERNAME       TO I-ERR-PARAM (1)
             WHEN OTHER      
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE
           . 
       F-PROCESS-LOGIN-END.
           EXIT.
      ******************************************************************
       F-VALIDATE-AND-SET-PAGE-NUMBER SECTION.
           EVALUATE TRUE
             WHEN WS-PAGE-NUMBER IS NUMERIC
               IF WS-PAGE-NUMBER = 0
                 MOVE 1              TO WS-VALIDATED-PAGE-NUMBER
               ELSE
                 MOVE WS-PAGE-NUMBER TO WS-VALIDATED-PAGE-NUMBER
               END-IF
             WHEN WS-PAGE-NUMBER = SPACES
               MOVE 1                TO WS-VALIDATED-PAGE-NUMBER
             WHEN OTHER
               SET O-DISP-ERR-ACC-WRONG-PAGE-FORMAT 
                                     TO TRUE
           END-EVALUATE
           .
       F-VALIDATE-AND-SET-PAGE-NUMBER-END.
           EXIT.    
      ******************************************************************
       F-CREATE-BUSR SECTION.
      *
           MOVE I-POST-BUSR-ROLE              TO WS-BF-ROLE
           PERFORM BUSR-PERMISSION-CHECK

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           INITIALIZE BUSRDB-INTERFACE
           SET I-BUSR-OP-POST                 TO TRUE
           MOVE I-POST-BUSR-USERNAME          TO I-BUSR-USERNAME
           MOVE I-POST-BUSR-PASSWORD          TO I-BUSR-PASSWORD
           MOVE I-POST-BUSR-ROLE              TO I-BUSR-ROLE
        
           SET PGNAME-BUSRDB                  TO TRUE
           CALL PROGNAME USING BUSRDB-INTERFACE

           EVALUATE TRUE
             WHEN BUSRDB-STATUS-OK
               MOVE O-BUSR-ID                 TO O-POST-BUSR-ID
               MOVE O-BUSR-USERNAME           TO O-POST-BUSR-USERNAME
               MOVE O-BUSR-ROLE               TO O-POST-BUSR-ROLE
             WHEN BUSRDB-STATUS-MULTIPLE-ERR
               SET O-DISP-ERR-DB-UNIQUE       TO TRUE
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL          TO TRUE
           END-EVALUATE
           .
      *
       F-CREATE-BUSR-END.
           EXIT. 
      ******************************************************************
       F-DELETE-BUSR SECTION.
      *
           *> GET ROLE OF THE BUSR TO BE REMOVED
           MOVE I-DEL-BUSR-ID                   TO WS-BF-BUSR-ID
           PERFORM UT-GET-ROLE

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           PERFORM BUSR-PERMISSION-CHECK

           IF NOT O-DISP-ERR-OK
             EXIT SECTION
           END-IF

           INITIALIZE BUSRDB-INTERFACE
           SET I-BUSR-OP-DELETE                 TO TRUE
           MOVE I-DEL-BUSR-ID                   TO I-BUSR-ID
     
           SET PGNAME-BUSRDB                    TO TRUE
           CALL PROGNAME USING BUSRDB-INTERFACE

           EVALUATE TRUE
             WHEN BUSRDB-STATUS-OK
               MOVE O-BUSR-ID                   TO O-DEL-BUSR-ID
             WHEN BUSRDB-STATUS-NOT-FOUND-ERR
               SET O-DISP-ERR-DB-ITEM-NOT-FOUND TO TRUE
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE
           .
      *
       F-DELETE-BUSR-END.
           EXIT. 
      ******************************************************************
       F-UPDATE-BUSR SECTION.
      *
           *> GET ROLE OF THE BUSR TO BE UPDATED
           MOVE I-PUT-BUSR-ID                   TO WS-BF-BUSR-ID
           PERFORM UT-GET-ROLE   
   
           IF NOT O-DISP-ERR-OK   
             EXIT SECTION   
           END-IF   
   
           PERFORM BUSR-PERMISSION-CHECK   
   
           *> ONLY ADMIN IS ALLOWED TO MODIFY ROLES
           IF WS-BF-ROLE <> I-PUT-BUSR-ROLE    
              AND NOT U-DISP-LOGIN-ADMIN   
             SET O-DISP-ERR-BUSR-CHANGE-ROLE    TO TRUE
             EXIT SECTION
           END-IF

           INITIALIZE BUSRDB-INTERFACE
           SET I-BUSR-OP-PUT                    TO TRUE
           MOVE I-PUT-BUSR-ID                   TO I-BUSR-ID
           MOVE I-PUT-BUSR-USERNAME             TO I-BUSR-USERNAME
           MOVE I-PUT-BUSR-ROLE                 TO I-BUSR-ROLE
     
           SET PGNAME-BUSRDB                    TO TRUE
           CALL PROGNAME USING BUSRDB-INTERFACE

           EVALUATE TRUE
             WHEN BUSRDB-STATUS-OK 
               MOVE O-BUSR-ID                   TO O-PUT-BUSR-ID
               MOVE O-BUSR-USERNAME             TO O-PUT-BUSR-USERNAME
               MOVE O-BUSR-ROLE                 TO O-PUT-BUSR-ROLE
             WHEN BUSRDB-STATUS-MULTIPLE-ERR
               SET O-DISP-ERR-DB-UNIQUE         TO TRUE
             WHEN OTHER
               SET O-DISP-ERR-DB-SQL            TO TRUE
           END-EVALUATE
           .
      *
       F-UPDATE-BUSR-END.
           EXIT. 
      ******************************************************************
       F-HANDLE-ERROR SECTION.
           MOVE O-DISP-ERROR-NO TO I-ERR-CODE
      *     
           SET PGNAME-ERROR     TO TRUE
           CALL PROGNAME USING ERROR-INTERFACE
      * 
           IF WS-ERROR-SOURCE = SPACE
             MOVE PGM-ID        TO WS-ERROR-SOURCE
           END-IF
      *     
           STRING FUNCTION TRIM(WS-ERROR-SOURCE) ": "
                  FUNCTION TRIM(O-ERR-MESSAGE)
             DELIMITED BY SIZE
             INTO O-DISP-ERROR-MESSAGE
           .
       F-HANDLE-ERROR-END.
           EXIT.
      ******************************************************************
       BUSR-PERMISSION-CHECK SECTION.
      *
           IF NOT WS-BF-ROLE-OK
             SET O-DISP-ERR-BUSR-UNKNOWN-ROLE      TO TRUE
             EXIT SECTION
           END-IF
           EVALUATE TRUE ALSO TRUE
      *      Allowed to operate on BaTe and BaCl
             WHEN U-DISP-LOGIN-ADMIN ALSO WS-BF-ROLE-BACL
               CONTINUE
             WHEN U-DISP-LOGIN-ADMIN ALSO WS-BF-ROLE-BATE
               CONTINUE

      *      Allowed to operate on BaCl
             WHEN U-DISP-LOGIN-TELLER ALSO WS-BF-ROLE-BACL
               CONTINUE

             WHEN OTHER
               SET O-DISP-ERR-AUTH-ROLE-FORBIDDEN  TO TRUE
               MOVE 1                              TO I-ERR-PARAM-COUNT
               MOVE U-DISP-LOGIN-ROLE              TO I-ERR-PARAM (1)
           END-EVALUATE       
           .
      *
       BUSR-PERMISSION-CHECK-END.
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
               MOVE 1                           TO I-ERR-PARAM-COUNT
               MOVE I-POST-LOGIN-USERNAME       TO I-ERR-PARAM (1)  
             WHEN BUSRDB-STATUS-BAD-PASSWORD
               SET O-DISP-ERR-BUSR-BAD-PASSWORD TO TRUE
               MOVE 1                           TO I-ERR-PARAM-COUNT 
               MOVE I-POST-LOGIN-USERNAME       TO I-ERR-PARAM (1) 
           END-EVALUATE
           .
       UT-GET-ROLE-END.
           EXIT. 
      ******************************************************************     
      *   COPY LOGGERUTILSECTIONS TO USE GENERIC SECTIONS FOR LOGGER
      ****************************************************************** 
       COPY LOGGERUTILSECTIONS.     
       