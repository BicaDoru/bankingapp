      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 BUSRDB.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.

       01 PGM-ID              PIC X(20) VALUE "BUSRDB              ".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 H-PAGE-NUMBER       PIC 9(04).
       01 H-PAGE-SIZE         PIC S9(04) VALUE 10.
       01 H-OFFSET            PIC S9(09).
       EXEC SQL INCLUDE H-BANKUSER    END-EXEC.
       EXEC SQL INCLUDE DBUTILSVARS   END-EXEC.

       EXEC SQL END DECLARE SECTION   END-EXEC.

       EXEC SQL INCLUDE SQLCA         END-EXEC.

       01 INTERNAL-VARS.
         05 BUSR-CNT          PIC 9(04).
         05 WS-PAGE-NUMBER    PIC 9(05).

       01 CONSTANTS.
         05 K-PAGE-SIZE       PIC 9(02) VALUE 10.
        
       01 FLAGS.
         05 FG-BUSR-FOUND     PIC X.
           88 FG-FOUND-Y      VALUE 'Y'.
           88 FG-FOUND-N      VALUE 'N'.
      ******************************************************************
      *                        COPYLIB IMPORTS 
      ******************************************************************
       COPY LOGGERINTERFACE.
      * 
       LINKAGE SECTION.
       COPY BUSRINTERFACE.
      ******************************************************************
       PROCEDURE DIVISION USING BUSRDB-INTERFACE.
      ******************************************************************
       MAIN SECTION.
           PERFORM F-INIT
      * 
           IF BUSRDB-STATUS-OK
             PERFORM F-PROCESS-REQUEST
           END-IF
      *    
           PERFORM F-FINISH
           .
       MAIN-END.
           GOBACK.
      ****************************************************************** 
       F-INIT SECTION.
           SET BUSRDB-STATUS-OK                TO TRUE
           INITIALIZE BUSRDB-OUT
      *    
           PERFORM UT-LOG-INIT
           MOVE PGM-ID                         TO U-LOG-RUNNABLE-PROG
           MOVE BUSRDB-IN                      TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-START
      *    
           PERFORM DB-INIT-CONNECTION
           IF FG-CONNECT-N
             SET BUSRDB-STATUS-ERROR-CONN      TO TRUE
             PERFORM UT-LOG-DBCONNECT-ERR
           END-IF
      * 
           .
       F-INIT-END.
           EXIT.     
      ******************************************************************
       F-FINISH SECTION.

           IF NOT U-SQL-OK 
           AND BUSRDB-STATUS-OK
             PERFORM DB-ERROR-DEFAULT
           END-IF
      *    
           IF NOT BUSRDB-STATUS-OK
             SET FG-DB-COMMIT-N        TO TRUE
           END-IF
           PERFORM DB-FINISH
      *    
           MOVE PGM-ID                 TO U-LOG-RUNNABLE-PROG
           MOVE BUSRDB-OUT             TO U-LOG-LINE
           PERFORM UT-LOG-MODULE-FINISH
           .
       F-FINISH-END.
           EXIT.
      ******************************************************************
       F-PROCESS-REQUEST SECTION.
      *
           EVALUATE TRUE
             WHEN I-BUSR-OP-AUTHORIZE
               PERFORM BUSR-AUTHORIZE-LOGIN

             WHEN I-BUSR-OP-LOGIN
               PERFORM BUSR-SELECT-LOGIN
  
             WHEN I-BUSR-OP-GETROLE
               PERFORM BUSR-GET-ROLE
  
             WHEN I-BUSR-OP-GETUSERS
               PERFORM BUSR-GET-LIST

             WHEN I-BUSR-OP-POST
               PERFORM BUSR-POST

             WHEN I-BUSR-OP-DELETE
               PERFORM BUSR-DELETE
            
             WHEN I-BUSR-OP-PUT
               PERFORM BUSR-PUT
           END-EVALUATE
           .
      *
       F-PROCESS-REQUEST-END.
           EXIT. 
      ******************************************************************
       BUSR-GET-LIST SECTION.
           PERFORM LOG-BUSR-GET-ALLUSER
           MOVE I-BUSR-PAGE-NUMBER       TO WS-PAGE-NUMBER
           IF WS-PAGE-NUMBER < 1
             MOVE 1                      TO WS-PAGE-NUMBER
           END-IF

           COMPUTE H-OFFSET = (WS-PAGE-NUMBER - 1) * K-PAGE-SIZE
           MOVE K-PAGE-SIZE              TO H-PAGE-SIZE
           
           PERFORM LOG-OFFSET-CALC

           PERFORM DB-OP-BUSR-LIST-PAGE

           IF NOT U-SQL-OK
             EXIT SECTION
           END-IF

           MOVE 0                        TO BUSR-CNT
           PERFORM DB-FE-BUSR-LIST-PAGE

           PERFORM UNTIL NOT U-SQL-OK
           OR BUSR-CNT >= K-PAGE-SIZE
             ADD 1                       TO BUSR-CNT
             MOVE H-BUSR-ID              TO O-BUSR-L-ID(BUSR-CNT)
             MOVE H-BUSR-USERNAME        TO O-BUSR-L-USERNAME(BUSR-CNT)
             MOVE H-BUSR-ROLE            TO O-BUSR-L-ROLE(BUSR-CNT) 
             PERFORM DB-FE-BUSR-LIST-PAGE
           END-PERFORM
           MOVE BUSR-CNT                 TO O-BUSR-COUNT
           IF U-SQL-OK 
           OR U-SQL-NO-DATA
             PERFORM DB-CL-BUSR-LIST-PAGE
           END-IF
           .
       BUSR-GET-LIST-END.
           EXIT.
      ******************************************************************
       BUSR-AUTHORIZE-LOGIN SECTION.
           PERFORM LOG-AUTHORIZE-LOGIN
           
           MOVE I-BUSR-ID                    TO H-BUSR-ID
           PERFORM DB-SE-BUSR2
           
           MOVE H-BUSR-ID                    TO O-BUSR-ID
           MOVE H-BUSR-ROLE                  TO O-BUSR-ROLE

           IF U-SQL-NO-DATA
             SET BUSRDB-STATUS-NOT-FOUND-ERR TO TRUE
           END-IF
           .
       BUSR-AUTHORIZE-LOGIN-END.
           EXIT.                           
      ******************************************************************
       BUSR-GET-ROLE SECTION.
           PERFORM LOG-BUSR-GET-ROLE
           
           MOVE I-BUSR-ID      TO H-BUSR-ID
           PERFORM DB-SE-BUSR3

           IF U-SQL-OK
             MOVE H-BUSR-ROLE  TO O-BUSR-ROLE
           END-IF
           .
       BUSR-GET-ROLE-END.
           EXIT.                               
      ******************************************************************
       BUSR-SELECT-LOGIN SECTION.
           PERFORM LOG-SELECT-LOGIN
           SET FG-FOUND-Y                     TO TRUE
      *    
           MOVE I-BUSR-USERNAME               TO H-BUSR-USERNAME
           PERFORM DB-OP-BUSR1
           PERFORM DB-FE-BUSR1
           
           IF U-SQL-OK
             IF H-BUSR-PASSWORD = I-BUSR-PASSWORD
               MOVE H-BUSR-ID                 TO O-BUSR-ID
               MOVE H-BUSR-USERNAME           TO O-BUSR-USERNAME
               MOVE H-BUSR-ROLE               TO O-BUSR-ROLE
             ELSE
               SET BUSRDB-STATUS-BAD-PASSWORD TO TRUE
             END-IF
           END-IF
           IF U-SQL-NO-DATA
             SET FG-FOUND-N                   TO TRUE
             SET BUSRDB-STATUS-BAD-USERNAME   TO TRUE
           END-IF
      * 
           PERFORM DB-CL-BUSR1
           .
       BUSR-SELECT-LOGIN-END.
           EXIT.
      ******************************************************************
       BUSR-POST SECTION.
      *
           MOVE I-BUSR-USERNAME                TO H-BUSR-USERNAME
           MOVE I-BUSR-PASSWORD                TO H-BUSR-PASSWORD
           MOVE I-BUSR-ROLE                    TO H-BUSR-ROLE

           PERFORM LOG-BUSR-POST

           PERFORM DB-INS-BUSR

           EVALUATE TRUE
             WHEN U-SQL-OK
               MOVE H-BUSR-ID                  TO O-BUSR-ID
               MOVE H-BUSR-USERNAME            TO O-BUSR-USERNAME
               MOVE H-BUSR-ROLE                TO O-BUSR-ROLE
             WHEN U-SQL-UNIQUE-DATA 
               SET BUSRDB-STATUS-MULTIPLE-ERR  TO TRUE
           END-EVALUATE
           .
      *
       BUSR-POST-END.
           EXIT. 
      ******************************************************************
       BUSR-DELETE SECTION.
      *
           MOVE I-BUSR-ID                      TO H-BUSR-ID
 
           PERFORM LOG-BUSR-DELETE 
 
           PERFORM DB-DEL-BUSR 
 
           EVALUATE TRUE 
             WHEN U-SQL-OK 
               MOVE H-BUSR-ID                  TO O-BUSR-ID
             WHEN U-SQL-NO-DATA
               SET BUSRDB-STATUS-NOT-FOUND-ERR TO TRUE
           END-EVALUATE
           .
      *
       BUSR-DELETE-END.
           EXIT. 
      ******************************************************************
       BUSR-PUT SECTION.
      *
           MOVE I-BUSR-ID                      TO H-BUSR-ID
           MOVE I-BUSR-USERNAME                TO H-BUSR-USERNAME
           MOVE I-BUSR-ROLE                    TO H-BUSR-ROLE
           
           PERFORM LOG-BUSR-PUT           
           
           PERFORM DB-UPD-BUSR           
           
           EVALUATE TRUE           
             WHEN U-SQL-OK           
               MOVE H-BUSR-ID                  TO O-BUSR-ID
               MOVE H-BUSR-USERNAME            TO O-BUSR-USERNAME
               MOVE H-BUSR-ROLE                TO O-BUSR-ROLE
             WHEN U-SQL-UNIQUE-DATA
               SET BUSRDB-STATUS-MULTIPLE-ERR  TO TRUE
           END-EVALUATE
           .
      *
       BUSR-PUT-END.
           EXIT. 
      ******************************************************************
       DB-UPD-BUSR SECTION.
      *
           EXEC SQL 
             UPDATE BANKUSER
             SET USERNAME = :H-BUSR-USERNAME,
                 ROLE = :H-BUSR-ROLE
             WHERE ID = :H-BUSR-ID
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-UPD-BUSR-END.
           EXIT. 
      ******************************************************************
       DB-DEL-BUSR SECTION.
      *
           EXEC SQL
             DELETE FROM BANKUSER
             WHERE ID = :H-BUSR-ID
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-DEL-BUSR-END.
           EXIT. 
      ******************************************************************
       DB-INS-BUSR SECTION.
      *
           EXEC SQL
             INSERT INTO BANKUSER (USERNAME, PASSWORD, ROLE)
             VALUES (:H-BUSR-USERNAME,
                     :H-BUSR-PASSWORD,
                     :H-BUSR-ROLE) 
           END-EXEC
           
           PERFORM DB-SQL-DEFAULT

           IF U-SQL-OK
             EXEC SQL
               SELECT lastval() INTO :H-BUSR-ID
             END-EXEC

             PERFORM DB-SQL-DEFAULT
           END-IF
           .
      *
       DB-INS-BUSR-END.
           EXIT. 
      ******************************************************************
       DB-OP-BUSR-LIST-PAGE SECTION.
           EXEC SQL
             DECLARE CURS_BUSR_LIST_PAGE CURSOR FOR
               SELECT ID, USERNAME, ROLE
                 FROM BANKUSER
                 ORDER BY ID
                 LIMIT :H-PAGE-SIZE OFFSET :H-OFFSET
           END-EXEC

           EXEC SQL
             OPEN CURS_BUSR_LIST_PAGE
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
       DB-OP-BUSR-LIST-PAGE-END.
           EXIT.
      ******************************************************************
       DB-FE-BUSR-LIST-PAGE SECTION.
           EXEC SQL
             FETCH CURS_BUSR_LIST_PAGE
             INTO :H-BUSR-ID,
                  :H-BUSR-USERNAME,
                  :H-BUSR-ROLE
           END-EXEC
      * 
           PERFORM DB-SQL-DEFAULT
           .
       DB-FE-BUSR-LIST-PAGE-END.
           EXIT.
      ******************************************************************
       DB-CL-BUSR-LIST-PAGE SECTION.
           EXEC SQL
             CLOSE CURS_BUSR_LIST_PAGE
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
       DB-CL-BUSR-LIST-PAGE-END.
           EXIT.
      ****************************************************************** 
       DB-OP-BUSR1 SECTION.
           EXEC SQL
             DECLARE CURS_BUSR1 CURSOR FOR
             SELECT id, password, role
             FROM BANKUSER
             WHERE USERNAME = TRIM(:H-BUSR-USERNAME)
           END-EXEC

           EXEC SQL
             OPEN CURS_BUSR1
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
       DB-OP-BUSR1-END.
           EXIT.
      ******************************************************************
       DB-SE-BUSR2 SECTION.
      *
           EXEC SQL
             SELECT USERNAME, ROLE INTO :H-BUSR-USERNAME 
                                        :H-BUSR-ROLE 
             FROM BANKUSER
             WHERE ID = :H-BUSR-ID
           END-EXEC
           
           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-SE-BUSR2-END.
           EXIT. 
      ******************************************************************
       DB-SE-BUSR3 SECTION.
      *
           EXEC SQL
             SELECT ROLE INTO :H-BUSR-ROLE FROM BANKUSER
             WHERE ID = :H-BUSR-ID
           END-EXEC
           
           PERFORM DB-SQL-DEFAULT
           .
      *
       DB-SE-BUSR3-END.
           EXIT. 
      ******************************************************************
       DB-FE-BUSR1 SECTION.
           EXEC SQL
           FETCH CURS_BUSR1 INTO  :H-BUSR-ID,
                                  :H-BUSR-PASSWORD,
                                  :H-BUSR-ROLE
           END-EXEC
      * 
           PERFORM DB-SQL-DEFAULT
           .
       DB-FE-BUSR1-END.
           EXIT.     
      ******************************************************************
       DB-CL-BUSR1 SECTION.
           EXEC SQL
             CLOSE CURS_BUSR1
           END-EXEC

           PERFORM DB-SQL-DEFAULT
           .
       DB-CL-BUSR1-END.
           EXIT. 
      ******************************************************************
       DB-ERROR-DEFAULT SECTION.
           SET BUSRDB-STATUS-SQL-ERROR TO TRUE
           MOVE U-SQLCODE              TO O-BUSRDB-SQLCODE
           .
       DB-ERROR-DEFAULT-END.
           EXIT.
      ******************************************************************
       LOG-BUSR-GET-ROLE SECTION.
           STRING
             "Retrieving role for USERID: ", 
             I-BUSR-ID 
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-BUSR-GET-ROLE-END.
           EXIT. 
      ******************************************************************
       LOG-AUTHORIZE-LOGIN SECTION.
           STRING
             'Authorizing login for USERID: ',
             I-BUSR-ID
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-AUTHORIZE-LOGIN-END.
           EXIT. 
      ******************************************************************
       LOG-SELECT-LOGIN SECTION.
           MOVE
             '*******************************************************'
                                             TO U-LOG-LINE
           PERFORM UT-LOG-ADD-LINE
      *     
           STRING
             'Attempting login for USER: ',
             I-BUSR-USERNAME
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-SELECT-LOGIN-END.
           EXIT.
      ******************************************************************
       LOG-BUSR-GET-ALLUSER SECTION.
           MOVE "Retrieving all users..." TO U-LOG-LINE
      * 
           PERFORM UT-LOG-SINGLE-LINE
           .
       LOG-BUSR-GET-ALLUSER-END.
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
       LOG-BUSR-POST SECTION.
      *
           STRING
             "Creating new BANKUSER: ",
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "USERNAME             : ",
             H-BUSR-USERNAME
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "ROLE                 : ",
             H-BUSR-ROLE
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           PERFORM UT-LOG-MULTI-LINE
           .
      *
       LOG-BUSR-POST-END.
           EXIT. 
      ******************************************************************
       LOG-BUSR-PUT SECTION.
      *
           STRING
             "Updating BANKUSER with ID: ",
             H-BUSR-ID
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "USERNAME             : ",
             H-BUSR-USERNAME
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE

           STRING
             "ROLE                 : ",
             H-BUSR-ROLE
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-ADD-LINE
      *     
           PERFORM UT-LOG-MULTI-LINE
           .
      *
       LOG-BUSR-PUT-END.
           EXIT. 
      ******************************************************************
       LOG-BUSR-DELETE SECTION.
      *
           STRING
             'Deleting BANKUSER with ID: ',
             H-BUSR-ID
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-SINGLE-LINE
           .
      *
       LOG-BUSR-DELETE-END.
           EXIT. 
      ******************************************************************
      *   COPY DBUTILSECTIONS TO USE GENERIC SECTIONS FOR DB CONNECTION
      ******************************************************************
       EXEC SQL INCLUDE DBUTILSECTIONS END-EXEC.
      ******************************************************************     
      *   COPY LOGGERUTILSECTIONS TO USE GENERIC SECTIONS FOR LOGGER
      ****************************************************************** 
       COPY LOGGERUTILSECTIONS.     
       