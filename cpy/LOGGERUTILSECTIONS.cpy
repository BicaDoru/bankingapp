      ******************************************************************
      * LOGGERUTILSECTIONS - generic reusable sections for logging
      ****************************************************************** 
      ******************************************************************
      * Initialize the logger
      ******************************************************************
       UT-LOG-INIT SECTION.
           INITIALIZE LOGGER-INTERFACE
           MOVE 1                  TO I-IDX-LOG-SPLIT
           SET U-LOG-PGNAME-LOGGER TO TRUE
           .
       UT-LOG-INIT-END.
           EXIT.
      ******************************************************************
      * Logs a "module started" line and the input data
      ******************************************************************
       UT-LOG-MODULE-START SECTION.
           MOVE U-LOG-LINE                   TO U-LOG-BUFFER-LINE
           MOVE SPACES                       TO U-LOG-LINE
      *    
           STRING
             "*** "
             FUNCTION TRIM(U-LOG-RUNNABLE-PROG) DELIMITED BY SIZE
             " STARTED ----- INPUT DATA: "
             U-LOG-BUFFER-LINE
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-SINGLE-LINE
           .
       UT-LOG-MODULE-START-END.
           EXIT.
      ******************************************************************
      * Logs a "module finished" line and the output data
      ******************************************************************
       UT-LOG-MODULE-FINISH SECTION.
           MOVE U-LOG-LINE                   TO U-LOG-BUFFER-LINE
           MOVE SPACES                       TO U-LOG-LINE
      *    
           STRING
             "*** "     
             FUNCTION TRIM(U-LOG-RUNNABLE-PROG) DELIMITED BY SIZE
             " FINISHED --- OUTPUT DATA: "
             U-LOG-BUFFER-LINE
             INTO U-LOG-LINE
           END-STRING
           PERFORM UT-LOG-SINGLE-LINE
           .
       UT-LOG-MODULE-FINISH-END.
           EXIT.
      ******************************************************************
      * Logs a DB connection fail
      ******************************************************************
       UT-LOG-DBCONNECT-ERR SECTION.
           MOVE SPACES                       TO U-LOG-LINE
      * 
           MOVE "ERROR CONNECTING TO THE DB" TO U-LOG-LINE
      * 
           PERFORM UT-LOG-SINGLE-LINE
           .
       UT-LOG-DBCONNECT-ERR-END.
           EXIT.     
      ******************************************************************
      * Logs a newline for readability
      ******************************************************************
       UT-LOG-NEWLINE SECTION.
           MOVE SPACES TO U-LOG-LINE
      * 
           PERFORM UT-LOG-SINGLE-LINE
           .
       UT-LOG-NEWLINE-END.
           EXIT. 
      ******************************************************************
      * Logs a LINE FOR SINGLE LINE
      ******************************************************************
       UT-LOG-SINGLE-LINE SECTION.
           SET FG-LOG-MODE-SINGLE-LINE TO TRUE
           MOVE U-LOG-LINE             TO I-LOG-LONG-MESSAGE
      *     
           CALL U-LOG-PGNAME USING LOGGER-INTERFACE
           MOVE SPACES                 TO U-LOG-LINE
           .
       UT-LOG-SINGLE-LINE-END.
           EXIT.      
      ******************************************************************
      * Prepare message for UT-LOG-MULTI-LINE
      ******************************************************************
       UT-LOG-ADD-LINE SECTION.
           MOVE U-LOG-LINE TO I-LOG-LINE-MESSAGE(I-IDX-LOG-SPLIT)
           SET I-IDX-LOG-SPLIT UP BY 1
           MOVE SPACES     TO U-LOG-LINE
           .
       UT-LOG-ADD-LINE-END.
           EXIT.               
      ******************************************************************
      * Logs a LINE FOR MULTI LINE
      ******************************************************************
       UT-LOG-MULTI-LINE SECTION.
           SET FG-LOG-MODE-MULTI-LINE TO TRUE
      * 
           CALL U-LOG-PGNAME USING LOGGER-INTERFACE
           MOVE SPACES                TO I-LOG-SPLIT-MESSAGE
           SET I-IDX-LOG-SPLIT        TO 1
           .
       UT-LOG-MULTI-LINE-END.
           EXIT.     
               