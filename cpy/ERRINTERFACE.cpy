       01 ERROR-INTERFACE.
         05 ERROR-IN.
           10 I-ERR-CODE                  PIC X(04).
           10 I-ERR-PARAM-COUNT           PIC 9(01).
           10 I-ERR-PARAMS.
             15 I-ERR-PARAM               PIC X(50) OCCURS 5 TIMES.
         05 ERROR-OUT.
           10 O-ERR-MESSAGE               PIC X(100).
           10 O-ERR-STATUS                PIC X(02).
             88 O-ERR-STATUS-OK           VALUE "00".
             88 O-ERR-STATUS-NOT-FOUND    VALUE "90".
