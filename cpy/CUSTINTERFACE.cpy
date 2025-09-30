       01 CUSTDB-INTERFACE.
         05 CUSTDB-IN.
           10 I-CUST-CUSTID                    PIC 9(05).
           10 I-CUST-USERNAME                  PIC X(50).
           10 I-CUST-ADDRESS                   PIC X(50).
           10 I-CUST-BANKUSERID                PIC 9(05).
           10 I-CUST-PAGE-NUMBER               PIC 9(05).
           10 I-CUST-OPERATION                 PIC X(08).
             88 I-CUST-OP-OK                   VALUE "GETITEM "
                                                     "GETLIST "
                                                     "POST    "
                                                     "DELETE  "
                                                     "PUT     ".
             88 I-CUST-OP-GET-ITEM             VALUE "GETITEM ".
             88 I-CUST-OP-GET-LIST             VALUE "GETLIST ".
             88 I-CUST-OP-POST                 VALUE "POST    ".
             88 I-CUST-OP-DELETE               VALUE "DELETE  ".
             88 I-CUST-OP-PUT                  VALUE "PUT     ".

         05 CUSTDB-STATUS                      PIC X(02).
           88 CUST-STATUS-OK                   VALUE "00".
           88 CUST-STATUS-NOT-FOUND-ERR        VALUE "90".
           88 CUST-STATUS-MULTIPLE-ERR         VALUE "91".
           88 CUST-STATUS-VAR-MIS-ERR          VALUE "92".
           88 CUST-STATUS-ERROR-CONN           VALUE "97".
           88 CUST-STATUS-SQL-ERROR            VALUE "99".

         05 O-CUSTDB-SQLCODE                   PIC S9(9).

         05 CUSTDB-OUT.
           10 O-CUST-COUNT                     PIC 9(02).
           10 O-CUST-ELEM                      OCCURS 20 TIMES.         
             15 O-CUST-ID                      PIC 9(05).
             15 O-CUST-USERNAME                PIC X(50).
             15 O-CUST-ADDRESS                 PIC X(50).
             15 O-CUST-BANKUSERID              PIC 9(05).
