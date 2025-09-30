       01 TRANSDB-INTERFACE.
         05 TRANSDB-IN.
           10 I-TRANS-OPERATION                PIC X(08).
             88 I-TRANS-OP-GET-LIST            VALUE "GETLIST ".
             88 I-TRANS-OP-POST                VALUE "POST    ".
           10 I-TRANS-PAGE-NUMBER              PIC 9(05).
           10 I-TRANS-ACCOUNTID                PIC 9(05).
           10 I-TRANS-TRANS-TYPE               PIC X(10).
           10 I-TRANS-AMMOUNT                  PIC 9(08)V99.
           10 I-TRANS-TIMESTAMP                PIC X(22).
           10 I-TRANS-ACCBALANCE               PIC 9(08)V99.

         05 TRANSDB-STATUS                     PIC X(02).
           88 TRANSDB-STATUS-OK                VALUE "00".
           88 TRANSDB-STATUS-NOT-FOUND-ERR     VALUE "90".
           88 TRANSDB-STATUS-MAX-RECORDS-ERR   VALUE "91".
           88 TRANSDB-STATUS-VAR-MIS-ERR       VALUE "92".
           88 TRANSDB-STATUS-ERROR-CONN        VALUE "97".
           88 TRANSDB-STATUS-SQL-ERR           VALUE "99".

         05 O-TRANSDB-SQLCODE                  PIC S9(9).

         05 TRANSDB-OUT.
           10 O-TRANSDB-COUNT                  PIC 9(02).
           10 O-TRANSDB-ELEM                   OCCURS 50 TIMES.
             15 O-TRANS-ACCOUNTID              PIC 9(05).
             15 O-TRANS-TRANS-TYPE             PIC X(10).
             15 O-TRANS-AMMOUNT                PIC 9(8)V99.
             15 O-TRANS-TIMESTAMP              PIC X(25).
             15 O-TRANS-ID                     PIC 9(05).
             15 O-TRANS-ACCBALANCE             PIC 9(08)V99.
