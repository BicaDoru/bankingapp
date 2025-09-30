       01 ACCDB-INTERFACE.
         05 ACCDB-IN.
           10 I-ACC-OPERATION                PIC X(08).
             88 I-ACC-OP-OK                  VALUE "GETLIST "
                                                   "GETITEM "
                                                   "PUT     "
                                                   "POST    "
                                                   "DELETE  ".
             88 I-ACC-OP-GETITEM             VALUE "GETITEM ".
             88 I-ACC-OP-GETLIST             VALUE "GETLIST ".
             88 I-ACC-OP-PUT                 VALUE "PUT     ".
             88 I-ACC-OP-POST                VALUE "POST    ".
             88 I-ACC-OP-DELETE              VALUE "DELETE  ".
           10 I-ACC-ACCOUNTID                PIC 9(05).
           10 I-ACC-PAGE-NUMBER              PIC 9(05). 
           10 I-ACC-CUSTOMERID               PIC 9(05).
           10 I-ACC-BALANCE                  PIC 9(08)V99.
           10 I-ACC-IBAN                     PIC X(30).
           10 I-ACC-CURRENCY                 PIC X(3).

         05 ACCDB-STATUS                     PIC X(02).
           88 ACCDB-STATUS-OK                VALUE "00".
           88 ACCDB-STATUS-NOT-FOUND-ERR     VALUE "90".
      *    TODO: does not seems to be used anywhere
           88 ACCDB-STATUS-MULTIPLE-ERR      VALUE "91".
      *    TODO: does not seems to be used anywhere
           88 ACCDB-STATUS-VAR-MIS-ERR       VALUE "92".
           88 ACCDB-STATUS-ERROR-CONN        VALUE "97".
           88 ACCDB-STATUS-SQL-ERROR         VALUE "99".

         05 O-ACCDB-SQLCODE                  PIC S9(9).

         05 ACCDB-OUT.
           10 O-ACC-COUNT                    PIC 9(02).
           10 O-ACC-ELEM                     OCCURS 20 TIMES.
             15 O-ACC-ACCOUNTID              PIC 9(05).
             15 O-ACC-CUSTOMERID             PIC 9(05).
             15 O-ACC-IBAN                   PIC X(30).
             15 O-ACC-CURRENCY               PIC X(05).
             15 O-ACC-BALANCE                PIC 9(08)V99.
             