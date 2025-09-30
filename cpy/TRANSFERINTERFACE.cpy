       01 TRANSFERDB-INTERFACE.
         05 TRANSFERDB-IN.
           10 I-TRANSFER-OPERATION            PIC X(08).
             88 I-TRANSFER-OP-POST            VALUE "POST    ".
             88 I-TRANSFER-OP-DELETE          VALUE "DELETE  ".
             88 I-TRANSFER-OP-GETLIST         VALUE "GETLIST ".
           10 I-TRANSFER-ID                   PIC 9(05).
           10 I-TRANSFER-SRCIBAN              PIC X(30).
           10 I-TRANSFER-DESTIBAN             PIC X(30).
           10 I-TRANSFER-AMOUNT               PIC 9(08)V99.
           10 I-TRANSFER-TIMESTAMP            PIC X(22).
           10 I-TRANSFER-CURRENCY             PIC X(3).
           10 I-TRANSFER-FILTER-IBAN          PIC X(30).
           10 I-TRANSFER-FILTER-CUSTID        PIC 9(05).
           10 I-TRANSFER-PAGE-NUMBER          PIC 9(05).

         05 TRANSFERDB-STATUS                 PIC X(02).
           88 TRANSFERDB-STATUS-OK            VALUE "00".
           88 TRANSFERDB-STATUS-ERROR-CONN    VALUE "97".
           88 TRANSFERDB-STATUS-SQL-ERR       VALUE "99".

         05 O-TRANSFER-SQLCODE                PIC S9(9).

         05 TRANSFERDB-OUT.
           10 O-TRANSFER-COUNT                PIC 9(02).
           10 O-TRANSFER-ELEM                 OCCURS 50 TIMES.
             15 O-TRANSFER-ID                 PIC 9(05).
             15 O-TRANSFER-SRCIBAN            PIC X(30).
             15 O-TRANSFER-DESTIBAN           PIC X(30).
             15 O-TRANSFER-AMOUNT             PIC 9(08)V99.
             15 O-TRANSFER-TIMESTAMP          PIC X(22).
             15 O-TRANSFER-CURRENCY           PIC X(3).
