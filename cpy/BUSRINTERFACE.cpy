       01 BUSRDB-INTERFACE.
         05 BUSRDB-IN.
           10 I-BUSR-USERNAME                    PIC X(50).
           10 I-BUSR-PASSWORD                    PIC X(60).
           10 I-BUSR-ROLE                        PIC X(04).
           10 I-BUSR-OPERATION                   PIC X(08).
             88 I-BUSR-OP-OK                     VALUE "LOGIN   "
                                                       "POST    "
                                                       "DELETE  "
                                                       "PUT     "
                                                       "AUTH    "
                                                       "GETROLE "
                                                       "GETUSERS".
             88 I-BUSR-OP-LOGIN                  VALUE "LOGIN   ".
             88 I-BUSR-OP-POST                   VALUE "POST    ".
             88 I-BUSR-OP-DELETE                 VALUE "DELETE  ".
             88 I-BUSR-OP-PUT                    VALUE "PUT     ".
             88 I-BUSR-OP-AUTHORIZE              VALUE "AUTH    ".
             88 I-BUSR-OP-GETROLE                VALUE "GETROLE ".
             88 I-BUSR-OP-GETUSERS               VALUE "GETUSERS".
           10 I-BUSR-ID                          PIC 9(05).
           10 I-BUSR-PAGE-NUMBER                 PIC 9(05).
         05 BUSRDB-STATUS                        PIC 9(02).
           88 BUSRDB-STATUS-OK                   VALUE "00".
           88 BUSRDB-STATUS-NOT-FOUND-ERR        VALUE "90".
           88 BUSRDB-STATUS-MAX-RECORDS-ERR      VALUE "91".
           88 BUSRDB-STATUS-VAR-MIS-ERR          VALUE "92".
           88 BUSRDB-STATUS-MULTIPLE-ERR         VALUE "93".
           88 BUSRDB-STATUS-BAD-USERNAME         VALUE "94".
           88 BUSRDB-STATUS-BAD-PASSWORD         VALUE "95".
           88 BUSRDB-STATUS-ERROR-CONN           VALUE "97".
           88 BUSRDB-STATUS-SQL-ERROR            VALUE "99".
      
         05 O-BUSRDB-SQLCODE                     PIC S9(9).
      
         05 BUSRDB-OUT.
           10 O-BUSR-COUNT                       PIC 9(02).
           10 O-BUSR-LIST-ELEM                   OCCURS 20 TIMES.
             15 O-BUSR-L-PASSWORD                PIC X(60).
             15 O-BUSR-L-ID                      PIC 9(05).
             15 O-BUSR-L-USERNAME                PIC X(50).
             15 O-BUSR-L-ROLE                    PIC X(04).
           10 O-BUSR-PASSWORD                    PIC X(60).
           10 O-BUSR-ID                          PIC 9(05).
           10 O-BUSR-USERNAME                    PIC X(50).
           10 O-BUSR-ROLE                        PIC X(04).
             88 O-BUSR-ADMIN                     VALUE "BaAd".
             88 O-BUSR-TELLER                    VALUE "BaTe".
             88 O-BUSR-CLIENT                    VALUE "BaCl".
                 