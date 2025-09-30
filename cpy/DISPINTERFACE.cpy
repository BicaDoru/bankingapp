       01 DISPATCHER-INTERFACE.
         05 DISPATCHER-IN.
           10 I-DISP-BANKUSERID                     PIC 9(05).
           10 FILLER                                PIC X.
           10 I-DISP-METHOD                         PIC X(08).
             88 I-DISP-METHOD-OK                    VALUE "GET     "
                                                          "PUT     "
                                                          "DELETE  "
                                                          "POST    "
                                                          "LOGIN   ".
             88 I-DISP-METHOD-GET                   VALUE "GET     ".
             88 I-DISP-METHOD-PUT                   VALUE "PUT     ".
             88 I-DISP-METHOD-DELETE                VALUE "DELETE  ".
             88 I-DISP-METHOD-POST                  VALUE "POST    ".
             88 I-DISP-METHOD-LOGIN                 VALUE "LOGIN   ".
           10 I-DISP-OBJECT                         PIC X(10).
             88 I-DISP-OBJ-OK                       VALUE "CUSTOMERS "
                                                          "CUSTOMER  "
                                                          "ACCOUNTS  "
                                                          "LOGIN     "
                                                          "USERS     "
                                                          "ACCOUNT   "
                                                          "TRANS     "
                                                          "TRANSFER  "
                                                          "USER      ".
             88 I-DISP-OBJ-CUST-ITEM                VALUE "CUSTOMER  ".
             88 I-DISP-OBJ-CUST-LIST                VALUE "CUSTOMERS ".
             88 I-DISP-OBJ-LOGIN                    VALUE "LOGIN     ".
             88 I-DISP-OBJ-BUSR-ITEM                VALUE "USER      ".
             88 I-DISP-OBJ-ACC-LIST                 VALUE "ACCOUNTS  ".
             88 I-DISP-OBJ-ACC-ITEM                 VALUE "ACCOUNT   ".
             88 I-DISP-OBJ-TRANS                    VALUE "TRANS     ".
             88 I-DISP-OBJ-TRANSFER                 VALUE "TRANSFER  ".
             88 I-DISP-OBJ-BUSR-LIST                VALUE "USERS     ".
      *    --- INPUT INTERFACES    
           10 I-DISP-DATA                           PIC X(194).
   
           10 I-DISP-GET-CUST-LIST        REDEFINES I-DISP-DATA.
             15 I-GET-CUST-LIST-PAGE-NUMBER         PIC 9(05).
             15 FILLER                              PIC X(189).
      *    
           10 I-DISP-GET-CUST-ITEM        REDEFINES I-DISP-DATA.
             15 I-GET-CUST-ITEM-ID                  PIC 9(05).
             15 FILLER                              PIC X.
             15 I-GET-CUST-ITEM-PAGE-NUMBER         PIC 9(05).
             15 FILLER                              PIC X(183).

           10 I-DISP-POST-CUST-ITEM       REDEFINES I-DISP-DATA.
             15 I-POST-CUST-ITEM-BANKUSERID         PIC 9(05).
             15 I-POST-CUST-ITEM-USNAME             PIC X(50).
             15 I-POST-CUST-ITEM-ADDRESS            PIC X(50).
             15 FILLER                              PIC X(89).

           10 I-DISP-DEL-CUST-ITEM        REDEFINES I-DISP-DATA.
             15 I-DEL-CUST-ITEM-CUSTID              PIC 9(05).
             15 FILLER                              PIC X(189).

           10 I-DISP-PUT-CUST-ITEM        REDEFINES I-DISP-DATA.
             15 I-PUT-CUST-ITEM-CUSTID              PIC 9(05).
             15 I-PUT-CUST-ITEM-USNAME              PIC X(50).
             15 I-PUT-CUST-ITEM-ADDRESS             PIC X(50).
             15 FILLER                              PIC X(89).
      *    
           10 I-DISP-GET-ACC-LIST         REDEFINES I-DISP-DATA.
             15 I-GET-ACC-CUSTID                    PIC 9(05).
             15 FILLER                              PIC X.
             15 I-GET-ACC-PAGE-NUMBER               PIC 9(05).
             15 FILLER                              PIC X(183).
      *
           10 I-DISP-POST-ACC-ITEM        REDEFINES I-DISP-DATA.
             15 I-POST-ACC-CUSTID                   PIC 9(05).
             15 FILLER                              PIC X.
             15 I-POST-ACC-IBAN                     PIC X(30).
             15 FILLER                              PIC X.
             15 I-POST-ACC-CURRENCY                 PIC X(3).
             15 FILLER                              PIC X(154).
      *
           10 I-DISP-PUT-ACC-ITEM         REDEFINES I-DISP-DATA.
             15 I-PUT-ACC-ID                        PIC 9(05).
             15 FILLER                              PIC X.
             15 I-PUT-ACC-BALANCE                   PIC 9(08)V99.
             15 FILLER                              PIC X(178).
      *
           10 I-DISP-DEL-ACC-ITEM         REDEFINES I-DISP-DATA.
             15 I-DEL-ACC-ID                        PIC 9(05).
             15 FILLER                              PIC X(189).
      *
           10 I-DISP-GET-TRANS            REDEFINES I-DISP-DATA.
             15 I-GET-TRANS-CUSTID                  PIC 9(05).
             15 I-GET-TRANS-ACCID                   PIC 9(05).
             15 FILLER                              PIC X.
             15 I-GET-TRANS-PAGE-NUMBER             PIC 9(05).
             15 I-GET-TRANS-DATE                    PIC X(10).
             15 FILLER                              PIC X(167).
      *    
           10 I-DISP-POST-TRANS           REDEFINES I-DISP-DATA.
             15 I-POST-TRANS-CUSTID                 PIC 9(05).
             15 I-POST-TRANS-ACCID                  PIC 9(05).
             15 I-POST-TRANS-TRTYPE                 PIC X(10).
             15 I-POST-TRANS-AMOUNT                 PIC 9(08)V99.
             15 I-POST-TRANS-BANKUSERID             PIC 9(05).
             15 FILLER                              PIC X(159).
      *
           10 I-DISP-POST-TRANSFER        REDEFINES I-DISP-DATA.
             15 I-POST-TRANSFER-SRCIBAN             PIC X(30).
             15 FILLER                              PIC X.
             15 I-POST-TRANSFER-DESTIBAN            PIC X(30).
             15 FILLER                              PIC X.
             15 I-POST-TRANSFER-AMOUNT              PIC 9(08)V99.
             15 FILLER                              PIC X(122).
      * 
           10 I-DISP-GET-TRANSFER         REDEFINES I-DISP-DATA.
             15 I-GET-TRANSFER-CUSTID               PIC 9(05).
             15 FILLER                              PIC X.
             15 I-GET-TRANSFER-ACCID                PIC 9(05).
             15 FILLER                              PIC X.
             15 I-GET-TRANSFER-PAGE-NUMBER          PIC 9(05).
             15 FILLER                              PIC X(177).
      *       
           10 I-DISP-GET-BUSR-LIST        REDEFINES I-DISP-DATA.
             15 I-GET-BUSR-PAGE-NUMBER              PIC 9(05).
             15 FILLER                              PIC X(189).         
      *
           10 I-DISP-POST-BUSR            REDEFINES I-DISP-DATA.
             15 I-POST-BUSR-USERNAME                PIC X(50).
             15 FILLER                              PIC X.
             15 I-POST-BUSR-PASSWORD                PIC X(60).
             15 FILLER                              PIC X.
             15 I-POST-BUSR-ROLE                    PIC X(4).
             15 FILLER                              PIC X(78).
      *  
           10 I-DISP-DEL-BUSR             REDEFINES I-DISP-DATA.
             15 I-DEL-BUSR-ID                       PIC 9(05).
             15 FILLER                              PIC X(189).
      *
           10 I-DISP-PUT-BUSR             REDEFINES I-DISP-DATA.
             15 I-PUT-BUSR-ID                       PIC 9(05).
             15 FILLER                              PIC X.
             15 I-PUT-BUSR-USERNAME                 PIC X(50).
             15 FILLER                              PIC X.
             15 I-PUT-BUSR-ROLE                     PIC X(4).
             15 FILLER                              PIC X(133).
      *
           10 I-DISP-LOGIN-DATA           REDEFINES I-DISP-DATA.
             15 I-POST-LOGIN-USERNAME               PIC X(50).
             15 I-POST-LOGIN-PASSWORD               PIC X(60).
             15 FILLER                              PIC X(84).
      *    --- OUTPUT INTERFACES
         05 DISPATCHER-OUT.
           10 O-DISP-ERROR.
             15 O-DISP-ERROR-NO                     PIC X(04).
               88 O-DISP-ERR-OK                     VALUE "0000".

               88 O-DISP-ERR-DB-ITEM-NOT-FOUND      VALUE "DB90".
               88 O-DISP-ERR-DB-UNIQUE              VALUE "DB93".
               88 O-DISP-ERR-DB-SQL                 VALUE "DB99".

               88 O-DISP-ERR-DISP-BAD-OBJECT        VALUE "DP01".
               88 O-DISP-ERR-DISP-BAD-ROUTE         VALUE "DP02".
               88 O-DISP-ERR-DISP-INVALID-LOGIN     VALUE "DP04".
      *        
               88 O-DISP-ERR-AUTH-ROLE-FORBIDDEN    VALUE "AU01".
               88 O-DISP-ERR-AUTH-TLR-TO-ADMIN      VALUE "AU02".
               88 O-DISP-ERR-AUTH-TLR-TO-TLR        VALUE "AU03".
               88 O-DISP-ERR-AUTH-TLR-TO-NONE       VALUE "AU04".
               88 O-DISP-ERR-AUTH-CLT-TO-OTHER      VALUE "AU05".

               88 O-DISP-ERR-CUST-BAD-METHOD        VALUE "CU01".
               88 O-DISP-ERR-ACC-BAD-TRTYPE         VALUE "AC02".
               88 O-DISP-ERR-ACC-NO-FUNDS           VALUE "AC03".
               88 O-DISP-ERR-ACC-WRONG-PAGE-FORMAT  VALUE "AC08".

               88 O-DISP-ERR-BUSR-BAD-USERNAME      VALUE "US02".
               88 O-DISP-ERR-BUSR-BAD-PASSWORD      VALUE "US03".
               88 O-DISP-ERR-BUSR-BAD-ROLE          VALUE "US04".
               88 O-DISP-ERR-BUSR-BAD-COMBO         VALUE "US05".
               88 O-DISP-ERR-BUSR-UNKNOWN-ROLE      VALUE "US06".
               88 O-DISP-ERR-BUSR-CHANGE-ROLE       VALUE "US07".

               88 O-DISP-ERR-TRANS-BAD-DATE-FORMAT  VALUE "TR01".
               88 O-DISP-ERR-TRANS-DATA-NOT-NUMERIC VALUE "TR03".
               88 O-DISP-ERR-TRANS-INVALID-MONTH    VALUE "TR04".
               88 O-DISP-ERR-TRANS-INVALID-DAY      VALUE "TR05".

               88 O-DISP-ERR-TRANSFER-BAD-METHOD    VALUE "TF01".
               88 O-DISP-ERR-TRANSFER-CURR-MISMATCH VALUE "TF02".
               88 O-DISP-ERR-TRANSFER-LOW-BALANCE   VALUE "TF03".

             15 O-DISP-ERROR-MESSAGE                PIC X(100).
           10 O-DISP-DATA-OUT                       PIC X(2500).

           10 O-DISP-GET-ACC-LIST         REDEFINES O-DISP-DATA-OUT.
             15 O-GET-ACC-LIST-COUNT                PIC 9(02).
             15 O-GET-ACC-LIST-ELEM                 OCCURS 20.
               20 O-GET-ACC-LIST-ID                 PIC 9(05).
               20 O-GET-ACC-LIST-CUSTOMERID         PIC 9(05).
               20 O-GET-ACC-LIST-IBAN               PIC X(30).
               20 O-GET-ACC-LIST-CURRENCY           PIC X(05).
               20 O-GET-ACC-LIST-BALANCE            PIC 9(08)V99.
             15 FILLER                              PIC X(1398).

           10 O-DISP-GET-ACC              REDEFINES O-DISP-DATA-OUT.
             15 O-GET-ACC-COUNT                     PIC 9(02).
             15 O-GET-ACC-ELEM                      OCCURS 10.
               20 O-GET-ACC-ACCOUNTID               PIC 9(05).
               20 O-GET-ACC-CUSTOMERID              PIC 9(05).
               20 O-GET-ACC-IBAN                    PIC X(30).
               20 O-GET-ACC-CURRENCY                PIC X(05).
               20 O-GET-ACC-BALANCE                 PIC 9(08)V99.
             15 FILLER                              PIC X(1948).
      *    
           10 O-DISP-GET-CUST-ITEM        REDEFINES O-DISP-DATA-OUT.
             15 O-GET-CUST-ITEM-ELEM.
               20 O-GET-CUST-ITEM-ID                PIC 9(05).
               20 O-GET-CUST-ITEM-USERNAME          PIC X(50).
               20 O-GET-CUST-ITEM-ADDRESS           PIC X(50).
               20 O-GET-CUST-ITEM-BANKUSERID        PIC 9(05).
             15 O-GET-CUST-ACC-DATA.
               20 O-GET-CUST-ACC-COUNT              PIC 9(02).  
               20 O-GET-CUST-ITEM-ACC-ELEM          OCCURS 10.
                 25 O-GET-CUST-ACC-ACCOUNTID        PIC 9(05).
                 25 O-GET-CUST-ACC-CUSTOMERID       PIC 9(05).
                 25 O-GET-CUST-ACC-IBAN             PIC X(30).
                 25 O-GET-CUST-ACC-CURRENCY         PIC X(05).
                 25 O-GET-CUST-ACC-BALANCE          PIC 9(08)V99.
             15 FILLER                              PIC X(1838).
      * 
           10 O-DISP-GET-CUST-LIST        REDEFINES O-DISP-DATA-OUT.
             15 O-GET-CUST-LIST-COUNT               PIC 9(02).
             15 O-GET-CUST-LIST-ELEM                OCCURS 10.
               20 O-GET-CUST-LIST-ID                PIC 9(05).
               20 O-GET-CUST-LIST-USERNAME          PIC X(50).
               20 O-GET-CUST-LIST-ADDRESS           PIC X(50).
               20 O-GET-CUST-LIST-BANKUSERID        PIC 9(05).
             15 FILLER                              PIC X(1398).
      *    
           10 O-DISP-POST-CUST-ITEM       REDEFINES O-DISP-DATA-OUT.
             15 O-POST-CUST-ITEM-ID                 PIC 9(05).
             15 O-POST-CUST-ITEM-USERNAME           PIC X(50).
             15 O-POST-CUST-ITEM-ADDRESS            PIC X(50).
             15 O-POST-CUST-ITEM-BANKUSERID         PIC 9(05).
             15 FILLER                              PIC X(2390).

           10 O-DISP-DEL-CUST-ITEM        REDEFINES O-DISP-DATA-OUT.
             15 O-DEL-CUST-ITEM-ID                  PIC 9(05).
             15 O-DEL-CUST-ITEM-USERNAME            PIC X(50).
             15 O-DEL-CUST-ITEM-ADDRESS             PIC X(50).
             15 O-DEL-CUST-ITEM-BANKUSERID          PIC 9(05).
             15 FILLER                              PIC X(2390).

           10 O-DISP-PUT-CUST-ITEM        REDEFINES O-DISP-DATA-OUT.
             15 O-PUT-CUST-ITEM-ID                  PIC 9(05).
             15 O-PUT-CUST-ITEM-USERNAME            PIC X(50).
             15 O-PUT-CUST-ITEM-ADDRESS             PIC X(50).
             15 O-PUT-CUST-ITEM-BANKUSERID          PIC 9(05).
             15 FILLER                              PIC X(2390).

           10 O-DISP-GET-TRANS            REDEFINES O-DISP-DATA-OUT.
             15 O-GET-TRANS-COUNT                   PIC 9(02).
             15 O-GET-TRANS-ELEM                    OCCURS 10.
               20 O-GET-TRANS-ACCOUNTID             PIC 9(05).
               20 O-GET-TRANS-TRANS-TYPE            PIC X(10).
               20 O-GET-TRANS-AMOUNT                PIC 9(08)V99.
               20 O-GET-TRANS-TIMESTAMP             PIC X(22).
               20 O-GET-TRANS-BALANCE               PIC 9(08)V99.
             15 FILLER                              PIC X(1928).
      *    
           10 O-DISP-POST-TRANS           REDEFINES O-DISP-DATA-OUT.
             15 O-POST-TRANS-ACCOUNTID              PIC 9(05).
             15 O-POST-TRANS-TRANS-TYPE             PIC X(10).
             15 O-POST-TRANS-AMOUNT                 PIC 9(08)V99.
             15 O-POST-TRANS-TIMESTAMP              PIC X(22).
             15 O-POST-TRANS-BALANCE                PIC 9(08)V99.
             15 FILLER                              PIC X(2443).
      *
           10 O-DISP-GET-TRANSFER         REDEFINES O-DISP-DATA-OUT.
             15 O-GET-TRANSFER-COUNT                PIC 9(02).
             15 O-GET-TRANSFER-ELEM                 OCCURS 10.
               20 O-GET-TRANSFER-ID                 PIC 9(05).
               20 O-GET-TRANSFER-SRCACCID           PIC 9(05).
               20 O-GET-TRANSFER-SRCIBAN            PIC X(30).
               20 O-GET-TRANSFER-DESTACCID          PIC 9(05).
               20 O-GET-TRANSFER-DESTIBAN           PIC X(30).
               20 O-GET-TRANSFER-AMOUNT             PIC 9(08)V99.
               20 O-GET-TRANSFER-TIMESTAMP          PIC X(22).
               20 O-GET-TRANSFER-CURRENCY           PIC X(03).
             15 FILLER                              PIC X(1398).
      *
           10 O-DISP-POST-TRANSFER        REDEFINES O-DISP-DATA-OUT.
             15 O-POST-TRANSFER-ID                  PIC 9(05).
             15 O-POST-TRANSFER-SRCIBAN             PIC X(30).
             15 O-POST-TRANSFER-DESTIBAN            PIC X(30).
             15 O-POST-TRANSFER-AMOUNT              PIC 9(08)V99.
             15 O-POST-TRANSFER-TIMESTAMP           PIC X(22).
             15 O-POST-TRANSFER-CURRENCY            PIC X(3).
             15 FILLER                              PIC X(2400).
      *
           10 O-DISP-POST-BUSR            REDEFINES O-DISP-DATA-OUT.
             15 O-POST-BUSR-ID                      PIC 9(05).
             15 O-POST-BUSR-USERNAME                PIC X(50).
             15 O-POST-BUSR-ROLE                    PIC X(4).
             15 FILLER                              PIC X(2441).
      * 
           10 O-DISP-DEL-BUSR             REDEFINES O-DISP-DATA-OUT.
             15 O-DEL-BUSR-ID                       PIC 9(05).
             15 FILLER                              PIC X(2495).
      *
           10 O-DISP-PUT-BUSR             REDEFINES O-DISP-DATA-OUT.
             15 O-PUT-BUSR-ID                       PIC 9(05).
             15 O-PUT-BUSR-USERNAME                 PIC X(50).
             15 O-PUT-BUSR-ROLE                     PIC X(4). 
             15 FILLER                              PIC X(2441).          
      *
           10 O-DISP-GET-BANKUSERS        REDEFINES O-DISP-DATA-OUT.
             15 O-GET-BUSR-COUNT                    PIC 9(02).
             15 O-GET-BUSR-ELEM                     OCCURS 10 TIMES.
               20 O-GET-BUSR-ID                     PIC 9(04).
               20 O-GET-BUSR-USERNAME               PIC X(50).
               20 O-GET-BUSR-ROLE                   PIC X(04).
             15 FILLER                              PIC X(1318).
      ******************************************************************
      *                    post login
      ****************************************************************** 
           10 O-DISP-POST-LOGIN          REDEFINES O-DISP-DATA-OUT.    
             15 O-POST-LOGIN-ID                     PIC 9(04).
             15 O-POST-LOGIN-USERNAME               PIC X(50).
             15 O-POST-LOGIN-ROLE                   PIC X(04).
               88 O-POST-LOGIN-ADMIN                VALUE "BaAd".
               88 O-POST-LOGIN-TELLER               VALUE "BaTe".
               88 O-POST-LOGIN-CLIENT               VALUE "BaCl".
             15 FILLER                              PIC X(2442). 
      ****************************************************************** 
      *                  authorize login
      ******************************************************************
         05 DISPATCHER-UTILS.
           10 U-DISP-BUSR-DATA.              
             15 U-DISP-LOGIN-ID                     PIC 9(04).
             15 U-DISP-LOGIN-USERNAME               PIC X(50).
             15 U-DISP-LOGIN-ROLE                   PIC X(04).
               88 U-DISP-LOGIN-ADMIN                VALUE "BaAd".
               88 U-DISP-LOGIN-TELLER               VALUE "BaTe".
               88 U-DISP-LOGIN-CLIENT               VALUE "BaCl".
