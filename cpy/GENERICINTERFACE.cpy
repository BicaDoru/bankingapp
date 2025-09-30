       01 GENERIC-INTERFACE.
         05 GENERIC-IN.
           10 I-GEN-TAB-NAME              PIC X(50).
           10 I-GEN-ROW-NO                PIC 9(2) VALUE 0.
           10 I-GEN-ROW-DATA.
             15 I-GEN-RECORD              OCCURS 50 TIMES.
               20 I-GEN-FIELD-CNT         PIC 9(2) VALUES 0.
               20 I-GEN-FIELDS            OCCURS 6 TIMES.
                 25 I-GEN-F-NAME          PIC X(30).
                 25 I-GEN-F-VAL           PIC X(50).
         05 GENERIC-STATUS                PIC X(03).
           88 GEN-STATUS-OK               VALUE "G00".
           88 GEN-ERR-NOT-WRITTEN-INFILE  VALUE "G92".
         05 GENERIC-OUT.
           10 O-JSON                      PIC X(10000).
             