       01 LOGGER-INTERFACE.
         05 LOGGER-IN.
           10 FG-LOG-MODE                  PIC X.
             88 FG-LOG-MODE-SINGLE-LINE    VALUE '1'.
             88 FG-LOG-MODE-MULTI-LINE     VALUE '2'.
           10 I-LOG-LONG-MESSAGE           PIC X(10000).
           10 I-LOG-SPLIT-MESSAGE          REDEFINES I-LOG-LONG-MESSAGE.
             15 I-LOG-LINE-MESSAGE         PIC X(500) OCCURS 20 TIMES.
           10 I-IDX-LOG-SPLIT              PIC 9(2).
         05 LOGGER-UTILS.  
           10 U-LOG-RUNNABLE-PROG          PIC X(20).
           10 U-LOG-LINE                   PIC X(10000).
           10 U-LOG-BUFFER-LINE            PIC X(10000).
           10 U-LOG-PGNAME                 PIC X(20).
             88 U-LOG-PGNAME-LOGGER        VALUE "LOGGER              ".
