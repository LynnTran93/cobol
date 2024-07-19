      ******************************************************************
      * Author: SHANKLEIN-MARUZANDI-MANINANG & LYNN-TRAN
      * Date: July 16, 2024
      * Purpose: PROJECT-2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STOCKS-FILE ASSIGN TO "../STOCKS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PORTFOLIO-FILE ASSIGN TO "../PORTFOLIO.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "../REPORT.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD PORTFOLIO-FILE.
           01 PORTFOLIO-RECORD.
               05 P-STOCK-SYMBOL       PIC X(7).
               05 P-SHARES             PIC 9(5).
               05 P-AVG-COST           PIC 9(4)V99.

           FD STOCKS-FILE.
           01 STOCKS-RECORD.
               05 S-STOCK-SYMBOL       PIC X(7).
               05 S-STOCK-NAME         PIC X(25).
               05 S-CLOSING-PRICE      PIC 9(4)V99.

           FD REPORT-FILE.
           01 REPORT-RECORD.
               05 REPORT-LINE          PIC X(120).

       WORKING-STORAGE SECTION.
           01 WS-STOCK-TABLE.
               05 STOCK-ENTRY OCCURS 20 TIMES INDEXED BY IDX.
                   10 STOCK-SYMBOL     PIC X(7).
                   10 STOCK-NAME       PIC X(25).
                   10 CLOSING-PRICE    PIC 9(4)V99.
           01 WS-END-OF-PORTFOLIO      PIC X VALUE 'N'.
               88 NO-MORE-PORTFOLIO-RECORD VALUE 'Y'.
           01 WS-END-OF-STOCKS         PIC X VALUE 'N'.
               88 NO-MORE-STOCKS-RECORD VALUE 'Y'.
           01 WS-PORTFOLIO-COUNT       PIC 9(5) VALUE 0.
           01 WS-REPORT-COUNT          PIC 9(5) VALUE 0.

           01 WS-STOCK-NAME            PIC X(25).
           01 WS-CLOSING-PRICE         PIC 9(4)V99.
           01 WS-ADJUSTED-COST-BASE    PIC 9(15)V99.
           01 WS-MARKET-VALUE          PIC 9(15)V99.
           01 WS-GAIN-LOSS             PIC 9(15)V99.

           01 WS-SHARES-FORMATTED              PIC ZZZZZ9.
           01 WS-AVG-COST-FORMATTED            PIC $$,$$9.99.
           01 WS-CLOSING-PRICE-FORMATTED       PIC $$,$$9.99.
           01 WS-ADJUSTED-COST-BASE-FORMATTED  PIC $$$,$$$.99.
           01 WS-MARKET-VALUE-FORMATTED        PIC $$$,$$9.99.
           01 WS-GAIN-LOSS-FORMATTED           PIC $$$,$$9.99.
           01 WS-PORTFOLIO-COUNT-FORMATTED     PIC ZZZ9.
           01 WS-REPORT-COUNT-FORMATTED        PIC ZZZ9.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           100-CREATE-STOCK-REPORT.
               PERFORM 201-INITIALIZE.
           DISPLAY "PROJECT2: LYNN TRAN & SHANKLEIN MARUZANDI MANINANG".
           DISPLAY "====================================================
      -    "============================================".
           DISPLAY "STOCK NAME                 #SHARES   UNIT COST  AT
      -    "CLOSING   COST BASE  MARKET VALUE   GAIN/LOSS".
           DISPLAY "====================================================
      -    "============================================".
               PERFORM 202-GENERATE-ONE-RECORD
                   UNTIL NO-MORE-PORTFOLIO-RECORD.
           DISPLAY "====================================================
      -    "============================================".
               PERFORM 203-TERMINATE.
           STOP RUN.

           201-INITIALIZE.
               PERFORM 301-OPEN-FILES.
               PERFORM 302-POPULATE-STOCK-TABLE.
               PERFORM 303-GENERATE-REPORT-HEADER.
               PERFORM 304-READ-PORTFOLIO-RECORD.

           301-OPEN-FILES.
               OPEN INPUT STOCKS-FILE.
               OPEN INPUT PORTFOLIO-FILE.
               OPEN OUTPUT REPORT-FILE.

           302-POPULATE-STOCK-TABLE.
             PERFORM VARYING IDX FROM 1 BY 1 UNTIL NO-MORE-STOCKS-RECORD
                   READ STOCKS-FILE INTO STOCKS-RECORD
                       AT END MOVE 'Y' TO WS-END-OF-STOCKS
                       NOT AT END
                           MOVE S-STOCK-SYMBOL TO STOCK-SYMBOL(IDX)
                           MOVE S-STOCK-NAME TO STOCK-NAME(IDX)
                           MOVE S-CLOSING-PRICE TO CLOSING-PRICE(IDX)
                   END-READ
               END-PERFORM.

           303-GENERATE-REPORT-HEADER.
           MOVE "=======================================================
      -    "=========================================" TO REPORT-RECORD.
           WRITE REPORT-RECORD.
           MOVE "STOCK NAME                 #SHARES   UNIT COST  AT CLOS
      -    "ING   COST BASE  MARKET VALUE   GAIN/LOSS" TO REPORT-RECORD.
           WRITE REPORT-RECORD.
           MOVE "=======================================================
      -    "=========================================" TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           304-READ-PORTFOLIO-RECORD.
               READ PORTFOLIO-FILE INTO PORTFOLIO-RECORD
                   AT END MOVE 'Y' TO WS-END-OF-PORTFOLIO
                   NOT AT END ADD 1 TO WS-PORTFOLIO-COUNT
               END-READ.

           202-GENERATE-ONE-RECORD.
               PERFORM 305-FIND-STOCK-INFO.
               PERFORM 306-COMPUTE-BASE.
               PERFORM 307-COMPUTE-MARKET-VALUE.
               PERFORM 308-COMPUTE-GAIN-LOSS.
               PERFORM 309-GENERATE-REPORT-RECORD.
               PERFORM 304-READ-PORTFOLIO-RECORD.

           203-TERMINATE.
               PERFORM 310-GENERATE-END-REPORT-LINE.
               PERFORM 311-SHOW-AUDIT-TRAIL.
               PERFORM 312-CLOSE-FILES.

           305-FIND-STOCK-INFO.
               SET IDX TO 1.
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 20 OR
                   STOCK-SYMBOL(IDX) = P-STOCK-SYMBOL
               END-PERFORM.
               IF STOCK-SYMBOL(IDX) = P-STOCK-SYMBOL
               MOVE STOCK-NAME(IDX) TO WS-STOCK-NAME
               MOVE CLOSING-PRICE(IDX) TO WS-CLOSING-PRICE
               INSPECT WS-CLOSING-PRICE REPLACING TRAILING SPACES BY '0'
                   DISPLAY ' '
               ELSE
                   MOVE 'UNKNOWN' TO WS-STOCK-NAME
                   MOVE 0 TO WS-CLOSING-PRICE
                   DISPLAY 'CLOSING PRICE NOT FOUND'
               END-IF.

           306-COMPUTE-BASE.
               COMPUTE WS-ADJUSTED-COST-BASE = P-SHARES * P-AVG-COST.

           307-COMPUTE-MARKET-VALUE.
               COMPUTE WS-MARKET-VALUE = P-SHARES * WS-CLOSING-PRICE.

           308-COMPUTE-GAIN-LOSS.
         COMPUTE WS-GAIN-LOSS = WS-MARKET-VALUE - WS-ADJUSTED-COST-BASE.

           309-GENERATE-REPORT-RECORD.

               MOVE WS-STOCK-NAME TO REPORT-RECORD(1:25).
               MOVE SPACES TO REPORT-RECORD(26:3).
               MOVE P-SHARES TO WS-SHARES-FORMATTED.
               MOVE WS-SHARES-FORMATTED TO REPORT-RECORD(29:8).
               MOVE SPACES TO REPORT-RECORD(37:2).
               MOVE P-AVG-COST TO WS-AVG-COST-FORMATTED.
               MOVE WS-AVG-COST-FORMATTED TO REPORT-RECORD(39:9).
               MOVE SPACES TO REPORT-RECORD(48:3).
               MOVE WS-CLOSING-PRICE TO WS-CLOSING-PRICE-FORMATTED.
               MOVE WS-CLOSING-PRICE-FORMATTED TO REPORT-RECORD(51:10).
               MOVE SPACES TO REPORT-RECORD(61:3).
               MOVE WS-ADJUSTED-COST-BASE TO
               WS-ADJUSTED-COST-BASE-FORMATTED.

               MOVE WS-ADJUSTED-COST-BASE-FORMATTED TO
               REPORT-RECORD(63:10).
               MOVE SPACES TO REPORT-RECORD(73:2).
               MOVE WS-MARKET-VALUE TO WS-MARKET-VALUE-FORMATTED.
               MOVE WS-MARKET-VALUE-FORMATTED TO REPORT-RECORD(75:10).
               MOVE SPACES TO REPORT-RECORD(85:2).
               MOVE WS-GAIN-LOSS TO WS-GAIN-LOSS-FORMATTED.
               MOVE WS-GAIN-LOSS-FORMATTED TO REPORT-RECORD(87:11).


               IF WS-MARKET-VALUE < WS-ADJUSTED-COST-BASE
                   MOVE '-' TO REPORT-RECORD(97:1)
               END-IF.

               ADD 1 TO WS-REPORT-COUNT.
               DISPLAY REPORT-RECORD
               WRITE REPORT-RECORD.

           310-GENERATE-END-REPORT-LINE.
               MOVE "===================================================
      -    "============================================="
               TO REPORT-RECORD
               WRITE REPORT-RECORD.

           311-SHOW-AUDIT-TRAIL.
               MOVE 'Records read: ' TO REPORT-RECORD.
               WRITE REPORT-RECORD.
               MOVE WS-PORTFOLIO-COUNT TO WS-PORTFOLIO-COUNT-FORMATTED
               MOVE WS-PORTFOLIO-COUNT-FORMATTED TO REPORT-RECORD
               WRITE REPORT-RECORD.
               MOVE 'Records written: ' TO REPORT-RECORD.
               WRITE REPORT-RECORD.
               MOVE WS-REPORT-COUNT TO WS-REPORT-COUNT-FORMATTED
               MOVE WS-REPORT-COUNT-FORMATTED TO REPORT-RECORD
               WRITE REPORT-RECORD.
           DISPLAY 'Records read: ' WS-PORTFOLIO-COUNT-FORMATTED.
           DISPLAY 'Records written: ' WS-REPORT-COUNT-FORMATTED.

           312-CLOSE-FILES.
               CLOSE STOCKS-FILE.
               CLOSE PORTFOLIO-FILE.
               CLOSE REPORT-FILE.

       END PROGRAM PROJECT2.
