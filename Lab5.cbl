      ******************************************************************
      * Author:Lynn Tran
      * Date:June 21, 2024
      * Purpose:Lab5
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCKS-LOOKUP.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STOCKFILE ASSIGN TO 'C:\STOCKS.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STOCKFILE.
       01 STOCKRECORD.
           05 STOCKSYMBOL         PIC X(7).
           05 STOCKNAME           PIC X(25).
           05 CLOSINGPRICE        PIC 9(4)V99.

       WORKING-STORAGE SECTION.
       01 WS-EOF                  PIC X VALUE 'N'.
       01 WS-RECORDCOUNT          PIC 9(2) VALUE 0.
       01 WS-STOCKSYMBOL          PIC X(7).
       01 WS-FOUND                PIC X VALUE 'N'.
       01 WS-INDEX                PIC 9(2) VALUE 0.
       01 STOCKSTABLE.
           05 STOCKENTRY OCCURS 20 TIMES INDEXED BY STK-INDEX.
               10 STOCKTABLESYMBOL    PIC X(7).
               10 STOCKTABLENAME      PIC X(25).
               10 STOCKTABLEPRICE     PIC 9(4)V99.
       01 STOCKSYMBOLINPUT        PIC X(7).
       01 PADDEDSTOCKSYMBOLINPUT  PIC X(7).
       01 DISPLAYNAME             PIC X(25).
       01 DISPLAYPRICE            PIC Z,ZZ9.99.

       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
           PERFORM 210-INITIALIZE
           PERFORM 220-READ-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               PERFORM 310-LOAD-TABLE
               PERFORM 220-READ-FILE
           END-PERFORM
           PERFORM 230-DISPLAY-MENU
           PERFORM UNTIL STOCKSYMBOLINPUT = 'EXIT'
               PERFORM 240-PAD-STOCK-SYMBOL
               PERFORM 320-LOOKUP-STOCK
               PERFORM 230-DISPLAY-MENU
           END-PERFORM
           STOP RUN.

       210-INITIALIZE.
           OPEN INPUT STOCKFILE
           MOVE 'N' TO WS-EOF.

       220-READ-FILE.
           READ STOCKFILE INTO STOCKRECORD
               AT END MOVE 'Y' TO WS-EOF.

       310-LOAD-TABLE.
           IF WS-RECORDCOUNT < 20
               ADD 1 TO WS-RECORDCOUNT
               SET STK-INDEX TO WS-RECORDCOUNT
               MOVE STOCKSYMBOL TO STOCKTABLESYMBOL(STK-INDEX)
               MOVE STOCKNAME TO STOCKTABLENAME(STK-INDEX)
               MOVE CLOSINGPRICE TO STOCKTABLEPRICE(STK-INDEX)
           ELSE
           DISPLAY 'Error:Table capacity reached. Records not loaded.'.

       230-DISPLAY-MENU.
           DISPLAY 'Enter stock symbol to lookup(or type EXIT to stop):'
           ACCEPT STOCKSYMBOLINPUT.

       240-PAD-STOCK-SYMBOL.
           MOVE STOCKSYMBOLINPUT TO PADDEDSTOCKSYMBOLINPUT
           INSPECT PADDEDSTOCKSYMBOLINPUT
           REPLACING TRAILING SPACES BY ' '
           PERFORM VARYING WS-INDEX
           FROM LENGTH OF STOCKSYMBOLINPUT BY 1 UNTIL WS-INDEX > 7
               MOVE ' ' TO PADDEDSTOCKSYMBOLINPUT(WS-INDEX:1)
           END-PERFORM.

       320-LOOKUP-STOCK.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING STK-INDEX FROM 1 BY 1
           UNTIL STK-INDEX > WS-RECORDCOUNT OR WS-FOUND = 'Y'
               IF PADDEDSTOCKSYMBOLINPUT = STOCKTABLESYMBOL(STK-INDEX)
                   MOVE 'Y' TO WS-FOUND
                   MOVE STOCKTABLENAME(STK-INDEX) TO DISPLAYNAME
                   MOVE STOCKTABLEPRICE(STK-INDEX) TO DISPLAYPRICE
               END-IF
           END-PERFORM
           IF WS-FOUND = 'Y'
               DISPLAY 'Stock Name: ' DISPLAYNAME
               DISPLAY 'Closing Price: ' DISPLAYPRICE
           ELSE
               DISPLAY 'Error: Stock symbol not found.'.
       END PROGRAM STOCKS-LOOKUP.
