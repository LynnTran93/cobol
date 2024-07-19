      ******************************************************************
      * Author:Lynn Tran
      * Date:July 19, 2024
      * Purpose:Lab 6
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAB-6.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARLIAMENT-FILE ASSIGN TO "C:\parliament.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD PARLIAMENT-FILE.
       01 PARLIAMENT-RECORD.
           05 PROVINCE-NAME        PIC X(25).
           05 LIBERAL-SEATS        PIC 99.
           05 CONSERVATIVE-SEATS   PIC 99.
           05 BQ-SEATS             PIC 99.
           05 NDP-SEATS            PIC 99.
           05 GREEN-SEATS          PIC 99.
           05 INDEPENDENT-SEATS    PIC 99.
           05 VACANT-SEATS         PIC 99.

       WORKING-STORAGE SECTION.
       01 WS-EOF                   PIC X(3) VALUE "No".
       01 WS-INDEX                 PIC 99 VALUE 0.
       01 WS-PARTY-ID              PIC 99.
       01 WS-INPUT-PARTY           PIC X(20).
       01 WS-MAX-SEATS             PIC 99.
       01 WS-MAX-PARTY             PIC 99.

       01 PROVINCE-TABLE.
         05 PROVINCE-NAME-TABLE OCCURS 14 TIMES INDEXED BY IDX-PROVINCE.
               10 PROVINCE-VALUE   PIC X(25).
       01 PARTY-SEATS-TABLE.
           05 PARTY-SEATS OCCURS 14 TIMES INDEXED BY IDX-PROVINCE2.
               10 SEAT-COUNTS OCCURS 7 TIMES PIC 99.

       01 PARTY-NAME-TABLE.
           05 PARTY-NAME OCCURS 7 TIMES INDEXED BY IDX-PARTY.
               10 NAME             PIC X(20) VALUE SPACES.


       PROCEDURE DIVISION.
       100-PRODUCE-PARTY-REPORT.
           PERFORM 210-INITIALIZE
           PERFORM 220-READ-PARLIAMENT-RECORD
           PERFORM UNTIL WS-EOF = "Yes"
               PERFORM 310-LOAD-TABLE
               PERFORM 220-READ-PARLIAMENT-RECORD
           END-PERFORM
           PERFORM 330-SETUP-PARTY-NAMES
           PERFORM 230-DISPLAY-MENU
           PERFORM UNTIL WS-INPUT-PARTY = "EXIT"
               PERFORM 240-GET-PARTY-ID
               PERFORM 320-DISPLAY-PROVINCES
               PERFORM 230-DISPLAY-MENU
           END-PERFORM
           PERFORM 230-CLOSE-RTN
           STOP RUN.

       210-INITIALIZE.
           OPEN INPUT PARLIAMENT-FILE.

       220-READ-PARLIAMENT-RECORD.
           READ PARLIAMENT-FILE INTO PARLIAMENT-RECORD
               AT END MOVE "Yes" TO WS-EOF
           END-READ.

       310-LOAD-TABLE.
           ADD 1 TO WS-INDEX
           SET IDX-PROVINCE TO WS-INDEX
           MOVE PROVINCE-NAME TO PROVINCE-VALUE(IDX-PROVINCE)
           MOVE LIBERAL-SEATS TO SEAT-COUNTS(IDX-PROVINCE, 1)
           MOVE CONSERVATIVE-SEATS TO SEAT-COUNTS(IDX-PROVINCE, 2)
           MOVE BQ-SEATS TO SEAT-COUNTS(IDX-PROVINCE, 3)
           MOVE NDP-SEATS TO SEAT-COUNTS(IDX-PROVINCE, 4)
           MOVE GREEN-SEATS TO SEAT-COUNTS(IDX-PROVINCE, 5)
           MOVE INDEPENDENT-SEATS TO SEAT-COUNTS(IDX-PROVINCE, 6)
           MOVE VACANT-SEATS TO SEAT-COUNTS(IDX-PROVINCE, 7).

       330-SETUP-PARTY-NAMES.
           MOVE "LIBERAL" TO NAME(1)
           MOVE "CONSERVATIVE" TO NAME(2)
           MOVE "BQ" TO NAME(3)
           MOVE "NDP" TO NAME(4)
           MOVE "GREEN PARTY" TO NAME(5)
           MOVE "INDEPENDENT" TO NAME(6)
           MOVE "VACANT" TO NAME(7).

       230-DISPLAY-MENU.
           DISPLAY "Enter Party name (Liberal, Conservative, BQ, NDP,
      -    "Green Party, Independent, Vacant) or type EXIT to stop:"
           ACCEPT WS-INPUT-PARTY
           MOVE FUNCTION UPPER-CASE(WS-INPUT-PARTY) TO WS-INPUT-PARTY.

       240-GET-PARTY-ID.
           MOVE 0 TO WS-PARTY-ID
           PERFORM VARYING IDX-PARTY FROM 1 BY 1 UNTIL IDX-PARTY > 7 OR
           WS-PARTY-ID > 0
               IF WS-INPUT-PARTY = NAME(IDX-PARTY)
                   MOVE IDX-PARTY TO WS-PARTY-ID
               END-IF
           END-PERFORM
           IF WS-PARTY-ID = 0
               DISPLAY "Invalid party name."
               MOVE "EXIT" TO WS-INPUT-PARTY.

       320-DISPLAY-PROVINCES.
           IF WS-PARTY-ID > 0
               PERFORM VARYING IDX-PROVINCE FROM 1 BY 1 UNTIL
               IDX-PROVINCE > WS-INDEX
                   MOVE 0 TO WS-MAX-SEATS
                   MOVE 0 TO WS-MAX-PARTY
               PERFORM VARYING IDX-PARTY FROM 1 BY 1 UNTIL IDX-PARTY > 7
                  IF SEAT-COUNTS(IDX-PROVINCE, IDX-PARTY) > WS-MAX-SEATS
               MOVE SEAT-COUNTS(IDX-PROVINCE, IDX-PARTY) TO WS-MAX-SEATS
               MOVE IDX-PARTY TO WS-MAX-PARTY
                  END-IF
               END-PERFORM
                   IF WS-MAX-PARTY = WS-PARTY-ID
                       DISPLAY PROVINCE-VALUE(IDX-PROVINCE)
                   END-IF
               END-PERFORM
           END-IF.

       230-CLOSE-RTN.
           CLOSE PARLIAMENT-FILE.

       END PROGRAM LAB-6.
