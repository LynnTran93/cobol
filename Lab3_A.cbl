      ******************************************************************
      * Author:Lynn Tran
      * Student Number: 041102082
      * Date: May 23, 2024
      * Purpose: Lab3
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Lab3A.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  SENDING-FIELDS.
           05  S-FLD-1  PIC 9(3) VALUE 123.
           05  S-FLD-2  PIC X(6) VALUE '1234bb'.
           05  S-FLD-3  PIC 9(3)V99 VALUE 123.45.
           05  S-FLD-4  PIC X(5) VALUE '12AB5'.
           05  S-FLD-5  PIC X(4) VALUE 'JOHN'.
           05  S-FLD-6  PIC X(6) VALUE '123456'.
           05  S-FLD-7  PIC A(6) VALUE 'bbABCD'.
           05  S-FLD-8  PIC 9(5) VALUE 12345.
           05  S-FLD-9  PIC 9(5) VALUE 12345.

       01  RECEIVING-FIELDS.
           05  R-FLD-1  PIC 9(3)V9.
           05  R-FLD-2  PIC X(5).
           05  R-FLD-3  PIC 9(3)V9.
           05  R-FLD-4  PIC 9(5).
           05  R-FLD-5  PIC X(6).
           05  R-FLD-6  PIC X(4).
           05  R-FLD-7  PIC X(4).
           05  R-FLD-8  PIC X(7).
           05  R-FLD-9  PIC X(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM QUESTION-1
           PERFORM QUESTION-2
           PERFORM QUESTION-3
           PERFORM QUESTION-4
           PERFORM QUESTION-5
           PERFORM QUESTION-6
           PERFORM QUESTION-7
           PERFORM QUESTION-8
           PERFORM QUESTION-9
           STOP RUN.

       QUESTION-1.
           MOVE S-FLD-1 TO R-FLD-1
           DISPLAY R-FLD-1.

       QUESTION-2.
           MOVE S-FLD-2 TO R-FLD-2
           DISPLAY R-FLD-2.

       QUESTION-3.
           MOVE S-FLD-3 TO R-FLD-3
           DISPLAY R-FLD-3.

       QUESTION-4.
           MOVE S-FLD-4 TO R-FLD-4
           DISPLAY R-FLD-4.

       QUESTION-5.
           MOVE S-FLD-5 TO R-FLD-5
           DISPLAY R-FLD-5.

       QUESTION-6.
           MOVE S-FLD-6 TO R-FLD-6
           DISPLAY R-FLD-6.

       QUESTION-7.
           MOVE S-FLD-7 TO R-FLD-7
           DISPLAY R-FLD-7.

       QUESTION-8.
           MOVE S-FLD-8 TO R-FLD-8
           DISPLAY R-FLD-8.

       QUESTION-9.
           MOVE S-FLD-9 TO R-FLD-9
           DISPLAY R-FLD-9.
