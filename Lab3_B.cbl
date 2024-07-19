      ******************************************************************
      * Author:Lynn Tran
      * Student Number: 041102082
      * Date: May 30, 2024
      * Purpose: Lab3
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Lab3B.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 FLDS.
           05 FLD-1    PIC 9(3) VALUE 145.
           05 FLD-2    PIC 9(3) VALUE 222.
           05 FLD-3    PIC 9(2) VALUE 10.
           05 FLD-4    PIC 9(3) VALUE 21.
           05 FLD-5    PIC 9(2) VALUE 33.
           05 FLD-6    PIC 9(4) VALUE 1111.

       01 INITIAL-VALUES.
           05 INIT-FLD-1    PIC 9(3) VALUE 145.
           05 INIT-FLD-2    PIC 9(3) VALUE 222.
           05 INIT-FLD-3    PIC 9(2) VALUE 10.
           05 INIT-FLD-4    PIC 9(3) VALUE 21.
           05 INIT-FLD-5    PIC 9(2) VALUE 33.
           05 INIT-FLD-6    PIC 9(4) VALUE 1111.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM RESET-VALUES
           PERFORM QUESTION-16
           PERFORM RESET-VALUES
           PERFORM QUESTION-17
           PERFORM RESET-VALUES
           PERFORM QUESTION-18
           PERFORM RESET-VALUES
           PERFORM QUESTION-19
           PERFORM RESET-VALUES
           PERFORM QUESTION-20
           PERFORM RESET-VALUES
           PERFORM QUESTION-21
           STOP RUN.

       RESET-VALUES.
           MOVE INIT-FLD-1 TO FLD-1
           MOVE INIT-FLD-2 TO FLD-2
           MOVE INIT-FLD-3 TO FLD-3
           MOVE INIT-FLD-4 TO FLD-4
           MOVE INIT-FLD-5 TO FLD-5
           MOVE INIT-FLD-6 TO FLD-6.

       QUESTION-16.
           MULTIPLY FLD-1 BY FLD-3 GIVING FLD-2
           DISPLAY 'MULTIPLY FLD-1 BY FLD-3 GIVING FLD-2:'
           DISPLAY 'FLD-1: ' FLD-1
           DISPLAY 'FLD-2: ' FLD-2
           DISPLAY 'FLD-3: ' FLD-3.

       QUESTION-17.
           DIVIDE FLD-5 BY FLD-3 GIVING FLD-6 ROUNDED
           DISPLAY 'DIVIDE FLD-5 BY FLD-3 GIVING FLD-6 ROUNDED:'
           DISPLAY 'FLD-3: ' FLD-3
           DISPLAY 'FLD-5: ' FLD-5
           DISPLAY 'FLD-6: ' FLD-6.

       QUESTION-18.
           ADD FLD-6 FLD-4 TO FLD-3
           DISPLAY 'ADD FLD-6 FLD-4 TO FLD-3:'
           DISPLAY 'FLD-3: ' FLD-3
           DISPLAY 'FLD-4: ' FLD-4
           DISPLAY 'FLD-6: ' FLD-6.

       QUESTION-19.
           SUBTRACT FLD-4 FLD-2 FLD-1 FROM FLD-5
           DISPLAY 'SUBTRACT FLD-4 FLD-2 FLD-1 FROM FLD-5:'
           DISPLAY 'FLD-1: ' FLD-1
           DISPLAY 'FLD-2: ' FLD-2
           DISPLAY 'FLD-4: ' FLD-4
           DISPLAY 'FLD-5: ' FLD-5.

       QUESTION-20.
           COMPUTE FLD-5 ROUNDED = (FLD-2 + FLD-4) / 3
           DISPLAY 'COMPUTE FLD-5 ROUNDED = (FLD-2 + FLD-4) / 3:'
           DISPLAY 'FLD-2: ' FLD-2
           DISPLAY 'FLD-4: ' FLD-4
           DISPLAY 'FLD-5: ' FLD-5.

       QUESTION-21.
           SUBTRACT FLD-3 FLD-4 FLD-5 FROM FLD-2
           DISPLAY 'SUBTRACT FLD-3 FLD-4 FLD-5 FROM FLD-2:'
           DISPLAY 'FLD-2: ' FLD-2
           DISPLAY 'FLD-3: ' FLD-3
           DISPLAY 'FLD-4: ' FLD-4
           DISPLAY 'FLD-5: ' FLD-5.
