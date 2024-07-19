      ******************************************************************
      * Author:Lynn Tran
      * Date:June 5, 2024
      * Purpose: Lab4
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TicketPricingWithout88.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-TOTAL-AMOUNT        PIC 9(5)V99 VALUE 0.
           01 WS-ENTRY-CATEGORY      PIC X(10).
           01 WS-TICKET-PRICE        PIC 9(3)V99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       PERFORM UNTIL WS-ENTRY-CATEGORY = 'DONE'
           DISPLAY "Enter the category (FAMILY, ADULT, STUDENT, YOUTH,
      -    "CHILD, MILITARY) or END to finish:"
           ACCEPT WS-ENTRY-CATEGORY
           IF WS-ENTRY-CATEGORY = 'FAMILY'
               ADD 80.00 TO WS-TICKET-PRICE
           ELSE IF WS-ENTRY-CATEGORY = 'ADULT'
               ADD 25.00 TO WS-TICKET-PRICE
           ELSE IF WS-ENTRY-CATEGORY = 'STUDENT'
               ADD 19.00 TO WS-TICKET-PRICE
           ELSE IF WS-ENTRY-CATEGORY = 'YOUTH'
               ADD 16.00 TO WS-TICKET-PRICE
           ELSE IF WS-ENTRY-CATEGORY = 'CHILD'
               ADD 0 TO WS-TICKET-PRICE
           ELSE IF WS-ENTRY-CATEGORY = 'MILITARY'
               ADD 12.50 TO WS-TICKET-PRICE
           ELSE
               DISPLAY "Invalid category. Please enter again."
               CONTINUE
        END-IF
        IF WS-ENTRY-CATEGORY = 'DONE'
            ADD WS-TICKET-PRICE TO WS-TOTAL-AMOUNT
        END-IF
       END-PERFORM
       DISPLAY "Total amount collected: " WS-TOTAL-AMOUNT
       STOP RUN.
