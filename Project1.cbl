      ******************************************************************
      * Author: Lynn Tran
      * Date: June 13, 2024
      * Purpose: Project 1
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-RECORDS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "C:\employees.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMPLOYEEID-IN        PIC 9(6).
           05 DEPTCODE-IN          PIC 9(3).
           05 LASTNAME-IN          PIC A(20).
           05 FIRSTNAME-IN         PIC A(20).
           05 YEARSOFSERVICE-IN    PIC 9(2).

       WORKING-STORAGE SECTION.
       01 WS-EOF                   PIC X VALUE 'N'.
       01 HEADER1                  PIC X(80)
           VALUE "EMPLOYEES WITH AT LEAST 5 YEARS EXPERIENCE".
       01 HEADER2                  PIC X(80)
         VALUE "EmpID Dept FirstName             LastName          YOS".
       01 WS-EMPLOYEE-RECORD.
           05 WS-EMPLOYEEID        PIC 9(6).
           05 WS-DEPTCODE          PIC 9(3).
           05 WS-LASTNAME          PIC A(20).
           05 WS-FIRSTNAME         PIC A(20).
           05 WS-YEARSOFSERVICE    PIC 9(2).
       01 ENTERDATA                PIC X.
           88 NOMOREDATA VALUE 'N'.

       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
           PERFORM 201-INITIALIZE-RTN.
           PERFORM 202-CREATE-EMPLOYEE UNTIL WS-EOF = 'Y'.
           PERFORM 203-DISPLAY-EMPLOYEES.
           PERFORM 204-CLOSE-RTN.
           DISPLAY "==================================================".
           STOP RUN.

       201-INITIALIZE-RTN.
           OPEN OUTPUT EMPLOYEE-FILE.

       202-CREATE-EMPLOYEE.
           PERFORM UNTIL WS-EOF = 'Y'
           DISPLAY "Do you want to enter a new employee record? (Y/N):"
               ACCEPT ENTERDATA
               IF ENTERDATA = 'Y'
                   PERFORM 301-GET-EMPLOYEE-DATA
                   WRITE EMPLOYEE-RECORD
               ELSE
                   MOVE 'Y' TO WS-EOF
               END-IF
           END-PERFORM
           CLOSE EMPLOYEE-FILE.

       301-GET-EMPLOYEE-DATA.
           DISPLAY "Employee ID:"
           ACCEPT WS-EMPLOYEEID
           DISPLAY "Department Code:"
           ACCEPT WS-DEPTCODE
           DISPLAY "First Name:"
           ACCEPT WS-FIRSTNAME
           DISPLAY "Last Name:"
           ACCEPT WS-LASTNAME
           DISPLAY "Years of Service:"
           ACCEPT WS-YEARSOFSERVICE
           MOVE WS-EMPLOYEEID TO EMPLOYEEID-IN
           MOVE WS-DEPTCODE TO DEPTCODE-IN
           MOVE WS-LASTNAME TO LASTNAME-IN
           MOVE WS-FIRSTNAME TO FIRSTNAME-IN
           MOVE WS-YEARSOFSERVICE TO YEARSOFSERVICE-IN.

       203-DISPLAY-EMPLOYEES.
           MOVE 'N' TO WS-EOF
           OPEN INPUT EMPLOYEE-FILE
           DISPLAY HEADER1
           DISPLAY HEADER2
           DISPLAY "=================================================="
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF YEARSOFSERVICE-IN >= 5
                     DISPLAY EMPLOYEEID-IN SPACE
                             DEPTCODE-IN SPACE SPACE
                             FIRSTNAME-IN SPACE
                             LASTNAME-IN
                             YEARSOFSERVICE-IN
                       END-IF
               END-READ
           END-PERFORM.

       204-CLOSE-RTN.
           CLOSE EMPLOYEE-FILE.
