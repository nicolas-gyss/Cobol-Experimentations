       IDENTIFICATION DIVISION.
       PROGRAM-ID. Decoupe.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 date-string PIC X(10) VALUE "2025/11/17".
       01 YEAR PIC X(4).
       01 MONTH PIC X(2).
       01 DAYS PIC X(2).

       PROCEDURE DIVISION.

           UNSTRING date-string DELIMITED BY "/"
              INTO YEAR
                   MONTH
                   DAYS
              
           END-UNSTRING.
      
           DISPLAY "YEAR = " YEAR.
           DISPLAY "MONTH = " MONTH.
           DISPLAY "DAY = " DAYS.

           STOP RUN.
           