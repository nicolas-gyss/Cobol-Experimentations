       IDENTIFICATION DIVISION.
       PROGRAM-ID. AnalyseRIB.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 RIB-RAW PIC X(30).
       01 BANK-CODE PIC X(5).
       01 AGENCY-CODE PIC X(5).
       01 ACCOUNT-NUMBER PIC X(11).
       01 KEY-NUMBER PIC X(2).
       01 COUNT-DASH PIC 9(2).


       PROCEDURE DIVISION.
           
           DISPLAY "Saisissez votre RIB (XXXXX-XXXXX-XXXXXXXXXXX-XX) :".
           ACCEPT RIB-RAW.
           
           INSPECT RIB-RAW TALLYING COUNT-DASH FOR ALL "-".

           IF COUNT-DASH = 3 THEN
              DISPLAY "RIB OK"
              
              UNSTRING RIB-RAW DELIMITED BY "-"
                 INTO  BANK-CODE
                    AGENCY-CODE
                    ACCOUNT-NUMBER
                    KEY-NUMBER
              END-UNSTRING
      
                 DISPLAY "BANK-CODE = " BANK-CODE
                 DISPLAY "AGENCY-CODE = " AGENCY-CODE
                 DISPLAY "ACCOUNT-NUMBER = " ACCOUNT-NUMBER
                 DISPLAY "KEY = " KEY-NUMBER
      
                 ELSE
              DISPLAY "Attention votre RIB ne semble pas bon."
           END-IF. 

           STOP RUN.
