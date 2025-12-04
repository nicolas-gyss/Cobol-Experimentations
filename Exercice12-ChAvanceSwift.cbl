       IDENTIFICATION DIVISION.
       PROGRAM-ID. BuildSwift.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 IBAN PIC X(34) VALUE "FR7630004000031234567890143".
       01 BENEF-NAME PIC X(20) VALUE "DUPONT JEAN".
       01 AMOUNT PIC X(10) VALUE "1500EUR".
       01 SWIFT-MSG PIC X(80).
       01 cpt PIC 9(2).

       PROCEDURE DIVISION.

           STRING "/IBAN/" IBAN "/NAME/" BENEF-NAME "/AMT/" AMOUNT
              INTO SWIFT-MSG
           END-STRING.

           MOVE 0 TO cpt.

           INSPECT SWIFT-MSG TALLYING cpt FOR ALL "/".

           IF cpt > 2 THEN
              DISPLAY "Code SWIFT valide." BACKGROUND-COLOR 2
              FOREGROUND-COLOR 15
              ELSE
              DISPLAY "Code SWIFT invalide." BACKGROUND-COLOR 12
              FOREGROUND-COLOR 15
           END-IF.

           STOP RUN.
