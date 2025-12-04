       IDENTIFICATION DIVISION.
       PROGRAM-ID. Majorite.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       01 age PIC S9(2).

       


       PROCEDURE DIVISION.

       DISPLAY "Quel est votre age ?".

       ACCEPT age.

       IF age < 0 THEN
           
           DISPLAY "Age invalide"

           ELSE IF age < 18 THEN

                 DISPLAY "Vous êtes mineur"
                
                ELSE

                    DISPLAY "Vous êtes majeur"
               END-IF
       END-IF



       STOP RUN.

