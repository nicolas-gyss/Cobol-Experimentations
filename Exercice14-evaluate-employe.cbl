       IDENTIFICATION DIVISION.
       PROGRAM-ID. EtatEmploye.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 etat PIC X(4).
       01 saisie PIC 9.
           88 saisieOK VALUE 1.
           88 saisieKO VALUE 0.
           
       PROCEDURE DIVISION.
       
       SET saisie TO 0.

       PERFORM UNTIL saisieOK
           DISPLAY "Saisissez l'état de l'employé (ACT / INAC / CNG)"
           ACCEPT etat

           IF etat = "ACT" OR etat = "INAC" OR etat = "CNG" THEN
              SET saisie TO 1
           END-IF

       END-PERFORM.

       EVALUATE etat
           WHEN "ACT"
              DISPLAY "Employé actif"
           WHEN "INAC"
              DISPLAY "Employé inactif"
           WHEN "CNG"
              DISPLAY "Employé en congés"
           WHEN OTHER
              DISPLAY "Etat de l'employé inconnue"           
       END-EVALUATE.

       STOP RUN.
