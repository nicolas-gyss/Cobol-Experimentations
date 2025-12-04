       IDENTIFICATION DIVISION.
       PROGRAM-ID. Moyenne.

       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
       01 nbnotes PIC 9(2).
       01 note PIC 9(2).
       01 sommes PIC 9(4).
       01 moyenne PIC 9(2)V9(2).
       01 cpt PIC 9(2).

       PROCEDURE DIVISION.

       DISPLAY "Combien de notes souhaitez-vous saisir ?".
       ACCEPT nbnotes.

       PERFORM VARYING cpt FROM 1 BY 1 UNTIL cpt > nbnotes 
           DISPLAY "Valeur de la note numéro " cpt
           ACCEPT note

           COMPUTE sommes = sommes + note

       END-PERFORM

      * Calcule de la moyenne
       COMPUTE moyenne = sommes / nbnotes.

       DISPLAY moyenne.

       IF moyenne >= 10 THEN 

           DISPLAY "Reussi"
       ELSE
           DISPLAY "Echoue"
       END-IF




       STOP RUN.
