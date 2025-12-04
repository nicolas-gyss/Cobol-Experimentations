       IDENTIFICATION DIVISION.
       PROGRAM-ID. FonctionSimple.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 prenom PIC x(25).
       01 age PIC x(2).
       01 agen PIC 9(2).
       01 lprenom PIC 9(2).


       PROCEDURE DIVISION.

           DISPLAY "Saisissez un prénom ('FIN' pour arreter):".
           ACCEPT prenom.

           PERFORM UNTIL prenom = "FIN"
           
              DISPLAY "Saisir votre age :"
              ACCEPT age
              
              COMPUTE agen = FUNCTION NUMVAL(age)
              
              DISPLAY "Bonjour " FUNCTION UPPER-CASE(prenom)
              
              PERFORM TEST-MAJEUR    
              
              
              INSPECT prenom TALLYING lprenom FOR CHARACTERS BEFORE
              SPACE
              
              DISPLAY "Votre prenom comporte " lprenom " caracteres"
             
              MOVE 0 TO lprenom
              
              DISPLAY "Saisissez un prénom ('FIN' pour arreter):"
              ACCEPT prenom

           END-PERFORM


       STOP RUN.

       TEST-MAJEUR.
           IF agen < 18 THEN
                 DISPLAY "Vous être mineur"
           ELSE
                 DISPLAY "Vous être majeur" 
           END-IF.
