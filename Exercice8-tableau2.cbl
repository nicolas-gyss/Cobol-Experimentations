       IDENTIFICATION DIVISION.
       PROGRAM-ID. SaisiAffiche.

       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
       
       01 tableau.
           02 tableau-entier PIC 9(2) OCCURS 5 TIMES.
       
       01 cpt PIC 9(2).
       01 ma-valeur PIC 9(2).
       01 somme PIC 9(3).

       PROCEDURE DIVISION.

           DISPLAY "Enregistrement de vos 5 valeurs".
           INITIALIZE somme.
           MOVE 0 TO somme.
           
           PERFORM VARYING cpt FROM 1 BY 1 UNTIL cpt > 5
              
              DISPLAY "Saisir la valeur " cpt
              ACCEPT ma-valeur
              
              MOVE ma-valeur TO tableau-entier(cpt)

           END-PERFORM.

           DISPLAY "Affichage des 5 valeurs saisies".

           PERFORM VARYING cpt FROM 1 BY 1 UNTIL cpt > 5
           
              DISPLAY "Valeur " cpt " saisie : " tableau-entier(cpt)
      *       Calcul de la somme
              COMPUTE somme = somme + tableau-entier(cpt)

           END-PERFORM
           
           DISPLAY "******************".
           DISPLAY "Somme des valeurs saisies: " somme.
           

           STOP RUN.

