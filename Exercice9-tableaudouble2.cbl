       IDENTIFICATION DIVISION.
       PROGRAM-ID. TableauDoubleMoy.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       01 tableau.
           02 t-etudiants OCCURS 3 TIMES.
              03 nom PIC X(20).
              03 t-matiere OCCURS 4 TIMES.
                 04 notes PIC 9(2).

       01 cpt-ligne PIC 9(2).
       01 cpt-col PIC 9(2).

       01 saisi-nom PIC X(20).
       01 saisi-note PIC 9(2).
       01 somme PIC 9(2).
       01 moyenne PIC 9(2)V9(2).

       PROCEDURE DIVISION.
       
           INITIALIZE somme.
           INITIALIZE moyenne.

           DISPLAY "Carnet scolaire".

           PERFORM VARYING cpt-ligne FROM 1 BY 1 UNTIL cpt-ligne > 3

              DISPLAY "Nom de l'étudiant " cpt-ligne
              ACCEPT saisi-nom

              MOVE saisi-nom TO nom(cpt-ligne)

              PERFORM VARYING cpt-col FROM 1 BY 1 UNTIL cpt-col > 4
                 
                 DISPLAY "Saisissez la note " cpt-col
                 " Etudiant " nom(cpt-ligne)
                 ACCEPT saisi-note

                 MOVE saisi-note TO notes(cpt-ligne,cpt-col)

              END-PERFORM

           END-PERFORM.

           DISPLAY "Calcul des moyennes en cours...".

           PERFORM VARYING cpt-ligne FROM 1 BY 1 UNTIL cpt-ligne > 3
              
              MOVE 0 TO moyenne
              MOVE 0 TO somme

              PERFORM VARYING cpt-col FROM 1 BY 1 UNTIL cpt-col > 4
                 
                 COMPUTE somme = somme + notes(cpt-ligne,cpt-col)
                       
              END-PERFORM
              
              COMPUTE moyenne = somme / 4

              DISPLAY nom(cpt-ligne) " : Moyenne = " moyenne 

           END-PERFORM.



           STOP RUN.

