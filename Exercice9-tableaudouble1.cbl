       IDENTIFICATION DIVISION.
       PROGRAM-ID. TableauDouble.

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


       PROCEDURE DIVISION.
       
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


           DISPLAY "Contrôle de votre saisie".
           DISPLAY "************************".

           PERFORM VARYING cpt-ligne FROM 1 BY 1 UNTIL cpt-ligne > 3
                 
                 DISPLAY nom(cpt-ligne) " : " notes(cpt-ligne,1) " "  
                 notes(cpt-ligne,2) " " notes(cpt-ligne,3) " "
                 notes(cpt-ligne,4)

           END-PERFORM.
           
           STOP RUN.

