       IDENTIFICATION DIVISION.
       PROGRAM-ID. Plusgrand.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       01 mon-tableau.
           02 tab-entier PIC 9(2) OCCURS 10 TIMES.

       01 cpt PIC 9(2).
       01 max PIC 9(2).
       01 valsaisi PIC 9(2).

       PROCEDURE DIVISION.

           INITIALIZE max.

           DISPLAY "Veuillez saisir 10 valeurs".
           
           PERFORM VARYING cpt FROM 1 BY 1 UNTIL cpt > 10
              
              DISPLAY "Valeur " cpt " : "
              ACCEPT valsaisi

              MOVE valsaisi TO tab-entier(cpt)

           END-PERFORM.

           DISPLAY "- Voici la liste des 10 valeurs saisies -".

           PERFORM VARYING cpt FROM 1 BY 1 UNTIL cpt > 10
              
              DISPLAY tab-entier(cpt)
              IF tab-entier(cpt) > max THEN
                 MOVE tab-entier(cpt) TO max
              END-IF

           END-PERFORM.

           DISPLAY "Valeur maximale saisie : " max.

           STOP RUN.
