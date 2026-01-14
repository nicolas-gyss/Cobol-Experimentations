       IDENTIFICATION DIVISION.
       PROGRAM-ID. BD101.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-SAISIE.
           02 WS-ACTION   PIC X(1).
           02 FILLER      PIC X.
           02 WS-IDPLAT   PIC X(3).
           02 FILLER      PIC X.
           02 WS-PRENOM   PIC X(20).

       01 FIN PIC X.
           88 SORTIE VALUE "F".

       01 RECHERCHE PIC X.
           88 TROUVE VALUE "O".
       
       01 WS-CPT-FRIGO PIC 9.
       01 WS-CPT-RAYON PIC 9.
       01 WS-CPT-EMPL  PIC 9.
       
       01 WS-SAVE-EMPLACEMENT.
           02 WS-SAVE-FRIGO PIC 9.
           02 WS-SAVE-RAYON PIC 9.
           01 WS-SAVE-EMPL  PIC 9.

       01 tableau.
           02 WS-FRIGOS OCCURS 4 TIMES.
              03 WS-RAYONS OCCURS 5 TIMES.
                 04 WS-EMPL OCCURS 4 TIMES.
                    05 WS-EMPL-PLAT   PIC X(3).
                    05 WS-EMPL-PRENOM PIC X(20).
       
       LINKAGE SECTION.


       PROCEDURE DIVISION.
       
       0000-INITIALISATION-DEB.
           PERFORM 8900-MSG-LANCEMENT-DEB
           THRU 8910-MSG-LANCEMENT-FIN.

      *************************************************************
      *    ZONE DE PROGRAMME
      *************************************************************
           MOVE "N" TO FIN.

           PERFORM WITH TEST AFTER UNTIL SORTIE

              DISPLAY "Que souhaitez-vous faire ?"
              DISPLAY "Usage : Action,ID-plat,Prenom"
              DISPLAY "Action: A,S,F | ID-plat (3) | Prenom (20)"
              ACCEPT WS-SAISIE
              
              EVALUATE WS-ACTION
                 WHEN "F"
                    SET SORTIE TO TRUE
                    DISPLAY "Appel recap"

                    PERFORM 6010-RECAP

                 WHEN "A"
                    DISPLAY "Ajout - Produit"
                    DISPLAY "Action : " WS-ACTION
                    DISPLAY "Plat : " WS-IDPLAT
                    DISPLAY "Prénom : " WS-PRENOM

                    PERFORM 6000-AJOUT


                 WHEN "S"
                    DISPLAY "Appel Suppression"
                    DISPLAY "Action : " WS-ACTION
                    DISPLAY "Plat : " WS-IDPLAT
                    DISPLAY "Prénom : " WS-PRENOM

                    PERFORM 6010-SUPPRESSION

                 WHEN OTHER
                    DISPLAY "Action inconnue"
              END-EVALUATE

           END-PERFORM.
      
           PERFORM 9998-FIN-NORMAL.

       0000-INITIALISATION-FIN.
           STOP RUN.               

      *************************************************************
      * FIN ZONE PROCEDURE
      *************************************************************

      *************************************************************
      * ZONE DE PARAGRAPHE DU LOGICIEL
      ***************************************************************

       8900-MSG-LANCEMENT-DEB.
           DISPLAY "***************************************".
           DISPLAY "* LANCEMENT PROGRAMME : BD101         *".
           DISPLAY "***************************************".
       
       8910-MSG-LANCEMENT-FIN.
           EXIT PROGRAM.


       6000-AJOUT.
      *    Ajoute un produit dans le premier emplacement libre d'un
      *    frigo.
           
      *    initialisation de la variable TROUVE
           SET RECHERCHE TO "N"

      *    Je parcours mes frigos et je m'arrete si je dépasse 4 OU
      *    j'ai trouvé un emplacement libre.     
           PERFORM VARYING WS-CPT-FRIGO FROM 1 BY 1
              UNTIL WS-CPT-FRIGO > 4 OR TROUVE
      *       Je parcours les frigos

              PERFORM VARYING WS-CPT-RAYON FROM 1 BY 1
                 UNTIL WS-CPT-RAYON > 5 OR TROUVE
      *          Je parcours les rayons           

                 PERFORM VARYING WS-CPT-EMPL FROM 1 BY 1
                    UNTIL WS-CPT-EMPL > 4 OR TROUVE
      *          Je parcours les emplacements
                 
                 IF WS-EMPL(WS-CPT-FRIGO,WS-CPT-RAYON,WS-CPT-EMPL) =
                 SPACES THEN
      *          Emplacement vide
                 MOVE WS-IDPLAT TO 
                 WS-EMPL-PLAT(WS-CPT-FRIGO,WS-CPT-RAYON,WS-CPT-EMPL)
                 MOVE WS-PRENOM TO
                 WS-EMPL-PRENOM(WS-CPT-FRIGO,WS-CPT-RAYON,WS-CPT-EMPL)
      *          Sauvegarde de l'emplacement trouvé
                 MOVE WS-CPT-FRIGO TO WS-SAVE-FRIGO
                 MOVE WS-CPT-RAYON TO WS-SAVE-RAYON
                 MOVE WS-CPT-EMPL TO WS-SAVE-EMPL
      *          J'indique qu'un emplacement est trouvé.                      
                 SET TROUVE TO TRUE

                 END-IF

                 END-PERFORM

              END-PERFORM

           END-PERFORM

           IF TROUVE THEN
              DISPLAY "Plat ajouté dans Frigo N° " WS-SAVE-FRIGO " | R
      -       "ayon " WS-SAVE-RAYON " | Emplacement " WS-SAVE-EMPL      
           ELSE
              DISPLAY "Il n'y a plus de place."
           END-IF


           EXIT PROGRAM.

       6010-RECAP.
      *    Affiche le contenu de tous les frigos

           PERFORM VARYING WS-CPT-FRIGO FROM 1 BY 1
              UNTIL WS-CPT-FRIGO > 4
              
              PERFORM VARYING WS-CPT-RAYON FROM 1 BY 1
                 UNTIL WS-CPT-RAYON > 5

                 PERFORM VARYING WS-CPT-EMPL FROM 1 BY 1
                    UNTIL WS-CPT-EMPL > 4
                    
                 DISPLAY "Contenu Frigo N°" WS-CPT-FRIGO " Rayon N°"
                 WS-CPT-RAYON " Empl. N°" WS-CPT-EMPL " : "
                 WS-EMPL-PLAT(WS-CPT-FRIGO,WS-CPT-RAYON,WS-CPT-EMPL)
                 " , " 
                 WS-EMPL-PRENOM(WS-CPT-FRIGO,WS-CPT-RAYON,WS-CPT-EMPL)
                 END-PERFORM

              END-PERFORM



           END-PERFORM                   

           EXIT PROGRAM.

       6010-SUPPRESSION.
      *    Supprime le contenu d'un emplacement et affiche un msg
      *    si la personne qui enlève le place et différente de celle
      *    qui l'a posé.

      *    initialisation de la variable TROUVE
           SET RECHERCHE TO "N"

      *    Je parcours mes frigos et je m'arrete si je dépasse 4 OU
      *    j'ai trouvé le plat.     

           PERFORM VARYING WS-CPT-FRIGO FROM 1 BY 1
              UNTIL WS-CPT-FRIGO > 4 OR TROUVE
      *       Je parcours les frigos

              PERFORM VARYING WS-CPT-RAYON FROM 1 BY 1
                 UNTIL WS-CPT-RAYON > 5 OR TROUVE
      *          Je parcours les rayons           


                 PERFORM VARYING WS-CPT-EMPL FROM 1 BY 1
                    UNTIL WS-CPT-EMPL > 4 OR TROUVE
      *          Je parcours les emplacements
                 
                 IF WS-EMPL-PLAT(WS-CPT-FRIGO,WS-CPT-RAYON,WS-CPT-EMPL)
                 = WS-IDPLAT THEN
      *          Je prends le plat de l'emplacement
                 MOVE SPACES TO 
                 WS-EMPL-PLAT(WS-CPT-FRIGO,WS-CPT-RAYON,WS-CPT-EMPL)

      *          Je vérifie le prénom de la personne
                 IF 
                 WS-EMPL-PRENOM(WS-CPT-FRIGO,WS-CPT-RAYON,WS-CPT-EMPL)
                 IS NOT = WS-PRENOM THEN
                    DISPLAY "La personne qui prend le plat n'est pas
      -          " la même qui l'a déposé"
                 END-IF

                 MOVE SPACES TO
                 WS-EMPL-PRENOM(WS-CPT-FRIGO,WS-CPT-RAYON,WS-CPT-EMPL)
      
      *          J'indique qu'un emplacement est trouvé.               
                 SET TROUVE TO TRUE


                 END-IF


                 END-PERFORM


              END-PERFORM


           END-PERFORM


           IF TROUVE THEN
              DISPLAY "Le plat a été supprimé" 
           ELSE
              DISPLAY "Le plat n'existe pas."
           END-IF


           EXIT PROGRAM.

       9998-FIN-NORMAL.
           DISPLAY "***************************************".
           DISPLAY "* FIN PROGRAMME : BD101               *".
           DISPLAY "***************************************".
           EXIT PROGRAM.

       
       END PROGRAM BD101.
