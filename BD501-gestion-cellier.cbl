       IDENTIFICATION DIVISION.
       PROGRAM-ID. BD501.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT F-CELLIER ASSIGN TO "./in/Cellier.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS FS-CELLIER.

       DATA DIVISION.
       FILE SECTION.
       FD F-CELLIER.
       01 F-CELLIER-CURRENT PIC X(47).

       WORKING-STORAGE SECTION.
       01 FS-CELLIER PIC X(2).

      ***************************************************************
      *    Variable qui contient ma chaine extraite du fichier après
      *    traitement.
      ***************************************************************
       01 WS-CELLIER-CURRENT.
           02 WS-CELLIER-CURRENT-ACTION  PIC X(1).
           02 WS-CELLIER-CURRENT-REGION  PIC X(20).
           02 WS-CELLIER-CURRENT-CEPAGE  PIC X(20).
           02 WS-CELLIER-CURRENT-QTE     PIC 9(3).

       01 CELLIER.
           02 CELLIER-REGION OCCURS 3 TIMES.
              03 WS-REGION   PIC X(20).
              03 CELLIER-REGION-CEPAGE OCCURS 4 TIMES.
                 04 WS-CEPAGE   PIC X(20).
                 04 WS-QTE      PIC 9(3).
       
       01 WS-QTES               PIC S9(3).

       01 WS-CPT-REGION PIC 9(1).
       01 WS-CPT-CEPAGE PIC 9(1).

      **********************************************************
      *    Booleen de controle
      **********************************************************
       01 REGION PIC X(1).
           88 R-OK VALUE "O".
      *    R-OK = 1 : La région est trouvée dans le cellier.
      *    R-OK = 0 : La région est n'est pas trouvée dans le cellier.

       01 CEPAGE PIC X(1).
           88 C-OK VALUE "O".
      *    C-OK = 1 : Le cepage est trouvé dans le cellier.
      *    C-OK = 0 : Le cépage n'est pas trouvé dans le cellier.

       01 ENTREE-CEP PIC X(1).
           88 ENT-CEP-OK VALUE "O".
      *    ENT-CEP-OK = 1 : Une place a été trouvé pour le cepage dans
      *                     le cellier. Les bouteilles sont ajoutées.
      *    ENT-CEP-OK = 0 : La région ne dispose plus d'emplacement de
      *                     libre pour ajouter ce cépage.
 
       01 ENTREE-REG PIC X(1).
           88 ENT-REG-OK VALUE "O".
      *    ENT-REG-OK = 1 : Une place a été trouvé pour cette région
      *                     dans le cellier. La région et les bouteilles
      *                     sont ajoutées dans le cellier.
      *    ENT-REG-OK = 0 : Le cellier ne peut pas accueillir de
      *                     nouvelles régions. Les bouteilles ne sont
      *                     pas enregistrées.

       LINKAGE SECTION.


       PROCEDURE DIVISION.
       
       0000-INITIALISATION-DEB.
           PERFORM 8900-MSG-LANCEMENT-DEB
           THRU 8910-MSG-LANCEMENT-FIN.

      *************************************************************
      *    ZONE DE PROGRAMME
      *************************************************************
      
      *    ETAPE 1 - Ouverture du fichier
           OPEN INPUT F-CELLIER.

           IF FS-CELLIER = "00" THEN
              DISPLAY "Ouverture du fichier Cellier.txt - OK"
           ELSE
              DISPLAY "Erreur d'ouverture - Code : " FS-CELLIER
           END-IF

      *    ETAPE 2 - Creation de la boucle de lecture du fichier
           PERFORM UNTIL FS-CELLIER = "10"

      *       ETAPE 3 - Recupération de la ligne courante du fichier
              READ F-CELLIER INTO F-CELLIER-CURRENT
                 AT END
                    DISPLAY "Fin de lecture du fichier."
                 NOT AT END
      *             ETAPE 4 - Traitement de la ligne courante
                    PERFORM 6010-DECOUPE-LIGNE              
      *             DEBUG - Affichage de la ligne courante découpée
      *              PERFORM 5000-DEBUG-INFO
      
      *             ETAPE 5 - Selection de l'action à réaliser
                    EVALUATE WS-CELLIER-CURRENT-ACTION
                       WHEN "E"
                          PERFORM 6020-ACTION-ENTREE

                       WHEN "S"
                          PERFORM 6030-ACTION-SORTIE

                       WHEN OTHER
                          DISPLAY "Action inconnue - Corrigé votre fic
      -                   "hier"                    
                    END-EVALUATE
      
              END-READ


           END-PERFORM

      *    ETAPE 6 - Afficher un récapitulatif du cellier.
           PERFORM 6100-RECAP-CELLIER.



      *    ETAPE 7 - Fermeture du fichier
           CLOSE F-CELLIER.

           IF FS-CELLIER = "00" THEN
              DISPLAY "Fermeture du fichier Cellier.txt - OK"
           ELSE
              DISPLAY "Erreur de fermeture - Code : " FS-CELLIER
           END-IF

      
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
           DISPLAY "* LANCEMENT PROGRAMME : BD501         *".
           DISPLAY "***************************************".
       
       8910-MSG-LANCEMENT-FIN.
           EXIT PROGRAM.

       6010-DECOUPE-LIGNE.
      *    Permet de découper la ligne courante en données exploitables
      *    par le programme.
           
           UNSTRING F-CELLIER-CURRENT
              DELIMITED BY ";"
              INTO
                 WS-CELLIER-CURRENT-ACTION
                 WS-CELLIER-CURRENT-REGION
                 WS-CELLIER-CURRENT-CEPAGE
                 WS-CELLIER-CURRENT-QTE
           END-UNSTRING
           EXIT PROGRAM.

       6020-ACTION-ENTREE.
      *    Réalise l'entrée d'une bouteille de vin. Cette action se
      *    lance uniquement lorsque le code "E" est trouvé dans la ligne
      *    courante du fichier.
           DISPLAY "Action Entrée"
      
      *    Initialisation
           SET REGION TO "N"
           SET CEPAGE TO "N"

      *    Je parcours le cellier à la recherche de ma région
           PERFORM VARYING WS-CPT-REGION FROM 1 BY 1
              UNTIL WS-CPT-REGION > 3 OR R-OK
              
                     IF WS-REGION(WS-CPT-REGION)
                        = WS-CELLIER-CURRENT-REGION THEN
      *                La région existe dans le cellier
      *                ATTENTION : il faut basculer R-OK à TRUE
      *                uniquement lorsque la fin de la boucle cepage est
      *                 atteinte
                       PERFORM VARYING WS-CPT-CEPAGE FROM 1 BY 1
                          UNTIL WS-CPT-CEPAGE > 4 OR C-OK

                          IF WS-CEPAGE(WS-CPT-REGION,WS-CPT-CEPAGE)
                          = WS-CELLIER-CURRENT-CEPAGE THEN
      *                   Le cepage est trouvé, j'ajoute mes bouteilles
                             ADD WS-CELLIER-CURRENT-QTE TO
                             WS-QTE(WS-CPT-REGION,WS-CPT-CEPAGE)                    
      *                  Je place mon indicateur de cepage trouvé à vrai                       
                             SET C-OK TO TRUE
                          END-IF
                  
                       END-PERFORM

      *                Je vérifie si j'ai trouvé mon cépage
                       IF NOT C-OK THEN
      *                   Le cépage n'a pas été trouvé
      *                --> Je recherche s'il y a de la place dans le
      *                    cellier avec la fonction CTRL-DISPO-C
                          PERFORM 6040-CTRL-DISPO-C
      
                          IF ENT-CEP-OK THEN
      *                      Un emplacement de cépage est disponible
                             DISPLAY "Le cépage '"
                             FUNCTION TRIM(WS-CELLIER-CURRENT-CEPAGE)
                           "' est ajouté au cellier dans la région '"
                            FUNCTION TRIM(WS-REGION(WS-CPT-REGION))"'"

                          ELSE
      *                      Aucun emplacement de cépage disponible                    
                             DISPLAY "La région "
                             WS-REGION(WS-CPT-REGION) " ne dispose pas
      -                      "d'emplacement libre pour des nouveaux cé
      -                      "pages."
                          END-IF                


                       ELSE
      *                   Cépage trouvé affiche ajout OK
                          DISPLAY "Bouteille de "
                          FUNCTION
                          TRIM(WS-CEPAGE(WS-CPT-REGION,WS-CPT-CEPAGE)) 
                          " ajoutée."
                       END-IF
                 
      
      *                L'ensemble des cépages de la région sont
      *                parcourus, je place mon indicateur région trouvé
      *                à vrai.
                       SET R-OK TO TRUE
                       
                     END-IF

           END-PERFORM

      *    Je vérifie si La région existe pas
           IF NOT R-OK THEN   
      *    Si la région n'exite pas NOT R-OK 
      *    --> Je recherche s'il y a de la place dans le
      *                    cellier avec la fonction CTRL-DISPO-R
              PERFORM 6050-CTRL-DISPO-R
              
              IF ENT-REG-OK THEN
      *                Si ENT-REG-OK
      *                ---> CODER affichage région crée et nom cepage
                 DISPLAY "La région '" 
                 FUNCTION TRIM(WS-CELLIER-CURRENT-REGION) "' et le cepag
      -          "e '" 
                 FUNCTION TRIM(WS-CELLIER-CURRENT-CEPAGE) "' ont été c
      -          "rée."           

              ELSE
      *                Sinon
      *                ---> CODER cellier ne peut plus accueillir de
      *                     nouvelle région.
                 DISPLAY "Le cellier ne peut pas accueillir de nouvelle
      -          "région."           

              END-IF                    
           END-IF



           EXIT PROGRAM.
      
       6030-ACTION-SORTIE.
      *    Réalise la sortie d'une bouteille de vin. Cette action se
      *    lance uniquement lorsque le code "S" est trouvé dans la ligne
      *    courante du fichier.
           DISPLAY "Action Sortie"
      
      *    initialisation
           SET REGION TO "N"
           SET CEPAGE TO "N"

      *    Recherche de la région en cours
           PERFORM VARYING WS-CPT-REGION FROM 1 BY 1
              UNTIL WS-CPT-REGION > 3 OR R-OK
              
              IF WS-REGION(WS-CPT-REGION) = 
                 WS-CELLIER-CURRENT-REGION THEN
      *          Region trouvée
      *          Recherche du cepage
                 PERFORM VARYING WS-CPT-CEPAGE FROM 1 BY 1
                    UNTIL WS-CPT-CEPAGE > 4 OR C-OK
                    
                    IF WS-CEPAGE(WS-CPT-REGION,WS-CPT-CEPAGE) =
                    WS-CELLIER-CURRENT-CEPAGE THEN
      *             cepage trouvé

      *             Je déduis mes quantités
                    SUBTRACT WS-CELLIER-CURRENT-QTE FROM
                    WS-QTE(WS-CPT-REGION,WS-CPT-CEPAGE) GIVING WS-QTES

      *             Si les quantités sont nulles où négative
                       IF WS-QTES <= 0 THEN
                          MOVE HIGH-VALUE TO
                          WS-QTE(WS-CPT-REGION,WS-CPT-CEPAGE)
                          DISPLAY "Réassort du cépage "
                          FUNCTION TRIM(WS-CELLIER-CURRENT-CEPAGE)
                          " à faire."

                       ELSE
                          MOVE WS-QTES TO
                          WS-QTE(WS-CPT-REGION,WS-CPT-CEPAGE)
                       END-IF

      *          Je place mon indicateur de cépage trouvé à vrai              
                    SET C-OK TO TRUE

                    END-IF
                    

                 END-PERFORM

      *          Je place mon indicateur de région trouvée à vrai
                 SET R-OK TO TRUE
              END-IF 

           END-PERFORM     


      *    Affichage du message si aucune bouteille de la région
      *    demandée n'a été trouvé
           IF NOT R-OK THEN
              DISPLAY "Le cellier ne contient pas de bouteilles de la re
      -       "gion '" FUNCTION TRIM(WS-CELLIER-CURRENT-REGION) "'"
           END-IF




           EXIT PROGRAM.

       6040-CTRL-DISPO-C.
      *    Vérifie si un emplacement de cépage est disponible pour la
      *    région en cours.
      
      *    initialisation     
           SET ENTREE-CEP TO "N"

           PERFORM VARYING WS-CPT-CEPAGE FROM 1 BY 1
              UNTIL WS-CPT-CEPAGE > 4 OR ENT-CEP-OK
              
              IF WS-CEPAGE(WS-CPT-REGION,WS-CPT-CEPAGE) = SPACES THEN
      *          Je trouve une emplacement vide --> j'enregistre mon
      *          cepage
                 MOVE WS-CELLIER-CURRENT-CEPAGE TO
                    WS-CEPAGE(WS-CPT-REGION,WS-CPT-CEPAGE)
                 MOVE WS-CELLIER-CURRENT-QTE TO
                    WS-QTE(WS-CPT-REGION,WS-CPT-CEPAGE)
      
      *          Je place mon indicateur d'ajout de cepage à vrai
                 SET ENT-CEP-OK TO TRUE   
                  
              END-IF


           END-PERFORM


           EXIT PROGRAM.
       
       6050-CTRL-DISPO-R.
      *    Vérifie si un emplacement de région est disponible dans le
      *    cellier.
            
      *    initialisation     
           SET ENTREE-REG TO "N"

           PERFORM VARYING WS-CPT-REGION FROM 1 BY 1
              UNTIL WS-CPT-REGION > 3 OR ENT-REG-OK
              
              IF WS-REGION(WS-CPT-REGION) = SPACES THEN
      *          Un espace de région est diponible --> j'enregistre
      *          la nouvelle région, mon nouveau cepage et la qte.
                 MOVE WS-CELLIER-CURRENT-REGION TO
                    WS-REGION(WS-CPT-REGION)
                 MOVE WS-CELLIER-CURRENT-CEPAGE TO
                    WS-CEPAGE(WS-CPT-REGION,1)
                 MOVE WS-CELLIER-CURRENT-QTE TO
                    WS-QTE(WS-CPT-REGION,1)   
      
      *          Je place mon indicateur de création de région à oui
                 SET ENT-REG-OK TO TRUE
              END-IF

           END-PERFORM


           EXIT PROGRAM.

       6100-RECAP-CELLIER.
      *    Permet d'afficher le cellier en fin d'execution du programme
           
           DISPLAY "----------------------CELLIER--------------------"
           DISPLAY "-------------------------------------------------"
           DISPLAY "|       REGION       |      CEPAGE        | QTE |"
           DISPLAY "-------------------------------------------------"

           PERFORM VARYING WS-CPT-REGION FROM 1 BY 1
              UNTIL WS-CPT-REGION > 3
              
              PERFORM VARYING WS-CPT-CEPAGE FROM 1 BY 1
                 UNTIL WS-CPT-CEPAGE > 4
                    DISPLAY "|" WS-REGION(WS-CPT-REGION)
                    "|" WS-CEPAGE(WS-CPT-REGION,WS-CPT-CEPAGE)
                    "| " WS-QTE(WS-CPT-REGION,WS-CPT-CEPAGE) " |"

              END-PERFORM
                 

           END-PERFORM

           DISPLAY "-------------------------------------------------"

           EXIT PROGRAM.
       
       5000-DEBUG-INFO.
      *    Permet d'afficher les informations de la ligne courante du
      *     fichier traité.
           DISPLAY "Ligne courante :" WS-CELLIER-CURRENT
           DISPLAY "Action : '" WS-CELLIER-CURRENT-ACTION "'"
           DISPLAY "Région : '" WS-CELLIER-CURRENT-REGION "'"
           DISPLAY "Cépage : '" WS-CELLIER-CURRENT-CEPAGE "'"
           DISPLAY "Qte    : " WS-CELLIER-CURRENT-QTE


           EXIT PROGRAM.

       9998-FIN-NORMAL.
           DISPLAY "***************************************".
           DISPLAY "* FIN PROGRAMME : BD501               *".
           DISPLAY "***************************************".
           EXIT PROGRAM.

       
       END PROGRAM BD501.
