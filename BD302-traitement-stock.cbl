       IDENTIFICATION DIVISION.
       PROGRAM-ID. BD302.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT F-LOGISTIQUE ASSIGN TO "./in/Jour1E-S.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS FS-LOGISTIQUE.
          

       DATA DIVISION.
       FILE SECTION.

       FD F-LOGISTIQUE.
       01 F-LOGI-LIGNE PIC X(80).



       WORKING-STORAGE SECTION.
      
      ************************************************************
      *    Booleen de contrôle
      ************************************************************
       01 TROUVE PIC X(1).
           88 T-OK VALUE "O".
      *    T-OK = 1 : Code produit présent dans le hangar
      *    T-OK = 0 : Code produit non présent dans le hangar

       01 STOCKER PIC X(1).
           88 S-OK VALUE "O".
      *    S-OK = 1 : Produit a pu être strocké dans le hangar
      *    S-OK = 0 : Hangar plein     
      *    Utilisé dans la fonction Entrée de produit

       01 RECHERCHE PIC X(1).
           88 R-OK VALUE "O".
      *    R-OK = 1 : Code produit présent dans le hangar
      *    R-OK = 0 : Code produit non présent dans le hangar     
      *    Utilisé dans la fonction Sortie de produit
      
       01 FS-LOGISTIQUE PIC X(2).
      *    File Status de mon fichier Jour1E-S.txt

      ************************************************************
      *    Variables de fonctionnement
      ************************************************************
       01 WS-IDM    PIC X(3).
       01 WS-QTE    PIC S9(5) VALUE 0.
       01 WS-QTEA   PIC Z(5).
       01 WS-POIDS  PIC S9(5)V9(2).

       01 F-LOGI-CURRENT.
               02 F-LOGI-CURRENT-ACTION   PIC X(1).
               02 FILLER                  PIC X(1).
               02 F-LOGI-CURRENT-IDM      PIC X(3).
               02 FILLER                  PIC X(1).
               02 F-LOGI-CURRENT-QTE      PIC X(5).
               02 FILLER                  PIC X(1).
               02 F-LOGI-CURRENT-POIDS.
                  03 F-LOGI-CURRENT-POIDS-U     PIC X(5).
                  03 FILLER VALUE ".".
                  03 F-LOGI-CURRENT-POIDS-D     PIC X(2).
       01 F-LOGI-CURRENT-POIDSC PIC S9(5)V9(2).
       
       01 HANGAR.
           02 WS-ALLEE OCCURS 5 TIMES.
              03 WS-NIVEAU OCCURS 3 TIMES.
                 04 WS-HANGAR-IDM PIC X(3).
                 04 WS-HANGAR-QTE PIC 9(5).
                 04 WS-HANGAR-POIDS PIC S9(5)V9(2).
       
       01 WS-HANGAR-QTECALC  PIC S9(5).

       01 WS-CPT-ALLEE    PIC 9(1).
       01 WS-CPT-NIVEAU   PIC 9(1).

       LINKAGE SECTION.


       PROCEDURE DIVISION.
       
       0000-INITIALISATION-DEB.
           PERFORM 8900-MSG-LANCEMENT-DEB
           THRU 8910-MSG-LANCEMENT-FIN.

      *************************************************************
      *    ZONE DE PROGRAMME
      *************************************************************

      *    Ouverture du fichier.
           OPEN INPUT F-LOGISTIQUE.

      *    Gestion du retour de l'ouverture
           IF FS-LOGISTIQUE = "00" THEN
              DISPLAY "Ouverture du fichier 'Jour1E-S.txt' OK."
           ELSE
              DISPLAY "Erreur d'ouverture - " FS-LOGISTIQUE
           END-IF
      
      *    Lecture du fichier complet
      *    A la fin de la lecture le File Status du fichier renvoi 10
           PERFORM UNTIL FS-LOGISTIQUE = "10"

              READ F-LOGISTIQUE INTO F-LOGI-LIGNE
                 NOT AT END
      *             Traitement de la ligne courante         
      *             Transfert de ma ligne fichier au format texte
      *             vers ma ligne courante découpée au format texte
      *             ATTENTION: Tous les calculs devront faire appel à la
      *             fonction NUMVAL.           
                    MOVE F-LOGI-LIGNE TO F-LOGI-CURRENT
                    DISPLAY "Ligne courante:"
                    DISPLAY F-LOGI-CURRENT
      *             DEBUG - Contrôle des informations logistiques 
      *             PERFORM 6000-CTRL-INFO                    

      *             Traitement de l'action demandée sur la ligne
      *             courante              
                    PERFORM 6010-ACTIONS
                    
                 AT END
                    DISPLAY "Fin de fichier"
              END-READ
        
           END-PERFORM

      *    Affichage du hangar après traitement
           PERFORM 6040-AFFICHE-HANGAR.
              
      *    Fermeture du fichier.
           CLOSE F-LOGISTIQUE.

      *    Gestion du retour de la fermeture
           IF FS-LOGISTIQUE = "00" THEN
              DISPLAY "Fermeture du fichier 'Jour1E-S.txt' OK."
           ELSE
              DISPLAY "Erreur de fermeture - " FS-LOGISTIQUE
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
           DISPLAY "* LANCEMENT PROGRAMME : BD302         *".
           DISPLAY "***************************************".
       
       8910-MSG-LANCEMENT-FIN.
           EXIT PROGRAM.

       6000-CTRL-INFO.
      *    Affiche le contenu des données de la ligne courante
           DISPLAY "Action        : " F-LOGI-CURRENT-ACTION
           DISPLAY "Code produit  : " F-LOGI-CURRENT-IDM
           DISPLAY "Qte produit   : " F-LOGI-CURRENT-QTE
           DISPLAY "Poids produit : " F-LOGI-CURRENT-POIDS
           DISPLAY "WS-HANGAR-POIDS(WS-CPT-ALLEE,WS-CPT-NIVEAU) : "
              WS-HANGAR-POIDS(WS-CPT-ALLEE,WS-CPT-NIVEAU)
           DISPLAY "WS-HANGAR-POIDS(WS-CPT-ALLEE,WS-CPT-NIVEAU) : "
              WS-HANGAR-POIDS(WS-CPT-ALLEE,WS-CPT-NIVEAU)         

      ****************** Zone de Test ***********************
      *     SUBTRACT 525.01 FROM FUNCTION NUMVAL(F-LOGI-CURRENT-POIDS)
      *        GIVING F-LOGI-CURRENT-POIDSC

      *     DISPLAY "Poids corrigé : " F-LOGI-CURRENT-POIDSC
           
      *     INITIALIZE WS-QTE
           
      *     COMPUTE WS-QTE = FUNCTION NUMVAL(F-LOGI-CURRENT-QTE) - 200
      *     MOVE WS-QTE TO WS-QTEA

      *     DISPLAY "Nouvelle qte  : " WS-QTE

      ************ Fin zone de test ************************

      
           EXIT PROGRAM.

       6010-ACTIONS.
           
           EVALUATE F-LOGI-CURRENT-ACTION
              WHEN "E"
      *          Execution de la procédure d'entrée           
                 PERFORM 6020-PROCEDURE-ENTREE

              WHEN "S"
      *          Execution de la procédure de sortie
                 PERFORM 6030-PROCEDURE-SORTIE

              WHEN OTHER
                 DISPLAY "Erreur de code action"
           END-EVALUATE


           EXIT PROGRAM.
       
       6020-PROCEDURE-ENTREE.
           DISPLAY "Lancement procédure Entrée"
      *    Initialisation de l'indicateur
           SET TROUVE TO "N"
           SET STOCKER TO "N"

      *    ETAPE 1 - Recherche de l'article + cumule
      *    Je recherche mon article dans le hangar et cumule les qte
      *    si je trouve l'article     
           PERFORM VARYING WS-CPT-ALLEE FROM 1 BY 1
              UNTIL WS-CPT-ALLEE > 5 OR T-OK
              
              PERFORM VARYING WS-CPT-NIVEAU FROM 1 BY 1
                 UNTIL WS-CPT-NIVEAU > 3 OR T-OK
                 
                 IF WS-HANGAR-IDM(WS-CPT-ALLEE,WS-CPT-NIVEAU) =
                    F-LOGI-CURRENT-IDM THEN
      *             Je trouve mon article

      *             -> Je procède au cumul des qte
                    ADD
                    FUNCTION NUMVAL(F-LOGI-CURRENT-QTE) TO
                         WS-HANGAR-QTE(WS-CPT-ALLEE,WS-CPT-NIVEAU)
                         
      *             -> Je procède au cumul du poids
                    ADD
                    FUNCTION NUMVAL(F-LOGI-CURRENT-POIDS) TO
                         WS-HANGAR-POIDS(WS-CPT-ALLEE,WS-CPT-NIVEAU)
               
      *             -> Je place mon indicateur T-OK à 1
                    SET T-OK TO TRUE                   
                    
                 END-IF

              END-PERFORM

           END-PERFORM

      *    ETAPE 2 - Contrôle du cumul.
      *    T-OK = 0, il faut enregistrer le produit dans le hangar à la
      *     première place vide
      *    T-OK = 1, le produit est déjà cumulé.       
           
           IF NOT T-OK THEN
           
           PERFORM VARYING WS-CPT-ALLEE FROM 1 BY 1
              UNTIL WS-CPT-ALLEE > 5 OR S-OK
              
                 PERFORM VARYING WS-CPT-NIVEAU FROM 1 BY 1
                    UNTIL WS-CPT-NIVEAU > 3 OR S-OK
                    
                    IF WS-HANGAR-IDM(WS-CPT-ALLEE,WS-CPT-NIVEAU) =
                       SPACES THEN

      *                 Je trouve un espace vide
      *                 -> Je stock mon produit
                       MOVE F-LOGI-CURRENT-IDM TO
                            WS-HANGAR-IDM(WS-CPT-ALLEE,WS-CPT-NIVEAU)
                       MOVE F-LOGI-CURRENT-QTE TO
                            WS-HANGAR-QTE(WS-CPT-ALLEE,WS-CPT-NIVEAU)

                       MOVE FUNCTION NUMVAL(F-LOGI-CURRENT-POIDS) TO 
                          F-LOGI-CURRENT-POIDSC                

                       MOVE F-LOGI-CURRENT-POIDSC TO     
                            WS-HANGAR-POIDS(WS-CPT-ALLEE,WS-CPT-NIVEAU)

                       SET S-OK TO TRUE                   
                       
                    END-IF
                 END-PERFORM
              END-PERFORM

           END-IF

      *    ETAPE 3 - Contrôle Stockage.
      *    S-OK = 0, il n'y a plus de place dans le hangar
      *    S-OK = 1, stockage réalisé
      
           IF NOT S-OK AND NOT T-OK THEN
              DISPLAY "Stockage IMPOSSIBLE. Le hangar est plein."
           END-IF



           EXIT PROGRAM.

       6030-PROCEDURE-SORTIE.
           DISPLAY "Lancement procédure Sortie"
      *    Initialisation de l'indicateur
           SET TROUVE TO "N"
           SET STOCKER TO "N"
           MOVE 0 TO WS-HANGAR-QTECALC

      *    ETAPE 1 - Recherche de l'article et traitement si l'article
      *    est présent dans le hangar.
           PERFORM VARYING WS-CPT-ALLEE FROM 1 BY 1
              UNTIL WS-CPT-ALLEE > 5 OR T-OK
              
              PERFORM VARYING WS-CPT-NIVEAU FROM 1 BY 1
                 UNTIL WS-CPT-NIVEAU > 3 OR T-OK
                 
      *          Je controle le code article
                 IF WS-HANGAR-IDM(WS-CPT-ALLEE,WS-CPT-NIVEAU) =
                    F-LOGI-CURRENT-IDM THEN
      *             Code article à sortir trouvé

      *             Je soustrais la quantité de mon fichier logistique
      *             à celle présente dans le hangar
                    SUBTRACT FUNCTION NUMVAL(F-LOGI-CURRENT-QTE) FROM
                    WS-HANGAR-QTE(WS-CPT-ALLEE,WS-CPT-NIVEAU) GIVING
                    WS-HANGAR-QTECALC
                    
                    MOVE WS-HANGAR-QTECALC TO
                    WS-HANGAR-QTE(WS-CPT-ALLEE,WS-CPT-NIVEAU)
                    
      
      *             Je soustrais le poids de mon fichier logistique
      *             à celle présente dans le hangar
                    SUBTRACT FUNCTION NUMVAL(F-LOGI-CURRENT-POIDS) FROM
                    WS-HANGAR-POIDS(WS-CPT-ALLEE,WS-CPT-NIVEAU)
      
      *             Si la quantité dans le hangar < 0, je place les 
      *             qte à 0
                    IF WS-HANGAR-QTECALC < 0 THEN
                       DISPLAY "Le produit N°"
                       WS-HANGAR-IDM(WS-CPT-ALLEE,WS-CPT-NIVEAU) 
                       " ne dispose pas d'une quantité suffisante"
      
      *                Je remplace les valeurs qte et poids de l'article
      *                par 0.
                       MOVE 0 TO
                       WS-HANGAR-QTE(WS-CPT-ALLEE,WS-CPT-NIVEAU)
                       WS-HANGAR-POIDS(WS-CPT-ALLEE,WS-CPT-NIVEAU)
                       
                    END-IF

      *             Si la quantité présente dans le hangar = 0 je libère
      *             la zone de stockage
                    IF WS-HANGAR-QTE(WS-CPT-ALLEE,WS-CPT-NIVEAU) = 0
                       THEN
                       MOVE SPACES TO
                       WS-NIVEAU(WS-CPT-ALLEE,WS-CPT-NIVEAU)
                    END-IF

      *             Je place mon indicateur TROUVE à 1
                    SET T-OK TO TRUE                            


                 END-IF           


              END-PERFORM

           END-PERFORM

      *    ETAPE 2 - Article non présent
      *    T-OK = 0, le produit n'existe pas dans le hangar
      *    T-OK = 1, le traitement de la sortie est réussie.
           IF T-OK THEN
              DISPLAY "Traitement de la ligne S réussie"
           ELSE
              DISPLAY "ATTENTION - Produit non disponible dans le stock"
           END-IF



           EXIT PROGRAM.


       6040-AFFICHE-HANGAR.
           DISPLAY "-- CONTENU DU HANGAR --".
           DISPLAY " Allée | Niveau |       Contenu     |".
           DISPLAY "-------------------------------------".
           PERFORM VARYING WS-CPT-ALLEE FROM 1 BY 1
              UNTIL WS-CPT-ALLEE > 5
                 
                 PERFORM VARYING WS-CPT-NIVEAU FROM 1 BY 1
                    UNTIL WS-CPT-NIVEAU > 3
                    
                    DISPLAY "   "WS-CPT-ALLEE "   |    "
                    WS-CPT-NIVEAU "   |"
                    WS-HANGAR-IDM(WS-CPT-ALLEE,WS-CPT-NIVEAU) ","
                    WS-HANGAR-QTE(WS-CPT-ALLEE,WS-CPT-NIVEAU) ","
                    WS-HANGAR-POIDS(WS-CPT-ALLEE,WS-CPT-NIVEAU)

                 END-PERFORM


           END-PERFORM.
           
           DISPLAY "-------------------------------------".

           EXIT PROGRAM.

       9998-FIN-NORMAL.
           DISPLAY "***************************************".
           DISPLAY "* FIN PROGRAMME : BD302               *".
           DISPLAY "***************************************".
           EXIT PROGRAM.

       
       END PROGRAM BD302.
