       IDENTIFICATION DIVISION.
       PROGRAM-ID. BC802.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WS-BOUCLE PIC X(1).
           88 WS-FIN VALUE "Y".
       
       01 WS-SAISIE PIC X(6).
       01 WS-ORDRE PIC X.
       01 WS-NUMID PIC X(4).

       01 COMPLET PIC X(2).
           88 COMPLET1 VALUE "y1".
           
       01 COMPLET-P2 PIC X(2).
           88 COMPLET2 VALUE "y2".

       01 WS-CPT-NIV PIC 9.
       01 WS-CPT-EMP PIC 9.

       01 WS-PARKING.
           02 WS-NIV OCCURS 2 TIMES INDEXED BY I-NIV.
              03 WS-EMPL PIC X(4) OCCURS 3 TIMES INDEXED BY I-EMPL.

       LINKAGE SECTION.


       PROCEDURE DIVISION.
       
       0000-INITIALISATION-DEB.
           PERFORM 8900-MSG-LANCEMENT-DEB
           THRU 8910-MSG-LANCEMENT-FIN.

      *************************************************************
      *    ZONE DE PROGRAMME
      *************************************************************
           
           PERFORM UNTIL WS-FIN
              
              DISPLAY "Saisissez votre ordre (<E/S>,id<2chiffres> OU F"
              ACCEPT WS-SAISIE
              
              UNSTRING WS-SAISIE
                 DELIMITED BY ","
                 INTO  WS-ORDRE
                       WS-NUMID
              END-UNSTRING

              EVALUATE WS-ORDRE
                 WHEN "E"
                    PERFORM 6010-FCT-ENTREE
                 WHEN "S"
                    PERFORM 6020-FCT-SORTIE
                 WHEN "F"
                    SET WS-FIN TO TRUE
                    PERFORM 6000-AFF-ETAT

                 WHEN OTHER
                    DISPLAY "Saisie inconnue"
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
           DISPLAY "* LANCEMENT PROGRAMME : BC802         *".
           DISPLAY "***************************************".
       
       8910-MSG-LANCEMENT-FIN.
           EXIT PROGRAM.
       
       6010-FCT-ENTREE.
           

           SET I-NIV TO 1
           SET I-EMPL TO 1
           SET COMPLET TO "y "
           SET COMPLET-P2 TO "y "

           SEARCH WS-EMPL VARYING I-EMPL
               AT END
               SET COMPLET1 TO TRUE

                 WHEN FUNCTION LENGTH(FUNCTION TRIM(
                  WS-EMPL(I-NIV,I-EMPL))) = 0
      *              DISPLAY "WS-EMPL : " WS-EMPL(I-NIV,I-EMPL)
      *              DISPLAY "I-NIV1  : " I-NIV
      *              DISPLAY "I-EMPL1 : " I-EMPL    
                    MOVE WS-NUMID TO WS-EMPL(I-NIV,I-EMPL)
                    DISPLAY "Entrée d'un véhicule"
             
           END-SEARCH
           
           IF COMPLET1 THEN

              SET I-NIV TO 2
              SET I-EMPL TO 1
              SEARCH WS-EMPL VARYING I-EMPL
                  AT END
                  SET COMPLET2 TO TRUE
                 
                    WHEN FUNCTION LENGTH(FUNCTION TRIM(
                     WS-EMPL(I-NIV,I-EMPL))) = 0
      *                 DISPLAY "WS-EMPL : " WS-EMPL(I-NIV,I-EMPL)
      *                 DISPLAY "I-NIV2  : " I-NIV
      *                 DISPLAY "I-EMPL2 : " I-EMPL    
                       MOVE WS-NUMID TO WS-EMPL(I-NIV,I-EMPL)
                       DISPLAY "Entrée d'un véhicule"
                
              END-SEARCH
           END-IF

           IF COMPLET1 AND COMPLET2 THEN
              DISPLAY "Le parking est plein !"
              ELSE
              DISPLAY "Parking libre"
           END-IF



           EXIT PROGRAM.

           6020-FCT-SORTIE.
               SET COMPLET TO "y "
  
               SET I-NIV TO 1
               SET I-EMPL TO 1
               SEARCH WS-EMPL VARYING I-EMPL
      *             AT END
      *             DISPLAY "Contrôle Niv. 1 réalisé !"
                  
                     WHEN WS-EMPL(I-NIV,I-EMPL) = WS-NUMID
      *                  DISPLAY "I-NIV  : " I-NIV
      *                  DISPLAY "I-EMPL : " I-EMPL    
                        MOVE SPACES TO WS-EMPL(I-NIV,I-EMPL)
                        SET COMPLET1 TO TRUE
      
                 
               END-SEARCH
               
                  SET I-NIV TO 2
                  SET I-EMPL TO 1
                  SEARCH WS-EMPL VARYING I-EMPL
      *                AT END
      *                DISPLAY "Contrôle Niv. 2 réalisé !"
                     
                        WHEN WS-EMPL(I-NIV,I-EMPL) = WS-NUMID
      *                     DISPLAY "I-NIV  : " I-NIV
      *                     DISPLAY "I-EMPL : " I-EMPL    
                           MOVE SPACES TO WS-EMPL(I-NIV,I-EMPL)
                           SET COMPLET2 TO TRUE
                    
                  END-SEARCH
               
               IF NOT COMPLET1 AND NOT COMPLET2 THEN
                  DISPLAY "Le véhicule n'a pas été trouvé !"
               ELSE
                  DISPLAY "Sortie d'un véhicule"
               END-IF

           EXIT PROGRAM.

       6000-AFF-ETAT.
           DISPLAY " "
           DISPLAY "---------------------"
           DISPLAY "Occupation du parking"
           DISPLAY "---------------------"
           
           PERFORM VARYING WS-CPT-NIV FROM 1 BY 1 UNTIL WS-CPT-NIV > 2
      *       Je parcours mon niveau

              PERFORM VARYING WS-CPT-EMP FROM 1 BY 1
                 UNTIL WS-CPT-EMP > 3        
      *          Je parcours les emplacements
                 
                 IF FUNCTION LENGTH(FUNCTION TRIM(
                  WS-EMPL(WS-CPT-NIV,WS-CPT-EMP)))>0
                 THEN
                    DISPLAY "Emplacement " WS-CPT-NIV " , " WS-CPT-EMP
                    "  est occupé par " WS-EMPL(WS-CPT-NIV,WS-CPT-EMP)
                 ELSE
                    DISPLAY "Emplacement " WS-CPT-NIV " , " WS-CPT-EMP
                    "  est vide"
                 END-IF 


              END-PERFORM

           END-PERFORM

           EXIT PROGRAM.


       9998-FIN-NORMAL.
           DISPLAY "***************************************".
           DISPLAY "* FIN PROGRAMME : BC802               *".
           DISPLAY "***************************************".
           EXIT PROGRAM.

       
       END PROGRAM BC802.
