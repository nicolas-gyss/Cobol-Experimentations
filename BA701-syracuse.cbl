       IDENTIFICATION DIVISION.
       PROGRAM-ID. BA701.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-NB-SAISIE    PIC 9(3) VALUE 0.
       01 WS-RESULTAT     PIC 9(3).
       01 WS-NB-BOUCLE    PIC 9(3).
       01 WS-RESTE        PIC 9(3).
       
       LINKAGE SECTION.


       PROCEDURE DIVISION.
       
       0000-INITIALISATION-DEB.
           PERFORM 8900-MSG-LANCEMENT-DEB
           THRU 8910-MSG-LANCEMENT-FIN.

      *************************************************************
      *    ZONE DE PROGRAMME
      *************************************************************

           PERFORM WITH TEST AFTER UNTIL WS-NB-SAISIE IS NOT ZERO
              DISPLAY "Saisissez un nombre"
              ACCEPT WS-NB-SAISIE
           END-PERFORM.
           
           INITIALIZE WS-NB-BOUCLE.

           PERFORM UNTIL WS-RESULTAT = 1
              
              COMPUTE WS-RESTE = FUNCTION MOD(WS-NB-SAISIE,2)

              IF WS-RESTE = 0 THEN
      *          Nombre saisi est pair
                 COMPUTE WS-RESULTAT = WS-NB-SAISIE / 2
              ELSE
      *          Nombre saisi est impair
                 COMPUTE WS-RESULTAT = (WS-NB-SAISIE * 3) + 1
              END-IF                
              
              MOVE WS-RESULTAT TO WS-NB-SAISIE
              ADD 1 TO WS-NB-BOUCLE

           END-PERFORM.

           DISPLAY "Fin de traitement.".
           DISPLAY "Valeur de WS-RESULTAT :" WS-RESULTAT.
           DISPLAY "Nombre de traitement :" WS-NB-BOUCLE.
      
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
           DISPLAY "* LANCEMENT PROGRAMME : BA701         *".
           DISPLAY "* Conjecture de Syracuse              *".
           DISPLAY "***************************************".
       
       8910-MSG-LANCEMENT-FIN.
           EXIT PROGRAM.

       9998-FIN-NORMAL.
           DISPLAY "***************************************".
           DISPLAY "* FIN PROGRAMME : BA701               *".
           DISPLAY "***************************************".
           EXIT PROGRAM.

       
       END PROGRAM BA701.
