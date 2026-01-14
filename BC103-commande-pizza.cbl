       IDENTIFICATION DIVISION.
       PROGRAM-ID. BC103.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-NB-PIZTHEO PIC 9(3)v9(2).
       01 WS-NB-PIZREEL PIC 9(3).
       01 WS-DECIMAL    PIC 9(1).
       01 WS-NB-CONVIVE PIC 9(2).

       LINKAGE SECTION.


       PROCEDURE DIVISION.
       
       0000-INITIALISATION-DEB.
           PERFORM 8900-MSG-LANCEMENT-DEB
           THRU 8910-MSG-LANCEMENT-FIN.

      *************************************************************
      *    ZONE DE PROGRAMME
      *************************************************************
           DISPLAY "Combien de convives sont invités ?".
           ACCEPT WS-NB-CONVIVE.

           COMPUTE WS-NB-PIZTHEO = WS-NB-CONVIVE * 1.1.
           
           MOVE WS-NB-PIZTHEO(4:1) TO WS-DECIMAL.

           IF WS-DECIMAL > 0 THEN
              MOVE WS-NB-PIZTHEO TO WS-NB-PIZREEL
              ADD 1 TO WS-NB-PIZREEL
           ELSE
              MOVE WS-NB-PIZTHEO TO WS-NB-PIZREEL
           END-IF.

           DISPLAY "WS-NB-PIZTHEO :" WS-NB-PIZTHEO.
           DISPLAY "WS-NB-PIZREEL :" WS-NB-PIZREEL.

      
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
           DISPLAY "* LANCEMENT PROGRAMME : BC103         *".
           DISPLAY "* Commandes de pizzas                 *".
           DISPLAY "***************************************".
       
       8910-MSG-LANCEMENT-FIN.
           EXIT PROGRAM.

       9998-FIN-NORMAL.
           DISPLAY "***************************************".
           DISPLAY "* FIN PROGRAMME : BC103               *".
           DISPLAY "***************************************".
           EXIT PROGRAM.

       
       END PROGRAM BC103.
