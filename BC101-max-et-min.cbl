       IDENTIFICATION DIVISION.
       PROGRAM-ID. BC101.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-NB1 PIC 9(3) VALUE 105.
       01 WS-NB2 PIC 9(3) VALUE 15.
       
       01 WS-CPT PIC 9(2) VALUE 1.

       01 WS-NBCOURANT PIC 9(3).
       01 WS-NB-MIN PIC 9(3).
       01 WS-NB-MAX PIC 9(3).
      
       01 tab.
           02 nombres.
              03 FILLER PIC 9(3) VALUE 32.
              03 FILLER PIC 9(3) VALUE 52.
              03 FILLER PIC 9(3) VALUE 12.
              03 FILLER PIC 9(3) VALUE 56.
              03 FILLER PIC 9(3) VALUE 120.
              03 FILLER PIC 9(3) VALUE 25.
              03 FILLER PIC 9(3) VALUE 65.
              03 FILLER PIC 9(3) VALUE 78.
              03 FILLER PIC 9(3) VALUE 25.
              03 FILLER PIC 9(3) VALUE 2.
           02 tab-nombre REDEFINES nombres.
              03 nombre PIC 9(3) OCCURS 10 TIMES.

       LINKAGE SECTION.


       PROCEDURE DIVISION.
       
       0000-INITIALISATION-DEB.
           PERFORM 8900-MSG-LANCEMENT-DEB
           THRU 8910-MSG-LANCEMENT-FIN.

      *************************************************************
      *    ZONE DE PROGRAMME
      *************************************************************
           DISPLAY "Nombres comparés :".
           DISPLAY "NB1 : " WS-NB1 " | NB2 :" WS-NB2.
           
           IF WS-NB1 < WS-NB2 THEN
              DISPLAY "NB1 est pus petit que NB2"
              DISPLAY WS-NB1
           ELSE  
              DISPLAY "NB2 est plus petit que NB1"
              DISPLAY WS-NB2
           END-IF.
           
           MOVE nombre(1) TO WS-NBCOURANT WS-NB-MIN.

           PERFORM VARYING WS-CPT FROM 1 BY 1 UNTIL WS-CPT > 10  

              IF nombre(WS-CPT) < WS-NBCOURANT THEN
                 MOVE nombre(WS-CPT) TO WS-NBCOURANT
              END-IF

           EVALUATE nombre(WS-CPT)
              WHEN < WS-NB-MIN
                 MOVE nombre(WS-CPT) TO WS-NB-MIN
              WHEN > WS-NB-MAX
                 MOVE nombre(WS-CPT) TO WS-NB-MAX
           END-EVALUATE
          
           END-PERFORM.
           
           DISPLAY "Nombre le plus petit : " WS-NBCOURANT.

           DISPLAY "NB-MIN : " WS-NB-MIN.
           DISPLAY "NB-MAX : " WS-NB-MAX.

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
           DISPLAY "* LANCEMENT PROGRAMME : BC101         *".
           DISPLAY "***************************************".
       
       8910-MSG-LANCEMENT-FIN.
           EXIT PROGRAM.

       9998-FIN-NORMAL.
           DISPLAY "***************************************".
           DISPLAY "* FIN PROGRAMME : BC101               *".
           DISPLAY "***************************************".
           EXIT PROGRAM.

       
       END PROGRAM BC101.
