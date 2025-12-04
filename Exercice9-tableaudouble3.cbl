       IDENTIFICATION DIVISION.
       PROGRAM-ID. SaisiValeur.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 tab.
           02 vendeur OCCURS 2 TIMES.
              03 nom-vendeur PIC X(15).
              03 tab-trim OCCURS 4 TIMES.
                 04 vente PIC ZZZ9(2).
       
       01 cpt-ligne PIC 9(1).
       01 cpt-col PIC 9(1).

       01 saisiTexte PIC X(15).
       01 saisiVente PIC 9(5).


       PROCEDURE DIVISION.

           INITIALIZE cpt-ligne.
           INITIALIZE cpt-col.

           DISPLAY "Tableau des ventes".

           PERFORM VARYING cpt-ligne FROM 1 BY 1 UNTIL cpt-ligne > 2

              DISPLAY "Nom du vendeur " cpt-ligne
              ACCEPT saisiTexte

              MOVE saisiTexte TO nom-vendeur(cpt-ligne)

              PERFORM VARYING cpt-col FROM 1 BY 1 UNTIL cpt-col > 4
                 
                 DISPLAY "Entrer les ventes de " nom-vendeur(cpt-ligne)
                 " trimestre " cpt-col " : "

                 ACCEPT saisiVente

                 MOVE saisiVente TO vente(cpt-ligne,cpt-col)

              END-PERFORM

           END-PERFORM.
           
           DISPLAY "*****************************".
           DISPLAY "* Affichage de votre saisie *".
           DISPLAY "*****************************".

           PERFORM VARYING cpt-ligne FROM 1 BY 1 UNTIL cpt-ligne > 2
              DISPLAY "*****************************"
              DISPLAY nom-vendeur(cpt-ligne) " | Trimestre 1 : " 
              vente(cpt-ligne,1) " | Trimestre 2 : " vente(cpt-ligne,2)
              " | Trimestre 3 : " vente(cpt-ligne,3)
              " | Trimestre 4 : " vente(cpt-ligne,4)

           END-PERFORM.

           STOP RUN.
