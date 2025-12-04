       IDENTIFICATION DIVISION.
       PROGRAM-ID. SommeColLig.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 tab.
           02 vendeur OCCURS 3 TIMES.
              03 nom-vendeur PIC X(15).
              03 tab-trim OCCURS 4 TIMES.
                 04 vente PIC 9(5).

       01 tab-annuel.
           02 a-vendeur OCCURS 3 TIMES.
              03 a-nom-vendeur PIC X(15).
              03 a-somme-vendeur PIC ZZZ9(2).
       
       01 tab-trim.
           02 trim-somme OCCURS 4.
              03 somme-trimestre PIC 9(5) VALUE ZERO.


       01 cpt-ligne PIC 9(1).
       01 cpt-col PIC 9(1).

       01 saisiTexte PIC X(15).
       01 saisiVente PIC 9(5).

       01 cal-somme-annuel PIC 9(5).

      

       PROCEDURE DIVISION.

           INITIALIZE cpt-ligne.
           INITIALIZE cpt-col.
           INITIALIZE cal-somme-annuel.

           DISPLAY "Tableau des ventes".

           PERFORM VARYING cpt-ligne FROM 1 BY 1 UNTIL cpt-ligne > 3

              DISPLAY "Nom du vendeur " cpt-ligne
              ACCEPT saisiTexte

              MOVE saisiTexte TO nom-vendeur(cpt-ligne)
              MOVE saisiTexte TO a-nom-vendeur(cpt-ligne)

      *       Initialisation de la variable de cal de somme à 0        
              MOVE 0 TO cal-somme-annuel

              PERFORM VARYING cpt-col FROM 1 BY 1 UNTIL cpt-col > 4
                     
                 DISPLAY "Entrer les ventes de " nom-vendeur(cpt-ligne)
                 " trimestre " cpt-col " : "

                 ACCEPT saisiVente

                 MOVE saisiVente TO vente(cpt-ligne,cpt-col)
                 
      *         Additionner les ventes trimestres pour le vendeur en
      *         cours
                COMPUTE cal-somme-annuel = cal-somme-annuel +
                saisiVente

      *         Pour chaque trimestre je prends la valeur presente dans
      *         mon tableau de somme-trimestre(cpt-col) et je
      *         l'additionne avec la valeur présente dans vente(cpt-l,
      *         cpt-col)
                COMPUTE somme-trimestre(cpt-col) = 
                somme-trimestre(cpt-col) + vente(cpt-ligne,cpt-col)


              END-PERFORM

      *       Enregistrement des ventes annuelles du vendeur en cours
              MOVE cal-somme-annuel TO a-somme-vendeur(cpt-ligne)       

           END-PERFORM.
           
           DISPLAY "***********************************".
           DISPLAY "* Affichage du tableau des ventes *".
           DISPLAY "***********************************".
           
           PERFORM VARYING cpt-ligne FROM 1 BY 1 UNTIL cpt-ligne > 3
              
              DISPLAY nom-vendeur(cpt-ligne) "T1 : " vente(cpt-ligne,1)
              " T2 : " vente(cpt-ligne,2)
              " T3 : " vente(cpt-ligne,3)
              " T4 : " vente(cpt-ligne,4)

           END-PERFORM.

           DISPLAY "***********************************".
           DISPLAY "*    Totaux annuel par vendeur    *".
           DISPLAY "***********************************".

           PERFORM VARYING cpt-ligne FROM 1 BY 1 UNTIL cpt-ligne > 3
              
              DISPLAY a-nom-vendeur(cpt-ligne) "Total : " 
              a-somme-vendeur(cpt-ligne)

           END-PERFORM.

           DISPLAY "***********************************".
           DISPLAY "*       Totaux par trimestre      *".
           DISPLAY "***********************************".

           PERFORM VARYING cpt-ligne FROM 1 BY 1 UNTIL cpt-ligne > 4
              
              DISPLAY "Trimestre " cpt-ligne " = "
              somme-trimestre(cpt-ligne)

           END-PERFORM.

           

           STOP RUN.
