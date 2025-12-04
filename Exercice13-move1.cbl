       IDENTIFICATION DIVISION.
       PROGRAM-ID. Move1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 prenom1 PIC X(10).
       01 prenom2 PIC X(20).
       01 shortprenom PIC X(3).
       
       01 size-src PIC 9(2).
       01 size-dest PIC 9(2).

       01 nombrezero PIC 9(4) VALUE 12.
       01 nombreSSzero PIC zz9(2).
       
       01 dateinit PIC X(10) VALUE "20251118".
       01 datefinal.
           02 jour PIC X(2).
           02 FILLER PIC X(1) VALUE "/".
           02 mois PIC X(2).
           02 FILLER PIC X(1) VALUE "/".
           02 annee PIC X(4).

       01 etudiant1.
           02 prenom-etu  PIC X(10) VALUE "Nicolas".
           02 nom-etu     PIC X(10) VALUE "Gyss".
           02 age-etu     PIC 9(2) VALUE 43.

       01 etudiant2.
           02 prenom-etu  PIC X(20).
           02 nom-etu     PIC X(20).
           02 adresse-etu     PIC X(21) VALUE "22 rue des chanvriers".
    
       PROCEDURE DIVISION.
           
           DISPLAY "*** Déplacement d'une valeur dans une variable ***"
           MOVE "Nicolas" TO prenom1.
           DISPLAY "Prenom1 : " prenom1.
           
           DISPLAY "*** Déplacement champ court vers large ***"
           MOVE prenom1 TO prenom2.
           DISPLAY "Prenom2 : " prenom2.
           DISPLAY prenom1 " se lance dans l'aventure.".
           DISPLAY prenom2 " se lance dans l'aventure.".
           

           DISPLAY "*** Déplacement champ large vers court ***"
      *    Controle de la taille des données copiées
           COMPUTE size-src = FUNCTION LENGTH(prenom2).
           COMPUTE size-dest = FUNCTION LENGTH(shortprenom).

           IF size-src <= size-dest THEN
              DISPLAY "Transfert complet de la donnée"
           ELSE
              DISPLAY "ATTENTION: Donnée tronquée !"
              MOVE prenom2 TO shortprenom
              DISPLAY "DONNEE SCR : " prenom2 "DONNEE DEST : "
              shortprenom
           END-IF.
^
           DISPLAY "*** Déplacement d'un nombre avec 0 vers sans 0 ***".
           DISPLAY "Nombrezero : " nombrezero.
           MOVE nombrezero TO nombreSSzero.
           DISPLAY "NombreSSzero : " nombreSSzero.
    

           DISPLAY "*** REFORMATAGE DE DONNEES ***".
           DISPLAY "dateinit : " dateinit.
           MOVE dateinit(7:2) TO jour.
           MOVE dateinit(5:2) TO mois.
           MOVE dateinit(1:4) TO annee.
           DISPLAY "datefinal : " datefinal.

           DISPLAY "*** CONTROLE AVANT TRANSFERT ***".

           IF nombrezero IS NUMERIC THEN
              DISPLAY "nombrezero est un nombre. Instruction MOVE réali
      -    "sée"
              ELSE
              DISPLAY "Ce n'est pas un nombre ! Instruction MOVE non ré
      -    "alisée."
           END-IF.                

           DISPLAY "*** TRANSFERT DE DONNEES ENTRES STRUCTURES ***".
           DISPLAY "-- Etudiant 1 avant transfert --".
           DISPLAY etudiant1.
           DISPLAY "-- Etudiant 2 avant transfert --".
           DISPLAY etudiant2.

           DISPLAY "-- TRANSFERT --".
           MOVE CORRESPONDING etudiant1 TO etudiant2.

           DISPLAY "-- Etudiant 1 après transfert --".
           DISPLAY etudiant1.
           DISPLAY "-- Etudiant 2 après transfert --".
           DISPLAY etudiant2.
                            
           STOP RUN.
