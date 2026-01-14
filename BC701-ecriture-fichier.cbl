       IDENTIFICATION DIVISION.
       PROGRAM-ID. BC701.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT F-ECARTS ASSIGN TO "./in/ECARTS.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS FS-F-ECARTS.

       SELECT F-ECART0 ASSIGN TO "./out/ECART0.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS FS-F-ECART0.
       
       SELECT F-ECARTR ASSIGN TO "./out/ECARTR.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS FS-F-ECARTR.
       
       SELECT F-ECARTA ASSIGN TO "./out/ECARTA.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS FS-F-ECARTA.

       DATA DIVISION.
       FILE SECTION.
       FD F-ECARTS.
       01 F-ECARTS-CURRENT.
           02 F-ECARTS-FAM   PIC X(3).
           02 F-ECARTS-DESC  PIC X(20).
           02 F-ECARTS-SIZE  PIC X(3).
           02 F-ECARTS-COUL  PIC X(20).
           02 F-ECARTS-CODE  PIC X(10).
           02 F-ECARTS-STOCK PIC 9(5).
           02 F-ECARTS-VENTE PIC 9(5).
           02 F-ECARTS-EMP   PIC X(2).
           02 F-ECARTS-PRIX  PIC 9(6)v9(2).
           02 FILLER         PIC X(5).

       FD F-ECART0.
       01 F-ECART0-CURRENT.
           02 F-ECART0-FAM   PIC X(3).
           02 F-ECART0-DESC  PIC X(20).
           02 F-ECART0-SIZE  PIC X(3).
           02 F-ECART0-COUL  PIC X(20).
           02 F-ECART0-CODE  PIC X(10).
           02 F-ECART0-STOCK PIC 9(5).
           02 F-ECART0-VENTE PIC 9(5).
           02 F-ECART0-EMP   PIC X(2).
           02 F-ECART0-PRIX  PIC 9(6)v9(2).
           02 FILLER         PIC X(5).

       FD F-ECARTR.
       01 F-ECARTR-CURRENT.
           02 F-ECARTR-FAM   PIC X(3).
           02 F-ECARTR-DESC  PIC X(20).
           02 F-ECARTR-SIZE  PIC X(3).
           02 F-ECARTR-COUL  PIC X(20).
           02 F-ECARTR-CODE  PIC X(10).
           02 F-ECARTR-STOCK PIC 9(5).
           02 F-ECARTR-VENTE PIC 9(5).
           02 F-ECARTR-EMP   PIC X(2).
           02 F-ECARTR-PRIX  PIC 9(6)v9(2).
           02 FILLER         PIC X(5).

       FD F-ECARTA.
       01 F-ECARTA-CURRENT.
           02 F-ECARTA-FAM   PIC X(3).
           02 F-ECARTA-DESC  PIC X(20).
           02 F-ECARTA-SIZE  PIC X(3).
           02 F-ECARTA-COUL  PIC X(20).
           02 F-ECARTA-CODE  PIC X(10).
           02 F-ECARTA-STOCK PIC 9(5).
           02 F-ECARTA-VENTE PIC 9(5).
           02 F-ECARTA-EMP   PIC X(2).
           02 F-ECARTA-PRIX  PIC 9(6)v9(2).
           02 FILLER         PIC X(5).


       WORKING-STORAGE SECTION.
       01 FS-F-ECARTS PIC X(2).
       01 FS-F-ECART0 PIC X(2).
       01 FS-F-ECARTR PIC X(2).
       01 FS-F-ECARTA PIC X(2).

       LINKAGE SECTION.


       PROCEDURE DIVISION.
       
       0000-INITIALISATION-DEB.
           PERFORM 8900-MSG-LANCEMENT-DEB
           THRU 8910-MSG-LANCEMENT-FIN.

      *************************************************************
      *    ZONE DE PROGRAMME
      *************************************************************

      *    Ouverture des fichiers
           OPEN INPUT F-ECARTS.
           IF FS-F-ECARTS = 00 THEN
              DISPLAY "Ouverture du fichier ECARTS.txt OK."
           ELSE
              DISPLAY "Erreur ouverture."
              DISPLAY "Code erreur : " FS-F-ECARTS
           END-IF.

           OPEN OUTPUT F-ECART0.
           IF FS-F-ECART0 = 00 THEN
              DISPLAY "Ouverture du fichier ECART0.txt OK."
           ELSE
              DISPLAY "Erreur ouverture ECART0.txt."
              DISPLAY "Code erreur : " FS-F-ECART0
           END-IF.

           OPEN OUTPUT F-ECARTR.
           IF FS-F-ECARTR = 00 THEN
              DISPLAY "Ouverture du fichier ECARTR.txt OK."
           ELSE
              DISPLAY "Erreur ouverture ECARTR.txt."
              DISPLAY "Code erreur : " FS-F-ECARTR
           END-IF.
           
           OPEN EXTEND F-ECARTA.
           IF FS-F-ECARTA = 00 THEN
              DISPLAY "Ouverture du fichier ECARTA.txt OK."
           ELSE
              DISPLAY "Erreur ouverture ECARTA.txt."
              DISPLAY "Code erreur : " FS-F-ECARTA
           END-IF.


      *    Lecture du fichier jusqu'au retour fin de fichier     
           PERFORM UNTIL FS-F-ECARTS = 10
              READ F-ECARTS INTO F-ECARTS-CURRENT
                 NOT AT END
      *             Copie de la ligne courante dans les zones courantes
      *             des autres fichiers
                    MOVE F-ECARTS-CURRENT TO
                       F-ECARTR-CURRENT
                       F-ECARTA-CURRENT

      *             Mise à zero des zones numériques
                    MOVE ZEROS TO
                       F-ECARTR-STOCK
                       F-ECARTR-VENTE
                       F-ECARTR-PRIX
                       F-ECARTA-PRIX
                       F-ECARTA-STOCK
                       F-ECARTA-VENTE
           
      *             Ecriture dans les fichiers
                    WRITE F-ECARTR-CURRENT
                    WRITE F-ECARTA-CURRENT

              END-READ
           END-PERFORM.
       
      *    Fermeture des fichiers.
           CLOSE F-ECARTS.
           DISPLAY "Fichier ECARTS.txt fermé.".

           CLOSE F-ECART0.
           DISPLAY "Fichier ECART0.txt fermé.".

           CLOSE F-ECARTA.
           DISPLAY "Fichier ECARTA.txt fermé.".

           CLOSE F-ECARTR.
           DISPLAY "Fichier ECARTR.txt fermé.".
      
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
           DISPLAY "* LANCEMENT PROGRAMME : BC701         *".
           DISPLAY "***************************************".
       
       8910-MSG-LANCEMENT-FIN.
           EXIT PROGRAM.

       9998-FIN-NORMAL.
           DISPLAY "***************************************".
           DISPLAY "* FIN PROGRAMME : BC701               *".
           DISPLAY "***************************************".
           EXIT PROGRAM.

       
       END PROGRAM BC701.
