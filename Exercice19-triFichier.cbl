      ***************************************************************
      * Classe les clients provenant du fichier persoNonTrie.txt
      * dans un nouveau fichier persoTrie.txt en allant du plus jeune
      * au plus ancien.
      * Utilisation de la procedure SORT
      ***************************************************************   
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. triFichier.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT persoNonTrie ASSIGN TO "./in/persoNonTrie.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS WS-FS-persoNonTrie.


           SELECT persoTrie ASSIGN TO "./out/persoTrie.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS WS-FS-persoTrie.

      *    Fichier de travail intermédiaire (Crée et supprimé après
      *    sortie de l'instuction SORT)
           SELECT WorkFile ASSIGN TO "./out/workfile.txt".


       DATA DIVISION.
       FILE SECTION.
      * File Description du fichier persoNonTrie
       FD persoNonTrie.
       01 persoNonTrie-L.
           02 IdClient-NT PIC 9(3).
           02 Prenom-NT PIC X(10).
           02 Nom-NT PIC X(10).
           02 Age-NT PIC X(2).

      * File Description du fichier persoTrie
       FD persoTrie.
       01 persoTrie-L.
           02 IdClient-T PIC 9(3).
           02 Prenom-T PIC X(10).
           02 Nom-T PIC X(10).
           02 Age-T PIC X(2).

      * Sort Description du fichier de travail WorkFile Temporaire
       SD WorkFile.
       01 WorkFile-Data.
           02 IdClient-W PIC 9(3).
           02 Prenom-W PIC X(10).
           02 Nom-W PIC X(10).
           02 Age-W PIC X(2).
       
       WORKING-STORAGE SECTION.

       01 WS-FS-persoNonTrie PIC X(2).
       01 WS-FS-persoTrie PIC X(2).
       01 EOF PIC X(1).
           88 EOFOK VALUE 'o'.

       PROCEDURE DIVISION.

      *    Ouverture du fichier à lire
           OPEN INPUT persoNonTrie.
           IF WS-FS-persoNonTrie = "00" THEN
              DISPLAY "Ouverture fichier persoNonTrie OK."
           ELSE
              DISPLAY "Erreur d'ouverture du fichier persoNonTrie."
           END-IF
           
           SET EOF TO 'n'.
           
           PERFORM UNTIL EOFOK
      *    Lecture du fichier persoNonTrie pour vérifier la taille des 
      *    lignes. (Normalement 25)
              READ persoNonTrie INTO persoNonTrie-L
                 AT END
                    SET EOF TO 'o'
                 NOT AT END
                    DISPLAY "Ligne courante " persoNonTrie-L
                    DISPLAY "Nb caractère : "
                    FUNCTION LENGTH(persoNonTrie-L)
              END-READ
          
           END-PERFORM.

           CLOSE persoNonTrie.
         
           SORT WorkFile ON ASCENDING KEY Age-W
                       USING persoNonTrie
                       GIVING persoTrie.

           STOP RUN.

       END PROGRAM triFichier.
