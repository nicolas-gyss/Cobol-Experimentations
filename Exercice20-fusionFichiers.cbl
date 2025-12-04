      ***************************************************************
      * Permet de fusionner plusieurs fichiers présents dans mon
      * dans dossier /in dans un seul fichier /out
      * Utilisation de la procedure MERGE
      ***************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. fusionFichier.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT fichier1 ASSIGN TO "./in/persoNonTrie1.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL.

           
           SELECT fichier2 ASSIGN TO "./in/persoNonTrie2.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL.

           SELECT fichier1T ASSIGN TO "./out/persoTrie1.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL.

           
           SELECT fichier2T ASSIGN TO "./out/persoTrie2.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL.              

           SELECT fichierOut ASSIGN TO "./out/persoCompletTri.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL.
              
           SELECT workfile ASSIGN TO "./out/workfile.txt".

       DATA DIVISION.
       FILE SECTION.
       FD fichier1.
       01 fichier1-I.
           02 IdPerso-I      PIC 9(3).
           02 PrenomPerso-I  PIC X(10).
           02 NomPerso-I     PIC X(10).
           02 Age-I          PIC 9(2).

       FD fichier2.
       01 fichier2-I.
           02 IdPerso2-I      PIC 9(3).
           02 PrenomPerso2-I  PIC X(10).
           02 NomPerso2-I     PIC X(10).
           02 Age2-I          PIC 9(2).

       FD fichier1T.
       01 fichier1T-I.
           02 IdPersoT-I      PIC 9(3).
           02 PrenomPersoT-I  PIC X(10).
           02 NomPersoT-I     PIC X(10).
           02 AgeT-I          PIC 9(2).

       FD fichier2T.
       01 fichier2T-I.
           02 IdPerso2T-I      PIC 9(3).
           02 PrenomPerso2T-I  PIC X(10).
           02 NomPerso2T-I     PIC X(10).
           02 Age2T-I          PIC 9(2).

       FD fichierOut.
       01 fichierOut-O.
           02 IdPerso-O      PIC 9(3).
           02 PrenomPerso-O  PIC X(10).
           02 NomPerso-O     PIC X(10).
           02 Age-O          PIC 9(2).


       SD workfile.
       01 fichier-WF.
           02 IdPerso-WF       PIC 9(3).
           02 PrenomPerso-WF   PIC X(10).
           02 NomPerso-WF      PIC X(10).
           02 Age-WF           PIC 9(2).
      
       PROCEDURE DIVISION.
      *    Classement préalable des informations du fichier1 dans l'
      *    ordre croissant.
           SORT workfile ON ASCENDING KEY Age-WF
              USING fichier1
              GIVING fichier1T.

      *    Classement préalable des informations du fichier2 dans l'
      *    ordre croissant.
           SORT workfile ON ASCENDING KEY Age-WF
              USING fichier2
              GIVING fichier2T.

      *    Fusion des 2 fichiers dans le fichier de sortie
           MERGE workfile ON ASCENDING KEY Age-WF
              USING fichier1T fichier2T
              GIVING fichierOut.


           STOP RUN.

       END PROGRAM fusionFichier.
