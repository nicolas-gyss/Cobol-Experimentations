      ****************************************************************
      * Programme qui permet de lire les informations des clients à
      * partir d'un fichier formaté et d'afficher l'ensemble des 
      * informations de la manière formatée suivante:
      *    ID : 01234
      *    PRENOM : HARRY
      *    NOM : POTTER
      *    DATE de NAISSANCE : 20/02/1990
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. lectureClient.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT fichierClient ASSIGN TO "./config/clients.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD fichierClient
       RECORD CONTAINS 34 CHARACTERS.
       01 client.
           02 id-client   PIC 9(5).
           02 nom         PIC X(12).
           02 prenom      PIC X(9).
           02 jour        PIC 9(2).
           02 mois        PIC 9(2).
           02 annee       PIC 9(4).

       WORKING-STORAGE SECTION.
       
       01 WS-lecture PIC X(1).
           88 KO VALUE "n".
       01 WS-cpt-enregistrement PIC 9(1) VALUE 1.


       PROCEDURE DIVISION.
           
           SET WS-lecture TO "y".

           OPEN INPUT fichierClient.

           PERFORM UNTIL KO

              READ fichierClient INTO client
                 AT END
                    SET WS-lecture TO "n"

                 NOT AT END              
                  DISPLAY "Client N°" WS-cpt-enregistrement
                  DISPLAY "ID : " id-client
                  DISPLAY "PRENOM : " prenom
                  DISPLAY "NOM : " nom
                  DISPLAY "DATE de NAISSANCE : " jour"/"mois"/"annee
                  DISPLAY "*****************************************"

              END-READ
              
             
              ADD 1 TO WS-cpt-enregistrement

           END-PERFORM.

           CLOSE fichierClient.
           STOP RUN.


       END PROGRAM lectureClient.
