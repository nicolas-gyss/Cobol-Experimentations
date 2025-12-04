       IDENTIFICATION DIVISION.
       PROGRAM-ID. LectureFichier.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    On associe un nom interne <FICHIER-TEST> au fichier ciblé
      *    Le fichier peut être donné par une variable ou directement
      *    avec le nom système situé dans le serveur.
           SELECT FICHIER-TEST ASSIGN TO FICHIER-PHYSIQUE
      *    On détermine la manière dont est lu le fichier.
      *    LINE SEQUENTIAL indique ligne / ligne
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
              
       DATA DIVISION.
      *    FILE SECTION permet de définir comment chaque ligne lue sera
      *    formatée lors de la lecture.
      *    Ici la ligne-courante sera de forme "00XXXXXXXXXXAAAAAAAAAA" 
       FILE SECTION.
       FD FICHIER-TEST.
       01 LIGNECOURANTE-TEST.
           02 DEPT PIC 9(2).
           02 NOM PIC X(10).
           02 TEL PIC X(10).
       
       WORKING-STORAGE SECTION.
       01 FICHIER-PHYSIQUE PIC X(25) VALUE "./config/test-lecture.txt".
       01 FIN-DE-FICHIER PIC x.
           88 OUI VALUE "o".
           88 NON VALUE "n".

       PROCEDURE DIVISION.

           DISPLAY "Lecture du fichier 'test-lecture.txt'".
      *    On ouvre le canal pour la lecture du fichier
           OPEN INPUT FICHIER-TEST.
           
           SET FIN-DE-FICHIER TO "n".

      *    La gestion de la fin de la lecture se fait de manière 
      *    manuelle. C'est uniquement lorsque l'instruction AT END
      *    est valide que nous pourrons sortir de la boucle.

           PERFORM UNTIL OUI   
      *    L'instruction READ permet de d'extraire 1 ligne définie par
      *    la commande ORGANIZATION de la FILE-CONTROL provenant du 
      *    fichier FICHIER-TEST et de la copier dans la variable
      *    LIGNECOURANTE-TEST
                 READ FICHIER-TEST INTO LIGNECOURANTE-TEST
                    AT END
                       SET FIN-DE-FICHIER TO "o"
                       
                    NOT AT END
      *                Les variables de la structure de données courante
      *                sont disponibles pour le traitement.              
                       DISPLAY "Ligne courante : " LIGNECOURANTE-TEST
                       DISPLAY "Departement : " DEPT 
                       DISPLAY "Nom : " NOM
                       DISPLAY "Téléphone : " TEL
                 END-READ
           END-PERFORM.
      
      *    On ferme le canal vers le fichier.
           CLOSE FICHIER-TEST.

           STOP RUN.


       END PROGRAM LectureFichier.
