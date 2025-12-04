       IDENTIFICATION DIVISION.
       PROGRAM-ID. calculVente.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT fichierVente ASSIGN TO "./config/vente-dpt2025.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD fichierVente.
       01 fichierVenteCourant.
           02 dept PIC X(2).
           02 montant PIC 9(7).

       WORKING-STORAGE SECTION.
       01 lecture PIC X(1).
           88 lectureOK VALUE "y".
           88 lectureKO VALUE "n".

       01 deptCourant PIC X(2) VALUE SPACES.
       01 somme PIC z(6)9(1) VALUE ZERO.
       01 ligne PIC 9(2) VALUE 1.

       PROCEDURE DIVISION.

      *    Ouverture du fichier
           OPEN INPUT fichierVente.

           SET lecture TO "y".
      *    Entrée dans la boucle de lecture
           PERFORM UNTIL lectureKO

      *       Lecture de la ligne courante.
              READ fichierVente INTO fichierVenteCourant
                 AT END
                    SET lecture TO "n"
     
                 NOT AT END
                    IF deptCourant NOT = dept

                       IF deptCourant NOT = SPACES
                          DISPLAY "Somme " deptCourant " : " somme
                       END-IF

                       MOVE dept TO deptCourant
                       MOVE 0 TO somme
                       COMPUTE somme = montant + FUNCTION NUMVAL(somme)
                    ELSE
                       COMPUTE somme = montant + FUNCTION NUMVAL(somme)
                    END-IF
     
                    COMPUTE ligne = ligne + 1

              END-READ              

           END-PERFORM.
      
      *    Fermeture du fichier     
           CLOSE fichierVente.



           STOP RUN.


     


       END PROGRAM calculVente.
