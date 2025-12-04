       IDENTIFICATION DIVISION.
       PROGRAM-ID. Comptage.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 Phrase PIC X(50) VALUE
            "Bonjour à tous, bonne journée à tous.".
       01 char-to-count PIC X(1) VALUE "o".
       01 nbCar PIC 9(2) VALUE 0.
       01 t-nbCar PIC z9(1).

       PROCEDURE DIVISION.

           INSPECT Phrase TALLYING nbCar FOR ALL
           char-to-count.

      *    Je veux afficher mon nombre sans les 0, je dois le transfor-
      *    mer en chaine de caractères puis utiliser la fonction trim
      *    pour supprimer les espaces.

           MOVE nbCar TO t-nbCar.

           DISPLAY "Phrase : " Phrase.
           DISPLAY "Nombre de lettre 'o' : " FUNCTION TRIM(t-nbCar).


           STOP RUN.




       