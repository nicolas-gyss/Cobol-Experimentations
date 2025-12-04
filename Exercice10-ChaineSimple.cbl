       IDENTIFICATION DIVISION.
       PROGRAM-ID. ChaineSimple.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 FIRST-NAME PIC X(20).
       01 LAST-NAME PIC X(20).
       01 FULL-NAME PIC X(40).
       01 l-firstname PIC 9(2).

       PROCEDURE DIVISION.

           DISPLAY "Votre prénom ?".
           ACCEPT FIRST-NAME.

           DISPLAY "Votre nom ?".
           ACCEPT LAST-NAME.

           INSPECT FIRST-NAME TALLYING l-firstname FOR CHARACTERS BEFORE
           SPACE.    

           STRING
      *       Appel de la fonction interne à Cobol TRIM qui permet de
      *       supprimer les espaces vides de la variable FIRST-NAME
      *       La procedure STRING va concaténer dans FULL-NAME la
      *       variable FIRST-NAME délimitée par sa taille, un espace
      *       et la variable LAST-NAME délimité par sa taille.     
                FUNCTION TRIM(FIRST-NAME) DELIMITED BY SIZE " "
                FUNCTION TRIM(LAST-NAME) DELIMITED BY SIZE 
                INTO FULL-NAME
           END-STRING.

           DISPLAY FULL-NAME.

           STOP RUN.

           