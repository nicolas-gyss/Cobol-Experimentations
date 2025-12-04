       IDENTIFICATION DIVISION.
           PROGRAM-ID. Sommes.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       01 n PIC 9(2).
       01 resultat PIC 9(3).
       01 cpt PIC 9(2).
       01 a-resultat PIC ZZZ.

       PROCEDURE DIVISION.

       DISPLAY "Addition de nombre".

       DISPLAY "Saisissez un nombre :".
       ACCEPT n.

       INITIALIZE resultat.

       PERFORM VARYING cpt FROM 1 BY 1 UNTIL cpt > n
           
           COMPUTE resultat = resultat + cpt

       END-PERFORM
       
       MOVE resultat TO a-resultat.

       DISPLAY "La somme est " a-resultat.
       
       STOP RUN.
