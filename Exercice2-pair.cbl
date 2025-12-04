       IDENTIFICATION DIVISION.
           PROGRAM-ID. Pair.

       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       01 cpt PIC 9(2).
       01 affichage PIC x(30).
       01 controle PIC 9(2).
       01 a-cpt PIC Z(2).
       01 cptd PIC 9(2).

       PROCEDURE DIVISION.

       INITIALIZE cptd.

       PERFORM VARYING cpt FROM 1 BY 1 UNTIL cpt > 20

           COMPUTE controle = FUNCTION MOD (cpt 2)


           IF controle = 0 THEN
              
              MOVE cpt TO a-cpt
              DISPLAY a-cpt LINE 5 COL cptd
              
              COMPUTE cptd = cptd + 3

           END-IF

       END-PERFORM
       
       DISPLAY affichage.


       STOP RUN.
