       IDENTIFICATION DIVISION.

       PROGRAM-ID. EstPremier.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 monNombre PIC 9(2).      
       01 cpt PIC 9(2).
       01 resultat PIC 9(2).

       PROCEDURE DIVISION.
    
           DISPLAY "Nombre à tester :".
           ACCEPT monNombre.

       PERFORM VARYING cpt FROM 2 BY 1 UNTIL cpt >= monNombre
           
           COMPUTE resultat = FUNCTION MOD(monNombre cpt)

           IF resultat = 0 THEN
              
              DISPLAY "Votre nombre n'est pas premier"
              STOP RUN
           END-IF
           
       END-PERFORM

       IF cpt = monNombre OR monNombre = 1 THEN

           DISPLAY "Votre nombre est premier"
       
       END-IF


           STOP RUN.
