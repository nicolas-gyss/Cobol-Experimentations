       IDENTIFICATION DIVISION.
       PROGRAM-ID. Evaluation.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 statut PIC X.
           88 celib VALUE "C".
           88 marie VALUE "M".

       01 age PIC 9(2).



       PROCEDURE DIVISION.

           DISPLAY "Veuillez saisir votre age".
           ACCEPT age.
      
      *    Avec logique booleenne

           DISPLAY "Etes-vous marié ou célibataire ? (M/C)".
           ACCEPT statut.
      
           DISPLAY "Logique Booleenne".

           EVALUATE TRUE ALSO age
              WHEN celib ALSO < 30
                 DISPLAY "Célibataire et jeune"
              WHEN celib ALSO >= 30
                 DISPLAY "Célibataire et mature"
              WHEN marie ALSO < 30
                 DISPLAY "Marié et jeune"
              WHEN marie ALSO >= 30
                 DISPLAY "Marié et mature"
              WHEN OTHER
                 DISPLAY "Autre statut"
           END-EVALUATE.
      
      *    Avec logique textuel
           DISPLAY "Logique Textuelle".
           
           EVALUATE statut ALSO age
              WHEN 'C' ALSO < 30
                 DISPLAY "Célibataire et jeune"
              WHEN 'C' ALSO >= 30
                 DISPLAY "Célibataire et mature"
              WHEN 'M' ALSO < 30
                 DISPLAY "Marié et jeune"
              WHEN 'M' ALSO >= 30
                 DISPLAY "Marié et mature"
              WHEN OTHER
                 DISPLAY "Autre statut"
           END-EVALUATE.
      


           STOP RUN.

          
