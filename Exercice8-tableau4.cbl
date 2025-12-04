       IDENTIFICATION DIVISION.
           PROGRAM-ID. RechercheVal.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       01 p-tab.
           02 prenom.
              03 FILLER PIC X(4) VALUE "ANNE".
              03 FILLER PIC X(4) VALUE "MARC".
              03 FILLER PIC X(4) VALUE "LUC ".
              03 FILLER PIC X(4) VALUE "SARA".
              03 FILLER PIC X(4) VALUE "JEAN".
              03 FILLER PIC X(4) VALUE "PAUL".
              03 FILLER PIC X(4) VALUE "EMMA".
              03 FILLER PIC X(4) VALUE "LEA ".
           02 tab-prenom REDEFINES prenom.
              03 tprenom PIC X(4) OCCURS 8 TIMES INDEXED BY iprenom.   
       
       01 cpt PIC 9(2).
       01 saisi PIC X(4).


       PROCEDURE DIVISION.
           
          

           DISPLAY "Affiche le tableau".
           
           PERFORM VARYING cpt FROM 1 BY 1 UNTIL cpt > 8
              
              DISPLAY tprenom(cpt)

           END-PERFORM.

           DISPLAY "Saisissez un prenom :".
           ACCEPT saisi.
           
           PERFORM UNTIL saisi = "FIN"
              
              SET iprenom TO 1
              
              SEARCH tprenom VARYING iprenom
                 AT END
                    DISPLAY "Prenom introuvable"
   
                 WHEN tprenom(iprenom) = saisi
   
                    DISPLAY "Prenom trouve"
                 
              END-SEARCH
              
              DISPLAY "Saisissez un prenom :"
              ACCEPT saisi
           
           END-PERFORM.

           STOP RUN.
