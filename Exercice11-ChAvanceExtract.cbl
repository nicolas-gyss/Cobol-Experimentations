       IDENTIFICATION DIVISION.
       PROGRAM-ID. Extraction.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 RAW-DATA PIC X(60) VALUE "Nom:Durand;Prénom:Alice;Ville:Paris"
      - "".

       01 Nom PIC x(10).
       01 Prenom PIC x(10).
       01 Ville PIC x(10).
       01 Result PIC x(30).
       01 count-a PIC 9(2).
       01 dechet PIC x(10).
       01 count-as PIC z9(1) .

       PROCEDURE DIVISION.

           UNSTRING RAW-DATA DELIMITED BY ";" OR ":"
              INTO dechet
                   Nom
                   dechet
                   Prenom
                   dechet
                   Ville
           END-UNSTRING.

           DISPLAY "NAME = " Nom.
           DISPLAY "FIRST = " Prenom.
           DISPLAY "CITY = " Ville.

           STRING FUNCTION TRIM(Prenom) " " FUNCTION TRIM(Nom) 
           " habite " FUNCTION TRIM(Ville) DELIMITED BY SIZE
           INTO Result
           END-STRING.
           
           DISPLAY Result.

           INSPECT Result TALLYING count-a FOR ALL "a".
           MOVE count-a TO count-as.

           DISPLAY "Nombre de lettre 'a' présentes : "
           FUNCTION TRIM(count-as).              

           STOP RUN.
           