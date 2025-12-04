       IDENTIFICATION DIVISION.
       PROGRAM-ID. LectureConfig.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * fichierConfig contient toutes les informations de configuration
      * Chaque ligne aura la structure <nom-info>=<valeur-info> 
       SELECT fichierConfig ASSIGN TO "./config/configOracle.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD fichierConfig.
       01 fichierConfigCurrentLine PIC X(50).
           
       WORKING-STORAGE SECTION.
       01 Lecture PIC X(1).
           88 LectureOK VALUE "o".
           88 LectureFin VALUE "n".

      * Nom du paramètre en cours de lecture
       01 ParamConfig PIC X(25) VALUE SPACES.
      * Valeur du paramètre en cours de lecture 
       01 ValeurConfig PIC X(15) VALUE SPACES.
      * Nombre de caractères du paramètre 
       01 ParamConfigL PIC 9(2) VALUE 0.
      * Nombre de caractères de la valeur 
       01 ValeurConfigL PIC 9(2) VALUE 0.

       01 cptParam PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
    
           SET Lecture TO 'o'.

      *    Ouverture du canal de lecture
           OPEN INPUT fichierConfig.

           PERFORM UNTIL LectureFin
              
              READ fichierConfig INTO fichierConfigCurrentLine
                 NOT AT END
                    DISPLAY "Ligne courante:" fichierConfigCurrentLine
      *    Analyse du document de configuration "configOracle.txt"
                    PERFORM analyseInstruction
      *    Configure les valeurs initiales de l'oracle
                    PERFORM initialisationOracle
                    ADD 1 TO cptParam
                    


                 AT END
                    DISPLAY "Fin de lecture"
                    SET Lecture TO 'n'
                    MOVE 0 TO cptParam
               END-READ

           END-PERFORM.     

           CLOSE fichierConfig.

           STOP RUN.

       analyseInstruction.
           UNSTRING fichierConfigCurrentLine
              DELIMITED BY "="
              INTO ParamConfig COUNT IN ParamConfigL
                   ValeurConfig COUNT IN ValeurConfigL
           END-UNSTRING.

           DISPLAY "Détail de la chaine de configuration "cptParam ":".
           DISPLAY "Parametre : " ParamConfig.
           DISPLAY "Valeur : " ValeurConfig.

       initialisationOracle.
           DISPLAY "**** Initialisation de l'Oracle *****".    


       END PROGRAM LectureConfig.

       
