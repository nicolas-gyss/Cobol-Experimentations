       IDENTIFICATION DIVISION.
       PROGRAM-ID. BC801.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT F-NOMBRE ASSIGN TO "./out/nombre.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       ACCESS MODE IS SEQUENTIAL
       FILE STATUS FS-F-NOMBRE.
       
       DATA DIVISION.
       FILE SECTION.
       FD F-NOMBRE.
       01 F-NOMBRE-CURRENT PIC X(100).
       
       WORKING-STORAGE SECTION.
       01 FS-F-NOMBRE PIC X(2).
       01 WS-COMPTEUR PIC 9(2).
       01 WS-FONCTION PIC 9.
       01 WS-CHAINE-TEMP PIC X(5).
       01 WS-POINTER PIC 9(2).
       01 WS-CHAINE-CONSTR PIC X(100) VALUE SPACES.
       
       01 WS-CPT-CHAINE PIC 9(2) VALUE ZEROS.

       01 WS-ENTIERS PIC S9(2) VALUE -10.
       01 WS-ENTIERSA PIC +9(2).

       01 WS-DECIMAL PIC 9V9(2) VALUE 0.10.
       01 WS-DECIMALA PIC 9.9(2).

       01 WS-DATE.
           02 WS-ANNEE PIC X(4).
           02 WS-MOIS PIC X(2).
           02 WS-JOUR PIC X(2).
           02 WS-HEURE PIC X(2).
           02 WS-MN    PIC X(2).
           02 WS-S     PIC X(2).
       
       01 WS-ANNEESIMPLE PIC X(2).
       01 WS-DATE-ENTIER PIC 9(7).
       01 WS-JOUR-ENTIER PIC 9.
       01 WS-NOM-JOUR     PIC X(10).
       01 WS-NOM-MOIS     PIC X(10).
       
       LINKAGE SECTION.


       PROCEDURE DIVISION.
       
       0000-INITIALISATION-DEB.
           PERFORM 8900-MSG-LANCEMENT-DEB
           THRU 8910-MSG-LANCEMENT-FIN.

      *************************************************************
      *    ZONE DE PROGRAMME
      *************************************************************
      
      *    Ouverture de mon fichier nombre.txt
           OPEN OUTPUT F-NOMBRE.
           IF FS-F-NOMBRE = "00" THEN
              DISPLAY "Ouverture du fichier nombre.txt OK"
           ELSE
              DISPLAY "Erreur d'ouverture : " FS-F-NOMBRE
           END-IF.
      
           PERFORM VARYING WS-FONCTION FROM 1 BY 1
                   UNTIL WS-FONCTION > 6

              EVALUATE WS-FONCTION
                 WHEN 1 
                    PERFORM 6000-FONCTION1
                 WHEN 2
                    PERFORM 6010-FONCTION2
                 WHEN 3
                    PERFORM 6020-FONCTION3
                 WHEN 4
                    PERFORM 6030-FONCTION4
                 WHEN 5
                    PERFORM 6040-FONCTION5
                 WHEN 6
                    PERFORM 6050-FONCTION6
               END-EVALUATE

           END-PERFORM.

      *    Fermeture du fichier nombre.txt.
           CLOSE F-NOMBRE.
           IF FS-F-NOMBRE = "00" THEN
              DISPLAY "Fermeture du fichier nombre.txt OK"
           ELSE
              DISPLAY "Erreur de fermeture : " FS-F-NOMBRE
           END-IF.

           PERFORM 9998-FIN-NORMAL.

       0000-INITIALISATION-FIN.
           STOP RUN.               

      *************************************************************
      * FIN ZONE PROCEDURE
      *************************************************************

      *************************************************************
      * ZONE DE PARAGRAPHE DU LOGICIEL
      ***************************************************************

       8900-MSG-LANCEMENT-DEB.
           DISPLAY "********************************************".
           DISPLAY "* LANCEMENT PROGRAMME : BC801              *".
           DISPLAY "* Ecrire dans un fichier nombre.txt        *".
           DISPLAY "* Ligne 1 : 0,1,2,3,4,5,6,7,8,9,10         *".
           DISPLAY "* Ligne 2 : -10,-9,...,9,10                *".
           DISPLAY "* Ligne 3 : 0.10,0.20,...,0.90,1.00        *".
           DISPLAY "* Ligne 4 : 12/01/2026                     *".           
           DISPLAY "* Ligne 5 : Lundi 12 janvier 2026 HH:MM:SS *".
           DISPLAY "* Ligne 6 : 01-12-26                       *".
           DISPLAY "********************************************".
       
       8910-MSG-LANCEMENT-FIN.
           EXIT PROGRAM.

       9998-FIN-NORMAL.
           DISPLAY "***************************************".
           DISPLAY "* FIN PROGRAMME : BC801               *".
           DISPLAY "***************************************".
           EXIT PROGRAM.


       6000-FONCTION1.
           DISPLAY "Ecriture de la ligne 1"
           MOVE 1 TO WS-POINTER
      *    MOVE ALL "*" TO WS-CHAINE-CONSTR
           
      *    Création de ma ligne courante
           PERFORM VARYING WS-COMPTEUR FROM 0 BY 1
                    UNTIL WS-COMPTEUR > 10
              
      *        DISPLAY "Boucle " WS-COMPTEUR
      *        DISPLAY "WS-COMPTEUR :" FUNCTION TRIM(WS-COMPTEUR)
      *        DISPLAY "WS-CHAINE-CONSTR : '" 
      *        FUNCTION TRIM(WS-CHAINE-CONSTR)"'"
 
      *    1 Extraction valeur presente dans ma zone de construction
              UNSTRING WS-CHAINE-CONSTR
                 DELIMITED BY SPACE
                 INTO WS-CHAINE-TEMP
                 COUNT IN WS-CPT-CHAINE
              END-UNSTRING

      *        DISPLAY "WS-CHAINE-TEMP : '"WS-CHAINE-TEMP"'"
      *        DISPLAY "WS-CPT-CHAINE: " WS-CPT-CHAINE      

                 IF WS-COMPTEUR NOT = 10 THEN
      *          Tant que je n'ai pas atteint le chiffre 10, j'ajoute
      *          une virgule à la fin de ma chaine de construction.
                       STRING FUNCTION TRIM(WS-COMPTEUR) "," 
                           INTO WS-CHAINE-CONSTR
                          WITH POINTER WS-POINTER
                           ON OVERFLOW 
                              DISPLAY "Erreur taille"
                        END-STRING
      
                 ELSE
                        STRING FUNCTION TRIM(WS-COMPTEUR)
                           INTO WS-CHAINE-CONSTR
                          WITH POINTER WS-POINTER
                           ON OVERFLOW 
                              DISPLAY "Erreur taille"
                        END-STRING
      
                 END-IF
              
      *        DISPLAY "WS-CHAINE-CONSTR : '" WS-CHAINE-CONSTR "'"
      *        DISPLAY " "

           END-PERFORM

           DISPLAY "Affiche la ligne crée :"
           DISPLAY WS-CHAINE-CONSTR

      *    Ecriture de la ligne dans le fichier
           WRITE F-NOMBRE-CURRENT FROM WS-CHAINE-CONSTR

           EXIT PROGRAM.

       6010-FONCTION2.
           DISPLAY "Ecriture de la ligne 2"

      *    Initialisation des variables utiles     
           INITIALIZE WS-COMPTEUR
           INITIALIZE WS-CHAINE-CONSTR
           INITIALIZE WS-CHAINE-TEMP
           MOVE 1 TO WS-POINTER

      *    Construction de la ligne 2
           PERFORM WITH TEST AFTER VARYING WS-COMPTEUR FROM 1 BY 1
              UNTIL WS-COMPTEUR>20
              
              UNSTRING WS-CHAINE-CONSTR
                 DELIMITED BY SPACE
                 INTO WS-CHAINE-TEMP
                 COUNT IN WS-CPT-CHAINE
               END-UNSTRING

      *       Pour l'affichage correct des signes il faut transférer
      *       la valeur signée dans une variable d'affichage formatée         
               MOVE WS-ENTIERS TO WS-ENTIERSA

               IF WS-COMPTEUR <= 20 THEN
                    STRING WS-ENTIERSA ","
                       INTO WS-CHAINE-CONSTR
                       WITH POINTER WS-POINTER
                       ON OVERFLOW
                          DISPLAY "Erreur taille"
                    END-STRING

               ELSE
                    STRING WS-ENTIERSA
                       INTO WS-CHAINE-CONSTR
                       WITH POINTER WS-POINTER
                       ON OVERFLOW
                          DISPLAY "Erreur taille"
                    END-STRING
               END-IF
              

      *        DISPLAY "Entrier signé : " WS-ENTIERS
              ADD 1 TO WS-ENTIERS

           END-PERFORM

              DISPLAY "Affiche la ligne créée :"
              DISPLAY WS-CHAINE-CONSTR
              
              WRITE F-NOMBRE-CURRENT FROM WS-CHAINE-CONSTR
           

           EXIT PROGRAM.

       6020-FONCTION3.
              DISPLAY "Ecriture de la ligne 3"

      *       Initialisation des variables de fonctionnement
              INITIALIZE WS-COMPTEUR
              INITIALIZE WS-CHAINE-CONSTR
              INITIALIZE WS-CHAINE-TEMP
              MOVE 1 TO WS-POINTER

              PERFORM TEST AFTER VARYING WS-COMPTEUR FROM 1 BY 1
                 UNTIL WS-COMPTEUR> 9                
                 
                 UNSTRING WS-CHAINE-CONSTR
                    DELIMITED BY SPACE
                    INTO WS-CHAINE-TEMP
                    COUNT IN WS-CPT-CHAINE
                 END-UNSTRING

                 MOVE WS-DECIMAL TO WS-DECIMALA

                 IF WS-COMPTEUR <= 9
                    
                    STRING WS-DECIMALA ","
                       INTO WS-CHAINE-CONSTR
                       WITH POINTER WS-POINTER
                       ON OVERFLOW
                          DISPLAY "Erreur taille"
                    END-STRING

                 ELSE

                    STRING WS-DECIMALA
                       INTO WS-CHAINE-CONSTR
                       WITH POINTER WS-POINTER
                       ON OVERFLOW
                          DISPLAY "Erreur taille"
                    END-STRING

                 END-IF

      *           DISPLAY "Nombre : " WS-DECIMAL
                 ADD 0.1 TO WS-DECIMAL

              END-PERFORM

              DISPLAY "Affiche la ligne créée :"
              DISPLAY WS-CHAINE-CONSTR

              WRITE F-NOMBRE-CURRENT FROM WS-CHAINE-CONSTR
            

           EXIT PROGRAM.

       6030-FONCTION4.
           DISPLAY "Ecriture de la ligne 4"
           
           INITIALIZE WS-CHAINE-CONSTR

           MOVE FUNCTION CURRENT-DATE TO WS-DATE

      *     DISPLAY "WS-DATE :" WS-DATE
      *     DISPLAY "WS-ANNEE :" WS-ANNEE
      *     DISPLAY "WS-MOIS :" WS-MOIS
      *     DISPLAY "WS-JOUR :" WS-JOUR
      *     DISPLAY "WS-HEURE :" WS-HEURE
      *     DISPLAY "WS-MN :" WS-MN
      *     DISPLAY "WS-S :" WS-S

           STRING WS-JOUR "/" WS-MOIS "/" WS-ANNEE
              INTO WS-CHAINE-CONSTR
           END-STRING

              DISPLAY "Affiche la ligne créée :"
              DISPLAY WS-CHAINE-CONSTR

           WRITE F-NOMBRE-CURRENT FROM WS-CHAINE-CONSTR

           EXIT PROGRAM.    

       6040-FONCTION5.
           DISPLAY "Ecriture de la ligne 5"
     
                INITIALIZE WS-CHAINE-CONSTR

      *       1 - je tranforme la date du jour en entier grâce à la 
      *           fonction INTEGER-OF-DATE(<WS-YYYY> * 10000 + <WS-MM> 
      *           * 100 + <WS-DD>)

      *       2 - Grâce au résultat du mod(<date-en-entier> + 6, 7) + 1
      *          je peux déterminer le nom du jour: 1= lundi, 2 = mardi,
      *          .... 7 = dimanche

      *       3 - A partir d'une table d'équivalence je peux afficher
      *         la date comme je le souhaite.

                
           MOVE FUNCTION INTEGER-OF-DATE(FUNCTION NUMVAL(WS-ANNEE)
           * 10000 + FUNCTION NUMVAL(WS-MOIS) * 100 
           + FUNCTION NUMVAL(WS-JOUR)) TO WS-DATE-ENTIER
               
      *    DISPLAY "WS-DATE-ENTIER :" WS-DATE-ENTIER
           
           COMPUTE WS-JOUR-ENTIER = FUNCTION MOD(WS-DATE-ENTIER + 6,7)
            + 1

      *     DISPLAY "WS-JOUR-ENTIER: " WS-JOUR-ENTIER
           
           EVALUATE WS-JOUR-ENTIER
              WHEN 1
                 MOVE "Lundi" TO WS-NOM-JOUR
              WHEN 2
                 MOVE "Mardi" TO WS-NOM-JOUR
              WHEN 3
                 MOVE "Mercredi" TO WS-NOM-JOUR
              WHEN 4
                 MOVE "Jeudi" TO WS-NOM-JOUR
              WHEN 5
                 MOVE "Vendredi" TO WS-NOM-JOUR
              WHEN 6
                 MOVE "Samedi" TO WS-NOM-JOUR
              WHEN 7
                 MOVE "Dimanche" TO WS-NOM-JOUR
           END-EVALUATE

           EVALUATE WS-MOIS
              WHEN 01
                 MOVE "Janvier" TO WS-NOM-MOIS
              WHEN 02
                 MOVE "Fevrier" TO WS-NOM-MOIS
              WHEN 03
                 MOVE "Mars" TO WS-NOM-MOIS
              WHEN 04
                 MOVE "Avril" TO WS-NOM-MOIS
              WHEN 05
                 MOVE "Mai" TO WS-NOM-MOIS
              WHEN 06
                 MOVE "Juin" TO WS-NOM-MOIS
              WHEN 07
                 MOVE "Juillet" TO WS-NOM-MOIS
              WHEN 08
                 MOVE "Aout" TO WS-NOM-MOIS
              WHEN 09
                 MOVE "Septembre" TO WS-NOM-MOIS
              WHEN 10
                 MOVE "Octobre" TO WS-NOM-MOIS
              WHEN 11
                 MOVE "Novembre" TO WS-NOM-MOIS
              WHEN 12
                 MOVE "Decembre" TO WS-NOM-MOIS
           END-EVALUATE

           STRING FUNCTION TRIM(WS-NOM-JOUR) " " WS-JOUR " "
              FUNCTION TRIM(WS-NOM-MOIS) " " WS-ANNEE " " WS-HEURE ":"
              WS-MN ":" WS-S
              INTO WS-CHAINE-CONSTR
           END-STRING

           DISPLAY "Affiche la ligne créée :"
           DISPLAY WS-CHAINE-CONSTR

           WRITE F-NOMBRE-CURRENT FROM WS-CHAINE-CONSTR
               
           EXIT PROGRAM.        
                            

       6050-FONCTION6.
           DISPLAY "Ecriture de la ligne 6"
           
           INITIALIZE WS-CHAINE-CONSTR
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
      *     DISPLAY "WS-DATE :" WS-DATE
      *     DISPLAY "WS-ANNEE :" WS-ANNEE
      *     DISPLAY "WS-MOIS :" WS-MOIS
           
           MOVE WS-ANNEE(3:2) TO WS-ANNEESIMPLE

           STRING  WS-MOIS "-" WS-JOUR "-" WS-ANNEESIMPLE
              INTO WS-CHAINE-CONSTR
           END-STRING
              DISPLAY "Affiche la ligne créée :"
              DISPLAY WS-CHAINE-CONSTR

           WRITE F-NOMBRE-CURRENT FROM WS-CHAINE-CONSTR
           EXIT PROGRAM.        
           
       
       END PROGRAM BC801.
