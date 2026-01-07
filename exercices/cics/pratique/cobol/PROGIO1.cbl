       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGIO1.
      *================================================================*
      * Programme : PROGIO1 - Echange donnees avec 3 MAPs (Chap VII)   *
      * Transaction : TIO1                                             *
      * Description : MAPSET decomposee en MAP1, MAP2, MAP3            *
      *               Controle precis du curseur par MAP               *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY MAPIO1.

       01  VAR1  PIC 9(10)  VALUE  0.
       01  VAR2  PIC 9(10)  VALUE  0.
       01  VAR3  PIC 9(10)  VALUE  0.
       01  VAR4  PIC 9999V99  VALUE  0.

       PROCEDURE DIVISION.

       0000-MAIN.

      *--- Etape 1 : Afficher MAP1 (zone SIMPLE) ---
           EXEC CICS SEND MAP('MAP1')
               MAPSET('MAPIO1') MAPONLY FREEKB ERASE
           END-EXEC.

      *--- Etape 2 : Attendre saisie champ SIMPLE ---
           EXEC CICS RECEIVE MAP ('MAP1')
               MAPSET('MAPIO1')
           END-EXEC.

      *--- Etape 3 : Calcul et preparation CHPOUT ---
           MOVE 1111111111 TO VAR1.
           MOVE 2222222222 TO VAR2.
           ADD VAR1 VAR2 GIVING VAR3.
           MOVE VAR3 TO CHPOUTO.

      *--- Etape 4 : Afficher MAP2 (zone CHPOUT) ---
           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPIO1') MAPONLY FREEKB
           END-EXEC.

           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPIO1') DATAONLY FREEKB
           END-EXEC.

      *--- Etape 5 : Afficher MAP3 (zone CHPIO) ---
           EXEC CICS SEND MAP('MAP3')
               MAPSET('MAPIO1') MAPONLY FREEKB
           END-EXEC.

      *--- Etape 6 : Attendre saisie champ CHPIO ---
           EXEC CICS RECEIVE MAP('MAP3')
               MAPSET('MAPIO1')
           END-EXEC.

      *--- Etape 7 : Calcul et affichage resultat ---
           MOVE CHPIOI TO VAR4.
           ADD 5555.55 TO VAR4.
           MOVE VAR4 TO CHPIOO.

           EXEC CICS SEND MAP('MAP3')
               MAPSET('MAPIO1') DATAONLY FREEKB
           END-EXEC.

      *--- Fin de transaction ---
           EXEC CICS
               RETURN
           END-EXEC.

           STOP RUN.
