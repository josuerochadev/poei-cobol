       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGIO.
      *================================================================*
      * Programme : PROGIO - Echange de donnees via MAP (Chapitre VII) *
      * Transaction : TRIO                                             *
      * Description : Demonstration SEND MAP / RECEIVE MAP             *
      *               Mode conversationnel simple                      *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY MAPIO.

       01  VAR1  PIC 9(10).
       01  VAR2  PIC 9(10).
       01  VAR3  PIC 9(10).
       01  VAR4  PIC 9999V99.

       PROCEDURE DIVISION.

       0000-MAIN.
      *--- Etape 1 : Afficher l'ecran vide ---
           EXEC CICS SEND MAP('MAP1')
               MAPSET('MAPIO') MAPONLY FREEKB ERASE
           END-EXEC.

      *--- Etape 2 : Attendre saisie champ SIMPLE ---
           EXEC CICS RECEIVE MAP ('MAP1')
               MAPSET('MAPIO')
           END-EXEC.

      *--- Etape 3 : Calcul et affichage CHPOUT ---
           MOVE 1111111111 TO VAR1.
           MOVE 2222222222 TO VAR2.
           ADD VAR1 VAR2 GIVING VAR3.
           MOVE VAR3 TO CHPOUTO.

           EXEC CICS SEND MAP('MAP1')
               MAPSET('MAPIO') DATAONLY FREEKB
           END-EXEC.

      *--- Etape 4 : Attendre saisie champ CHPIO ---
           EXEC CICS RECEIVE MAP ('MAP1')
               MAPSET('MAPIO')
           END-EXEC.

      *--- Etape 5 : Calcul et affichage resultat CHPIO ---
           MOVE CHPIOI TO VAR4.
           ADD 5555.55 TO VAR4.
           MOVE VAR4 TO CHPIOO.

           EXEC CICS SEND MAP('MAP1')
               MAPSET('MAPIO') DATAONLY FREEKB
           END-EXEC.

      *--- Fin de transaction ---
           EXEC CICS RETURN
           END-EXEC.
