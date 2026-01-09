       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGNAME.
      *================================================================*
      * Programme : PROGNAME - Saisie et affichage nom                 *
      * Transaction : TRNM                                             *
      * Description : Saisir un nom et afficher un message de          *
      *               bienvenue (Exercice 2 - Couche Presentation)     *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- Copybook MAP (genere par assemblage BMS) ---
       COPY MAPNAME.

       PROCEDURE DIVISION.

      *================================================================*
       MAIN-PARA.
      *================================================================*

      *    1. Afficher l'ecran vide (premier envoi)
           EXEC CICS SEND MAP('MAP1')
                MAPSET('MAPNAME')
                MAPONLY
                FREEKB
                ERASE
           END-EXEC.

      *    2. Attendre la saisie utilisateur
           EXEC CICS RECEIVE MAP('MAP1')
                MAPSET('MAPNAME')
           END-EXEC.

      *    3. Construire le message de bienvenue
           STRING '*** HELLO '   DELIMITED SIZE
                  NAMEINI        DELIMITED SPACE
                  ' ***'         DELIMITED SIZE
                  INTO NAMEOUTO
           END-STRING.

      *    4. Renvoyer l'ecran avec le message
           EXEC CICS SEND MAP('MAP1')
                MAPSET('MAPNAME')
                DATAONLY
                FREEKB
           END-EXEC.

      *    5. Terminer la transaction
           EXEC CICS RETURN
           END-EXEC.
