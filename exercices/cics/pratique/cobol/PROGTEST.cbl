       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGTEST.
      *================================================================*
      * Programme : PROGTEST - Affichage MAP MAPTEST                   *
      * Transaction : TR01                                             *
      * Description : Programme simple affichant l'ecran de bienvenue  *
      *               (Couche Presentation - Chapitre IX)              *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- Copybook MAP (genere par assemblage BMS) ---
       COPY MAPTEST.

       PROCEDURE DIVISION.

      *================================================================*
       MAIN-PARA.
      *================================================================*
      *    Afficher la MAP MAPTEST
           EXEC CICS SEND MAP('MAP1')
                     MAPSET('MAPTEST')
                     MAPONLY
                     ERASE
           END-EXEC.

      *    Retour CICS
           EXEC CICS RETURN
           END-EXEC.
