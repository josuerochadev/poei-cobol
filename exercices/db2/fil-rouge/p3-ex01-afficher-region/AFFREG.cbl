       IDENTIFICATION DIVISION.
       PROGRAM-ID. AFFREG.
      *---------------------------------------------------------
      * P3 EXERCICE 1 : Afficher la region Marseille (02)
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * SQLCA pour gestion erreurs DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      * DCLGEN pour la table REGION
           EXEC SQL
               INCLUDE REGION
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-SELECT-REGION
           PERFORM 9000-FIN
           STOP RUN.

       1000-SELECT-REGION.
           EXEC SQL
               SELECT CODE_REGION, NOM_REGION
               INTO :CODE-REGION, :NOM-REGION
               FROM REGION
               WHERE CODE_REGION = '02'
           END-EXEC

           IF SQLCODE = 0
               DISPLAY '================================'
               DISPLAY 'REGION MARSEILLE'
               DISPLAY '================================'
               DISPLAY 'CODE   : ' CODE-REGION
               DISPLAY 'NOM    : ' NOM-REGION
               DISPLAY '================================'
           ELSE
               DISPLAY 'ERREUR SQL - SQLCODE : ' SQLCODE
           END-IF.

       9000-FIN.
           DISPLAY 'FIN DU PROGRAMME AFFREG'.
