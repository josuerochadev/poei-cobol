       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSCLI.
      *---------------------------------------------------------
      * P3 EXERCICE 2 : Inserer un nouveau client
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variables host pour DB2
       01 WS-NUM-COMPTE     PIC X(03).
       01 WS-CODE-REGION    PIC X(02).
       01 WS-CODE-NATCPT    PIC X(02).
       01 WS-NOM-CLIENT     PIC X(10).
       01 WS-PREN-CLIENT    PIC X(10).
       01 WS-DATE-NAIS      PIC X(10).
       01 WS-SEXE           PIC X(01).
       01 WS-CODE-PROF      PIC X(02).
       01 WS-SIT-FAM        PIC X(01).
       01 WS-ADRESSE        PIC X(20).
       01 WS-SOLDE          PIC S9(8)V99 COMP-3.
       01 WS-POS            PIC X(02).

      * SQLCA pour gestion erreurs DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INIT-DONNEES
           PERFORM 2000-INSERT-CLIENT
           PERFORM 9000-FIN
           STOP RUN.

       1000-INIT-DONNEES.
      * Exemple de nouveau client
           MOVE '021'               TO WS-NUM-COMPTE
           MOVE '01'                TO WS-CODE-REGION
           MOVE '25'                TO WS-CODE-NATCPT
           MOVE 'DUPONT'            TO WS-NOM-CLIENT
           MOVE 'MARC'              TO WS-PREN-CLIENT
           MOVE '1995-06-15'        TO WS-DATE-NAIS
           MOVE 'M'                 TO WS-SEXE
           MOVE '10'                TO WS-CODE-PROF
           MOVE 'C'                 TO WS-SIT-FAM
           MOVE '25 RUE NEUVE'      TO WS-ADRESSE
           MOVE 1800.00             TO WS-SOLDE
           MOVE 'CR'                TO WS-POS.

       2000-INSERT-CLIENT.
           EXEC SQL
               INSERT INTO CLIENT
               (NUM_COMPTE, CODE_REGION, CODE_NATCPT,
                NOM_CLIENT, PREN_CLIENT, DATE_NAIS,
                SEXE, CODE_PROF, SIT_FAM,
                ADRESSE, SOLDE, POS)
               VALUES
               (:WS-NUM-COMPTE, :WS-CODE-REGION, :WS-CODE-NATCPT,
                :WS-NOM-CLIENT, :WS-PREN-CLIENT, :WS-DATE-NAIS,
                :WS-SEXE, :WS-CODE-PROF, :WS-SIT-FAM,
                :WS-ADRESSE, :WS-SOLDE, :WS-POS)
           END-EXEC

           IF SQLCODE = 0
               DISPLAY 'CLIENT INSERE AVEC SUCCES'
               DISPLAY 'NUM COMPTE : ' WS-NUM-COMPTE
               DISPLAY 'NOM        : ' WS-NOM-CLIENT
               EXEC SQL COMMIT END-EXEC
           ELSE
               DISPLAY 'ERREUR INSERTION - SQLCODE : ' SQLCODE
               EXEC SQL ROLLBACK END-EXEC
           END-IF.

       9000-FIN.
           DISPLAY 'FIN DU PROGRAMME INSCLI'.
