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

      * Variable pour saisie du solde
       01 WS-SOLDE-IN       PIC X(10).

      * SQLCA pour gestion erreurs DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-LIRE-DONNEES
           PERFORM 2000-INSERT-CLIENT
           PERFORM 9000-FIN
           STOP RUN.

       1000-LIRE-DONNEES.
      * Lecture des donnees depuis SYSIN (JCL In-Stream)
           ACCEPT WS-NUM-COMPTE
           ACCEPT WS-CODE-REGION
           ACCEPT WS-CODE-NATCPT
           ACCEPT WS-NOM-CLIENT
           ACCEPT WS-PREN-CLIENT
           ACCEPT WS-DATE-NAIS
           ACCEPT WS-SEXE
           ACCEPT WS-CODE-PROF
           ACCEPT WS-SIT-FAM
           ACCEPT WS-ADRESSE
           ACCEPT WS-SOLDE-IN
           ACCEPT WS-POS

      * Conversion du solde (texte -> numerique)
           COMPUTE WS-SOLDE = FUNCTION NUMVAL(WS-SOLDE-IN)

           DISPLAY 'DONNEES LUES DEPUIS SYSIN'
           DISPLAY 'NUM COMPTE : ' WS-NUM-COMPTE
           DISPLAY 'NOM        : ' WS-NOM-CLIENT.

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
