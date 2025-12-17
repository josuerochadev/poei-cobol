       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAJCLI.
      *---------------------------------------------------------
      * P3 EXERCICE 4 : Mise a jour client (adresse, solde, pos)
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variables host pour DB2
       01 WS-NUM-COMPTE     PIC X(03).
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
           PERFORM 2000-UPDATE-CLIENT
           PERFORM 9000-FIN
           STOP RUN.

       1000-INIT-DONNEES.
      * Exemple de mise a jour du client 005
           MOVE '005'               TO WS-NUM-COMPTE
           MOVE '20 AVENUE FOCH'    TO WS-ADRESSE
           MOVE 2800.00             TO WS-SOLDE
           MOVE 'CR'                TO WS-POS.

       2000-UPDATE-CLIENT.
           EXEC SQL
               UPDATE CLIENT
               SET ADRESSE = :WS-ADRESSE,
                   SOLDE = :WS-SOLDE,
                   POS = :WS-POS
               WHERE NUM_COMPTE = :WS-NUM-COMPTE
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   DISPLAY 'CLIENT MIS A JOUR AVEC SUCCES'
                   DISPLAY 'NUM COMPTE : ' WS-NUM-COMPTE
                   DISPLAY 'NV ADRESSE : ' WS-ADRESSE
                   DISPLAY 'NV SOLDE   : ' WS-SOLDE
                   DISPLAY 'NV POS     : ' WS-POS
                   EXEC SQL COMMIT END-EXEC
               WHEN 100
                   DISPLAY 'CLIENT NON TROUVE : ' WS-NUM-COMPTE
               WHEN OTHER
                   DISPLAY 'ERREUR UPDATE - SQLCODE : ' SQLCODE
                   EXEC SQL ROLLBACK END-EXEC
           END-EVALUATE.

       9000-FIN.
           DISPLAY 'FIN DU PROGRAMME MAJCLI'.
